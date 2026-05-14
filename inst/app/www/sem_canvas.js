
/* ─── Dr.AIStat SEM Canvas Engine ─────────────────────────────────────────── */
/*
 * INITIALIZATION STRATEGY
 * ────────────────────────
 * The canvas HTML lives inside output$canvas_ui (renderUI), so #sem-svg does
 * not exist when this script first loads.  Three complementary triggers fire
 * initSemCanvas() once the elements are in the DOM:
 *
 *   1. MutationObserver  – watches document for #sem-svg being added
 *   2. shiny:value event – fires when Shiny pushes canvas_ui content
 *   3. Inline <script>   – appended at the end of the renderUI HTML string
 *      (belt-and-suspenders; may not execute in all jQuery versions)
 *
 * A _semInitialized flag prevents double-wiring.
 */

/* ── Canvas initializer ──────────────────────────────────────────────────── */
window.initSemCanvas = function () {
  var svg = document.getElementById('sem-svg');
  if (!svg) return;                        /* DOM not ready – caller retries  */
  if (window._semInitialized) return;      /* already wired – nothing to do   */
  window._semInitialized = true;

  'use strict';
  var SVG_NS = 'http://www.w3.org/2000/svg';
  var NODE_W = 175, NODE_H_BASE = 72, IND_LINE_H = 16, IND_PAD = 38;

  var model    = { nodes: {}, edges: [] };
  var nextId   = 1;
  var mode     = 'select';
  var selId    = null;
  var arrowSrc = null;
  var drag     = null;

  var eLayer = document.getElementById('sem-edge-layer');
  var nLayer = document.getElementById('sem-node-layer');
  var tip    = document.getElementById('sem-tip');

  /* ── SVG defs ──────────────────────────────────────────────────────────── */
  var defs = document.createElementNS(SVG_NS, 'defs');

  var filt = document.createElementNS(SVG_NS, 'filter');
  filt.setAttribute('id','card-shadow');
  filt.setAttribute('x','-10%'); filt.setAttribute('y','-10%');
  filt.setAttribute('width','120%'); filt.setAttribute('height','120%');
  var fe = document.createElementNS(SVG_NS,'feDropShadow');
  fe.setAttribute('dx','0'); fe.setAttribute('dy','2');
  fe.setAttribute('stdDeviation','3');
  fe.setAttribute('flood-color','rgba(0,0,0,.18)');
  filt.appendChild(fe); defs.appendChild(filt);

  function mkMarker(id, col) {
    var m = document.createElementNS(SVG_NS,'marker');
    m.setAttribute('id', id);
    m.setAttribute('viewBox','0 0 10 10');
    m.setAttribute('refX','8'); m.setAttribute('refY','5');
    m.setAttribute('markerWidth','7'); m.setAttribute('markerHeight','7');
    m.setAttribute('orient','auto-start-reverse');
    var p = document.createElementNS(SVG_NS,'path');
    p.setAttribute('d','M0,0 L0,10 L10,5 z');
    p.setAttribute('fill', col);
    m.appendChild(p); defs.appendChild(m);
  }
  mkMarker('arr-direct',    '#1565C0');
  mkMarker('arr-mediation', '#1565C0');  /* same as direct — topology detects mediators */
  mkMarker('arr-moderation','#6A1B9A');
  mkMarker('arr-covariance','#555');
  svg.insertBefore(defs, svg.firstChild);

  var EDGE_STYLE = {
    direct:     { color:'#1565C0', dash:'none', marker:'arr-direct'     },
    mediation:  { color:'#1565C0', dash:'none', marker:'arr-direct'     }, /* treated as structural */
    moderation: { color:'#6A1B9A', dash:'4,4',  marker:'arr-moderation' },
    covariance: { color:'#555',    dash:'8,4',  marker:'arr-covariance' }
  };

  /* ── Mediator detection (for node colouring) ──────────────────────── */
  function isMediatorNode(nid) {
    var hasIn  = model.edges.some(function(e) {
      return e.to === nid && (e.type==='direct'||e.type==='mediation');
    });
    var hasOut = model.edges.some(function(e) {
      return e.from === nid && (e.type==='direct'||e.type==='mediation');
    });
    return hasIn && hasOut;
  }

  /* ── Geometry helpers ──────────────────────────────────────────────────── */
  function nodeH(n) { return IND_PAD + Math.max(0, n.indicators.length) * IND_LINE_H + 8; }

  function borderPt(n, angle) {
    var cx = n.x + NODE_W/2, cy = n.y + nodeH(n)/2;
    var hw = NODE_W/2, hh = nodeH(n)/2;
    var cos = Math.cos(angle), sin = Math.sin(angle);
    var t = (Math.abs(cos)*hh > Math.abs(sin)*hw) ? hw/Math.abs(cos) : hh/Math.abs(sin);
    return { x: cx + t*cos - Math.sign(cos)*2, y: cy + t*sin - Math.sign(sin)*2 };
  }

  function edgePts(e) {
    var fn = model.nodes[e.from], tn = model.nodes[e.to];
    if (!fn || !tn) return null;
    var fcx = fn.x + NODE_W/2, fcy = fn.y + nodeH(fn)/2;
    var tcx = tn.x + NODE_W/2, tcy = tn.y + nodeH(tn)/2;
    var angle = Math.atan2(tcy - fcy, tcx - fcx);
    return { s: borderPt(fn, angle), e: borderPt(tn, angle + Math.PI) };
  }

  /* ── SVG point converter ───────────────────────────────────────────────── */
  function svgPt(evt) {
    var pt = svg.createSVGPoint();
    pt.x = evt.clientX; pt.y = evt.clientY;
    return pt.matrixTransform(svg.getScreenCTM().inverse());
  }

  /* ── Render ────────────────────────────────────────────────────────────── */
  function render() {
    eLayer.innerHTML = '';
    nLayer.innerHTML = '';
    Object.values(model.edges).forEach(drawEdge);
    Object.values(model.nodes).forEach(drawNode);
  }

  /* ── Bezier midpoint helper ────────────────────────────────────────────── */
  /* Returns the point at t=0.5 on the quadratic Bezier P0-CP-P2             */
  function bezierMid(p0, cp, p2) {
    return {
      x: 0.25*p0.x + 0.5*cp.x + 0.25*p2.x,
      y: 0.25*p0.y + 0.5*cp.y + 0.25*p2.y
    };
  }

  function drawEdge(e) {
    var st  = EDGE_STYLE[e.type] || EDGE_STYLE.direct;
    var sel = e.id === selId;

    /* ── Moderation: arrow lands on the midpoint of the IV→DV path ──────── */
    /* Find any direct/mediation path that goes to the same DV node          */
    var startPt, endPt;
    if (e.type === 'moderation') {
      var mainEdge = null;
      for (var mi = 0; mi < model.edges.length; mi++) {
        var de = model.edges[mi];
        if (de.id !== e.id && de.to === e.to &&
            (de.type === 'direct' || de.type === 'mediation')) {
          mainEdge = de; break;
        }
      }
      if (mainEdge) {
        var mPts = edgePts(mainEdge);
        if (mPts) {
          /* Compute the curve midpoint of the main (IV→DV) Bezier */
          var dx_m = mPts.e.x - mPts.s.x, dy_m = mPts.e.y - mPts.s.y;
          var perp_m = Math.atan2(-dx_m, dy_m);
          var bend_m = Math.min(60, Math.sqrt(dx_m*dx_m+dy_m*dy_m)*0.18);
          var cp_m   = { x: (mPts.s.x+mPts.e.x)/2 + Math.cos(perp_m)*bend_m,
                         y: (mPts.s.y+mPts.e.y)/2 + Math.sin(perp_m)*bend_m };
          endPt = bezierMid(mPts.s, cp_m, mPts.e);

          /* Startpoint: moderator node border toward endPt */
          var modNode = model.nodes[e.from];
          if (modNode) {
            var ang_mod = Math.atan2(
              endPt.y - (modNode.y + nodeH(modNode)/2),
              endPt.x - (modNode.x + NODE_W/2));
            startPt = borderPt(modNode, ang_mod);
          }
        }
      }
    }

    /* ── Normal edges (or moderation fallback when no main path found) ───── */
    if (!startPt || !endPt) {
      var pts = edgePts(e);
      if (!pts) return;
      startPt = pts.s; endPt = pts.e;
    }

    var dx   = endPt.x - startPt.x, dy = endPt.y - startPt.y;
    var perp = Math.atan2(-dx, dy);
    var bend = Math.min(60, Math.sqrt(dx*dx+dy*dy)*0.18);
    /* Moderation uses a straight line (no curve) so the arrow is clearly
       perpendicular to the main path — this matches standard SEM diagrams */
    var cpx  = (e.type === 'moderation')
               ? (startPt.x + endPt.x)/2
               : (startPt.x+endPt.x)/2 + Math.cos(perp)*bend;
    var cpy  = (e.type === 'moderation')
               ? (startPt.y + endPt.y)/2
               : (startPt.y+endPt.y)/2 + Math.sin(perp)*bend;

    var path = document.createElementNS(SVG_NS,'path');
    path.setAttribute('d', 'M'+startPt.x+','+startPt.y+
                           ' Q'+cpx+','+cpy+
                           ' '+endPt.x+','+endPt.y);
    path.setAttribute('stroke', sel ? '#F57F17' : st.color);
    path.setAttribute('stroke-width', sel ? '3' : '2');
    path.setAttribute('fill','none');
    path.setAttribute('stroke-dasharray', st.dash);
    path.setAttribute('marker-end', 'url(#'+st.marker+')');
    path.style.cursor = 'pointer';
    path.addEventListener('mousedown', (function(eid) {
      return function(ev) {
        ev.stopPropagation();
        var edge = model.edges.find(function(x){ return x.id===eid; });
        if (!edge) return;
        if (mode === 'delete') { removeEdge(eid); return; }
        selId = eid; arrowSrc = null;
        render(); showEdgePanel(edge);
      };
    })(e.id));
    eLayer.appendChild(path);

    /* Label — positioned at mid-curve */
    var lx = cpx + Math.cos(perp)*12;
    var ly = cpy + Math.sin(perp)*12;
    var lt = document.createElementNS(SVG_NS,'text');
    lt.setAttribute('x', lx); lt.setAttribute('y', ly);
    lt.setAttribute('text-anchor','middle'); lt.setAttribute('font-size','10');
    lt.setAttribute('fill', st.color); lt.setAttribute('font-weight','bold');
    lt.setAttribute('pointer-events','none');
    lt.textContent = e.type.charAt(0).toUpperCase()+e.type.slice(1);
    eLayer.appendChild(lt);
  }

  function drawNode(n) {
    var h   = nodeH(n);
    var sel = n.id === selId;
    var src = n.id === arrowSrc;
    var g   = document.createElementNS(SVG_NS,'g');
    g.setAttribute('data-id', n.id);

    var shadow = document.createElementNS(SVG_NS,'rect');
    shadow.setAttribute('x', n.x+3); shadow.setAttribute('y', n.y+3);
    shadow.setAttribute('width', NODE_W); shadow.setAttribute('height', h);
    shadow.setAttribute('rx','10'); shadow.setAttribute('fill','rgba(0,0,0,.10)');
    shadow.setAttribute('pointer-events','none');
    g.appendChild(shadow);

    var isMed = isMediatorNode(n.id);
    /* header colour: blue=normal, green=mediator, orange=arrow-source, dark=selected */
    var hdrColor = sel ? '#1565C0' : src ? '#E65100' : isMed ? '#2E7D32' : '#2196A6';
    var brdColor = sel ? '#1565C0' : src ? '#F57F17' : isMed ? '#2E7D32' : '#2196A6';

    var body = document.createElementNS(SVG_NS,'rect');
    body.setAttribute('x', n.x); body.setAttribute('y', n.y);
    body.setAttribute('width', NODE_W); body.setAttribute('height', h);
    body.setAttribute('rx','10');
    body.setAttribute('fill', sel ? '#EFF6FF' : src ? '#FFF8E1' : isMed ? '#F1F8E9' : 'white');
    body.setAttribute('stroke', brdColor);
    body.setAttribute('stroke-width', sel||src ? '3' : isMed ? '2.5' : '1.5');
    g.appendChild(body);

    var hdr = document.createElementNS(SVG_NS,'rect');
    hdr.setAttribute('x', n.x); hdr.setAttribute('y', n.y);
    hdr.setAttribute('width', NODE_W); hdr.setAttribute('height','30');
    hdr.setAttribute('rx','10');
    hdr.setAttribute('fill', hdrColor);
    hdr.setAttribute('pointer-events','none');
    g.appendChild(hdr);
    var hfix = document.createElementNS(SVG_NS,'rect');
    hfix.setAttribute('x', n.x); hfix.setAttribute('y', n.y+18);
    hfix.setAttribute('width', NODE_W); hfix.setAttribute('height','12');
    hfix.setAttribute('fill', hdrColor);
    hfix.setAttribute('pointer-events','none');
    g.appendChild(hfix);

    var title = document.createElementNS(SVG_NS,'text');
    title.setAttribute('x', n.x + NODE_W/2); title.setAttribute('y', n.y+19);
    title.setAttribute('text-anchor','middle');
    title.setAttribute('dominant-baseline','middle');
    title.setAttribute('font-size','12'); title.setAttribute('font-weight','bold');
    title.setAttribute('fill','white'); title.setAttribute('pointer-events','none');
    title.textContent = n.label;
    g.appendChild(title);

    var badge = document.createElementNS(SVG_NS,'text');
    badge.setAttribute('x', n.x+NODE_W-8); badge.setAttribute('y', n.y+19);
    badge.setAttribute('text-anchor','end');
    badge.setAttribute('dominant-baseline','middle');
    badge.setAttribute('font-size','8');
    badge.setAttribute('fill','rgba(255,255,255,.75)');
    badge.setAttribute('pointer-events','none');
    badge.textContent = isMed ? '[M]' : (n.type === 'formative' ? '[F]' : '[R]');
    g.appendChild(badge);

    var divLine = document.createElementNS(SVG_NS,'line');
    divLine.setAttribute('x1', n.x+8); divLine.setAttribute('y1', n.y+32);
    divLine.setAttribute('x2', n.x+NODE_W-8); divLine.setAttribute('y2', n.y+32);
    divLine.setAttribute('stroke','#d0e8f0'); divLine.setAttribute('stroke-width','1');
    divLine.setAttribute('pointer-events','none');
    g.appendChild(divLine);

    n.indicators.forEach(function(ind, i) {
      var t = document.createElementNS(SVG_NS,'text');
      t.setAttribute('x', n.x+12); t.setAttribute('y', n.y+IND_PAD + i*IND_LINE_H);
      t.setAttribute('font-size','10'); t.setAttribute('fill','#1A3A5C');
      t.setAttribute('pointer-events','none');
      t.textContent = '▸ ' + ind;
      g.appendChild(t);
    });
    if (n.indicators.length === 0) {
      var hint = document.createElementNS(SVG_NS,'text');
      hint.setAttribute('x', n.x+NODE_W/2); hint.setAttribute('y', n.y+52);
      hint.setAttribute('text-anchor','middle'); hint.setAttribute('font-size','9');
      hint.setAttribute('fill','#999'); hint.setAttribute('pointer-events','none');
      hint.textContent = 'Click to assign items →';
      g.appendChild(hint);
    }

    var hit = document.createElementNS(SVG_NS,'rect');
    hit.setAttribute('x', n.x); hit.setAttribute('y', n.y);
    hit.setAttribute('width', NODE_W); hit.setAttribute('height', h);
    hit.setAttribute('rx','10'); hit.setAttribute('fill','transparent');
    hit.style.cursor = mode==='arrow' ? 'crosshair' : mode==='delete' ? 'not-allowed' : 'grab';

    hit.addEventListener('mousedown', (function(nid) {
      return function(ev) {
        ev.stopPropagation();
        var nd = model.nodes[nid]; if (!nd) return;
        if (mode === 'delete') { removeNode(nid); return; }
        if (mode === 'arrow') {
          if (!arrowSrc) {
            arrowSrc = nid; selId = null; render();
            showTip('Now click the TARGET construct to draw a path.');
          } else if (arrowSrc !== nid) {
            var existing = model.edges.find(function(e){
              return (e.from===arrowSrc&&e.to===nid)||(e.from===nid&&e.to===arrowSrc);
            });
            if (!existing) {
              var eid = 'e' + nextId++;
              model.edges.push({ id:eid, from:arrowSrc, to:nid, type:'direct' });
              selId = eid;
              showEdgePanel(model.edges.find(function(e){ return e.id===eid; }));
            } else { showTip('Path already exists!'); }
            arrowSrc = null; render(); syncR();
          }
          return;
        }
        selId = nid; arrowSrc = null;
        drag = { id:nid, ox:nd.x, oy:nd.y, sx:ev.clientX, sy:ev.clientY };
        render(); showNodePanel(model.nodes[nid]); syncR();
      };
    })(n.id));

    hit.addEventListener('dblclick', function(ev) {
      ev.stopPropagation();
      var lbl = document.getElementById('sem-node-label');
      if (lbl) { lbl.focus(); lbl.select(); }
    });

    g.appendChild(hit);
    nLayer.appendChild(g);
  }

  /* ── Mouse events ──────────────────────────────────────────────────────── */
  svg.addEventListener('mousedown', function(ev) {
    if (ev.target===svg || ev.target===eLayer || ev.target===nLayer) {
      if (mode==='addNode') {
        var pt = svgPt(ev);
        addNode(pt.x - NODE_W/2, pt.y - NODE_H_BASE/2);
        setMode('select');
      } else {
        selId=null; arrowSrc=null; render(); hidePanel(); hideTip();
      }
    }
  });
  document.addEventListener('mousemove', function(ev) {
    if (!drag) return;
    var n = model.nodes[drag.id]; if (!n) return;
    n.x = Math.max(0, drag.ox + ev.clientX - drag.sx);
    n.y = Math.max(0, drag.oy + ev.clientY - drag.sy);
    render();
  });
  document.addEventListener('mouseup', function() {
    if (drag) { drag = null; syncR(); }
  });

  /* ── Node & edge management ────────────────────────────────────────────── */
  function addNode(x, y) {
    var id    = 'n' + nextId++;
    var count = Object.keys(model.nodes).length;
    model.nodes[id] = { id:id, label:'LV'+(count+1), type:'latent',
                        x:x, y:y, indicators:[] };
    selId = id; arrowSrc = null;
    render(); showNodePanel(model.nodes[id]); syncR();
    showTip('Construct added! Click it to assign indicators in the right panel.');
  }
  function removeNode(id) {
    delete model.nodes[id];
    model.edges = model.edges.filter(function(e){ return e.from!==id && e.to!==id; });
    selId=null; render(); hidePanel(); syncR();
  }
  function removeEdge(id) {
    model.edges = model.edges.filter(function(e){ return e.id!==id; });
    selId=null; render(); hidePanel(); syncR();
  }

  /* ── Panel helpers ─────────────────────────────────────────────────────── */
  function showNodePanel(n) {
    document.getElementById('sem-panel-node').style.display = 'block';
    document.getElementById('sem-panel-edge').style.display = 'none';
    document.getElementById('sem-panel-none').style.display = 'none';
    document.getElementById('sem-node-label').value = n.label;
    document.getElementById('sem-node-type').value  = n.type;
    document.getElementById('sem-node-id').value    = n.id;
    buildIndCheckboxes(n);
    if (typeof Shiny !== 'undefined')
      Shiny.setInputValue('mod10-sel_node', n.id, {priority:'event'});
  }
  function showEdgePanel(e) {
    document.getElementById('sem-panel-node').style.display = 'none';
    document.getElementById('sem-panel-edge').style.display = 'block';
    document.getElementById('sem-panel-none').style.display = 'none';
    document.getElementById('sem-edge-id').value   = e.id;
    document.getElementById('sem-edge-type').value = e.type;
    var fn = model.nodes[e.from], tn = model.nodes[e.to];
    document.getElementById('sem-edge-info').textContent =
      (fn?fn.label:e.from) + '  →  ' + (tn?tn.label:e.to);
  }
  function hidePanel() {
    document.getElementById('sem-panel-node').style.display = 'none';
    document.getElementById('sem-panel-edge').style.display = 'none';
    document.getElementById('sem-panel-none').style.display = 'block';
  }
  function showTip(msg) {
    tip.textContent = msg; tip.style.display = 'block';
    setTimeout(function(){ tip.style.display='none'; }, 4000);
  }
  function hideTip() { tip.style.display = 'none'; }

  /* ── Indicator checkboxes ─────────────────────────────────────────────── */
  function buildIndCheckboxes(n) {
    var cont = document.getElementById('sem-ind-container');
    var cols = window._semCols || [];
    cont.innerHTML = cols.length === 0
      ? '<p style="color:#999;font-size:11px;padding:4px;">Upload data first to see columns</p>'
      : '';
    cols.forEach(function(col) {
      var wrap = document.createElement('label');
      wrap.style.cssText = 'display:flex;align-items:center;gap:5px;margin:2px 0;font-size:11px;cursor:pointer;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:100%;';
      var cb = document.createElement('input');
      cb.type='checkbox'; cb.value=col;
      cb.checked = n.indicators.indexOf(col) !== -1;
      cb.addEventListener('change', (function(c) {
        return function() {
          var nid = document.getElementById('sem-node-id').value;
          var nd  = model.nodes[nid]; if (!nd) return;
          if (cb.checked) { if (nd.indicators.indexOf(c)<0) nd.indicators.push(c); }
          else { nd.indicators = nd.indicators.filter(function(i){ return i!==c; }); }
          render(); syncR();
        };
      })(col));
      wrap.appendChild(cb);
      var span = document.createElement('span');
      span.textContent=col; span.title=col;
      wrap.appendChild(span);
      cont.appendChild(wrap);
    });
  }

  /* ── Toolbar ───────────────────────────────────────────────────────────── */
  function setMode(m) {
    mode = m;
    if (m !== 'arrow') arrowSrc = null;
    document.querySelectorAll('.sem-tb-btn').forEach(function(b) {
      b.classList.toggle('sem-tb-active', b.dataset.m === m);
    });
    svg.style.cursor = (m==='addNode'||m==='arrow') ? 'crosshair' : 'default';
    render();
    if (m==='addNode')  showTip('Click on the canvas to place a construct.');
    else if (m==='arrow')  showTip('Click SOURCE construct, then TARGET construct.');
    else if (m==='delete') showTip('Click any construct or path to delete it.');
    else hideTip();
  }

  document.querySelectorAll('.sem-tb-btn').forEach(function(b) {
    b.addEventListener('click', function(){ setMode(b.dataset.m); });
  });

  var lblEl = document.getElementById('sem-node-label');
  if (lblEl) {
    lblEl.addEventListener('input', function() {
      var id = document.getElementById('sem-node-id').value;
      if (model.nodes[id]) { model.nodes[id].label = lblEl.value; render(); }
    });
    lblEl.addEventListener('blur', syncR);
    lblEl.addEventListener('keydown', function(e){ if(e.key==='Enter') lblEl.blur(); });
  }
  var typeEl = document.getElementById('sem-node-type');
  if (typeEl) {
    typeEl.addEventListener('change', function() {
      var id = document.getElementById('sem-node-id').value;
      if (model.nodes[id]) { model.nodes[id].type = typeEl.value; render(); syncR(); }
    });
  }
  var etypeEl = document.getElementById('sem-edge-type');
  if (etypeEl) {
    etypeEl.addEventListener('change', function() {
      var eid  = document.getElementById('sem-edge-id').value;
      var edge = model.edges.find(function(e){ return e.id===eid; });
      if (edge) { edge.type = etypeEl.value; render(); syncR(); }
    });
  }

  var delBtn = document.getElementById('sem-delete-sel');
  if (delBtn) delBtn.addEventListener('click', function() {
    if (!selId) return;
    if (model.nodes[selId]) removeNode(selId); else removeEdge(selId);
  });
  var clrBtn = document.getElementById('sem-clear-all');
  if (clrBtn) clrBtn.addEventListener('click', function() {
    if (!Object.keys(model.nodes).length) return;
    if (confirm('Clear all constructs and paths from the canvas?')) {
      model={nodes:{},edges:[]}; nextId=1; selId=null; arrowSrc=null;
      render(); hidePanel(); syncR();
    }
  });
  var runBtn = document.getElementById('sem-run-btn');
  if (runBtn) runBtn.addEventListener('click', function() {
    syncR();
    if (typeof Shiny !== 'undefined')
      Shiny.setInputValue('mod10-canvas_run', Date.now(), {priority:'event'});
  });

  /* ── Shiny sync ────────────────────────────────────────────────────────── */
  function syncR() {
    if (typeof Shiny === 'undefined') return;
    Shiny.setInputValue('mod10-canvas_model', JSON.stringify({
      nodes: Object.values(model.nodes),
      edges: model.edges
    }), {priority:'event'});
  }

  /* ── Shiny message handlers ────────────────────────────────────────────── */
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('sem_load_model', function(m) {
      model.nodes={}; model.edges=[];
      (m.nodes||[]).forEach(function(n){ model.nodes[n.id]=n; });
      model.edges = m.edges||[];
      nextId=1; render(); hidePanel(); syncR();
    });
  }

  /* ── Initial state ─────────────────────────────────────────────────────── */
  setMode('select');
  hidePanel();

  window._semEngine = { model:model, addNode:addNode, removeNode:removeNode,
                        removeEdge:removeEdge, syncR:syncR, setMode:setMode,
                        render:render, hidePanel:hidePanel,
                        buildIndCheckboxes:buildIndCheckboxes };

  /* Tell R the canvas is ready — R resends sem_cols if data already loaded.
   * Fixes the race where data is uploaded BEFORE canvas tab renders, so the
   * original sem_cols message arrives before our handler is registered. */
  if (typeof Shiny !== 'undefined') {
    setTimeout(function() {
      Shiny.setInputValue('mod10-canvas_ready', Date.now(), {priority:'event'});
    }, 50);
  }

  console.log('[SEM Canvas] Initialized successfully.');
}; /* end window.initSemCanvas */


/* ══════════════════════════════════════════════════════════════════════════
 * AUTO-INITIALIZATION
 * Three independent triggers — whichever fires first wins.
 * ══════════════════════════════════════════════════════════════════════════ */
(function () {
  /* sem_cols registered at page-load level to avoid race condition:
     Shiny sends sem_cols immediately when data loads, but initSemCanvas
     runs after a 30ms delay -- registering here ensures the message is
     never lost.                                                        */
  if (typeof Shiny !== 'undefined') {
    Shiny.addCustomMessageHandler('sem_cols', function(cols) {
      window._semCols = cols;
      /* If canvas is already up and a node is selected, refresh panel */
      if (window._semEngine && window._semEngine.buildIndCheckboxes) {
        var nodeId = document.getElementById('sem-node-id');
        if (nodeId && nodeId.value && window._semEngine.model.nodes[nodeId.value])
          window._semEngine.buildIndCheckboxes(
            window._semEngine.model.nodes[nodeId.value]);
      }
    });
  }

  /* shared tryInit — sets flag to false so initSemCanvas() proceeds */
  function tryInit() {
    if (window._semInitialized) return;
    if (!document.getElementById('sem-svg')) return;
    window.initSemCanvas();
  }

  /* ── Trigger 1: MutationObserver ─────────────────────────────────────── */
  /* Fires the moment Shiny injects canvas_ui HTML into the DOM             */
  if (window.MutationObserver) {
    var obs = new MutationObserver(function() {
      if (document.getElementById('sem-svg') && !window._semInitialized) {
        obs.disconnect();
        tryInit();
      }
    });
    obs.observe(document.documentElement, { childList: true, subtree: true });
  }

  /* ── Trigger 2: shiny:value event ───────────────────────────────────── */
  /* Fires when Shiny pushes a new value for canvas_ui output              */
  document.addEventListener('shiny:value', function(e) {
    if (e.detail && e.detail.name && e.detail.name.indexOf('canvas_ui') !== -1) {
      /* small delay to let jQuery finish injecting HTML */
      setTimeout(tryInit, 30);
    }
  });

  /* Also listen via jQuery if available (older Shiny uses jQuery events)  */
  if (window.$) {
    $(document).on('shiny:value', function(e) {
      if (e.name && e.name.indexOf('canvas_ui') !== -1) {
        setTimeout(tryInit, 30);
      }
    });
  }

  /* ── Trigger 3: Shiny connected + periodic retry ─────────────────────── */
  /* Belt-and-suspenders: retry every 200ms for the first 10s after Shiny  */
  /* connects, in case the other triggers miss the window.                  */
  if (window.$) {
    $(document).on('shiny:connected', function() {
      var attempts = 0;
      var timer = setInterval(function() {
        attempts++;
        if (window._semInitialized || attempts > 50) { clearInterval(timer); return; }
        tryInit();
      }, 200);
    });
  }

})();
