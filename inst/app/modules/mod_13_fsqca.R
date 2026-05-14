# ══════════════════════════════════════════════════════════════════════
#  Module 13 — fsQCA v4.0  (Dr.AIStat)
#  11 unique features for best-in-class fsQCA analysis
#  Ragin (2008); Schneider & Wagemann (2012)
# ══════════════════════════════════════════════════════════════════════

# ── CSS ───────────────────────────────────────────────────────────────
FSQCA_CSS <- "
  .fsq-guide  { background:linear-gradient(135deg,#E3F2FD,#E8F5E9);
                border-left:4px solid #2196A6; border-radius:0 6px 6px 0;
                padding:.55rem .9rem; margin:.35rem 0 .65rem;
                font-size:.79rem; color:#1A3A5C; line-height:1.55; }
  .fsq-alert  { background:#FFF3E0; border-left:4px solid #E07B39;
                border-radius:0 6px 6px 0; padding:.5rem .9rem;
                margin:.35rem 0; font-size:.79rem; color:#5D4037; }
  .fsq-danger { background:#FFEBEE; border-left:4px solid #C0392B;
                border-radius:0 6px 6px 0; padding:.5rem .9rem;
                margin:.35rem 0; font-size:.79rem; color:#7B1FA2; }
  .fsq-badge-g { background:#1E6438; color:white; padding:2px 7px;
                 border-radius:8px; font-size:.75rem; font-weight:bold; }
  .fsq-badge-a { background:#E07B39; color:white; padding:2px 7px;
                 border-radius:8px; font-size:.75rem; font-weight:bold; }
  .fsq-badge-r { background:#C0392B; color:white; padding:2px 7px;
                 border-radius:8px; font-size:.75rem; font-weight:bold; }
  .fsq-badge-b { background:#1A3A5C; color:white; padding:2px 7px;
                 border-radius:8px; font-size:.75rem; font-weight:bold; }
  .fsq-section { font-weight:bold; font-size:.85rem; color:#1A3A5C;
                 margin:.6rem 0 .2rem; }
"

# ── Guide-box helper ──────────────────────────────────────────────────
gb <- function(..., type = "info") {
  cls <- switch(type, info="fsq-guide", warn="fsq-alert", danger="fsq-danger", "fsq-guide")
  tags$div(class = cls, ...)
}

# ══════════════════════════════════════════════════════════════════════
#  PURE HELPERS  (no Shiny dependency)
# ══════════════════════════════════════════════════════════════════════

# ── Parse solution string → condition matrix ──────────────────────────
parse_sol_mat <- function(sol_terms, conds) {
  mat <- matrix(NA_real_, nrow=length(sol_terms), ncol=length(conds))
  colnames(mat) <- conds; rownames(mat) <- sol_terms
  for (i in seq_along(sol_terms)) {
    for (p in trimws(strsplit(trimws(sol_terms[i]),"\\*")[[1]])) {
      if (startsWith(p,"~")) { idx <- which(conds==substring(p,2)); if(length(idx)) mat[i,idx] <- 0 }
      else                   { idx <- which(conds==p);              if(length(idx)) mat[i,idx] <- 1 }
    }
  }
  mat
}

# ── Safe extraction of solution terms ────────────────────────────────
get_sol_terms <- function(sol_obj)
  tryCatch({ s <- sol_obj$solution[[1]]; if(is.character(s)&&length(s)>0) s else NULL },
           error=function(e) NULL)

# ── Safe extraction of incl.cov df from QCA_pof ──────────────────────
extract_pof_df <- function(obj) {
  if (is.null(obj)) return(NULL)
  ic <- if (inherits(obj,"QCA_pof")) obj$incl.cov
        else if (is.data.frame(obj)||is.matrix(obj)) as.data.frame(obj)
        else NULL
  if (is.null(ic)||nrow(ic)==0) return(NULL); ic
}

# ── Direct necessity formulas (Ragin 2008) ───────────────────────────
nc_direct <- function(x, y) {
  num  <- sum(pmin(x,y),na.rm=TRUE)
  c(Consistency = round(num/(sum(y,na.rm=TRUE)+1e-10),4),
    RoN         = round((sum(y,na.rm=TRUE)-num)/(length(y)-sum(x,na.rm=TRUE)+1e-10),4),
    Coverage    = round(num/(sum(x,na.rm=TRUE)+1e-10),4))
}

# ── Direct sufficiency formulas ───────────────────────────────────────
suff_direct <- function(x, y)
  c(Consistency = round(sum(pmin(x,y),na.rm=TRUE)/(sum(x,na.rm=TRUE)+1e-10),4),
    Coverage    = round(sum(pmin(x,y),na.rm=TRUE)/(sum(y,na.rm=TRUE)+1e-10),4))

# ── Fast fuzzy solution membership (no QCA call needed) ──────────────
compute_sol_mem <- function(sol_terms, cd_sub) {
  n <- nrow(cd_sub)
  if (length(sol_terms)==0) return(rep(0,n))
  term_mat <- vapply(sol_terms, function(term) {
    parts <- trimws(strsplit(trimws(term),"\\*")[[1]])
    pm <- vapply(parts, function(p) {
      if (startsWith(p,"~")) { cn <- substring(p,2); if(cn %in% names(cd_sub)) 1-as.numeric(cd_sub[[cn]]) else rep(.5,n) }
      else                   { if(p  %in% names(cd_sub)) as.numeric(cd_sub[[p]]) else rep(.5,n) }
    }, numeric(n))
    if (is.matrix(pm)) apply(pm,1,min) else pm
  }, numeric(n))
  if (is.matrix(term_mat)) apply(term_mat,1,max) else term_mat
}

# ── Feature 10: Bootstrap CI for solution fit ─────────────────────────
bootstrap_fit <- function(sol_terms, outc, cd_sub, n_boot=500) {
  n   <- nrow(cd_sub)
  y   <- as.numeric(cd_sub[[outc]])
  res <- matrix(NA_real_, n_boot, 2, dimnames=list(NULL,c("Consistency","Coverage")))
  for (b in seq_len(n_boot)) {
    idx  <- sample(n,replace=TRUE)
    yb   <- y[idx]
    sb   <- compute_sol_mem(sol_terms, cd_sub[idx,,drop=FALSE])
    res[b,1] <- sum(pmin(sb,yb))/(sum(sb)+1e-10)
    res[b,2] <- sum(pmin(sb,yb))/(sum(yb)+1e-10)
  }
  data.frame(
    Metric    = c("Consistency","Coverage"),
    Observed  = round(c(sum(pmin(compute_sol_mem(sol_terms,cd_sub),y))/(sum(compute_sol_mem(sol_terms,cd_sub))+1e-10),
                        sum(pmin(compute_sol_mem(sol_terms,cd_sub),y))/(sum(y)+1e-10)), 4),
    Mean_Boot = round(colMeans(res,na.rm=TRUE),4),
    Lower_95  = round(apply(res,2,quantile,.025,na.rm=TRUE),4),
    Upper_95  = round(apply(res,2,quantile,.975,na.rm=TRUE),4),
    SD        = round(apply(res,2,sd,na.rm=TRUE),4)
  )
}

# ── Feature 1: Robustness grid ────────────────────────────────────────
robustness_grid <- function(cd_sub, outc, conds, sol_type,
                            incl_range=c(0.75,0.80,0.85,0.90),
                            ncut_range=c(1,2,3)) {
  rows <- list()
  for (inc in incl_range) for (nc in ncut_range) {
    r <- tryCatch({
      tt  <- QCA::truthTable(cd_sub, outcome=outc,
                             conditions=paste(conds,collapse=","),
                             incl.cut=inc, n.cut=nc, show.cases=TRUE)
      so  <- QCA::minimize(tt, details=TRUE, solution=sol_type)
      trms <- get_sol_terms(so)
      n_paths <- if(!is.null(trms)) length(trms) else NA
      sol_str <- if(!is.null(trms)) paste(trms,collapse=" + ") else "—"
      consist <- NA_real_; cov_r <- NA_real_
      if (!is.null(trms)&&length(trms)>0) {
        sm <- compute_sol_mem(trms,cd_sub); yv <- as.numeric(cd_sub[[outc]])
        consist <- round(sum(pmin(sm,yv))/(sum(sm)+1e-10),4)
        cov_r   <- round(sum(pmin(sm,yv))/(sum(yv)+1e-10),4)
      }
      data.frame(Inclusion=inc, N_cut=nc, N_paths=n_paths,
                 Consistency=consist, Coverage=cov_r,
                 Solution=sol_str, stringsAsFactors=FALSE)
    }, error=function(e)
      data.frame(Inclusion=inc, N_cut=nc, N_paths=NA,
                 Consistency=NA_real_, Coverage=NA_real_,
                 Solution="Error", stringsAsFactors=FALSE))
    rows <- c(rows,list(r))
  }
  do.call(rbind,rows)
}

# ── Feature 2: Contradiction detection ───────────────────────────────
check_contradictions <- function(tt_df, incl_cut=0.8) {
  if (!"incl" %in% names(tt_df)) return(NULL)
  has_cases  <- if("n" %in% names(tt_df)) !is.na(tt_df$n) & tt_df$n > 0 else rep(TRUE, nrow(tt_df))
  contra     <- tt_df[has_cases & !is.na(tt_df$incl) & tt_df$incl < 0.5, , drop=FALSE]
  borderline <- tt_df[has_cases & !is.na(tt_df$incl) & tt_df$incl >= 0.5 & tt_df$incl < incl_cut, , drop=FALSE]
  list(
    contradictions = contra,
    borderline     = borderline,
    n_contra       = nrow(contra),
    n_borderline   = nrow(borderline),
    total_rows     = sum(has_cases, na.rm=TRUE)
  )
}

# ── Feature 6: Calibration diagnostic ────────────────────────────────
calib_diagnostic <- function(cd, vars) {
  do.call(rbind, lapply(vars, function(v) {
    x <- suppressWarnings(as.numeric(as.character(cd[[v]]))); x <- x[!is.na(x)]
    n <- length(x)
    n_amb  <- sum(x>=0.45&x<=0.55); n_half <- sum(abs(x-0.5)<0.001)
    pct    <- round(100*n_amb/n,1)
    status <- if(pct>15)"⚠ High" else if(pct>10)"⚡ Moderate" else "✅ Good"
    data.frame(Variable=v, N=n,
               `Full-in (>0.8)`=sum(x>0.8), `Crossover (0.45–0.55)`=n_amb,
               `Full-out (<0.2)`=sum(x<0.2), `% Ambiguous`=pct,
               `Exact-0.5 cases`=n_half, Status=status,
               stringsAsFactors=FALSE, check.names=FALSE)
  }))
}

# ── Feature 7: Coverage decomposition ────────────────────────────────
coverage_decomp <- function(sol_terms, outc, cd_sub) {
  y   <- as.numeric(cd_sub[[outc]])
  sol <- compute_sol_mem(sol_terms, cd_sub)
  overall_cov <- sum(pmin(sol,y),na.rm=TRUE)/(sum(y,na.rm=TRUE)+1e-10)

  rows <- lapply(seq_along(sol_terms), function(i) {
    si <- compute_sol_mem(sol_terms[i], cd_sub)
    raw  <- sum(pmin(si,y),na.rm=TRUE)/(sum(y,na.rm=TRUE)+1e-10)
    # Unique: cov of this path minus cov already explained by others
    others_mem <- if(length(sol_terms)>1) compute_sol_mem(sol_terms[-i],cd_sub) else rep(0,nrow(cd_sub))
    uniq <- sum(pmax(pmin(si,y)-pmin(others_mem,y),0),na.rm=TRUE)/(sum(y,na.rm=TRUE)+1e-10)
    data.frame(Path=sol_terms[i], Raw_Coverage=round(raw,4), Unique_Coverage=round(uniq,4))
  })
  list(paths=do.call(rbind,rows), overall=round(overall_cov,4))
}

# ── Feature 8: Necessity scorecard ───────────────────────────────────
necessity_scorecard <- function(nc_tbl, sol_terms, conds) {
  # Conditions appearing in sufficient solution
  sol_conds <- unique(unlist(lapply(sol_terms, function(t) {
    parts <- trimws(strsplit(trimws(t),"\\*")[[1]])
    sapply(parts, function(p) if(startsWith(p,"~")) substring(p,2) else p)
  })))

  nc_tbl$Traffic_Light <- ifelse(nc_tbl$Consistency>=0.9,"🟢 Necessary",
                           ifelse(nc_tbl$Consistency>=0.7,"🟡 Borderline",
                                                          "🔴 Not Necessary"))
  nc_tbl$In_Solution   <- ifelse(nc_tbl$Condition %in% sol_conds |
                                 sub("^~","",nc_tbl$Condition) %in% sol_conds,
                                 "✓ Yes","—")
  nc_tbl$INUS_Flag     <- ifelse(nc_tbl$Consistency>=0.9 & nc_tbl$In_Solution=="✓ Yes",
                                 "⚠ INUS — verify", "—")
  nc_tbl
}

# ── Feature 4: Three-solution comparison ─────────────────────────────
three_sol_compare <- function(cd_sub, outc, conds, incl, n_cut) {
  types  <- c(c="Conservative", p="Parsimonious", i="Intermediate")
  result <- lapply(names(types), function(stype) {
    tryCatch({
      tt  <- QCA::truthTable(cd_sub, outcome=outc,
                             conditions=paste(conds,collapse=","),
                             incl.cut=incl, n.cut=n_cut, show.cases=TRUE)
      so  <- QCA::minimize(tt, details=TRUE, solution=stype)
      trms <- get_sol_terms(so)
      sol_str <- if(!is.null(trms)) paste(trms,collapse=" + ") else "—"
      consist <- NA_real_; cov_r <- NA_real_
      if (!is.null(trms)) {
        sm <- compute_sol_mem(trms,cd_sub); yv <- as.numeric(cd_sub[[outc]])
        consist <- round(sum(pmin(sm,yv))/(sum(sm)+1e-10),4)
        cov_r   <- round(sum(pmin(sm,yv))/(sum(yv)+1e-10),4)
      }
      list(type=stype, label=types[[stype]], terms=trms,
           sol_str=sol_str, consist=consist, cov_r=cov_r)
    }, error=function(e)
      list(type=stype, label=types[[stype]], terms=NULL,
           sol_str="Error", consist=NA, cov_r=NA))
  })
  names(result) <- names(types)
  result
}

# ── Feature 9: LaTeX table generator ─────────────────────────────────
to_latex <- function(nc_sc, sol_fit_df, outc) {
  lines <- c(
    "% ── Necessary Conditions ──",
    "\\begin{table}[h]\\centering",
    paste0("\\caption{Analysis of Necessary Conditions — ", outc, "}"),
    "\\begin{tabular}{lrrrl}\\hline",
    "Condition & Consistency & RoN & Coverage & Assessment\\\\\\hline"
  )
  for (i in seq_len(nrow(nc_sc))) {
    r <- nc_sc[i,]
    flag <- if(!is.na(r$Consistency)&&r$Consistency>=0.9) "Necessary"
            else if(!is.na(r$Consistency)&&r$Consistency>=0.7) "Borderline" else "Not necessary"
    lines <- c(lines,
      paste0(r$Condition," & ",sprintf("%.3f",r$Consistency)," & ",
             sprintf("%.3f",r$RoN)," & ",sprintf("%.3f",r$Coverage)," & ",flag,"\\\\"))
  }
  lines <- c(lines,"\\hline\\end{tabular}","\\label{tab:nec}","\\end{table}","",
             "% ── Sufficient Solution ──",
             "\\begin{table}[h]\\centering",
             paste0("\\caption{Parameters of Fit — ", outc, "}"),
             "\\begin{tabular}{lrrrr}\\hline",
             "Solution & Consistency & PRI & Raw Cov. & Unique Cov.\\\\\\hline")
  if (!is.null(sol_fit_df)&&nrow(sol_fit_df)>0) {
    get_col <- function(df,nms) { co <- intersect(nms,names(df)); if(length(co)) df[[co[1]]] else rep(NA,nrow(df)) }
    for (i in seq_len(nrow(sol_fit_df))) {
      r <- sol_fit_df[i,]
      lines <- c(lines,
        paste0(r[[1]]," & ",
               sprintf("%.3f",suppressWarnings(as.numeric(get_col(sol_fit_df,c("incl","Consistency"))[i])))," & ",
               sprintf("%.3f",suppressWarnings(as.numeric(get_col(sol_fit_df,c("PRI"))[i])))," & ",
               sprintf("%.3f",suppressWarnings(as.numeric(get_col(sol_fit_df,c("cov.r","Coverage"))[i])))," & ",
               sprintf("%.3f",suppressWarnings(as.numeric(get_col(sol_fit_df,c("cov.u"))[i]))),"\\\\"))
    }
  }
  lines <- c(lines,"\\hline\\end{tabular}","\\label{tab:suf}","\\end{table}")
  paste(lines,collapse="\n")
}

# ── Core analysis runner ───────────────────────────────────────────────
run_one_analysis <- function(cd_sub, outc, conds, incl, n_cut, sol,
                             neg_out=FALSE, dir_exp=NULL) {
  label <- if(neg_out) paste0("~",outc) else outc
  tt  <- QCA::truthTable(cd_sub, outcome=outc,
                         conditions=paste(conds,collapse=","),
                         incl.cut=incl, n.cut=n_cut,
                         show.cases=TRUE, sort.by=c("incl","n"),
                         decreasing=TRUE, neg.out=neg_out)
  min_args <- list(tt, details=TRUE, show.cases=TRUE, solution=sol)
  if (!is.null(dir_exp)&&sol=="i"&&length(dir_exp)>0) min_args$dir.exp <- dir_exp
  sol_obj <- do.call(QCA::minimize, min_args)

  y_vec <- if(neg_out) 1-cd_sub[[outc]] else cd_sub[[outc]]

  # Necessary conditions
  make_nc_row <- function(cx, negate=FALSE) {
    xv  <- if(negate) 1-cd_sub[[cx]] else cd_sub[[cx]]
    lbl <- if(negate) paste0("~",cx) else cx
    rel <- paste0("Necessity (X \u2190 ",label,")")
    tryCatch({
      v <- nc_direct(xv,y_vec)
      data.frame(Condition=lbl,Relation=rel,Consistency=v["Consistency"],
                 RoN=v["RoN"],Coverage=v["Coverage"],stringsAsFactors=FALSE)
    }, error=function(e)
      data.frame(Condition=lbl,Relation=rel,Consistency=NA_real_,
                 RoN=NA_real_,Coverage=NA_real_,stringsAsFactors=FALSE))
  }
  nc_tbl <- do.call(rbind, c(lapply(conds,make_nc_row,negate=FALSE),
                              lapply(conds,make_nc_row,negate=TRUE)))
  rownames(nc_tbl) <- NULL

  # Sufficiency per condition
  make_suff_row <- function(cx, negate=FALSE) {
    xv  <- if(negate) 1-cd_sub[[cx]] else cd_sub[[cx]]
    lbl <- if(negate) paste0("~",cx) else cx
    tryCatch({
      v <- suff_direct(xv,y_vec)
      data.frame(Condition=lbl,Consistency=v["Consistency"],Coverage=v["Coverage"],
                 stringsAsFactors=FALSE)
    }, error=function(e)
      data.frame(Condition=lbl,Consistency=NA_real_,Coverage=NA_real_,
                 stringsAsFactors=FALSE))
  }
  suff_tbl <- do.call(rbind, c(lapply(conds,make_suff_row,FALSE),
                                lapply(conds,make_suff_row,TRUE)))
  rownames(suff_tbl) <- NULL

  # Solution fit via QCA::pof
  sol_fit <- tryCatch({
    trms <- get_sol_terms(sol_obj)
    if(is.null(trms)) stop("no terms")
    pf <- QCA::pof(setms=paste(trms,collapse=" + "),outcome=outc,data=cd_sub,relation="sufficiency")
    ic <- pf$incl.cov; if(is.null(ic)) stop("no incl.cov")
    cbind(Solution=rownames(ic), round(as.data.frame(ic),4))
  }, error=function(e) NULL)

  list(tt=tt, sol=sol_obj, nc_tbl=nc_tbl, suff_tbl=suff_tbl,
       sol_fit=sol_fit, label=label, neg_out=neg_out)
}

# ══════════════════════════════════════════════════════════════════════
#  PLOTS
# ══════════════════════════════════════════════════════════════════════

# Solution diagram
render_sol_diagram <- function(mat, sol_fit_df, NAVY,TEAL,AMBER,GREEN, title_str="") {
  n_t <- nrow(mat); n_c <- ncol(mat)
  if(n_t==0||n_c==0) return(plot_ly()|>layout(title="No solution"))
  flip <- function(i) n_t-i+1
  pi  <- which(!is.na(mat)&mat==1,arr.ind=TRUE)
  ai  <- which(!is.na(mat)&mat==0,arr.ind=TRUE)
  fig <- plot_ly()
  row_shapes <- lapply(seq_len(n_t),function(i) list(
    type="rect",x0=0.3,x1=n_c+0.7,y0=flip(i)-0.45,y1=flip(i)+0.45,
    fillcolor=if(i%%2==0)"#F7FBFF" else "white",
    line=list(color="transparent"),layer="below"))
  sep_shape <- list(type="line",x0=n_c+0.6,x1=n_c+0.6,y0=0.4,y1=n_t+0.6,
                    line=list(color="#CCC",width=1.5,dash="dot"))
  all_shapes <- c(row_shapes,list(sep_shape))
  if(nrow(pi)>0) fig <- add_trace(fig, x=pi[,2],y=flip(pi[,1]),type="scatter",mode="markers",
    marker=list(symbol="circle",size=26,color=TEAL,line=list(color=NAVY,width=2)),
    name="Present (●)",
    text=paste0(rownames(mat)[pi[,1]],"—",colnames(mat)[pi[,2]],": Present"),
    hovertemplate="%{text}<extra></extra>")
  if(nrow(ai)>0) fig <- add_trace(fig, x=ai[,2],y=flip(ai[,1]),type="scatter",mode="markers",
    marker=list(symbol="circle-open",size=26,color=AMBER,line=list(color=AMBER,width=3)),
    name="Absent (○)",
    text=paste0(rownames(mat)[ai[,1]],"—",colnames(mat)[ai[,2]],": Absent"),
    hovertemplate="%{text}<extra></extra>")

  cx0<-n_c+1.0; cv0<-n_c+2.2; bw<-0.9; all_anns<-list()
  if (!is.null(sol_fit_df)&&nrow(sol_fit_df)>0) {
    get_v <- function(df,cols) tryCatch(as.numeric(df[[intersect(cols,names(df))[1]]]),error=function(e)rep(NA,nrow(df)))
    cons_v <- get_v(sol_fit_df,c("incl","Consistency"))
    cov_v  <- get_v(sol_fit_df,c("cov.r","Coverage"))
    nr <- min(n_t,length(cons_v))
    for (i in seq_len(nr)) {
      yi <- flip(i); con <- cons_v[i]; cov <- cov_v[i]
      if(!is.na(con)) {
        all_shapes<-c(all_shapes,list(
          list(type="rect",x0=cx0,x1=cx0+bw,y0=yi-0.2,y1=yi+0.2,fillcolor="#E8F5E9",line=list(color="#C8E6C9"),layer="below"),
          list(type="rect",x0=cx0,x1=cx0+con*bw,y0=yi-0.2,y1=yi+0.2,fillcolor=GREEN,line=list(color=GREEN),layer="above")))
        all_anns<-c(all_anns,list(list(x=cx0+bw+0.05,y=yi,text=sprintf("<b>%.3f</b>",con),showarrow=FALSE,xanchor="left",font=list(size=10,color=GREEN))))
      }
      if(!is.na(cov)) {
        all_shapes<-c(all_shapes,list(
          list(type="rect",x0=cv0,x1=cv0+bw,y0=yi-0.2,y1=yi+0.2,fillcolor="#E3F2FD",line=list(color="#BBDEFB"),layer="below"),
          list(type="rect",x0=cv0,x1=cv0+cov*bw,y0=yi-0.2,y1=yi+0.2,fillcolor=TEAL,line=list(color=TEAL),layer="above")))
        all_anns<-c(all_anns,list(list(x=cv0+bw+0.05,y=yi,text=sprintf("<b>%.3f</b>",cov),showarrow=FALSE,xanchor="left",font=list(size=10,color=TEAL))))
      }
    }
    all_anns<-c(all_anns,list(
      list(x=cx0+bw/2,y=n_t+0.75,text="<b>Consist.</b>",showarrow=FALSE,font=list(size=11,color=GREEN)),
      list(x=cv0+bw/2,y=n_t+0.75,text="<b>Coverage</b>",showarrow=FALSE,font=list(size=11,color=TEAL))))
  }
  fig |> layout(
    title=list(text=title_str,font=list(size=13,color=NAVY)),
    xaxis=list(tickmode="array",tickvals=seq_len(n_c),ticktext=colnames(mat),
               range=c(0.3,n_c+3.5),zeroline=FALSE,showgrid=FALSE),
    yaxis=list(tickmode="array",tickvals=seq_len(n_t),ticktext=rev(rownames(mat)),
               zeroline=FALSE,showgrid=FALSE),
    shapes=all_shapes,annotations=all_anns,
    legend=list(orientation="h",x=0,y=-0.12),
    plot_bgcolor="white",paper_bgcolor="white",
    margin=list(l=160,r=80,t=55,b=55))
}

# Necessity bar chart
render_nc_plot <- function(nc_tbl,NAVY,TEAL,AMBER,GREEN,title_str="") {
  if(is.null(nc_tbl)||nrow(nc_tbl)==0) return(plot_ly()|>layout(title="No data"))
  nc <- nc_tbl[order(nc_tbl$Consistency,decreasing=FALSE),]
  cols <- ifelse(nc$Consistency>=0.9,GREEN,ifelse(nc$Consistency>=0.7,TEAL,AMBER))
  plot_ly(nc,x=~Consistency,y=~Condition,type="bar",orientation="h",
          marker=list(color=cols,line=list(color="white",width=0.4)),
          text=~paste0(sprintf("%.3f",Consistency)," | RoN:",sprintf("%.3f",RoN),
                       " | Cov:",sprintf("%.3f",Coverage)),
          hovertemplate="%{y}: %{text}<extra></extra>",name="Consistency") |>
    add_segments(x=0.9,xend=0.9,y=-0.5,yend=nrow(nc)-0.5,
                 line=list(color=GREEN,width=2,dash="dash"),
                 name="Necessity threshold (0.9)") |>
    add_segments(x=0.5,xend=0.5,y=-0.5,yend=nrow(nc)-0.5,
                 line=list(color="#999",width=1.5,dash="dot"),
                 name="Relevance threshold (0.5)") |>
    layout(title=list(text=title_str,font=list(size=13,color=NAVY)),
           xaxis=list(title="Consistency",range=c(0,1.05)),
           yaxis=list(title="",automargin=TRUE),
           legend=list(orientation="h",x=0,y=-0.18),
           plot_bgcolor="white",paper_bgcolor="white",bargap=0.35)
}

# Sufficiency bar chart
render_suff_plot <- function(suff_df,NAVY,TEAL,AMBER,GREEN,title_str="") {
  if(is.null(suff_df)||nrow(suff_df)==0) return(plot_ly()|>layout(title="No data"))
  s <- suff_df[order(suff_df$Consistency,decreasing=FALSE),]
  plot_ly() |>
    add_trace(data=s,x=~Consistency,y=~Condition,type="bar",orientation="h",
              name="Consistency",marker=list(color=TEAL,opacity=0.85),
              hovertemplate="%{y}: Consist.=%{x:.3f}<extra></extra>") |>
    add_trace(data=s,x=~Coverage,y=~Condition,type="bar",orientation="h",
              name="Coverage",marker=list(color=AMBER,opacity=0.75),
              hovertemplate="%{y}: Cover.=%{x:.3f}<extra></extra>") |>
    add_segments(x=0.8,xend=0.8,y=-0.5,yend=nrow(s)-0.5,
                 line=list(color=GREEN,width=1.5,dash="dash"),
                 showlegend=FALSE) |>
    layout(title=list(text=title_str,font=list(size=13,color=NAVY)),
           barmode="group",xaxis=list(title="Score",range=c(0,1.05)),
           yaxis=list(title="",automargin=TRUE),
           legend=list(orientation="h",x=0,y=-0.18),
           plot_bgcolor="white",paper_bgcolor="white",bargap=0.3)
}

# Coverage decomposition chart
render_cov_decomp <- function(decomp,NAVY,TEAL,AMBER) {
  if(is.null(decomp)) return(plot_ly()|>layout(title="No data"))
  d <- decomp$paths
  d$Path <- substr(d$Path,1,30)
  plot_ly(d) |>
    add_trace(x=~Path,y=~Raw_Coverage,type="bar",name="Raw Coverage",
              marker=list(color=TEAL,opacity=0.8),
              hovertemplate="%{x}<br>Raw Coverage: %{y:.3f}<extra></extra>") |>
    add_trace(x=~Path,y=~Unique_Coverage,type="bar",name="Unique Coverage",
              marker=list(color=AMBER,opacity=0.8),
              hovertemplate="%{x}<br>Unique Coverage: %{y:.3f}<extra></extra>") |>
    add_segments(x=-0.5,xend=nrow(d)-0.5,y=decomp$overall,yend=decomp$overall,
                 line=list(color=NAVY,width=2,dash="dot"),
                 showlegend=TRUE,name=paste0("Overall (",decomp$overall,")")) |>
    layout(title=list(text="Coverage Decomposition per Solution Path",
                      font=list(size=13,color=NAVY)),
           barmode="overlay",
           xaxis=list(title="Solution Path",tickangle=-20),
           yaxis=list(title="Coverage",range=c(0,1.05)),
           legend=list(orientation="h",x=0,y=-0.22),
           plot_bgcolor="white",paper_bgcolor="white",bargap=0.35)
}

# Feature 3: Case typicality/deviance scatter
render_typicality <- function(sol_terms,conds,cd_sub,outc,NAVY,TEAL,AMBER,GREEN) {
  y  <- as.numeric(cd_sub[[outc]])
  sm <- compute_sol_mem(sol_terms,cd_sub)
  # Quadrant classification
  quad <- ifelse(sm>=0.5&y>=0.5,"🟢 Typical (confirms theory)",
           ifelse(sm>=0.5&y< 0.5,"🔴 Deviant (fits path, lacks outcome)",
           ifelse(sm< 0.5&y>=0.5,"🔵 Other-path member (covered elsewhere)",
                                  "⚪ Irrelevant (low path & low outcome)")))
  col_map <- c("🟢 Typical (confirms theory)"=GREEN,
               "🔴 Deviant (fits path, lacks outcome)"="#C0392B",
               "🔵 Other-path member (covered elsewhere)"=TEAL,
               "⚪ Irrelevant (low path & low outcome)"="#BDC3C7")
  pt_col <- col_map[quad]

  plot_ly(x=jitter(sm,.01),y=jitter(y,.01),type="scatter",mode="markers",
          color=quad,colors=col_map,
          marker=list(size=9,opacity=0.8,line=list(color="white",width=0.5)),
          hovertemplate=paste0("Solution membership: %{x:.3f}<br>",
                               outc,": %{y:.3f}<br>%{text}<extra></extra>"),
          text=quad) |>
    add_segments(x=0,xend=1,y=0,yend=1,line=list(color="#999",dash="dash",width=1.2),
                 showlegend=FALSE,hoverinfo="skip") |>
    add_segments(x=0.5,xend=0.5,y=0,yend=1,line=list(color="#ddd",width=1),
                 showlegend=FALSE,hoverinfo="skip") |>
    add_segments(x=0,xend=1,y=0.5,yend=0.5,line=list(color="#ddd",width=1),
                 showlegend=FALSE,hoverinfo="skip") |>
    add_annotations(x=0.75,y=0.75,text="<b>Typical</b>",showarrow=FALSE,
                    font=list(color=GREEN,size=10)) |>
    add_annotations(x=0.75,y=0.25,text="<b>Deviant</b>",showarrow=FALSE,
                    font=list(color="#C0392B",size=10)) |>
    layout(
      title=list(text=paste0("Case Typicality & Deviance — ",outc),
                 font=list(size=13,color=NAVY)),
      xaxis=list(title="Membership in Solution",range=c(-0.05,1.05),zeroline=FALSE),
      yaxis=list(title=paste0("Membership in ",outc),range=c(-0.05,1.05),zeroline=FALSE),
      legend=list(orientation="h",x=0,y=-0.2,font=list(size=10)),
      plot_bgcolor="white",paper_bgcolor="white")
}

# Feature 1: Robustness heatmap
render_robustness <- function(grid_df,NAVY,TEAL,GREEN,AMBER) {
  if(is.null(grid_df)) return(plot_ly()|>layout(title="No data"))
  grid_df$Label <- paste0("Consist.=",round(grid_df$Consistency,3),
                          "\nPaths=",grid_df$N_paths,
                          "\n",substr(grid_df$Solution,1,40))
  plot_ly(grid_df,x=~factor(Inclusion),y=~factor(N_cut),z=~Consistency,
          type="heatmap",colorscale=list(c(0,"#FFEBEE"),c(0.5,"#FFF9C4"),c(1,GREEN)),
          zmin=0,zmax=1,
          text=~Label,hovertemplate="Incl=%{x}, N-cut=%{y}<br>%{text}<extra></extra>",
          showscale=TRUE,colorbar=list(title="Consistency",len=0.7)) |>
    layout(title=list(text="Robustness Check — Consistency across Threshold Combinations",
                      font=list(size=13,color=NAVY)),
           xaxis=list(title="Inclusion Threshold"),
           yaxis=list(title="Min. Cases (n.cut)"),
           plot_bgcolor="white",paper_bgcolor="white")
}

# Bootstrap CI plot
render_boot_plot <- function(boot_df,NAVY,TEAL,GREEN,AMBER) {
  if(is.null(boot_df)) return(plot_ly()|>layout(title="No data"))
  plot_ly(boot_df,x=~Metric,y=~Observed,type="bar",name="Observed",
          marker=list(color=c(TEAL,AMBER),opacity=0.9),
          error_y=list(type="data",
                       array=boot_df$Upper_95-boot_df$Observed,
                       arrayminus=boot_df$Observed-boot_df$Lower_95,
                       color=NAVY,thickness=2,width=6),
          hovertemplate=paste0("%{x}<br>Observed: %{y:.3f}<br>",
                               "95% CI: [",sprintf("%.3f",boot_df$Lower_95),
                               ", ",sprintf("%.3f",boot_df$Upper_95),"]<extra></extra>")) |>
    add_segments(x=-0.5,xend=1.5,y=0.8,yend=0.8,
                 line=list(color=GREEN,width=1.5,dash="dash"),
                 showlegend=TRUE,name="Recommended threshold (0.80)") |>
    layout(title=list(text=paste0("Bootstrap 95% Confidence Intervals (n=",boot_df$N_boot[1]," if exists, else 500 iterations)"),
                      font=list(size=13,color=NAVY)),
           yaxis=list(title="Score",range=c(0,1.05)),
           xaxis=list(title=""),
           showlegend=TRUE,legend=list(orientation="h",x=0,y=-0.15),
           plot_bgcolor="white",paper_bgcolor="white",bargap=0.5)
}

# Set distributions
render_dist_plot <- function(cd,vars,NAVY,TEAL,AMBER) {
  fig <- plot_ly(); cols <- RColorBrewer::brewer.pal(max(3,min(length(vars),9)),"Set2")
  for (i in seq_along(vars)) {
    v <- vars[i]; vals <- suppressWarnings(as.numeric(as.character(cd[[v]])))
    vals <- vals[!is.na(vals)]; if(length(vals)<2) next
    fig <- add_trace(fig,y=vals,type="violin",name=v,
                     box=list(visible=TRUE),meanline=list(visible=TRUE),
                     fillcolor=cols[((i-1)%%length(cols))+1],opacity=0.7,
                     line=list(color=NAVY,width=1))
  }
  fig |> layout(
    title=list(text="Fuzzy-Set Membership Distributions",font=list(size=13,color=NAVY)),
    xaxis=list(title="Variable"),
    yaxis=list(title="Fuzzy Membership (0–1)",range=c(-0.05,1.05)),
    shapes=list(list(type="line",x0=-0.5,x1=length(vars)-0.5,y0=0.5,y1=0.5,
                     line=list(color="#999",width=1.5,dash="dash"))),
    plot_bgcolor="white",paper_bgcolor="white",showlegend=FALSE,violingap=0.3)
}

# XY plot
render_xy <- function(x_vals,y_vals,x_lab,y_lab,diag_label,rel,set_col,out_col,NAVY,TEAL,AMBER) {
  jx <- jitter(x_vals,.01); jy <- jitter(y_vals,.01)
  consist <- round(sum(pmin(x_vals,y_vals),na.rm=TRUE)/(sum(x_vals,na.rm=TRUE)+1e-10),4)
  cov_r   <- round(sum(pmin(x_vals,y_vals),na.rm=TRUE)/(sum(y_vals,na.rm=TRUE)+1e-10),4)
  plot_ly() |>
    add_trace(x=jx,y=jy,type="scatter",mode="markers",
              marker=list(color=ifelse(jx<=jy,TEAL,AMBER),size=8,opacity=0.75,
                          line=list(color="white",width=0.5)),
              hovertemplate=paste0(x_lab,": %{x:.3f}<br>",y_lab,": %{y:.3f}<extra></extra>"),
              name="Cases") |>
    add_segments(x=0,xend=1,y=0,yend=1,line=list(color="#999",dash="dash",width=1.5),
                 showlegend=FALSE,hoverinfo="skip") |>
    add_annotations(x=0.85,y=0.82,text=diag_label,showarrow=FALSE,
                    font=list(color="#999",size=10)) |>
    add_annotations(x=0.5,y=0.03,
                    text=paste0("Consistency = ",consist," | Coverage = ",cov_r,
                                " | N = ",length(x_vals)),
                    showarrow=FALSE,font=list(size=11,color=NAVY),
                    bgcolor="white",bordercolor=NAVY,borderwidth=1) |>
    layout(title=list(text=paste0("XY Plot — ",rel,": ",
                                  if(rel=="S") paste0(set_col," \u2286 ",out_col)
                                  else paste0(set_col," \u2287 ",out_col)),
                      font=list(color=NAVY)),
           xaxis=list(title=x_lab,range=c(-0.05,1.05),zeroline=FALSE),
           yaxis=list(title=y_lab,range=c(-0.05,1.05),zeroline=FALSE),
           plot_bgcolor="white",paper_bgcolor="white")
}

# Three-solution comparison heatmap
render_three_sol <- function(three_res,conds,NAVY,TEAL,AMBER,GREEN) {
  if(is.null(three_res)) return(plot_ly()|>layout(title="No data"))
  types <- c("c","p","i"); labels <- c("Conservative","Parsimonious","Intermediate")
  all_terms_per_type <- lapply(types,function(t) {
    tr <- three_res[[t]]$terms
    if(is.null(tr)) return(NULL)
    # All conditions appearing in any path (with direction)
    parts <- unlist(lapply(tr,function(term) trimws(strsplit(trimws(term),"\\*")[[1]])))
    unique(parts)
  })
  all_parts <- unique(unlist(all_terms_per_type))
  if(length(all_parts)==0) return(plot_ly()|>layout(title="No solution terms found"))
  # Build presence matrix
  mat <- matrix(0,nrow=length(all_parts),ncol=3,
                dimnames=list(all_parts,labels))
  for (j in seq_along(types)) {
    pts <- all_terms_per_type[[j]]
    if(!is.null(pts)) mat[all_parts %in% pts, j] <- 1
  }
  row_sums <- rowSums(mat)
  # Reorder: appears in most solutions first
  mat <- mat[order(row_sums,decreasing=TRUE),,drop=FALSE]
  row_sums <- rowSums(mat)

  colors <- ifelse(row_sums==3,"Core (all 3)",
             ifelse(row_sums==2,"Shared (2 of 3)","Unique (1 only)"))
  col_pal <- c("Core (all 3)"=GREEN,"Shared (2 of 3)"=TEAL,"Unique (1 only)"=AMBER)
  pt_cols <- col_pal[colors]

  fig <- plot_ly()
  for (j in seq_len(ncol(mat))) for (i in seq_len(nrow(mat))) {
    if(mat[i,j]==1) {
      fig <- add_trace(fig,x=j,y=nrow(mat)-i+1,type="scatter",mode="markers",
                       marker=list(symbol="square",size=28,color=pt_cols[i],
                                   line=list(color=NAVY,width=1.5)),
                       showlegend=FALSE,hoverinfo="skip")
    }
  }
  # Legend proxies
  for (lbl in names(col_pal)) {
    fig <- add_trace(fig,x=NA,y=NA,type="scatter",mode="markers",
                     marker=list(symbol="square",size=14,color=col_pal[[lbl]]),
                     name=lbl,showlegend=TRUE)
  }
  # Fit stats as annotations
  fit_anns <- lapply(seq_along(types),function(j) {
    tr <- three_res[[types[j]]]
    txt <- if(!is.na(tr$consist)) paste0("<b>",labels[j],"</b><br>Consist=",tr$consist,"<br>Cov=",tr$cov_r)
           else paste0("<b>",labels[j],"</b><br>—")
    list(x=j,y=0.0,text=txt,showarrow=FALSE,font=list(size=9,color=NAVY),
         yanchor="top",align="center")
  })
  fig |> layout(
    title=list(text="Three-Solution Comparison Matrix",font=list(size=13,color=NAVY)),
    xaxis=list(tickmode="array",tickvals=1:3,ticktext=labels,
               range=c(0.3,3.7),zeroline=FALSE,showgrid=FALSE),
    yaxis=list(tickmode="array",tickvals=seq_len(nrow(mat)),
               ticktext=rev(rownames(mat)),zeroline=FALSE,showgrid=TRUE,
               gridcolor="#eee",range=c(-0.6,nrow(mat)+0.6)),
    annotations=fit_anns,
    legend=list(orientation="h",x=0,y=-0.15),
    plot_bgcolor="white",paper_bgcolor="white",
    margin=list(l=160,r=20,t=55,b=90))
}

# ══════════════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════════════
mod13_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(FSQCA_CSS)),
    hero_html("🔬","fsQCA v4.0 — Fuzzy Set Qualitative Comparative Analysis",
              "11-Feature Suite · Solution Diagram · Robustness · Bootstrap CI · ~Y Analysis · 🎯 What-If Counterfactual Simulator"),
    uiOutput(ns("global_data_banner")),
    fileInput(ns("file"),"Upload data (.xlsx or .csv)",accept=c(".xlsx",".xls",".csv")),
    uiOutput(ns("main_ui")),
  )
}

# ══════════════════════════════════════════════════════════════════════
#  SERVER
# ══════════════════════════════════════════════════════════════════════
mod13_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$global_data_banner <- renderUI({
      gd <- global_shared_data()
      if (is.null(gd)) return(NULL)
      tags$div(
        style = paste0("background:#e6f4ea; border:1px solid #81c784; border-radius:7px;",
                       " padding:8px 14px; margin-bottom:8px;",
                       " display:flex; align-items:center; justify-content:space-between;"),
        tags$span(style="color:#1b5e20; font-size:.82rem;",
          tags$b("\U0001f4cb Global dataset ready: "),
          sprintf("%s  (%d rows \u00d7 %d cols)", global_shared_name(), nrow(gd), ncol(gd))
        ),
        actionButton(ns("load_global"), "\u2b07 Use This Dataset",
                     class="btn-success btn-sm", style="padding:3px 10px; font-size:.78rem;")
      )
    })
    observeEvent(input$load_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) {
        data_rv(gd)
        showNotification(paste0("Using: ", global_shared_name()), type="message", duration=3)
      }
    })


    raw_df     <- reactive({ req(input$file); read_uploaded(input$file$datapath,input$file$name) })
    num_cols_r <- reactive({ req(raw_df()); numeric_cols(raw_df()) })

    # ── Main UI ───────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      req(raw_df()); nc <- num_cols_r()
      tagList(
        fluidRow(box(title="1. Select Variables",width=12,status="primary",solidHeader=TRUE,
          gb("Choose one",tags$b("outcome (Y)"),"and two to six",tags$b("conditions (X)."),
             "All must be numeric. If not yet calibrated to fuzzy 0–1, use Step 2."),
          fluidRow(
            column(4,
              tags$span(style=paste0("background:",NAVY,";color:white;border-radius:4px;padding:.1rem .5rem;"),"Outcome (Y)"),
              br(),br(),
              pickerInput(ns("outcome"),NULL,choices=nc,options=list(`live-search`=TRUE))),
            column(8,
              tags$span(style=paste0("background:",TEAL,";color:white;border-radius:4px;padding:.1rem .5rem;"),"Conditions (X₁ … Xₙ — 2–6)"),
              br(),br(),
              pickerInput(ns("conditions"),NULL,choices=nc,multiple=TRUE,
                         options=list(`actions-box`=TRUE,`live-search`=TRUE)))))),
        fluidRow(box(title="2. Calibration Settings",width=12,status="primary",solidHeader=TRUE,
          gb(tags$b("Direct calibration:")," three thresholds transform raw scores → fuzzy 0–1 membership.",
             " Full-out (low end), Crossover (0.5 point), Full-in (high end).",
             " Use 'Raw' if values are already 0–1; 'Crisp' to dichotomise at a cutpoint."),
          uiOutput(ns("calib_ui")),br(),
          actionButton(ns("run_calib"),"⚙ Calibrate Data",class="btn-info btn-sm"))),
        uiOutput(ns("tt_ui_wrap")),
        uiOutput(ns("results_ui")),
        uiOutput(ns("sim_ui")),
        box(width=12, status="info", solidHeader=FALSE,
          title=tags$span("\U0001f916 AI Interpretation", style="font-weight:bold;color:#1A3A5C;"),
          tags$div(style="padding:.5rem 0;",
            tags$div(style="background:#EFF8FF;border-left:4px solid #2196A6;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;font-size:.85rem;",
              tags$b("How to use:")," Run an analysis, then click its button for a detailed academic interpretation.", tags$br(),
              tags$a("\U0001f511 FREE Groq key \u2192 console.groq.com/keys",
                     href="https://console.groq.com/keys",target="_blank",style="color:#2196A6;font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_tt"),     "\U0001f4cb Truth Table",            class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_sol"),    "\U0001f535 Solution Terms",          class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_nec"),    "\u26a1 Necessary Conditions",       class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_robust"), "\U0001f4ca Robustness",              class="btn-warning btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_all"),    "\U0001f4cb Full fsQCA Summary",      class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        ),
        creator_footer()
      )
    })

    # ── Calibration rows UI ───────────────────────────────────────────
    output$calib_ui <- renderUI({
      req(input$outcome,input$conditions)
      df <- raw_df(); all_v <- c(input$outcome,input$conditions)
      rows <- lapply(all_v,function(v) fluidRow(
        column(2,tags$b(v,style="line-height:2.2;")),
        column(2,selectInput(ns(paste0("ctype_",v)),NULL,
                             choices=c("Direct (fuzzy)"="direct","Raw (0-1)"="raw","Crisp (binary)"="crisp"),
                             selected="direct")),
        column(2,numericInput(ns(paste0("t1_",v)),"Full-out",
                              value=round(quantile(df[[v]],0.10,na.rm=TRUE),3),step=0.001)),
        column(2,numericInput(ns(paste0("t2_",v)),"Crossover",
                              value=round(median(df[[v]],na.rm=TRUE),3),step=0.001)),
        column(2,numericInput(ns(paste0("t3_",v)),"Full-in",
                              value=round(quantile(df[[v]],0.90,na.rm=TRUE),3),step=0.001)),
        column(2,numericInput(ns(paste0("crisp_",v)),"Crisp cut",
                              value=round(median(df[[v]],na.rm=TRUE),3),step=0.001))
      ))
      tagList(fluidRow(column(2,tags$b("Variable")),column(2,tags$b("Type")),
                       column(2,tags$b("Threshold 1")),column(2,tags$b("Threshold 2")),
                       column(2,tags$b("Threshold 3")),column(2,tags$b("Crisp cut"))),rows)
    })

    # ── Calibrate ─────────────────────────────────────────────────────
    calib_data <- eventReactive(input$run_calib, {
      req(raw_df(),input$outcome,input$conditions)
      df <- raw_df(); all_v <- c(input$outcome,input$conditions); out_df <- df
      tryCatch({
        for (v in all_v) {
          raw <- suppressWarnings(as.numeric(as.character(df[[v]])))
          if(all(is.na(raw))) stop(paste0("'",v,"' has no numeric values."))
          ctype <- input[[paste0("ctype_",v)]]
          if(ctype=="raw")   { out_df[[v]] <- pmax(0,pmin(1,raw))
          } else if(ctype=="crisp") { out_df[[v]] <- as.numeric(raw>=input[[paste0("crisp_",v)]])
          } else {
            t1 <- input[[paste0("t1_",v)]]; t2 <- input[[paste0("t2_",v)]]; t3 <- input[[paste0("t3_",v)]]
            if(t1>=t2||t2>=t3) stop(paste0("Thresholds for '",v,"' must satisfy: t1 < t2 < t3"))
            out_df[[v]] <- QCA::calibrate(raw,type="fuzzy",thresholds=c(t1,t2,t3))
          }
        }
        out_df
      }, error=function(e){ showNotification(paste("Calibration error:",e$message),type="error",duration=12); NULL })
    })

    # ── Feature 6: Calibration diagnostic (auto-computed after calibration) ──
    calib_diag <- reactive({
      req(calib_data(),input$outcome,input$conditions)
      calib_diagnostic(calib_data(),c(input$outcome,input$conditions))
    })

    # ── Step-3 UI (appears after calibration) ─────────────────────────
    output$tt_ui_wrap <- renderUI({
      req(calib_data())
      tagList(
        fluidRow(box(title="✅ Calibrated Data Preview",width=12,status="success",solidHeader=TRUE,
          DT::dataTableOutput(ns("calib_tbl")))),

        # Feature 6: Calibration Quality Diagnostic
        fluidRow(box(title="🔬 Feature 6 — Calibration Quality Diagnostic",width=12,
                     status="warning",solidHeader=TRUE,
          gb("QCA convention: fewer than",tags$b("10–15% of cases"),"should sit in the ambiguous zone (0.45–0.55). ",
             "Cases",tags$b("exactly at 0.5"),"are problematic — QCA is undefined there. ",
             "Adjust thresholds if Status shows ⚠ High."),
          DT::dataTableOutput(ns("calib_diag_tbl")),
          br(),
          plotlyOutput(ns("calib_dist"),height="320px"))),

        fluidRow(box(title="3. Truth Table & Minimisation Settings",width=12,
                     status="primary",solidHeader=TRUE,
          fluidRow(
            column(3,numericInput(ns("incl_cut"),"Inclusion threshold",value=0.8,min=0.5,max=1,step=0.05),
                   tags$small("Rows below this consistency coded as 0 (outcome absent).")),
            column(3,numericInput(ns("n_cut"),"Min. cases per row",value=1,min=0,max=20),
                   tags$small("Rows with fewer cases are logical remainders.")),
            column(3,selectInput(ns("sol_type"),"Solution type",
                                 choices=c("Conservative"="c","Parsimonious"="p","Intermediate"="i"),
                                 selected="c"),
                   checkboxInput(ns("run_neg_y"),"Also analyse ~Y (negated outcome)",FALSE)),
            column(3,br(),actionButton(ns("run_fsqca"),"▶ Run fsQCA",class="btn-primary btn-block"))
          ),
          # Feature 5: Directional expectations (shown when Intermediate selected)
          conditionalPanel(
            condition=paste0("input['",ns("sol_type"),"'] == 'i'"),
            tags$hr(),
            gb(tags$b("Feature 5 — Directional Expectations (Intermediate Solution)"),
               br(),"For each condition, specify whether theory predicts it should be",
               tags$b("PRESENT"),"or",tags$b("ABSENT"),"in the solution. ",
               "This controls which logical remainders are used. ",
               "Leave 'No expectation' if you have no directional hypothesis."),
            uiOutput(ns("dir_exp_ui"))
          )
        ))
      )
    })

    output$calib_tbl <- DT::renderDataTable({
      req(calib_data(),input$outcome,input$conditions)
      cols <- c(input$outcome,input$conditions)
      tool_dt(round(calib_data()[,cols,drop=FALSE],4),"Calibrated fuzzy-set scores")
    })

    output$calib_diag_tbl <- DT::renderDataTable({
      req(calib_diag())
      d <- calib_diag()
      dt <- DT::datatable(d,rownames=FALSE,options=list(pageLength=10,dom="t"),
                          class="compact stripe")
      DT::formatStyle(dt,"Status",
        backgroundColor=DT::styleEqual(
          c("✅ Good","⚡ Moderate","⚠ High"),
          c("#E8F5E9","#FFF9C4","#FFEBEE")))
    })

    output$calib_dist <- renderPlotly({
      req(calib_data(),input$outcome,input$conditions)
      vars <- c(input$outcome,input$conditions)
      render_dist_plot(calib_data(),vars,NAVY,TEAL,AMBER)
    })

    # Feature 5: Directional expectations UI
    output$dir_exp_ui <- renderUI({
      req(input$conditions)
      rows <- lapply(input$conditions, function(cx) fluidRow(
        column(3,tags$b(cx,style="line-height:2;")),
        column(5,selectInput(ns(paste0("direxp_",cx)),NULL,
                             choices=c("No expectation"="none",
                                       "Expected PRESENT (1)"="1",
                                       "Expected ABSENT (0)"="0"),
                             selected="none",width="100%"))
      ))
      tagList(fluidRow(column(3,tags$b("Condition")),column(5,tags$b("Directional Expectation"))),rows)
    })

    # ── Main fsQCA reactive ────────────────────────────────────────────
    fsqca_result <- eventReactive(input$run_fsqca, {
      req(calib_data(),input$outcome,input$conditions)
      if(!requireNamespace("QCA",quietly=TRUE)){ showNotification("QCA package required.",type="error"); return(NULL) }

      cd <- calib_data(); outc <- input$outcome; conds <- input$conditions
      incl <- input$incl_cut; n_cut <- input$n_cut; sol <- input$sol_type

      tryCatch({
        all_v  <- c(outc,conds)
        cd_sub <- cd[,all_v,drop=FALSE]
        for (col in all_v)
          cd_sub[[col]] <- suppressWarnings(as.numeric(as.character(cd_sub[[col]])))
        cd_sub <- cd_sub[complete.cases(cd_sub),,drop=FALSE]
        if(nrow(cd_sub)<2) stop("Too few complete cases after removing NAs.")
        for (col in all_v) {
          rng <- range(cd_sub[[col]],na.rm=TRUE)
          if(rng[1]< -0.001||rng[2]>1.001)
            stop(paste0("'",col,"' has values outside [0,1]. Re-calibrate."))
        }

        # Feature 5: build dir_exp list
        dir_exp_list <- NULL
        if(sol=="i") {
          de <- Filter(Negate(is.null),lapply(setNames(conds,conds),function(cx){
            v <- input[[paste0("direxp_",cx)]]; if(!is.null(v)&&v!="none") as.integer(v) else NULL }))
          if(length(de)>0) dir_exp_list <- de
        }

        # Run main Y analysis
        y_res <- run_one_analysis(cd_sub,outc,conds,incl,n_cut,sol,
                                  neg_out=FALSE,dir_exp=dir_exp_list)

        # Optional ~Y
        neg_res <- if(isTRUE(input$run_neg_y)) {
          tryCatch(run_one_analysis(cd_sub,outc,conds,incl,n_cut,sol,neg_out=TRUE),
                   error=function(e){ showNotification(paste("~Y error:",e$message),type="warning",duration=10); NULL })
        } else NULL

        list(y=y_res, neg=neg_res, outc=outc, conds=conds, cd=cd_sub,
             sol_type=sol, incl=incl, n_cut=n_cut, do_neg=isTRUE(input$run_neg_y))
      }, error=function(e){ showNotification(paste("fsQCA error:",e$message),type="error",duration=15); NULL })
    })

    # ── Three-solution comparison (lazy, triggered by its own button) ──
    three_sol_res <- eventReactive(input$run_three_sol, {
      req(fsqca_result())
      res <- fsqca_result()
      withProgress(message="Running 3-solution comparison…",{
        three_sol_compare(res$cd,res$outc,res$conds,res$incl,res$n_cut)
      })
    })

    # ── Robustness grid (lazy, triggered by its own button) ────────────
    robust_res <- eventReactive(input$run_robust, {
      req(fsqca_result())
      res <- fsqca_result()
      incl_min <- input$robust_inc_min %||% 0.75
      incl_max <- input$robust_inc_max %||% 0.90
      incl_step <- input$robust_inc_step %||% 0.05
      incl_range <- seq(incl_min, incl_max, by = incl_step)
      withProgress(message=paste0("Running robustness grid (",length(incl_range),"×3 combinations)…"),{
        robustness_grid(res$cd,res$outc,res$conds,res$sol_type, incl_range=incl_range)
      })
    })

    # ── Bootstrap CI (fast, lazy) ──────────────────────────────────────
    boot_res <- eventReactive(input$run_boot, {
      req(fsqca_result())
      res   <- fsqca_result()
      trms  <- get_sol_terms(res$y$sol)
      n_b   <- max(100,min(2000,as.integer(if(is.null(input$n_boot)||is.na(input$n_boot)) 500 else input$n_boot)))
      withProgress(message=paste0("Bootstrap CI (",n_b," iterations)…"),{
        bd <- bootstrap_fit(trms,res$outc,res$cd,n_boot=n_b)
        bd$N_boot <- n_b; bd
      })
    })

    # ── Results UI ─────────────────────────────────────────────────────
    output$results_ui <- renderUI({
      req(fsqca_result())
      res <- fsqca_result()
      tagList(
        # ── Main Y box ──
        fluidRow(box(title=paste0("✅ fsQCA Results — ",res$outc," (Outcome Present)"),
                     width=12,status="success",solidHeader=TRUE,
          tabBox(width=12,
            # Tab 1: Truth Table
            tabPanel("📋 Truth Table",
              gb("Each row = one configuration (combination of conditions). ",
                 tags$b("OUT=1")," rows are consistent with the outcome; ",
                 tags$b("OUT=0")," are not. ",
                 tags$b("incl")," = consistency (≥ inclusion threshold → OUT=1). ",
                 tags$b("n")," = number of cases in this configuration. ",
                 "Rows highlighted red = ",tags$b("contradictions")," (incl < 0.5 = more cases WITHOUT outcome than with)."),
              uiOutput(ns("contra_summary")),
              DT::dataTableOutput(ns("tt_tbl"))
            ),
            # Tab 2: Solution Diagram
            tabPanel("🔵 Solution Diagram",
              gb(tags$b("How to read:")," Each row = one solution path. Each column = one condition. ",
                 HTML("<span class='fsq-badge-b'>●</span>")," filled = condition must be PRESENT; ",
                 HTML("<span class='fsq-badge-a'>○</span>")," open = condition must be ABSENT; ",
                 "blank = irrelevant (don't care). ",
                 "Bars on the right show consistency and coverage for each path. ",
                 tags$b("Consistency ≥ 0.80 and Coverage ≥ 0.25")," are typical benchmarks."),
              plotlyOutput(ns("sol_diagram"),height="420px")
            ),
            # Tab 3: Three-Solution Comparison
            tabPanel("🔀 3-Solution Comparison",
              gb(tags$b("Feature 4 — Three-Solution Comparison:"),
                 " Conservative uses no remainders (most complex). ",
                 " Parsimonious uses all remainders (most parsimonious but may include implausible assumptions). ",
                 " Intermediate uses only theory-guided remainders (recommended for most papers). ",
                 tags$b("Green squares")," = condition appears in all 3 solutions (core). ",
                 tags$b("Teal")," = in 2 of 3 (shared). ",
                 tags$b("Amber")," = in only 1 (unique to that solution type)."),
              actionButton(ns("run_three_sol"),"▶ Compare All 3 Solutions",class="btn-info btn-sm"),
              br(),br(),
              plotlyOutput(ns("three_sol_plot"),height="460px"),
              br(),
              DT::dataTableOutput(ns("three_sol_tbl"))
            ),
            # Tab 4: Solution Fit
            tabPanel("🔑 Solution Fit",
              gb(tags$b("incl")," (consistency): proportion of cases whose outcome score ≤ their solution score. ≥ 0.80 required. ",
                 tags$b("PRI")," (Proportional Reduction of Inconsistency): corrects for simultaneous subset relations. ",
                 tags$b("cov.r")," (raw coverage): proportion of outcome cases explained by this path. ",
                 tags$b("cov.u")," (unique coverage): proportion of outcome cases explained ONLY by this path."),
              DT::dataTableOutput(ns("sol_tbl")),
              br(),
              plotlyOutput(ns("cov_decomp_plot"),height="340px")
            ),
            # Tab 5: Necessary Conditions + Scorecard
            tabPanel("⚡ Necessary Conditions",
              gb(tags$b("Necessity:")," condition X is necessary if all (or nearly all) cases with the outcome ALSO have X. ",
                 tags$b("Consistency ≥ 0.90")," = likely necessary (Schneider & Wagemann, 2012). ",
                 tags$b("RoN ≥ 0.50")," = condition is relevant (not trivially necessary because it is very common). ",
                 tags$b("INUS flag")," = condition is necessary AND also appears in the sufficient solution — worth highlighting in your paper."),
              plotlyOutput(ns("nc_plot"),height="400px"),
              br(),
              DT::dataTableOutput(ns("nc_scorecard_tbl"))
            ),
            # Tab 6: Sufficiency Analysis
            tabPanel("📊 Sufficiency Analysis",
              gb(tags$b("Individual sufficiency:")," tests each single condition alone (before combining into paths). ",
                 "Low individual consistency does NOT mean a condition is unimportant — in QCA, ",
                 "conditions often only matter in combination with others. ",
                 tags$b("Use this to spot obviously non-relevant conditions before running the full analysis.")),
              plotlyOutput(ns("suff_plot"),height="400px"),
              br(),
              DT::dataTableOutput(ns("suff_tbl"))
            ),
            # Tab 7: Case Typicality
            tabPanel("🎯 Case Typicality",
              gb(tags$b("Feature 3 — Case Typicality & Deviance:"),
                 " x-axis = case's membership in the solution (all paths combined). y-axis = outcome membership. ",
                 HTML("<b class='sig-yes'>Green (upper-right):</b>")," Typical cases — fit the solution AND have the outcome. These are your 'poster child' cases. ",
                 HTML("<b style='color:#C0392B;'>Red (upper-left):</b>")," Deviant cases — fit the solution but LACK the outcome. Require theoretical explanation. ",
                 HTML("<b style='color:#2196A6;'>Blue (lower-right):</b>")," Covered by other paths. ",
                 "⚪ Lower-left: irrelevant. ",
                 tags$b("Dashed line")," = the consistency diagonal (points above = consistent subset relation)."),
              plotlyOutput(ns("typicality_plot"),height="460px")
            ),
            # Tab 8: XY Plot
            tabPanel("📊 XY Plot",
              fluidRow(
                column(4,selectInput(ns("xy_set"),"Set to plot:",choices=NULL)),
                column(4,selectInput(ns("xy_relation"),"Relation:",
                                     choices=c("Sufficiency"="S","Necessity"="N"),selected="S")),
                column(4,br(),actionButton(ns("run_xy"),"📊 Plot",class="btn-info btn-sm"))
              ),
              gb("Points",tags$b("above"),"the diagonal = consistent subset relation (support the claim). ",
                 "Points",tags$b("below")," = inconsistent cases. ",
                 "Sufficiency plot: x = condition, y = outcome. Necessity plot: x = outcome, y = condition."),
              plotlyOutput(ns("xy_plot"),height="460px")
            ),
            # Tab 9: APA + LaTeX
            tabPanel("📝 APA + LaTeX",
              tabsetPanel(
                tabPanel("APA Write-up",verbatimTextOutput(ns("apa_fsqca"))),
                tabPanel("LaTeX Tables",
                  gb(tags$b("Feature 9 — LaTeX Table Export:")," Copy the code below directly into your LaTeX manuscript. ",
                     "Generates both the Necessary Conditions table and the Solution Parameters of Fit table."),
                  verbatimTextOutput(ns("latex_output")))
              )
            ),
            # Tab 10: Download
            tabPanel("📥 Download",
              downloadButton(ns("dl_fsqca"),"📥 Download Full Results (Excel)"),
              br(),br(),
              gb("The Excel file contains: Calibrated Data, Truth Table, Solution Fit, ",
                 "Necessary Conditions, Sufficiency Analysis, and (if run) Robustness Grid and ~Y results.")
            ),
            # Tab 11: Truth Table Heatmap
            tabPanel("🔥 Truth Table Heatmap",
              tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
                tags$b("Truth Table Configuration Heatmap"),
                tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                  "Each row = one empirically relevant configuration. Each column = one condition or metric. ",
                  tags$b("Dark cells"), " = condition present (1) / high value; ",
                  tags$b("Light cells"), " = condition absent (0). ",
                  "Rows are sorted by outcome column (OUT=1 on top). ",
                  "Hover for configuration details.")),
              withSpinner(plotlyOutput(ns("tt_heatmap"), height = "500px"))
            ),
            # Tab 12: Coverage/Consistency Dashboard
            tabPanel("📊 Coverage Dashboard",
              tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
                tags$b("Solution Path Coverage & Consistency Dashboard"),
                tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                  "Grouped bar chart showing consistency (incl), PRI, raw coverage (cov.r), and unique coverage (cov.u) ",
                  "for each solution path. ",
                  tags$b("Dashed red line"), " = consistency benchmark (0.80); ",
                  tags$b("dashed amber line"), " = coverage benchmark (0.25). ",
                  "All paths above both benchmarks are considered robust.")),
              withSpinner(plotlyOutput(ns("coverage_dashboard"), height = "440px"))
            )
          )
        )),

        # ── Advanced Analytics box ──
        fluidRow(box(title="🔬 Advanced Analytics",width=12,
                     status="info",solidHeader=TRUE,collapsible=TRUE,collapsed=TRUE,
          tabBox(width=12,
            # Feature 1: Robustness
            tabPanel("📉 Robustness Check",
              gb(tags$b("Feature 1 — Robustness / Sensitivity Analysis:"),
                 " Re-runs your analysis across a 4×3 grid of inclusion thresholds (0.75–0.90) and n.cut values (1–3). ",
                 tags$b("Green cells")," = high consistency solutions (≥ 0.85). ",
                 tags$b("Yellow")," = moderate. ",tags$b("Red")," = low or failed. ",
                 "A stable solution should remain consistent across most of the grid. ",
                 "If results change dramatically, calibration or threshold choices need re-examination."),
              fluidRow(
                column(4,selectInput(ns("robust_sol_type"),"Solution type for robustness:",
                                     choices=c("Conservative"="c","Parsimonious"="p","Intermediate"="i"),
                                     selected="c")),
                column(3,numericInput(ns("robust_inc_min"), "Min. threshold:", value=0.75, min=0.50, max=0.95, step=0.05)),
                column(3,numericInput(ns("robust_inc_max"), "Max. threshold:", value=0.90, min=0.55, max=0.99, step=0.05))
              ),
              fluidRow(
                column(3,numericInput(ns("robust_inc_step"), "Step size:", value=0.05, min=0.01, max=0.10, step=0.01)),
                column(3,br(),actionButton(ns("run_robust"),"📉 Run Robustness Check",class="btn-warning btn-sm"))
              ),
              br(),
              plotlyOutput(ns("robust_heatmap"),height="350px"),
              br(),
              DT::dataTableOutput(ns("robust_tbl"))
            ),
            # Feature 10: Bootstrap CI
            tabPanel("📊 Bootstrap CI",
              gb(tags$b("Feature 10 — Bootstrap Confidence Intervals:"),
                 " Re-samples your cases with replacement 500 times (default) and re-computes ",
                 "solution consistency and coverage each time. ",
                 "The 95% CI shows how stable your fit statistics are. ",
                 tags$b("Narrow CI + high observed value = robust result."),
                 " Wide CI = solution fit is sensitive to which cases are included — note this in your limitations. ",
                 "This runs in seconds (analytical, no QCA re-minimisation needed)."),
              fluidRow(
                column(4,numericInput(ns("n_boot"),"Bootstrap iterations:",value=500,min=100,max=2000,step=100)),
                column(4,br(),actionButton(ns("run_boot"),"📊 Run Bootstrap CI",class="btn-info btn-sm"))
              ),
              br(),
              plotlyOutput(ns("boot_plot"),height="360px"),
              br(),
              DT::dataTableOutput(ns("boot_tbl"))
            ),

            # Venn Diagram tab
            tabPanel("🔵 Venn Diagram",
              gb(tags$b("Venn Diagram of Set Memberships:"),
                 " Shows how many cases are 'in' each set (membership > 0.5) and how sets overlap. ",
                 "Select 2 or 3 conditions to compare. Based on your calibrated data. ",
                 "Useful for spotting which combinations of conditions co-occur frequently."),
              fluidRow(
                column(4, uiOutput(ns("venn_cond1_ui"))),
                column(4, uiOutput(ns("venn_cond2_ui"))),
                column(4, uiOutput(ns("venn_cond3_ui")))
              ),
              fluidRow(
                column(3, checkboxInput(ns("venn_3set"), "Show 3-set Venn (select 3 conditions)", value=FALSE)),
                column(3, br(), actionButton(ns("run_venn"), "🔵 Draw Venn Diagram", class="btn-primary btn-sm"))
              ),
              br(),
              plotlyOutput(ns("venn_plot"), height="480px"),
              br(),
              DT::dataTableOutput(ns("venn_counts_tbl"))
            )
          )
        )),

        # ── ~Y box (conditional) ──
        if(isTRUE(isolate(fsqca_result()$do_neg))) {
          fluidRow(box(title=paste0("↩ fsQCA Results — ~",fsqca_result()$outc," (Outcome ABSENT)"),
                       width=12,status="warning",solidHeader=TRUE,collapsible=TRUE,
            gb(tags$b("Asymmetric analysis:")," fsQCA is asymmetric — the conditions that produce the ",
               tags$b("presence")," of an outcome are NOT simply the negation of conditions producing its ",
               tags$b("absence"),".",
               " Compare this solution with the Y solution above. Different solutions indicate genuine asymmetry."),
            tabBox(width=12,
              tabPanel("📋 Truth Table (~Y)",DT::dataTableOutput(ns("tt_tbl_neg"))),
              tabPanel("🔵 Solution Diagram (~Y)",plotlyOutput(ns("sol_diagram_neg"),height="420px")),
              tabPanel("🔑 Solution Fit (~Y)",DT::dataTableOutput(ns("sol_tbl_neg"))),
              tabPanel("⚡ Necessary Conditions (~Y)",
                plotlyOutput(ns("nc_plot_neg"),height="400px"),br(),
                DT::dataTableOutput(ns("nc_tbl_neg"))),
              tabPanel("📄 Solution Text (~Y)",verbatimTextOutput(ns("sol_text_neg")))
            )
          ))
        }
      )
    })

    # ── Observer: XY set choices ──────────────────────────────────────
    observe({
      req(fsqca_result())
      updateSelectInput(session,"xy_set",choices=fsqca_result()$conds,
                        selected=fsqca_result()$conds[1])
    })

    # ════════════════════════════════════════════════════════════════
    #  Y-ANALYSIS OUTPUTS
    # ════════════════════════════════════════════════════════════════

    # Contradiction summary banner
    output$contra_summary <- renderUI({
      req(fsqca_result())
      tt_df <- tryCatch(as.data.frame(fsqca_result()$y$tt$tt),error=function(e) NULL)
      if(is.null(tt_df)) return(NULL)
      cc <- check_contradictions(tt_df,fsqca_result()$incl)
      if(is.null(cc)) return(NULL)
      if(cc$n_contra>0)
        tags$div(class="fsq-danger",
          tags$b(paste0("⚠ Feature 2 — ",cc$n_contra," contradictory configuration(s) detected")),
          " (consistency < 0.5 despite having cases). These are highlighted in the table below. ",
          "Resolution options: (a) accept as logical remainders, (b) add a condition that differentiates cases, ",
          "or (c) re-examine calibration.")
      else if(cc$n_borderline>0)
        tags$div(class="fsq-alert",
          tags$b(paste0("ℹ ",cc$n_borderline," borderline configuration(s)")),
          " have consistency between 0.50 and your inclusion threshold. ",
          "These are excluded from the solution but are not full contradictions.")
      else
        tags$div(class="fsq-guide",HTML("<span class='fsq-badge-g'>✓</span> No contradictions detected. All configurations with cases are cleanly consistent or inconsistent."))
    })

    # Truth Table
    output$tt_tbl <- DT::renderDataTable({
      req(fsqca_result())
      tt_df <- tryCatch(as.data.frame(fsqca_result()$y$tt$tt),error=function(e) data.frame())
      cc <- if("incl" %in% names(tt_df) && "n" %in% names(tt_df))
              check_contradictions(tt_df,fsqca_result()$incl) else NULL
      dt <- DT::datatable(tt_df,rownames=FALSE,
                          options=list(pageLength=20,scrollX=TRUE,dom="Bfrtip",
                                       buttons=c("copy","csv","excel")),
                          extensions="Buttons",class="compact stripe hover")
      if(!is.null(cc) && !is.null(cc$contradictions) && nrow(cc$contradictions)>0) {
        bad_rows <- which(rownames(tt_df) %in% rownames(cc$contradictions) |
                          (!is.na(tt_df$incl) & tt_df$incl < 0.5 &
                           !is.na(tt_df$n) & tt_df$n > 0))
        dt <- DT::formatStyle(dt,columns=0,target="row",
                              backgroundColor=DT::styleRow(bad_rows,"#FFEBEE"))
      }
      dt
    })

    # Solution Diagram
    output$sol_diagram <- renderPlotly({
      req(fsqca_result())
      res <- fsqca_result(); trms <- get_sol_terms(res$y$sol)
      if(is.null(trms)) return(plot_ly()|>layout(title="No solution terms"))
      render_sol_diagram(parse_sol_mat(trms,res$conds),res$y$sol_fit,
                         NAVY,TEAL,AMBER,GREEN,
                         title_str=paste0("Solution Diagram — ",res$outc,
                                          " (",switch(res$sol_type,c="Conservative",
                                                      p="Parsimonious",i="Intermediate"),")"))
    })

    # Three-solution comparison
    output$three_sol_plot <- renderPlotly({
      req(three_sol_res())
      render_three_sol(three_sol_res(),fsqca_result()$conds,NAVY,TEAL,AMBER,GREEN)
    })

    output$three_sol_tbl <- DT::renderDataTable({
      req(three_sol_res())
      tr <- three_sol_res()
      df <- data.frame(
        Solution_Type = c("Conservative","Parsimonious","Intermediate"),
        Formula       = sapply(c("c","p","i"),function(t) tr[[t]]$sol_str),
        N_Paths       = sapply(c("c","p","i"),function(t) length(tr[[t]]$terms)),
        Consistency   = sapply(c("c","p","i"),function(t) tr[[t]]$consist),
        Coverage      = sapply(c("c","p","i"),function(t) tr[[t]]$cov_r),
        stringsAsFactors=FALSE)
      tool_dt(df,"Three-Solution Comparison")
    })

    # Solution Fit Table
    output$sol_tbl <- DT::renderDataTable({
      req(fsqca_result())
      sf <- fsqca_result()$y$sol_fit
      if(!is.null(sf)&&nrow(sf)>0){ rownames(sf)<-NULL; tool_dt(sf,"Solution Parameters of Fit") }
      else {
        ic <- NULL
        for(sl in c("IC","PS","CS","pof_S")){ ic <- extract_pof_df(tryCatch(fsqca_result()$y$sol[[sl]],error=function(e)NULL)); if(!is.null(ic)) break }
        if(!is.null(ic)){ df <- cbind(Solution=rownames(ic),round(as.data.frame(ic),4)); rownames(df)<-NULL; tool_dt(df,"Solution Parameters of Fit") }
        else DT::datatable(data.frame(Note="See Solution Text tab above."),rownames=FALSE)
      }
    })

    # Coverage decomposition
    output$cov_decomp_plot <- renderPlotly({
      req(fsqca_result())
      res <- fsqca_result(); trms <- get_sol_terms(res$y$sol)
      if(is.null(trms)) return(plot_ly()|>layout(title="No solution terms"))
      render_cov_decomp(coverage_decomp(trms,res$outc,res$cd),NAVY,TEAL,AMBER)
    })

    # Necessary Conditions + Scorecard
    output$nc_plot <- renderPlotly({
      req(fsqca_result())
      render_nc_plot(fsqca_result()$y$nc_tbl,NAVY,TEAL,AMBER,GREEN,
                     paste0("Necessary Conditions — ",fsqca_result()$outc))
    })

    output$nc_scorecard_tbl <- DT::renderDataTable({
      req(fsqca_result())
      res  <- fsqca_result()
      trms <- get_sol_terms(res$y$sol)
      sc   <- if(!is.null(trms)) necessity_scorecard(res$y$nc_tbl,trms,res$conds)
              else res$y$nc_tbl
      dt <- DT::datatable(sc,rownames=FALSE,
                          options=list(pageLength=20,dom="Bfrtip",buttons=c("copy","csv","excel")),
                          extensions="Buttons",class="compact stripe hover",
                          caption="Feature 8 — Necessity Scorecard (Traffic-light + INUS flag)")
      if("Traffic_Light" %in% names(sc))
        dt <- DT::formatStyle(dt,"Traffic_Light",
                              backgroundColor=DT::styleEqual(
                                c("🟢 Necessary","🟡 Borderline","🔴 Not Necessary"),
                                c("#E8F5E9","#FFF9C4","#FFEBEE")))
      dt
    })

    # Sufficiency
    output$suff_plot <- renderPlotly({
      req(fsqca_result())
      render_suff_plot(fsqca_result()$y$suff_tbl,NAVY,TEAL,AMBER,GREEN,
                       paste0("Individual Sufficiency — ",fsqca_result()$outc))
    })
    output$suff_tbl <- DT::renderDataTable({
      req(fsqca_result()); tool_dt(fsqca_result()$y$suff_tbl,"Individual Condition Sufficiency")
    })

    # Case Typicality
    output$typicality_plot <- renderPlotly({
      req(fsqca_result())
      res  <- fsqca_result(); trms <- get_sol_terms(res$y$sol)
      if(is.null(trms)) return(plot_ly()|>layout(title="Run analysis first"))
      render_typicality(trms,res$conds,res$cd,res$outc,NAVY,TEAL,AMBER,GREEN)
    })

    # XY Plot
    xy_data <- eventReactive(input$run_xy,{
      req(fsqca_result(),input$xy_set)
      res <- fsqca_result(); cd <- res$cd; sc <- input$xy_set; oc <- res$outc
      rel <- input$xy_relation
      if(rel=="S") list(x=cd[[sc]],y=cd[[oc]],x_lab=paste0(sc," (condition)"),
                        y_lab=paste0(oc," (outcome)"),diag_label="Sufficiency diagonal",
                        rel="S",set_col=sc,out_col=oc)
      else         list(x=cd[[oc]],y=cd[[sc]],x_lab=paste0(oc," (outcome)"),
                        y_lab=paste0(sc," (condition)"),diag_label="Necessity diagonal",
                        rel="N",set_col=sc,out_col=oc)
    })
    output$xy_plot <- renderPlotly({
      req(xy_data())
      xy <- xy_data()
      render_xy(xy$x,xy$y,xy$x_lab,xy$y_lab,xy$diag_label,
                xy$rel,xy$set_col,xy$out_col,NAVY,TEAL,AMBER)
    })

    # APA Summary
    output$apa_fsqca <- renderText({
      req(fsqca_result())
      res <- fsqca_result(); y_r <- res$y
      sol_name <- switch(res$sol_type,c="conservative",p="parsimonious",i="intermediate")
      trms <- get_sol_terms(y_r$sol)
      nec_pass <- y_r$nc_tbl[!is.na(y_r$nc_tbl$Consistency)&y_r$nc_tbl$Consistency>=0.9,]
      paste0(
        "fsQCA ANALYSIS — APA-STYLE WRITE-UP\n",
        "─────────────────────────────────────────────────────────\n\n",
        "METHOD\nA fuzzy-set qualitative comparative analysis (fsQCA) was conducted ",
        "following Ragin (2008) and Schneider and Wagemann (2012). The analysis examined ",
        "conditions — ",paste(res$conds,collapse=", ")," — as predictors of ",res$outc,
        ". Variables were calibrated into fuzzy-set membership scores (0–1) using direct ",
        "calibration. The crossover point (0.5) represents maximum ambiguity of membership.\n\n",
        "CALIBRATION QUALITY\nCalibration diagnostics were checked for cases in the ambiguous ",
        "zone (0.45–0.55). Cases exactly at 0.5 were flagged per QCA convention.\n\n",
        "TRUTH TABLE\nA truth table was constructed with an inclusion threshold of ",
        sprintf("%.2f",res$incl),", requiring a minimum of ",res$n_cut,
        " case(s) per row. Rows below the threshold or with insufficient cases were treated as ",
        "logical remainders.\n\n",
        "MINIMISATION (",toupper(sol_name)," SOLUTION)\n",
        "Boolean minimisation (Quine-McCluskey algorithm) produced the following ",sol_name,
        " solution: ",if(!is.null(trms)) paste(trms,collapse=" + ") else "[see solution text]",".\n",
        if(!is.null(y_r$sol_fit)&&nrow(y_r$sol_fit)>0){
          sf <- y_r$sol_fit
          g <- function(cols) tryCatch(sprintf("%.3f",mean(as.numeric(sf[[intersect(cols,names(sf))[1]]]),na.rm=TRUE)),error=function(e)"—")
          paste0("Overall solution consistency = ",g(c("incl","Consistency")),
                 "; solution coverage = ",g(c("cov.r","Coverage")),".\n\n")
        } else "\n\n",
        "NECESSARY CONDITIONS\n",
        if(nrow(nec_pass)>0) paste0("The following conditions met the necessity threshold (consistency ≥ 0.90):\n",
          paste0("  ",nec_pass$Condition," (consistency = ",sprintf("%.3f",nec_pass$Consistency),
                 ", RoN = ",sprintf("%.3f",nec_pass$RoN),")",collapse="\n"),"\n\n")
        else "No single condition met the necessity threshold (consistency ≥ 0.90).\n\n",
        if(isTRUE(res$do_neg)&&!is.null(res$neg)){
          neg_trms <- get_sol_terms(res$neg$sol)
          paste0("NEGATED OUTCOME (~",res$outc,")\nA parallel analysis for the absence of ",res$outc,
                 " yielded: ",if(!is.null(neg_trms)) paste(neg_trms,collapse=" + ") else "[see ~Y tab]",
                 ". Comparison with the Y solution confirms asymmetric causality.\n\n")
        } else "",
        "REFERENCES\n",
        "Ragin, C. C. (2008). Redesigning social inquiry. University of Chicago Press.\n",
        "Schneider, C. Q., & Wagemann, C. (2012). Set-theoretic methods. Cambridge University Press.\n\n",
        "[Dr.AIStat v3.0 — Dr. Aneeq Inam | ORCID: 0000-0001-7682-2244]"
      )
    })

    # LaTeX output (Feature 9)
    output$latex_output <- renderText({
      req(fsqca_result())
      res  <- fsqca_result(); trms <- get_sol_terms(res$y$sol)
      sc   <- if(!is.null(trms)) necessity_scorecard(res$y$nc_tbl,trms,res$conds)
              else res$y$nc_tbl
      to_latex(sc, res$y$sol_fit, res$outc)
    })

    # ── ADVANCED ANALYTICS ────────────────────────────────────────────

    # Robustness
    output$robust_heatmap <- renderPlotly({
      req(robust_res())
      render_robustness(robust_res(),NAVY,TEAL,GREEN,AMBER)
    })
    output$robust_tbl <- DT::renderDataTable({
      req(robust_res())
      d <- robust_res()
      dt <- tool_dt(d,"Robustness Grid — Consistency across Threshold Combinations")
      dt
    })

    # Bootstrap CI
    output$boot_plot <- renderPlotly({
      req(boot_res())
      render_boot_plot(boot_res(),NAVY,TEAL,GREEN,AMBER)
    })
    output$boot_tbl <- DT::renderDataTable({
      req(boot_res()); tool_dt(boot_res(),"Bootstrap Confidence Intervals (95%)")
    })

    # Download
    output$dl_fsqca <- downloadHandler(
      filename=function() paste0("fsQCA_Results_",Sys.Date(),".xlsx"),
      content=function(file) {
        req(fsqca_result())
        res <- fsqca_result()
        cd  <- res$cd[,c(res$outc,res$conds),drop=FALSE]
        tt  <- tryCatch(as.data.frame(res$y$tt$tt),error=function(e) data.frame())
        sheets <- list("Calibrated Data"=cd,"Truth Table"=tt,
                       "Necessary Conds"=res$y$nc_tbl,"Sufficiency"=res$y$suff_tbl)
        if(!is.null(res$y$sol_fit)) sheets[["Solution Fit"]] <- res$y$sol_fit
        if(!is.null(robust_res())) sheets[["Robustness Grid"]] <- robust_res()
        if(!is.null(boot_res()))   sheets[["Bootstrap CI"]]    <- boot_res()
        if(isTRUE(res$do_neg)&&!is.null(res$neg)){
          sheets[["Truth Table (~Y)"]] <- tryCatch(as.data.frame(res$neg$tt$tt),error=function(e) data.frame())
          sheets[["Nec Conds (~Y)"]]   <- res$neg$nc_tbl
        }
        write_xlsx(sheets,file)
      }
    )

    # ════════════════════════════════════════════════════════════════
    #  ~Y OUTPUTS
    # ════════════════════════════════════════════════════════════════

    output$tt_tbl_neg <- DT::renderDataTable({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      tt_df <- tryCatch(as.data.frame(res$neg$tt$tt),error=function(e) data.frame())
      tool_dt(tt_df,paste0("Truth Table — ~",res$outc))
    })

    output$sol_diagram_neg <- renderPlotly({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      trms <- get_sol_terms(res$neg$sol)
      if(is.null(trms)) return(plot_ly()|>layout(title="No solution terms"))
      render_sol_diagram(parse_sol_mat(trms,res$conds),res$neg$sol_fit,
                         NAVY,TEAL,AMBER,GREEN,
                         paste0("Solution Diagram — ~",res$outc))
    })

    output$sol_tbl_neg <- DT::renderDataTable({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      sf <- res$neg$sol_fit
      if(!is.null(sf)&&nrow(sf)>0){ rownames(sf)<-NULL; tool_dt(sf,paste0("Solution Fit — ~",res$outc)) }
      else DT::datatable(data.frame(Note="See Solution Text tab."),rownames=FALSE)
    })

    output$nc_plot_neg <- renderPlotly({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      render_nc_plot(res$neg$nc_tbl,NAVY,TEAL,AMBER,GREEN,
                     paste0("Necessary Conditions — ~",res$outc))
    })

    output$nc_tbl_neg <- DT::renderDataTable({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      tool_dt(res$neg$nc_tbl,paste0("Necessary Conditions — ~",res$outc))
    })

    output$sol_text_neg <- renderText({
      req(fsqca_result()); res <- fsqca_result(); req(!is.null(res$neg))
      tryCatch(capture.output(print(res$neg$sol))|>paste(collapse="\n"),
               error=function(e) paste("Display error:",e$message))
    })

    # ════════════════════════════════════════════════════════════════
    #  FEATURE 11 — What-If Counterfactual Simulator
    #  ML (Random Forest) + fsQCA necessity/sufficiency math
    #  Interactive: raw-value inputs ↔ fuzzy sliders (both modes)
    # ════════════════════════════════════════════════════════════════

    # ── Simulator outer UI (appears only after fsQCA has been run) ────
    output$sim_ui <- renderUI({
      req(fsqca_result())
      res <- fsqca_result()
      ns  <- session$ns
      fluidRow(box(
        title = "🎯 Feature 11 — What-If Counterfactual Simulator",
        width = 12, status = "primary", solidHeader = TRUE,
        gb(
          tags$b("How it works:"), " Train a Random Forest model on your calibrated fuzzy scores, ",
          "then use the interactive sliders to change any condition value and instantly see — ",
          "graphically and mathematically — how it affects the predicted outcome, ",
          "which fsQCA solution paths the hypothetical case belongs to, and which conditions ",
          "are driving the prediction up or down.",
          type = "info"
        ),
        # ── Step 1: Train ────────────────────────────────────────────
        tags$div(class = "fsq-section", "Step 1 — Train Predictive Model"),
        fluidRow(
          column(4,
            numericInput(ns("sim_n_trees"), "Number of trees", value = 500, min = 100, max = 2000, step = 100),
            actionButton(ns("train_rf"), "🤖 Train Random Forest", class = "btn-primary btn-sm")
          ),
          column(8, uiOutput(ns("rf_summary_ui")))
        ),
        br(),
        plotlyOutput(ns("rf_importance_plot"), height = "260px"),
        br(),
        # ── Step 2: Simulator ────────────────────────────────────────
        uiOutput(ns("sim_step2_ui"))
      ))
    })

    # ── Random Forest model (event-driven) ───────────────────────────
    rf_model_data <- eventReactive(input$train_rf, {
      req(fsqca_result())
      if (!requireNamespace("ranger", quietly = TRUE)) {
        showNotification("Installing 'ranger' package for Random Forest…", type = "message", duration = 8)
        utils::install.packages("ranger", quiet = TRUE, repos = "https://cloud.r-project.org")
      }
      if (!requireNamespace("ranger", quietly = TRUE)) {
        showNotification("Could not install 'ranger'. Please run: install.packages('ranger')", type = "error")
        return(NULL)
      }
      res    <- fsqca_result()
      cd     <- res$cd; outc <- res$outc; conds <- res$conds
      n_tr   <- max(100L, min(2000L, as.integer(input$sim_n_trees %||% 500L)))
      withProgress(message = "Training Random Forest…", value = 0.5, {
        tryCatch({
          fml  <- as.formula(paste(outc, "~", paste(conds, collapse = " + ")))
          mod  <- ranger::ranger(fml, data = cd, num.trees = n_tr,
                                 importance = "permutation", seed = 42)
          imp  <- ranger::importance(mod)
          base <- mean(cd[[outc]], na.rm = TRUE)
          list(model = mod, importance = imp, baseline = base,
               oob_err = mod$prediction.error, outc = outc, conds = conds, cd = cd)
        }, error = function(e) {
          showNotification(paste("RF training error:", e$message), type = "error", duration = 12)
          NULL
        })
      })
    })

    # ── RF summary box ────────────────────────────────────────────────
    output$rf_summary_ui <- renderUI({
      req(rf_model_data())
      m   <- rf_model_data()
      r2  <- round(1 - m$oob_err / var(m$cd[[m$outc]], na.rm = TRUE), 3)
      rmse <- round(sqrt(m$oob_err), 4)
      tagList(
        tags$p(tags$b("Model ready ✓"), style = "color:#1E6438; margin-bottom:4px;"),
        tags$p(sprintf("OOB R² ≈ %.3f  |  OOB RMSE ≈ %.4f", max(0, r2), rmse),
               style = "font-size:.85rem; color:#333;"),
        tags$p(sprintf("Outcome: %s  |  Conditions: %s  |  Cases: %d",
                        m$outc, paste(m$conds, collapse=", "), nrow(m$cd)),
               style = "font-size:.8rem; color:#666;"),
        gb("✅ Model trained. Adjust condition sliders below to run what-if simulations.", type = "info")
      )
    })

    # ── Feature Importance plot ───────────────────────────────────────
    output$rf_importance_plot <- renderPlotly({
      req(rf_model_data())
      m   <- rf_model_data()
      imp <- sort(m$importance, decreasing = FALSE)
      bar_cols <- ifelse(imp == max(imp), AMBER, TEAL)
      plot_ly(x = imp, y = names(imp), type = "bar", orientation = "h",
              marker = list(color = paste0("#", bar_cols)),
              hovertemplate = "<b>%{y}</b><br>Importance: %{x:.4f}<extra></extra>") |>
        layout(
          title = list(text = "Permutation Feature Importance (Random Forest)", font = list(color = paste0("#", NAVY), size = 14)),
          xaxis = list(title = "Mean Decrease in Accuracy", zeroline = FALSE),
          yaxis = list(title = NULL),
          plot_bgcolor = "white", paper_bgcolor = "white",
          margin = list(l = 120)
        )
    })

    # ── Step 2 UI (appears after model is trained) ────────────────────
    output$sim_step2_ui <- renderUI({
      req(rf_model_data())
      m  <- rf_model_data()
      ns <- session$ns
      tagList(
        tags$hr(),
        tags$div(class = "fsq-section", "Step 2 — Adjust Conditions & Explore What-If Scenarios"),
        gb(
          tags$b("Input mode:"),
          tags$b(" Raw values") , " auto-calibrate to fuzzy scores using the same thresholds you set above. ",
          tags$b("Fuzzy sliders (0–1)") , " let you set membership scores directly. Both modes update predictions in real-time.",
          type = "info"
        ),
        fluidRow(
          column(3,
            radioButtons(ns("sim_mode"), "Input Mode:",
                         choices = c("Raw Values" = "raw",
                                     "Fuzzy Scores (0–1)" = "fuzzy",
                                     "Both" = "both"),
                         selected = "both"),
            actionButton(ns("sim_reset"), "↺ Reset to Data Means", class = "btn-default btn-sm btn-block"),
            br(),
            tags$div(class = "fsq-section", "Hypothetical Case Values"),
            uiOutput(ns("sim_inputs_ui"))
          ),
          column(9,
            fluidRow(
              column(6,
                tags$div(class = "fsq-section", "ML Predicted Outcome"),
                plotlyOutput(ns("sim_gauge"), height = "200px")
              ),
              column(6,
                tags$div(class = "fsq-section", "Condition Contributions"),
                plotlyOutput(ns("sim_contrib_plot"), height = "200px")
              )
            ),
            br(),
            fluidRow(
              column(7,
                tags$div(class = "fsq-section", "XY Plot — Case Position vs. Real Data"),
                plotlyOutput(ns("sim_xy_plot"), height = "320px")
              ),
              column(5,
                tags$div(class = "fsq-section", "fsQCA Path Membership"),
                verbatimTextOutput(ns("sim_qca_text")),
                br(),
                tags$div(class = "fsq-section", "Mathematical Summary"),
                verbatimTextOutput(ns("sim_math_text"))
              )
            )
          )
        )
      )
    })

    # ── Dynamic condition input widgets ───────────────────────────────
    output$sim_inputs_ui <- renderUI({
      req(rf_model_data(), fsqca_result())
      m    <- rf_model_data()
      res  <- fsqca_result()
      ns   <- session$ns
      mode <- input$sim_mode %||% "both"
      cd   <- m$cd

      rows <- lapply(m$conds, function(cx) {
        mean_val  <- round(mean(cd[[cx]], na.rm = TRUE), 3)
        sd_val    <- round(sd(cd[[cx]], na.rm = TRUE), 3)
        raw_min   <- round(min(cd[[cx]], na.rm = TRUE), 3)
        raw_max   <- round(max(cd[[cx]], na.rm = TRUE), 3)

        # Determine raw thresholds from calib settings
        ctype <- input[[paste0("ctype_", cx)]] %||% "raw"
        t1 <- input[[paste0("t1_", cx)]] %||% raw_min
        t2 <- input[[paste0("t2_", cx)]] %||% mean_val
        t3 <- input[[paste0("t3_", cx)]] %||% raw_max

        raw_id   <- paste0("sim_raw_",  cx)
        fuzzy_id <- paste0("sim_fz_",   cx)

        tagList(
          tags$b(cx, style = "font-size:.85rem; color:#1A3A5C;"),
          if (mode %in% c("raw", "both"))
            numericInput(ns(raw_id),
                         paste0("Raw value [", raw_min, " – ", raw_max, "]"),
                         value = mean_val, step = 0.01, min = raw_min - abs(raw_min),
                         max = raw_max + abs(raw_max)),
          if (mode %in% c("fuzzy", "both"))
            sliderInput(ns(fuzzy_id), "Fuzzy score (0–1)",
                        min = 0, max = 1, value = mean_val, step = 0.01),
          tags$small(paste0("Mean: ", mean_val, "  SD: ", sd_val),
                     style = "color:#888; display:block; margin-bottom:6px;"),
          tags$hr(style = "margin:4px 0;")
        )
      })
      tagList(rows)
    })

    # ── Reactive: get current simulated fuzzy values from UI ──────────
    sim_fuzzy_vals <- reactive({
      req(rf_model_data(), fsqca_result())
      m    <- rf_model_data()
      res  <- fsqca_result()
      mode <- input$sim_mode %||% "both"
      cd   <- m$cd

      vals <- setNames(
        vapply(m$conds, function(cx) {
          fz_id  <- paste0("sim_fz_",  cx)
          raw_id <- paste0("sim_raw_", cx)

          if (mode == "fuzzy") {
            v <- input[[fz_id]]; if (is.null(v)) mean(cd[[cx]], na.rm = TRUE) else v

          } else if (mode == "raw") {
            rv <- input[[raw_id]]; if (is.null(rv)) return(mean(cd[[cx]], na.rm = TRUE))
            # Re-calibrate raw → fuzzy using stored thresholds
            ctype <- input[[paste0("ctype_", cx)]] %||% "raw"
            if (ctype == "raw") { pmax(0, pmin(1, rv))
            } else if (ctype == "crisp") { as.numeric(rv >= (input[[paste0("crisp_", cx)]] %||% 0.5))
            } else {
              t1 <- input[[paste0("t1_", cx)]] %||% 0
              t2 <- input[[paste0("t2_", cx)]] %||% 0.5
              t3 <- input[[paste0("t3_", cx)]] %||% 1
              if (t1 < t2 && t2 < t3)
                as.numeric(QCA::calibrate(rv, type = "fuzzy", thresholds = c(t1, t2, t3)))
              else pmax(0, pmin(1, (rv - t1) / (t3 - t1 + 1e-10)))
            }

          } else {  # "both" — prefer fuzzy slider if available
            fzv <- input[[fz_id]]
            if (!is.null(fzv)) fzv else mean(cd[[cx]], na.rm = TRUE)
          }
        }, numeric(1L)),
        m$conds
      )
      vals
    })

    # ── Reactive: ML prediction + contributions ───────────────────────
    sim_prediction <- reactive({
      req(rf_model_data(), sim_fuzzy_vals())
      m    <- rf_model_data()
      vals <- sim_fuzzy_vals()
      new_row <- as.data.frame(as.list(vals))

      # ML prediction for hypothetical case
      pred <- tryCatch(
        predict(m$model, data = new_row)$predictions,
        error = function(e) NA_real_
      )

      # Per-condition contributions: ablation (remove each condition → baseline mean)
      contribs <- vapply(m$conds, function(cx) {
        alt_row       <- new_row
        alt_row[[cx]] <- mean(m$cd[[cx]], na.rm = TRUE)   # replace with mean
        alt_pred <- tryCatch(predict(m$model, data = alt_row)$predictions, error = function(e) pred)
        pred - alt_pred   # positive = condition is pushing outcome UP
      }, numeric(1L))

      list(pred = pred, contribs = contribs, baseline = m$baseline,
           vals = vals, outc = m$outc)
    })

    # ── Gauge: predicted outcome ──────────────────────────────────────
    output$sim_gauge <- renderPlotly({
      req(sim_prediction())
      sp   <- sim_prediction()
      pred <- round(sp$pred, 3)
      base <- round(sp$baseline, 3)
      delta <- round(pred - base, 3)
      d_col <- if (delta >= 0) paste0("#", GREEN) else "#C0392B"
      d_lbl <- if (delta >= 0) paste0("+", delta) else as.character(delta)

      plot_ly(
        type = "indicator", mode = "gauge+number+delta",
        value = pred,
        number = list(font = list(size = 36, color = paste0("#", NAVY))),
        delta = list(reference = base, increasing = list(color = paste0("#", GREEN)),
                     decreasing = list(color = "#C0392B")),
        gauge = list(
          axis = list(range = list(0, 1), tickcolor = paste0("#", NAVY)),
          bar   = list(color = paste0("#", TEAL)),
          steps = list(
            list(range = c(0, 0.33), color = "#FFEBEE"),
            list(range = c(0.33, 0.67), color = "#FFF9C4"),
            list(range = c(0.67, 1),    color = "#E8F5E9")
          ),
          threshold = list(line = list(color = paste0("#", AMBER), width = 4),
                           thickness = 0.85, value = base)
        ),
        title = list(text = paste0("Predicted: ", pred,
                                   "  <span style='color:", d_col, "'>Δ", d_lbl, "</span>",
                                   "<br><span style='font-size:10px;color:#888'>Amber line = data mean (", base, ")</span>"),
                     font = list(size = 12))
      ) |> layout(margin = list(t = 60, b = 10, l = 30, r = 30),
                  paper_bgcolor = "white")
    })

    # ── Contribution bar chart ────────────────────────────────────────
    output$sim_contrib_plot <- renderPlotly({
      req(sim_prediction())
      sp   <- sim_prediction()
      ctb  <- sort(sp$contribs)
      cols <- ifelse(ctb >= 0, paste0("#", GREEN), "#C0392B")

      plot_ly(x = ctb, y = names(ctb), type = "bar", orientation = "h",
              marker = list(color = cols),
              hovertemplate = "<b>%{y}</b><br>Contribution: %{x:.4f}<extra></extra>") |>
        layout(
          title = list(text = "Per-Condition Contributions", font = list(color = paste0("#", NAVY), size = 12)),
          xaxis = list(title = "Impact on predicted outcome\n(vs. replacing with mean)",
                       zeroline = TRUE, zerolinecolor = "#CCC"),
          yaxis = list(title = NULL),
          plot_bgcolor = "white", paper_bgcolor = "white",
          margin = list(l = 110, t = 40, b = 40)
        )
    })

    # ── XY Plot: hypothetical case vs. real data ──────────────────────
    output$sim_xy_plot <- renderPlotly({
      req(sim_prediction(), fsqca_result())
      sp  <- sim_prediction()
      res <- fsqca_result()
      trms <- get_sol_terms(res$y$sol)

      cd   <- res$cd
      outc <- res$outc

      # Real cases: solution membership vs outcome
      sol_mem_real <- if (!is.null(trms)) compute_sol_mem(trms, cd) else rep(0.5, nrow(cd))
      real_y       <- as.numeric(cd[[outc]])

      # Hypothetical case membership
      hyp_vals <- sp$vals
      hyp_row  <- as.data.frame(as.list(hyp_vals))
      sol_mem_hyp <- if (!is.null(trms)) compute_sol_mem(trms, hyp_row) else 0.5
      hyp_y <- sp$pred

      fig <- plot_ly() |>
        # Real cases
        add_trace(x = sol_mem_real, y = real_y, type = "scatter", mode = "markers",
                  marker = list(color = paste0("#", NAVY), opacity = 0.45, size = 7,
                                line = list(color = "white", width = 0.5)),
                  name = "Real cases",
                  hovertemplate = "Sol. membership: %{x:.3f}<br>Outcome: %{y:.3f}<extra>Real case</extra>") |>
        # Hypothetical case
        add_trace(x = sol_mem_hyp, y = hyp_y, type = "scatter", mode = "markers",
                  marker = list(color = paste0("#", AMBER), size = 18, symbol = "star",
                                line = list(color = paste0("#", NAVY), width = 2)),
                  name = "Hypothetical case",
                  hovertemplate = paste0("Sol. membership: ", round(sol_mem_hyp, 3),
                                         "<br>Predicted outcome: ", round(hyp_y, 3),
                                         "<extra>★ Hypothetical</extra>")) |>
        # Crossover lines
        add_segments(x = 0.5, xend = 0.5, y = 0, yend = 1,
                     line = list(color = "#CCC", dash = "dot", width = 1.5),
                     showlegend = FALSE) |>
        add_segments(x = 0, xend = 1, y = 0.5, yend = 0.5,
                     line = list(color = "#CCC", dash = "dot", width = 1.5),
                     showlegend = FALSE) |>
        layout(
          title = list(
            text = "XY Plot — Solution Membership vs. Outcome<br><sup>★ = your hypothetical case  |  dashed lines = 0.5 crossover</sup>",
            font = list(color = paste0("#", NAVY), size = 13)),
          xaxis = list(title = "Solution Membership (fuzzy set score)", range = c(-0.05, 1.05), zeroline = FALSE),
          yaxis = list(title = paste0("Outcome — ", outc), range = c(-0.05, 1.05), zeroline = FALSE),
          plot_bgcolor = "white", paper_bgcolor = "white",
          legend = list(orientation = "h", y = -0.15)
        )
      fig
    })

    # ── fsQCA path membership text ────────────────────────────────────
    output$sim_qca_text <- renderText({
      req(sim_prediction(), fsqca_result())
      sp   <- sim_prediction()
      res  <- fsqca_result()
      trms <- get_sol_terms(res$y$sol)
      if (is.null(trms)) return("Run fsQCA first to see solution path membership.")

      hyp_row <- as.data.frame(as.list(sp$vals))

      lines <- c("── Solution Path Membership ──", "")
      for (term in trms) {
        mem <- compute_sol_mem(term, hyp_row)
        status <- if (mem >= 0.5) "✅ MEMBER" else if (mem >= 0.3) "⚡ Marginal" else "❌ Non-member"
        lines <- c(lines, sprintf("Path: %-30s", term),
                          sprintf("  Membership: %.3f  %s", mem, status), "")
      }

      # Overall solution membership
      overall <- compute_sol_mem(trms, hyp_row)
      lines <- c(lines,
        "── Necessity Assessment ──", "")
      for (cx in res$conds) {
        cv  <- sp$vals[[cx]]
        y_v <- sp$pred
        nc  <- nc_direct(cv, y_v)
        lines <- c(lines,
          sprintf("%-20s  Consist=%.3f  RoN=%.3f  Cov=%.3f",
                  cx, nc["Consistency"], nc["RoN"], nc["Coverage"]))
      }
      paste(lines, collapse = "\n")
    })

    # ── Mathematical summary ──────────────────────────────────────────
    output$sim_math_text <- renderText({
      req(sim_prediction(), fsqca_result())
      sp   <- sim_prediction()
      res  <- fsqca_result()

      lines <- c("── Hypothetical Case Values ──", "")
      for (cx in names(sp$vals))
        lines <- c(lines, sprintf("%-20s  %.4f", cx, sp$vals[[cx]]))

      lines <- c(lines, "",
        "── Prediction ──",
        sprintf("  ML Predicted Outcome : %.4f", sp$pred),
        sprintf("  Data Mean (baseline) : %.4f", sp$baseline),
        sprintf("  Delta vs. baseline   : %+.4f", sp$pred - sp$baseline),
        "",
        "── Top Condition Effects ──")

      ctb_sorted <- sort(abs(sp$contribs), decreasing = TRUE)
      for (cx in names(ctb_sorted)) {
        dir <- if (sp$contribs[[cx]] >= 0) "↑ pushes UP  " else "↓ pushes DOWN"
        lines <- c(lines,
          sprintf("  %-20s %s  Δ = %+.4f", cx, dir, sp$contribs[[cx]]))
      }

      # Sufficiency check for each individual condition
      lines <- c(lines, "", "── Single-Condition Sufficiency ──")
      for (cx in res$conds) {
        cv <- sp$vals[[cx]]
        sf <- suff_direct(cv, sp$pred)
        lines <- c(lines, sprintf("  %-20s  Consist=%.3f  Cov=%.3f", cx, sf["Consistency"], sf["Coverage"]))
      }

      paste(lines, collapse = "\n")
    })

    # ══════════════════════════════════════════════════════════════════
    #  VENN DIAGRAM OUTPUTS
    # ══════════════════════════════════════════════════════════════════

    # Dynamic selectors for Venn conditions
    output$venn_cond1_ui <- renderUI({
      req(fsqca_result())
      conds <- fsqca_result()$conds
      selectInput(ns("venn_c1"), "Condition 1", choices=conds, selected=conds[1])
    })
    output$venn_cond2_ui <- renderUI({
      req(fsqca_result())
      conds <- fsqca_result()$conds
      selectInput(ns("venn_c2"), "Condition 2", choices=conds,
                  selected=if(length(conds)>=2) conds[2] else conds[1])
    })
    output$venn_cond3_ui <- renderUI({
      req(fsqca_result())
      conds <- fsqca_result()$conds
      selectInput(ns("venn_c3"), "Condition 3 (optional)", choices=conds,
                  selected=if(length(conds)>=3) conds[3] else conds[1])
    })

    venn_data <- eventReactive(input$run_venn, {
      req(fsqca_result(), input$venn_c1, input$venn_c2)
      cd <- fsqca_result()$cd_sub
      c1 <- input$venn_c1; c2 <- input$venn_c2
      use3 <- isTRUE(input$venn_3set) && !is.null(input$venn_c3)
      c3   <- if (use3) input$venn_c3 else NULL

      A <- as.numeric(cd[[c1]]) > 0.5
      B <- as.numeric(cd[[c2]]) > 0.5
      n  <- nrow(cd)

      if (!use3) {
        list(mode=2, labels=c(c1,c2), n=n,
             A_only = sum( A & !B, na.rm=TRUE),
             B_only = sum(!A &  B, na.rm=TRUE),
             AB     = sum( A &  B, na.rm=TRUE),
             none   = sum(!A & !B, na.rm=TRUE))
      } else {
        C <- as.numeric(cd[[c3]]) > 0.5
        list(mode=3, labels=c(c1,c2,c3), n=n,
             A_only   = sum( A & !B & !C, na.rm=TRUE),
             B_only   = sum(!A &  B & !C, na.rm=TRUE),
             C_only   = sum(!A & !B &  C, na.rm=TRUE),
             AB_only  = sum( A &  B & !C, na.rm=TRUE),
             AC_only  = sum( A & !B &  C, na.rm=TRUE),
             BC_only  = sum(!A &  B &  C, na.rm=TRUE),
             ABC      = sum( A &  B &  C, na.rm=TRUE),
             none     = sum(!A & !B & !C, na.rm=TRUE))
      }
    })

    output$venn_plot <- renderPlotly({
      req(venn_data())
      vd <- venn_data()

      if (vd$mode == 2) {
        # 2-set Venn using plotly shapes
        lbs <- vd$labels
        plotly::plot_ly() |>
          plotly::layout(
            title=list(text=paste("Venn Diagram:", lbs[1], "vs", lbs[2]),
                       font=list(size=14, color="#1A3A5C")),
            shapes=list(
              list(type="circle", xref="x", yref="y",
                   x0=-2.2, y0=-1.6, x1=1.4, y1=1.6,
                   fillcolor="rgba(66,133,244,0.30)",
                   line=list(color="#4285F4", width=3)),
              list(type="circle", xref="x", yref="y",
                   x0=-0.4, y0=-1.6, x1=3.2, y1=1.6,
                   fillcolor="rgba(234,67,53,0.30)",
                   line=list(color="#EA4335", width=3))
            ),
            annotations=list(
              list(x=-1.8, y=1.9, text=paste0("<b>", lbs[1], "</b>"),
                   showarrow=FALSE, font=list(size=13, color="#1565C0")),
              list(x=2.8,  y=1.9, text=paste0("<b>", lbs[2], "</b>"),
                   showarrow=FALSE, font=list(size=13, color="#C62828")),
              list(x=-1.4, y=0, text=paste0("<b>", vd$A_only, "</b><br>only ", lbs[1]),
                   showarrow=FALSE, font=list(size=12)),
              list(x=2.3,  y=0, text=paste0("<b>", vd$B_only, "</b><br>only ", lbs[2]),
                   showarrow=FALSE, font=list(size=12)),
              list(x=0.5,  y=0, text=paste0("<b>", vd$AB, "</b><br>both"),
                   showarrow=FALSE, font=list(size=12, color="#1A3A5C")),
              list(x=0.5, y=-2.2,
                   text=paste0("Neither: <b>", vd$none, "</b>  |  Total N = ", vd$n),
                   showarrow=FALSE, font=list(size=11, color="#555"))
            ),
            xaxis=list(range=c(-3.5, 4.5), visible=FALSE, zeroline=FALSE),
            yaxis=list(range=c(-2.8, 2.8), visible=FALSE, zeroline=FALSE, scaleanchor="x"),
            paper_bgcolor="white", plot_bgcolor="white",
            margin=list(l=20, r=20, t=50, b=20))
      } else {
        # 3-set Venn — triangular arrangement
        lbs <- vd$labels
        plotly::plot_ly() |>
          plotly::layout(
            title=list(text=paste("3-Set Venn:", paste(lbs, collapse=" / ")),
                       font=list(size=14, color="#1A3A5C")),
            shapes=list(
              list(type="circle", xref="x", yref="y",
                   x0=-2, y0=-0.3, x1=1.5, y1=3.2,
                   fillcolor="rgba(66,133,244,0.25)",
                   line=list(color="#4285F4", width=3)),
              list(type="circle", xref="x", yref="y",
                   x0=0,  y0=-0.3, x1=3.5, y1=3.2,
                   fillcolor="rgba(234,67,53,0.25)",
                   line=list(color="#EA4335", width=3)),
              list(type="circle", xref="x", yref="y",
                   x0=-1, y0=-2.0, x1=2.5, y1=1.3,
                   fillcolor="rgba(52,168,83,0.25)",
                   line=list(color="#34A853", width=3))
            ),
            annotations=list(
              list(x=-1.5, y=3.4, text=paste0("<b>",lbs[1],"</b><br>only: ",vd$A_only),
                   showarrow=FALSE, font=list(size=11,color="#1565C0")),
              list(x=3.0,  y=3.4, text=paste0("<b>",lbs[2],"</b><br>only: ",vd$B_only),
                   showarrow=FALSE, font=list(size=11,color="#C62828")),
              list(x=0.75, y=-2.4, text=paste0("<b>",lbs[3],"</b><br>only: ",vd$C_only),
                   showarrow=FALSE, font=list(size=11,color="#1E6438")),
              list(x=0.75, y=2.7, text=paste0(lbs[1],"∩",lbs[2],": <b>",vd$AB_only,"</b>"),
                   showarrow=FALSE, font=list(size=10)),
              list(x=-0.6, y=0.5, text=paste0(lbs[1],"∩",lbs[3],": <b>",vd$AC_only,"</b>"),
                   showarrow=FALSE, font=list(size=10)),
              list(x=2.1,  y=0.5, text=paste0(lbs[2],"∩",lbs[3],": <b>",vd$BC_only,"</b>"),
                   showarrow=FALSE, font=list(size=10)),
              list(x=0.75, y=1.4, text=paste0("A∩B∩C: <b>",vd$ABC,"</b>"),
                   showarrow=FALSE, font=list(size=11,color="#1A3A5C")),
              list(x=0.75, y=-3.0,
                   text=paste0("Neither: <b>",vd$none,"</b>  |  Total N = ",vd$n),
                   showarrow=FALSE, font=list(size=11,color="#555"))
            ),
            xaxis=list(range=c(-3,5), visible=FALSE, zeroline=FALSE),
            yaxis=list(range=c(-3.5,4.5), visible=FALSE, zeroline=FALSE, scaleanchor="x"),
            paper_bgcolor="white", plot_bgcolor="white",
            margin=list(l=20,r=20,t=50,b=20))
      }
    })

    output$venn_counts_tbl <- DT::renderDataTable({
      req(venn_data())
      vd <- venn_data()
      if (vd$mode == 2) {
        lbs <- vd$labels
        df <- data.frame(
          Region      = c(paste("Only", lbs[1]),
                          paste("Only", lbs[2]),
                          paste(lbs[1], "AND", lbs[2]),
                          "Neither"),
          Cases       = c(vd$A_only, vd$B_only, vd$AB, vd$none),
          `% of N`    = round(c(vd$A_only, vd$B_only, vd$AB, vd$none)/vd$n*100, 1),
          check.names = FALSE
        )
      } else {
        lbs <- vd$labels
        df <- data.frame(
          Region      = c(paste("Only", lbs[1]),
                          paste("Only", lbs[2]),
                          paste("Only", lbs[3]),
                          paste(lbs[1],"∩",lbs[2],"only"),
                          paste(lbs[1],"∩",lbs[3],"only"),
                          paste(lbs[2],"∩",lbs[3],"only"),
                          paste(lbs[1],"∩",lbs[2],"∩",lbs[3]),
                          "Neither"),
          Cases       = c(vd$A_only, vd$B_only, vd$C_only,
                          vd$AB_only, vd$AC_only, vd$BC_only,
                          vd$ABC, vd$none),
          check.names = FALSE
        )
        df$`% of N` <- round(df$Cases/vd$n*100, 1)
      }
      tool_dt(df, "Set Membership Counts (membership > 0.5 = 'in set')")
    })

    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r13 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k13 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Truth Table
    observeEvent(input$ai_btn_tt, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      ctx <- tryCatch({
        res <- fsqca_result(); req(res)
        tt <- res$tt
        paste0("fsQCA TRUTH TABLE\nOutcome: ", input$outcome_var,
               "\nConditions: ", paste(input$condition_vars, collapse=", "),
               "\nConsistency threshold: ", input$consist_thresh,
               "\nFrequency threshold: ", input$freq_thresh,
               "\n\nTruth Table:\n", paste(capture.output(print(tt)), collapse="\n"))
      }, error=function(e) "Please run fsQCA first.")
      output$ai_output <- renderUI({ .ai_r13(call_gemini(paste0(
        "You are an expert in fuzzy-set Qualitative Comparative Analysis writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of the fsQCA truth table. Include:\n",
        "1. Explain what a truth table represents: all 2^k logical combinations of conditions\n",
        "2. Report how many rows meet the frequency threshold vs logical remainders\n",
        "3. Identify rows with high consistency (> .80) that are included in the solution\n",
        "4. Explain the consistency threshold choice and its theoretical justification\n",
        "5. Discuss cases in each high-consistency row — what do they have in common?\n",
        "6. Identify any contradictory rows (same configuration, different outcomes) and how they're handled\n",
        "7. Discuss the diversity of configurations covered (empirical coverage)\n",
        "8. Write APA-style Results paragraph for truth table analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Solution Terms
    observeEvent(input$ai_btn_sol, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      ctx <- tryCatch({
        res <- fsqca_result(); req(res)
        sol <- res$solution
        paste0("fsQCA SOLUTION TERMS\nOutcome: ", input$outcome_var,
               "\n\nSolutions:\n", paste(capture.output(print(sol)), collapse="\n"))
      }, error=function(e) "Please run fsQCA first.")
      output$ai_output <- renderUI({ .ai_r13(call_gemini(paste0(
        "You are an expert in fsQCA and configurational analysis writing for a top-tier management journal.\n\n",
        "Task: DETAILED interpretation of fsQCA solution terms. Include:\n",
        "1. Distinguish conservative, parsimonious, and intermediate solutions — which is reported and why\n",
        "2. For each solution term (configuration): report coverage and consistency\n",
        "   - Coverage: proportion of outcome explained by this path (like R²)\n",
        "   - Consistency: degree to which cases sharing this configuration also share the outcome (like precision)\n",
        "3. Report overall solution coverage and consistency\n",
        "4. Apply Ragin (2008) benchmarks: consistency ≥ .80 (mandatory); coverage ≥ .25 (acceptable)\n",
        "5. Interpret each configuration substantively: what combination of conditions leads to the outcome?\n",
        "6. Identify INUS conditions (Insufficient but Necessary parts of Unnecessary but Sufficient conditions)\n",
        "7. Compare configurations: are there multiple equifinal paths to the outcome?\n",
        "8. Write complete APA-style Results paragraph using Ragin (2008) and Fiss (2011) notation\n\n",
        "Formal academic prose (6-7 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Necessary Conditions
    observeEvent(input$ai_btn_nec, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      ctx <- tryCatch({
        res <- fsqca_result(); req(res)
        nec <- tryCatch(res$necessary, error=function(e) NULL)
        nec_txt <- if (!is.null(nec)) paste(capture.output(print(nec)), collapse="\n") else "Run necessary conditions analysis"
        paste0("NECESSARY CONDITIONS ANALYSIS\nOutcome: ", input$outcome_var,
               "\n\nNecessary Conditions:\n", nec_txt)
      }, error=function(e) "Please run fsQCA first.")
      output$ai_output <- renderUI({ .ai_r13(call_gemini(paste0(
        "You are an expert in fsQCA and configurational theory writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of necessary conditions analysis. Include:\n",
        "1. Define necessary conditions: a condition is necessary if it is present in ALL cases of the outcome\n",
        "2. Apply the Coverage and Consistency thresholds for necessity: consistency > .90 required\n",
        "3. For each tested condition: report consistency and coverage of necessity\n",
        "4. Discuss trivial vs non-trivial necessary conditions (coverage distinguishes them)\n",
        "5. Report SUIN conditions (Sufficient but Unnecessary parts of Insufficient but Necessary conditions)\n",
        "6. Discuss the theoretical meaning of necessary conditions in the causal model\n",
        "7. Contrast with sufficient conditions (solution terms)\n",
        "8. Write APA-style Results paragraph for necessary conditions following Schneider & Wagemann (2012)\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Robustness
    observeEvent(input$ai_btn_robust, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      ctx <- tryCatch({
        rr <- tryCatch(robust_res(), error=function(e) NULL)
        br <- tryCatch(boot_res(), error=function(e) NULL)
        parts <- character(0)
        if (!is.null(rr)) parts <- c(parts, paste0("Robustness:\n", paste(head(capture.output(print(rr)),30), collapse="\n")))
        if (!is.null(br)) parts <- c(parts, paste0("Bootstrap CI:\n", paste(head(capture.output(print(br)),30), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("ROBUSTNESS & BOOTSTRAP RESULTS\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run robustness checks first.")
      output$ai_output <- renderUI({ .ai_r13(call_gemini(paste0(
        "You are an expert in fsQCA robustness testing writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of fsQCA robustness checks. Include:\n",
        "1. Calibration robustness: does the solution change with ±5% threshold adjustments?\n",
        "2. Consistency threshold robustness: do results hold at .75, .80, and .85 thresholds?\n",
        "3. Bootstrap consistency CIs: do CIs exclude zero for all paths?\n",
        "4. Frequency threshold robustness: does solution change with different n cutoffs?\n",
        "5. Discuss what variations were found and whether they threaten core conclusions\n",
        "6. Assess overall solution stability across different analytical choices\n",
        "7. Follow Skaaning (2011) and Schneider & Wagemann (2012) robustness standards\n",
        "8. Write APA-style Results paragraph for robustness analysis\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 5. Full fsQCA Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      ctx <- tryCatch({
        res <- fsqca_result(); req(res)
        sol_txt <- paste(head(capture.output(print(res$solution)), 40), collapse="\n")
        tt_txt  <- paste(head(capture.output(print(res$tt)), 30), collapse="\n")
        paste0("fsQCA FULL ANALYSIS\nOutcome: ", input$outcome_var,
               "\nConditions: ", paste(input$condition_vars, collapse=", "),
               "\n\nTruth Table (top):\n", tt_txt,
               "\n\nSolution Terms:\n", sol_txt)
      }, error=function(e) "Please run fsQCA first.")
      output$ai_output <- renderUI({ .ai_r13(call_gemini(paste0(
        "You are an expert in fsQCA and configurational comparative methods writing for a top-tier management journal.\n\n",
        "Task: Write a COMPREHENSIVE fsQCA Results section. Include:\n",
        "1. Calibration description: how were conditions and outcome calibrated to fuzzy scores?\n",
        "2. Necessary conditions: which conditions are necessary? Consistency and coverage reported\n",
        "3. Truth table: number of rows, empirically relevant configurations, consistency threshold justification\n",
        "4. Solution terms: each configuration with coverage, consistency, case examples\n",
        "5. Solution types: conservative, parsimonious, intermediate — which is preferred and why\n",
        "6. Equifinality: multiple paths to the outcome — theoretical interpretation\n",
        "7. Robustness checks: stability across calibration and threshold variations\n",
        "8. Causal asymmetry: compare configurations for outcome presence vs absence\n\n",
        "Use APA 7 and Ragin (2008) standards. Formal academic prose (8-10 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k13()); return() }
      NULL # (use buttons above for specific analyses)
    }, ignoreInit = TRUE)

    # ── Truth Table Heatmap ───────────────────────────────────────────────
    output$tt_heatmap <- renderPlotly({
      req(fsqca_result())
      res <- fsqca_result()
      tryCatch({
        tt  <- res$y$tt
        if (is.null(tt)) return(plotly_empty() |> layout(title = "No truth table available"))

        # Extract condition columns + incl + OUT
        cond_cols <- res$conds
        keep_cols <- c(cond_cols, "n", "incl", "OUT")
        keep_cols <- intersect(keep_cols, colnames(tt))
        df <- as.data.frame(tt)[, keep_cols, drop = FALSE]

        # Sort: OUT=1 first, then by incl descending
        if ("OUT" %in% names(df)) {
          df <- df[order(-as.numeric(as.character(df$OUT)),
                         -as.numeric(as.character(df$incl %||% 0))), ]
        }

        # Create row labels (configuration strings)
        if (length(cond_cols) > 0) {
          row_labels <- apply(df[, cond_cols, drop = FALSE], 1, function(r) {
            parts <- mapply(function(nm, val) {
              v <- suppressWarnings(as.numeric(as.character(val)))
              if (!is.na(v) && v >= 0.5) nm else paste0("~", nm)
            }, cond_cols, r)
            paste(parts, collapse = "·")
          })
        } else {
          row_labels <- paste0("Config ", seq_len(nrow(df)))
        }
        row_labels <- factor(row_labels, levels = rev(row_labels))

        # Build z matrix: conditions (0/1) + incl + OUT
        num_df <- as.data.frame(lapply(df, function(x) suppressWarnings(as.numeric(as.character(x)))))
        num_df[is.na(num_df)] <- 0

        z_mat  <- t(as.matrix(num_df))
        x_vals <- levels(row_labels)
        y_vals <- rownames(z_mat)

        # Custom colorscale: 0 = light blue, 0.5 = white, 1 = NAVY
        colorscale_ht <- list(
          c(0,   "#EBF4F7"),
          c(0.5, "#AACFE4"),
          c(1,   NAVY)
        )

        hover_txt <- matrix(
          paste0(rep(y_vals, each = ncol(z_mat)), " = ",
                 round(as.vector(t(z_mat)), 3),
                 "<br>Config: ", rep(x_vals, times = nrow(z_mat))),
          nrow = nrow(z_mat), ncol = ncol(z_mat)
        )

        plot_ly(
          x         = x_vals,
          y         = y_vals,
          z         = z_mat,
          type      = "heatmap",
          colorscale = colorscale_ht,
          text      = hover_txt,
          hovertemplate = "%{text}<extra></extra>",
          showscale = TRUE
        ) |>
          layout(
            title = list(
              text = paste0("Truth Table Heatmap — ", res$outc,
                            " (", nrow(df), " configurations)"),
              font = list(color = NAVY, size = 14)
            ),
            xaxis = list(title = "Configuration", tickangle = -45,
                         tickfont = list(size = 9)),
            yaxis = list(title = ""),
            plot_bgcolor  = "white",
            paper_bgcolor = "white",
            margin = list(b = 120)
          )
      }, error = function(e) {
        plotly_empty() |> layout(title = paste("Error:", e$message))
      })
    })

    # ── Coverage/Consistency Dashboard ────────────────────────────────────
    output$coverage_dashboard <- renderPlotly({
      req(fsqca_result())
      res <- fsqca_result()
      tryCatch({
        sol <- res$y$sol
        if (is.null(sol)) return(plotly_empty() |> layout(title = "No solution available"))

        # Extract fit metrics from solution
        fit_df <- tryCatch({
          pf <- sol$pof.model
          if (is.null(pf)) stop("No pof.model")
          rd <- as.data.frame(pf)
          rd
        }, error = function(e) {
          # Fallback: try solution inclusions
          tryCatch({
            pf2 <- sol$solution.list
            if (is.null(pf2)) stop("no sol list")
            # Build a basic metrics table
            terms <- get_sol_terms(sol)
            data.frame(Term = terms, incl = NA_real_, stringsAsFactors = FALSE)
          }, error = function(e2) NULL)
        })

        if (is.null(fit_df) || nrow(fit_df) == 0)
          return(plotly_empty() |> layout(title = "Solution fit metrics not available"))

        # Look for standard QCA column names
        metric_map <- list(
          Consistency  = c("incl", "inclS", "inclusion"),
          PRI          = c("PRI", "pri"),
          Raw_Coverage = c("cov.r", "covr", "coverage.r"),
          Unique_Cov   = c("cov.u", "covu", "coverage.u")
        )

        get_col <- function(df, candidates) {
          found <- intersect(candidates, colnames(df))
          if (length(found) == 0) return(rep(NA_real_, nrow(df)))
          suppressWarnings(as.numeric(as.character(df[[found[1]]])))
        }

        row_nms <- if (!is.null(rownames(fit_df))) rownames(fit_df) else paste0("Path ", seq_len(nrow(fit_df)))
        # Filter to individual paths + overall row
        row_nms <- make.unique(row_nms)

        metrics <- data.frame(
          Path        = row_nms,
          Consistency = get_col(fit_df, metric_map$Consistency),
          PRI         = get_col(fit_df, metric_map$PRI),
          Raw_Cov     = get_col(fit_df, metric_map$Raw_Coverage),
          Unique_Cov  = get_col(fit_df, metric_map$Unique_Cov),
          stringsAsFactors = FALSE
        )
        metrics <- metrics[!apply(is.na(metrics[,-1]), 1, all), ]
        if (nrow(metrics) == 0)
          return(plotly_empty() |> layout(title = "No fit metrics to display"))

        paths <- metrics$Path
        cols  <- c(NAVY, TEAL, AMBER, GREEN)
        metric_nms <- c("Consistency", "PRI", "Raw_Cov", "Unique_Cov")
        disp_nms   <- c("Consistency (incl)", "PRI", "Raw Coverage", "Unique Coverage")

        fig <- plot_ly()
        for (i in seq_along(metric_nms)) {
          vals <- metrics[[metric_nms[i]]]
          if (all(is.na(vals))) next
          fig <- fig |> add_bars(
            x    = paths,
            y    = vals,
            name = disp_nms[i],
            marker = list(color = cols[i], opacity = 0.85),
            hovertemplate = paste0(disp_nms[i], ": %{y:.3f}<br>Path: %{x}<extra></extra>")
          )
        }

        # Benchmark lines
        fig <- fig |>
          add_segments(x = -0.5, xend = length(paths) - 0.5,
                       y = 0.80, yend = 0.80,
                       line = list(color = "red", dash = "dash", width = 1.5),
                       name = "Consistency benchmark (0.80)",
                       showlegend = TRUE, hoverinfo = "skip") |>
          add_segments(x = -0.5, xend = length(paths) - 0.5,
                       y = 0.25, yend = 0.25,
                       line = list(color = AMBER, dash = "dash", width = 1.5),
                       name = "Coverage benchmark (0.25)",
                       showlegend = TRUE, hoverinfo = "skip") |>
          layout(
            barmode = "group",
            title   = list(
              text = paste0("Coverage & Consistency Dashboard — ", res$outc),
              font = list(color = NAVY, size = 14)
            ),
            xaxis  = list(title = "Solution Path", tickangle = -30),
            yaxis  = list(title = "Value", range = c(0, 1.05)),
            legend = list(orientation = "h", y = -0.2),
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          )
        fig
      }, error = function(e) {
        plotly_empty() |> layout(title = paste("Error:", e$message))
      })
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
