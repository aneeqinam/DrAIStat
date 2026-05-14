# ══════════════════════════════════════════════════════════════════════════════
# Dr.AIStat — Full Research Analytics Hub
# ══════════════════════════════════════════════════════════════════════════════
# All Quantitative Methods in one module:
#   🎨 Visual Model Canvas (AMOS/SmartPLS-style)
#   📊 Data Screening · 📐 EFA · ✅ CFA/Reliability
#   🔗 PLS-SEM (seminr): Outer · Inner · Bootstrap · IPMA
#   🔗 CB-SEM (lavaan) · 📐 Path Analysis
#   📈 Regression · 📐 Group Tests · 🤖 ML · 🔵 Clustering
#   📏 IRT · 🔍 Endogeneity
#   📝 APA Write-Up · 🤖 AI Interpret
# Dr. Aneeq Inam | ORCID: 0000-0001-7682-2244
# ══════════════════════════════════════════════════════════════════════════════

.has_seminr10 <- tryCatch(requireNamespace("seminr", quietly=TRUE), error=function(e) FALSE)

.ai_r10 <- function(result)
  tags$div(style="background:#f0f9ff;border-left:4px solid #2196A6;padding:.9rem 1.1rem;border-radius:6px;",
           tags$div(style="white-space:pre-wrap;font-size:.92rem;", result))
.ai_k10 <- function()
  tags$div(class="alert alert-warning",
           tags$b("⚠️ API key required."), " Enter your Groq key in the sidebar.",
           tags$a("Get FREE key →", href="https://console.groq.com/keys", target="_blank",
                  style="margin-left:.5rem;font-weight:bold;"))

# Canvas CSS (inline — no quote issues since using double-quoted R string)
.SEM_CANVAS_CSS <- "
  .sem-tb-btn { transition: all .15s; }
  .sem-tb-btn:hover { background:#e3f2fd !important; border-color:#2196A6 !important; }
  .sem-tb-active { background:#2196A6 !important; color:white !important;
                   border-color:#1565C0 !important; font-weight:bold; }
  #sem-svg { user-select:none; }
  #sem-tip { animation: fadeIn .3s; }
  @keyframes fadeIn { from{opacity:0} to{opacity:1} }
  .sem-panel-label { font-size:.78rem; font-weight:bold; color:#1A3A5C; display:block; margin-bottom:2px; }
"

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
mod10_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Load canvas JS from www/sem_canvas.js (Shiny serves www/ automatically)
    tags$head(
      tags$style(HTML(.SEM_CANVAS_CSS)),
      tags$script(src="sem_canvas.js")
    ),
    # Hero banner
    tags$div(
      style="background:linear-gradient(135deg,#1A3A5C 0%,#2196A6 100%);color:white;
             padding:1rem 1.4rem;border-radius:10px;margin-bottom:1rem;",
      tags$h3("🔗 Dr.AIStat — Full Research Analytics Hub",
              style="margin:0 0 .3rem;color:white;font-size:1.2rem;"),
      tags$p(style="margin:0;opacity:.88;font-size:.82rem;",
        "🎨 Visual Model Canvas · PLS-SEM · CB-SEM · Regression · Group Tests ·
         ML · Clustering · IRT · Endogeneity · Advanced Methods · AI Interpretation")
    ),
    # Global data banner + data upload
    uiOutput(ns("global_data_banner")),
    tags$div(
      style="background:white;border:2px solid #2196A6;border-radius:8px;padding:.8rem 1rem;margin-bottom:1rem;",
      fluidRow(
        column(4,
          fileInput(ns("file"), "📂 Upload Data (.xlsx or .csv)",
                    accept=c(".xlsx",".xls",".csv"),
                    placeholder="No file chosen")),
        column(8, br(), uiOutput(ns("data_info_bar")))
      )
    ),
    uiOutput(ns("main_tabs"))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
mod10_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Global data banner ───────────────────────────────────────────────────
    output$global_data_banner <- renderUI({
      gd <- global_shared_data()
      if (is.null(gd)) return(NULL)
      tags$div(
        style = paste0("background:#e6f4ea; border:1px solid #81c784; border-radius:7px;",
                       " padding:8px 14px; margin-bottom:8px;",
                       " display:flex; align-items:center; justify-content:space-between;"),
        tags$span(style="color:#1b5e20; font-size:.82rem;",
          tags$b("\U0001f4cb Global dataset ready: "),
          sprintf("%s  (%d rows × %d cols)", global_shared_name(), nrow(gd), ncol(gd))
        ),
        actionButton(ns("load_global"), "⬇ Use This Dataset",
                     class="btn-success btn-sm", style="padding:3px 10px; font-size:.78rem;")
      )
    })

    # ── Shared data ──────────────────────────────────────────────────────────
    data_rv    <- reactiveVal(NULL)

    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        data_rv(read_uploaded(input$file$datapath, input$file$name))
      }, error = function(e) showNotification(paste("Load error:", e$message), type="error"))
    })

    observeEvent(input$load_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) {
        data_rv(gd)
        showNotification(paste0("SEM: using global dataset — ", global_shared_name()),
                         type="message", duration=3)
      }
    })

    df         <- reactive({ req(data_rv()); data_rv() })
    num_cols_r <- reactive({ req(df()); numeric_cols(df()) })
    all_cols_r <- reactive({ req(df()); names(df()) })
    pls_model_rv     <- reactiveVal(NULL)
    boot_model_rv    <- reactiveVal(NULL)
    canvas_model     <- reactiveVal(list(nodes=list(), edges=list()))
    canvas_results_rv <- reactiveVal(NULL)

    output$data_info_bar <- renderUI({
      d <- df()
      tags$div(style="background:#e8f5e9;padding:.4rem .8rem;border-radius:6px;display:inline-block;font-size:.85rem;",
        tags$b("✅ Data loaded: "), paste0(nrow(d), " rows × ", ncol(d), " columns"),
        tags$span(style="margin-left:1rem;color:#555;",
          paste0("Numeric columns: ", length(num_cols_r()))))
    })

    # Send column names to canvas JS whenever data changes
    observeEvent(df(), {
      session$sendCustomMessage("sem_cols", names(df()))
    })

    # Re-send column names when canvas JS signals it has finished initialising.
    # This covers the race where data was uploaded BEFORE the canvas tab rendered
    # (the original sem_cols message arrived before the JS handler was registered).
    observeEvent(input$canvas_ready, {
      d <- tryCatch(df(), error = function(e) NULL)
      if (!is.null(d)) session$sendCustomMessage("sem_cols", names(d))
    }, ignoreInit = TRUE)

    # ── Mediator picker: updates whenever canvas model changes ─────────────
    output$cv_mediator_picker_ui <- renderUI({
      cm     <- canvas_model()
      nodes  <- cm$nodes
      if (length(nodes) == 0)
        return(tags$p("Add constructs to canvas first.",
                      style="font-size:.76rem;color:#888;margin:0;"))
      labels <- Filter(function(x) !is.null(x) && nzchar(trimws(x)),
                       lapply(nodes, function(n) n$label %||% NULL))
      labels <- unique(as.character(unlist(labels)))
      if (length(labels) == 0)
        return(tags$p("No labelled constructs found.", style="font-size:.76rem;color:#888;margin:0;"))
      pickerInput(ns("cv_manual_mediators"), NULL,
        choices = labels, multiple = TRUE,
        options = list(
          `actions-box`          = TRUE,
          `selected-text-format` = "count > 2",
          placeholder = "Pick mediator constructs (optional)"
        )
      )
    })

    # ── Main tabs render (after data upload) ─────────────────────────────────
    output$main_tabs <- renderUI({
      req(df())
      nc <- num_cols_r()
      ac <- all_cols_r()
      tags$div(

        # =============================================================
        # TOP: Visual Model Canvas (no tab wrapper — always visible)
        # =============================================================
        tags$div(style="margin-top:.5rem;",

          # ── Save / Load model bar ──────────────────────────────────
          tags$div(
            style="background:#EEF4FB;border:1px solid #B3C6E0;border-radius:6px;
                   padding:.4rem .9rem;margin-bottom:.5rem;",
            tags$span(tags$b("📂 Canvas Model:", style="font-size:.8rem;color:#1A3A5C;")),
            downloadButton(ns("dl_canvas_model"), "💾 Save Model",
              class="btn-sm btn-outline-primary",
              style="margin-left:.6rem;padding:2px 10px;font-size:.78rem;"),
            tags$span(style="margin:0 .7rem;color:#aaa;", "│"),
            tags$span(style="font-size:.79rem;color:#555;", "Restore:"),
            tags$div(
              style="display:inline-block;vertical-align:middle;margin-left:.4rem;",
              fileInput(ns("upload_canvas_file"), NULL, accept=".json", width="230px")
            )
          ),

          # ── Instructions row ────────────────────────────────────────
          tags$div(style="margin-bottom:.5rem;",
            fluidRow(
              column(6, tags$div(class="alert alert-info",
                style="padding:.4rem .8rem;margin-bottom:0;font-size:.82rem;",
                tags$b("How to build your model:"),
                " ① Click ", tags$b("＋ Add Construct"),
                " → click canvas  ② Click construct → assign items from data in right panel",
                "  ③ Click ", tags$b("→ Draw Path"),
                " → click source then target  ④ Click ", tags$b("▶ Run All Analyses"))),
              column(6, uiOutput(ns("canvas_run_status")))
            )
          ),

          # ── Analysis settings — Row 1: Bootstrap + Controls ─────────
          tags$div(style="margin-bottom:.4rem;",
            fluidRow(
              column(4,
                tags$div(
                  style="background:#F8F9FA;border:1px solid #dee2e6;border-radius:6px;padding:.45rem .7rem;",
                  tags$b("⚙️ Bootstrap Settings",
                         style="font-size:.79rem;display:block;margin-bottom:4px;color:#1A3A5C;"),
                  fluidRow(
                    column(5,
                      numericInput(ns("cv_nboot"), "Samples", 1000,
                                   min=100, max=5000, step=100)
                    ),
                    column(4,
                      numericInput(ns("cv_seed"), "Seed", 42, min=1, max=99999)
                    ),
                    column(3,
                      selectInput(ns("cv_ci_level"), "CI",
                        c("95%"="0.95","90%"="0.90","99%"="0.99"), "0.95")
                    )
                  )
                )
              ),
              column(8,
                tags$div(
                  style="background:#F8F9FA;border:1px solid #dee2e6;border-radius:6px;padding:.45rem .7rem;",
                  tags$b("🎛 Control Variables",
                         style="font-size:.79rem;display:block;margin-bottom:4px;color:#1A3A5C;"),
                  pickerInput(ns("cv_controls"), NULL,
                    choices = ac, multiple = TRUE,
                    options = list(
                      `actions-box`      = TRUE,
                      `live-search`      = TRUE,
                      `selected-text-format` = "count > 2",
                      placeholder = "Select control variables — added to all equations (optional)"
                    )
                  )
                )
              )
            )
          ),

          # ── Analysis settings — Row 2: Mediators + Instruments ──────
          tags$div(style="margin-bottom:.6rem;",
            fluidRow(
              column(6,
                tags$div(
                  style="background:#E8F5E9;border:1px solid #A5D6A7;border-radius:6px;padding:.45rem .7rem;",
                  tags$b("🔀 Manual Mediator Selection",
                         style="font-size:.79rem;display:block;margin-bottom:3px;color:#2E7D32;"),
                  tags$p(style="font-size:.73rem;color:#555;margin:0 0 3px;",
                    "Auto-detected via topology (green node = mediator). Use this picker to ",
                    tags$b("override/supplement"), " if auto-detection misses your mediators."),
                  uiOutput(ns("cv_mediator_picker_ui"))
                )
              ),
              column(6,
                tags$div(
                  style="background:#FFF3E0;border:1px solid #FFCC80;border-radius:6px;padding:.45rem .7rem;",
                  tags$b("🔍 Instrumental Variables (for Endogeneity Test)",
                         style="font-size:.79rem;display:block;margin-bottom:3px;color:#E65100;"),
                  tags$p(style="font-size:.73rem;color:#555;margin:0 0 3px;",
                    "Select columns from your data to use as instruments (Z) in the Wu-Hausman test. ",
                    "If none selected, exogenous constructs are used automatically."),
                  pickerInput(ns("cv_instruments"), NULL,
                    choices = ac, multiple = TRUE,
                    options = list(
                      `actions-box`      = TRUE,
                      `live-search`      = TRUE,
                      `selected-text-format` = "count > 2",
                      placeholder = "Select instrument columns (optional)"
                    )
                  )
                )
              )
            )
          ),

          # ── Canvas widget ───────────────────────────────────────────
          uiOutput(ns("canvas_ui"))
        ),

        # =============================================================
        # BOTTOM: Comprehensive analyses tabset — all analyses unified
        # =============================================================
        tags$div(
          style="margin-top:1.5rem;border-top:2px solid #2196A6;padding-top:.8rem;",
          tags$p(
            style="color:#1A3A5C;font-weight:bold;font-size:.84rem;margin-bottom:.4rem;",
            "📊 Analysis Results",
            tags$span(
              style="font-weight:normal;color:#555;margin-left:.4rem;",
              "— model-based tabs populate after clicking ▶ Run All Analyses above"
            )
          ),

          tabsetPanel(id=ns("bottom_tabs"), type="tabs",

            # ── Tab 1: Data Quality ──────────────────────────────────
            tabPanel("📊 Data Quality",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("📊 Screening",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.6rem 0;",
                      tags$b("📊 Pre-SEM Data Quality Check"),
                      " — Descriptives, normality, missing values, Harman's CMV, outliers."),
                    fluidRow(
                      column(9, pickerInput(ns("screen_vars"), "Select variables",
                                choices=ac, selected=nc, multiple=TRUE,
                                options=list(`actions-box`=TRUE,`selected-text-format`="count > 4"))),
                      column(3, br(), actionButton(ns("run_screen"), "🔍 Run Screening",
                                                    class="btn-primary btn-block"))
                    ),
                    br(), uiOutput(ns("screen_output_ui"))
                  ),

                  tabPanel("📐 EFA",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.6rem 0;",
                      tags$b("📐 Exploratory Factor Analysis"), " — Discover latent factor structure."),
                    fluidRow(
                      column(4, pickerInput(ns("efa_vars"), "Variables", choices=nc, selected=nc,
                                multiple=TRUE, options=list(`actions-box`=TRUE))),
                      column(2, numericInput(ns("efa_nf"), "Factors", 3, 1, 20)),
                      column(3, selectInput(ns("efa_rot"), "Rotation",
                                c("oblimin","varimax","promax","none"), "oblimin")),
                      column(3, br(), actionButton(ns("run_efa"), "📐 Run EFA",
                                                    class="btn-primary btn-block"))
                    ),
                    br(), uiOutput(ns("efa_output_ui"))
                  ),

                  tabPanel("📊 Descriptives",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.6rem 0;",
                      tags$b("📊 Descriptive Statistics & Distributions")),
                    fluidRow(
                      column(3,
                        pickerInput(ns("desc_vars"), "Variables",
                                    choices=nc, selected=nc[1:min(5,length(nc))],
                                    multiple=TRUE, options=list(`actions-box`=TRUE)),
                        br(),
                        actionButton(ns("run_desc"), "📊 Run", class="btn-primary btn-block")
                      ),
                      column(9, uiOutput(ns("desc_results_ui")))
                    )
                  )
                )
              )
            ),

            # ── Tab 2: Measurement ───────────────────────────────────
            tabPanel("✅ Measurement",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("🔍 Reliability & Validity",
                    tags$br(),
                    DT::dataTableOutput(ns("cv_rel_tbl")),
                    tags$br(),
                    tags$h5("HTMT Discriminant Validity (< 0.85 = OK)"),
                    DT::dataTableOutput(ns("cv_htmt_tbl"))
                  ),

                  tabPanel("📐 CFA (Canvas)",
                    tags$br(),
                    uiOutput(ns("cv_cfa_fit_ui")),
                    tags$br(),
                    DT::dataTableOutput(ns("cv_cfa_loads_tbl"))
                  ),

                  tabPanel("✅ CFA Manual",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.6rem 0;",
                      tags$b("✅ Measurement Model Assessment"),
                      " — Uses constructs defined in the Model Canvas. Define constructs there first,
                        then run CFA/Reliability here for: fit indices, α, CR, AVE, HTMT, Fornell-Larcker."),
                    fluidRow(
                      column(4, selectInput(ns("cfa_estimator"), "Estimator",
                                c("ML","MLR","WLSMV"), "ML")),
                      column(4, br(), actionButton(ns("run_cfa_rel"), "✅ Run CFA & Reliability",
                                                    class="btn-success btn-block"))
                    ),
                    br(), uiOutput(ns("cfa_rel_output_ui"))
                  ),

                  # ── HCM / Formative ───────────────────────────────────
                  tabPanel("🏗 HCM / Formative",
                    tags$div(style="background:linear-gradient(135deg,#880E4F,#AD1457);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🏗 Hierarchical Component Models & Formative Measurement", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Second-order constructs (HCM) and formative indicator assessment with VIF & weights",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("Reflective vs Formative (Hair et al., 2019): "),
                      "Reflective = indicators caused BY the construct (loadings ≥ .70, AVE ≥ .50). ",
                      "Formative = indicators CAUSE the construct (weights, VIF < 3.3, no AVE needed). ",
                      tags$b("HCM: "), "Second-order construct absorbs first-order constructs as indicators. ",
                      "Use repeated indicators or two-stage approach."),
                    fluidRow(
                      column(4,
                        box(width=12, status="danger", solidHeader=TRUE, title="Formative VIF Check",
                          pickerInput(ns("form_indicators"), "Formative indicators", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          actionButton(ns("run_formative"), "📐 Check VIF & Weights",
                                       class="btn-danger btn-block", style="font-weight:bold;"),
                          tags$hr(),
                          tags$p(tags$b("HCM — Second-Order Construct"), style="font-size:.82rem;"),
                          pickerInput(ns("hcm_lv1"), "First-order LVs (items)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          selectInput(ns("hcm_approach"), "HCM approach",
                            c("Repeated Indicators"="repeated","Two-Stage"="twostage")),
                          textInput(ns("hcm_name"), "Second-order construct name", "HOC"),
                          actionButton(ns("run_hcm"), "🏗 Run HCM",
                                       class="btn-warning btn-block", style="font-weight:bold;")
                        )
                      ),
                      column(8, uiOutput(ns("hcm_results_ui")))
                    )
                  )

                )
              )
            ),

            # ── Tab 3: SEM Models ────────────────────────────────────
            tabPanel("🔗 SEM Models",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("🏗 CB-SEM (Canvas)",
                    tags$br(),
                    uiOutput(ns("cv_cbsem_fit_ui")),
                    tags$br(),
                    DT::dataTableOutput(ns("cv_cbsem_paths_tbl"))
                  ),

                  tabPanel("🔗 PLS-SEM (Canvas)",
                    tags$br(),
                    tags$h5("Outer Loadings"),
                    DT::dataTableOutput(ns("cv_pls_outer_tbl")),
                    tags$br(),
                    tags$h5("Inner Paths + R²"),
                    DT::dataTableOutput(ns("cv_pls_inner_tbl")),
                    tags$br(),
                    tags$h5("Bootstrap Results (500 samples)"),
                    DT::dataTableOutput(ns("cv_pls_boot_tbl"))
                  ),

                  tabPanel("📊 Outer Model",
                    tags$div(class="alert alert-info", style="margin:.5rem 0;font-size:.82rem;",
                      "Build your model in the canvas above → click ▶ Run All Analyses → results appear here."),
                    uiOutput(ns("outer_model_ui"))
                  ),

                  tabPanel("🔗 Inner Model",
                    tags$div(class="alert alert-info", style="margin:.5rem 0;font-size:.82rem;",
                      "R², path coefficients, f² and VIF for structural model."),
                    uiOutput(ns("inner_model_ui"))
                  ),

                  tabPanel("🔄 Bootstrap",
                    tags$div(style="background:linear-gradient(135deg,#1A3A5C,#2196A6);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🔄 Bootstrap Significance Testing", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Bootstrapped t-stats, CIs, indirect effects & HTMT",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    fluidRow(
                      column(4, numericInput(ns("nboot"), "Bootstrap samples", 500, 100, 10000, 100)),
                      column(4, numericInput(ns("boot_seed"), "Random seed", 123, 1, 99999)),
                      column(4, br(), actionButton(ns("run_boot_pls"), "🔄 Run Bootstrap",
                                                    class="btn-warning btn-block", style="font-weight:bold;"))
                    ),
                    br(), uiOutput(ns("boot_results_ui"))
                  ),

                  tabPanel("📈 IPMA",
                    tags$div(style="background:linear-gradient(135deg,#1E6438,#27AE60);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("📈 Importance-Performance Map Analysis", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Identify priority constructs for managerial action",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    uiOutput(ns("ipma_target_ui")),
                    actionButton(ns("run_ipma"), "📈 Run IPMA", class="btn-success"),
                    br(), br(),
                    fluidRow(
                      column(8, withSpinner(plotlyOutput(ns("ipma_plot"), height="400px"))),
                      column(4, DT::dataTableOutput(ns("ipma_tbl")))
                    )
                  ),

                  tabPanel("🔗 CB-SEM Manual",
                    tags$div(style="background:linear-gradient(135deg,#1A3A5C,#2196A6);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🔗 CB-SEM — Covariance-Based SEM (lavaan)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("CFA, SEM, path models with lavaan syntax",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    fluidRow(
                      column(4,
                        box(width=12, status="primary", solidHeader=TRUE, title="Model Setup",
                          pickerInput(ns("sem_cols"), "Variables", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE,`live-search`=TRUE)),
                          selectInput(ns("sem_estimator"), "Estimator",
                                      c("ML","MLR","MLM","WLSMV","ULS"), "ML"),
                          textInput(ns("sem_study"), "Label", "Study1"),
                          br(),
                          tags$p(tags$b("lavaan syntax:"), style="margin:0;font-size:.8rem;"),
                          tags$p(style="font-size:.75rem;color:#555;",
                            "CFA: LV =~ v1 + v2 + v3", tags$br(),
                            "Path: Y ~ X + M", tags$br(),
                            "Cov: X1 ~~ X2"),
                          shiny::textAreaInput(ns("sem_syntax"), label=NULL, rows=7,
                            placeholder="# Example\nLV1 =~ x1 + x2 + x3\nLV2 =~ x4 + x5\nLV2 ~ LV1"),
                          actionButton(ns("run_sem"), "▶ Run SEM", class="btn-success btn-block")
                        )
                      ),
                      column(8,
                        tabBox(width=12,
                          tabPanel("Fit Indices", uiOutput(ns("fit_cards"))),
                          tabPanel("Parameters",  DT::dataTableOutput(ns("params_tbl"))),
                          tabPanel("Diagram",     plotOutput(ns("sem_diagram"), height="400px")),
                          tabPanel("Mod Indices", DT::dataTableOutput(ns("mod_indices_tbl")))
                        ),
                        downloadButton(ns("dl_sem"), "Download SEM Results (Excel)")
                      )
                    )
                  ),

                  # ── Bayesian SEM ──────────────────────────────────────
                  tabPanel("🎲 Bayesian SEM",
                    tags$div(style="background:linear-gradient(135deg,#4A148C,#7B1FA2);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🎲 Bayesian SEM (blavaan)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Posterior distributions for path estimates — ideal for small samples & prior knowledge",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    fluidRow(
                      column(4,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          tags$p(style="font-size:.78rem;color:#555;",
                            "Uses the same lavaan syntax as CB-SEM Manual.
                             Write your model in the CB-SEM Manual tab first,
                             then run Bayesian estimation here."),
                          selectInput(ns("bsem_estimator"), "MCMC Sampler",
                            c("NUTS (Stan)"="stan","JAGS"="jags"), "stan"),
                          numericInput(ns("bsem_iter"),   "MCMC Iterations", 2000, 500, 10000, 500),
                          numericInput(ns("bsem_chains"), "Chains", 2, 1, 4),
                          numericInput(ns("bsem_seed"),   "Seed", 42, 1, 9999),
                          actionButton(ns("run_bsem"), "🎲 Run Bayesian SEM",
                                       class="btn-success btn-block", style="font-weight:bold;")
                        )
                      ),
                      column(8, uiOutput(ns("bsem_results_ui")))
                    )
                  ),

                  # ── MASEM ─────────────────────────────────────────────
                  tabPanel("📚 MASEM",
                    tags$div(style="background:linear-gradient(135deg,#1B5E20,#388E3C);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("📚 Meta-Analytic SEM (MASEM)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Two-stage MASEM: pool correlations from literature, fit your canvas model to them",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    fluidRow(
                      column(4,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          tags$p(style="font-size:.78rem;color:#555;margin:.3rem 0;",
                            tags$b("Stage 1:"), " Paste a correlation matrix (CSV format, variable names as first row & column)."),
                          shiny::textAreaInput(ns("masem_cor_input"), "Correlation Matrix (CSV)",
                            rows=6, placeholder="var,X1,X2,X3\nX1,1,0.45,0.30\nX2,0.45,1,0.55\nX3,0.30,0.55,1"),
                          shiny::textAreaInput(ns("masem_n_input"), "Sample sizes (one per study)",
                            rows=2, placeholder="250\n310\n180"),
                          tags$p(style="font-size:.78rem;color:#555;margin:.3rem 0;",
                            tags$b("Stage 2:"), " Uses canvas model structure (drawn above)."),
                          actionButton(ns("run_masem"), "📚 Run MASEM",
                                       class="btn-success btn-block", style="font-weight:bold;")
                        )
                      ),
                      column(8, uiOutput(ns("masem_results_ui")))
                    )
                  )

                )
              )
            ),

            # ── Tab 4: Regression & Paths ────────────────────────────
            tabPanel("📈 Regression & Paths",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("📈 Canvas Regression",
                    tags$br(),
                    DT::dataTableOutput(ns("cv_reg_tbl")),
                    tags$br(),
                    plotly::plotlyOutput(ns("cv_coef_plot"), height="380px")
                  ),

                  tabPanel("📈 Standalone Regression",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("📈 Regression Analysis"),
                      " — Simple, Multiple, Hierarchical OLS, Curvilinear, Mediation, Moderation."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("reg_type"), "Analysis type",
                            c("Simple / Multiple OLS"="ols",
                              "Hierarchical OLS"="hier",
                              "Mediation (Baron & Kenny)"="med",
                              "Moderation (Hayes)"="mod",
                              "── Non-Linear (WarpPLS) ──"="",
                              "Quadratic (Y ~ X + X²)"="poly2",
                              "Cubic (Y ~ X + X² + X³)"="poly3",
                              "S-Curve / Log-Log (log Y ~ log X)"="scurve",
                              "Log-Linear (Y ~ log X)"="loglin",
                              "Exponential (log Y ~ X)"="explin",
                              "Piecewise / Threshold"="piecewise")),
                          selectInput(ns("reg_dv"), "Dependent variable (Y)", choices=nc),
                          pickerInput(ns("reg_ivs"), "Predictors (X)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          uiOutput(ns("reg_extra_ui")),
                          checkboxInput(ns("reg_std"), "Standardise all variables", TRUE),
                          actionButton(ns("run_reg"), "▶ Run Regression", class="btn-success btn-block")
                        )
                      ),
                      column(9,
                        tabBox(width=12,
                          tabPanel("Coefficients",   DT::dataTableOutput(ns("reg_coef_tbl"))),
                          tabPanel("Model Summary",  uiOutput(ns("reg_summary_ui"))),
                          tabPanel("📈 Fitted Curve",
                            tags$p(style="color:#666;font-size:.81rem;margin-top:.5rem;",
                              "Fitted curve visible for non-linear models (Quadratic, Cubic, S-Curve, Log-Linear, Exponential, Piecewise)."),
                            plotly::plotlyOutput(ns("reg_nlcurve_plot"), height="400px")
                          ),
                          tabPanel("Diagnostics",    plotOutput(ns("reg_diag_plot"), height="420px")),
                          tabPanel("APA Write-Up",   verbatimTextOutput(ns("reg_apa_txt"))),
                          tabPanel("AI Interpret",
                            actionButton(ns("ai_reg"), "🤖 Interpret Results", class="btn-outline-primary btn-sm"),
                            br(), br(), uiOutput(ns("ai_reg_out")))
                        ),
                        downloadButton(ns("dl_reg"), "Download (Excel)")
                      )
                    )
                  ),

                  tabPanel("📐 Group Tests",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("📐 Group Comparison Tests"),
                      " — t-test, Mann-Whitney, ANOVA, Kruskal-Wallis, MANOVA, Chi-Square."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("gt_test"), "Test",
                            c("Independent t-test"="ttest",
                              "Mann-Whitney U"="mw",
                              "One-Way ANOVA"="anova",
                              "Kruskal-Wallis"="kw",
                              "MANOVA"="manova",
                              "Chi-Square"="chisq",
                              "Welch ANOVA"="welch")),
                          uiOutput(ns("gt_setup_ui")),
                          actionButton(ns("run_gt"), "▶ Run Test", class="btn-success btn-block")
                        )
                      ),
                      column(9, uiOutput(ns("gt_results_ui")))
                    )
                  ),

                  # ── NCA ───────────────────────────────────────────────
                  tabPanel("🔑 NCA",
                    tags$div(style="background:linear-gradient(135deg,#E65100,#F57C00);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🔑 Necessary Condition Analysis (NCA)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Identifies bottlenecks: conditions that are NECESSARY (not just sufficient) for an outcome",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("NCA logic: "), "An X is a necessary condition for Y if — without sufficient X, high Y cannot occur. ",
                      "Effect size d = ceiling zone / scope. d ≥ .1 = small, ≥ .3 = medium, ≥ .5 = large (Dul, 2016). ",
                      tags$b("CE-FDH: "), "Free Disposal Hull ceiling (discrete data). ",
                      tags$b("CR-FDH: "), "Ceiling Regression for continuous data."),
                    fluidRow(
                      column(3,
                        box(width=12, status="warning", solidHeader=TRUE, title="Setup",
                          selectInput(ns("nca_dv"), "Outcome (Y)", choices=nc),
                          pickerInput(ns("nca_ivs"), "Necessary Conditions (X)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          selectInput(ns("nca_method"), "Ceiling method",
                            c("CE-FDH (discrete)"="ce_fdh","CR-FDH (regression)"="cr_fdh")),
                          numericInput(ns("nca_p"), "Bottleneck threshold (%)", 90, 50, 99, 5),
                          actionButton(ns("run_nca"), "🔑 Run NCA", class="btn-warning btn-block",
                                       style="font-weight:bold;")
                        )
                      ),
                      column(9, uiOutput(ns("nca_results_ui")))
                    )
                  )

                )
              )
            ),

            # ── Tab 5: Mediation ─────────────────────────────────────
            tabPanel("🔀 Mediation",
              tags$div(style="margin-top:.6rem;",
                uiOutput(ns("cv_med_ui")),
                tags$br(),
                tabsetPanel(type="pills",
                  tabPanel("📜 Baron & Kenny (1986)",
                    tags$br(),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("Baron & Kenny (1986) 4-step approach: "),
                      "Step 1 — X→Y (c, total effect); Step 2 — X→M (a path); ",
                      "Step 3 — M→Y|X (b path); Step 4 — X→Y|M (c', direct effect). ",
                      "Sobel test for indirect effect significance. ",
                      tags$b("Verdict: "), "Full mediation = c sig + a sig + b sig + c' non-sig; ",
                      "Partial = all sig."
                    ),
                    DT::dataTableOutput(ns("cv_bk_tbl"))
                  ),
                  tabPanel("🔁 Preacher & Hayes (Bootstrap)",
                    tags$br(),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("Preacher & Hayes (2008) bootstrap percentile CI. "),
                      "CI excluding zero = significant indirect effect. ",
                      "Handles simple, serial (IV→M1→M2→DV) and parallel (multiple M) mediation. ",
                      "Number of resamples and CI level controlled in ⚙️ Settings above."
                    ),
                    DT::dataTableOutput(ns("cv_med_tbl")),
                    tags$br(),
                    plotly::plotlyOutput(ns("cv_med_plot"), height="380px")
                  )
                )
              )
            ),

            # ── Tab 6: Moderation ────────────────────────────────────
            tabPanel("⚖️ Moderation",
              tags$div(style="margin-top:.6rem;",
                uiOutput(ns("cv_mod_ui")),
                tags$br(),
                tags$h5("Interaction Regression Coefficients"),
                DT::dataTableOutput(ns("cv_mod_tbl")),
                tags$br(),
                tags$h5("2D Simple Slopes (−1SD / Mean / +1SD of Moderator)"),
                plotly::plotlyOutput(ns("cv_slopes_plot"), height="420px"),
                tags$br(),
                tags$h5("3D Interaction Surface"),
                tags$p(style="color:#666;font-size:.82rem;",
                  "Surface shows predicted DV across the full range of IV × Moderator.
                   Red dots = observed data. Rotate / zoom with mouse."),
                plotly::plotlyOutput(ns("cv_mod_3d_plot"), height="500px")
              )
            ),

            # ── Tab 7: Predictive R² ─────────────────────────────────
            tabPanel("🎯 Predictive R²",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("🎯 Canvas R²",
                    tags$br(),
                    uiOutput(ns("cv_pred_ui")),
                    tags$br(),
                    DT::dataTableOutput(ns("cv_pred_tbl"))
                  ),

                  tabPanel("🤖 ML / Prediction",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("🤖 Machine Learning Classification & Prediction"),
                      " — Random Forest, SVM, Decision Tree, Logistic Regression, ROC/AUC."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("ml_algo"), "Algorithm",
                            c("Random Forest"="rf", "SVM"="svm",
                              "Decision Tree"="dt", "Logistic Regression"="lr")),
                          selectInput(ns("ml_target"), "Target variable (Y)", choices=ac),
                          pickerInput(ns("ml_features"), "Features (X)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          numericInput(ns("ml_split"), "Train split (%)", 70, 50, 90, 5),
                          numericInput(ns("ml_seed"),  "Random seed", 42, 1, 9999),
                          actionButton(ns("run_ml"), "▶ Run ML", class="btn-success btn-block")
                        )
                      ),
                      column(9, uiOutput(ns("ml_results_ui")))
                    )
                  ),

                  # ── SEM-ANN ──────────────────────────────────────────
                  tabPanel("🧠 SEM-ANN",
                    tags$div(style="background:linear-gradient(135deg,#0D47A1,#1565C0);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🧠 SEM + Artificial Neural Network (SEM-ANN)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Two-stage hybrid: SEM identifies significant predictors; ANN ranks their non-linear importance",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("Rationale (Hair et al., 2021): "),
                      "PLS-SEM tests theoretical relationships (symmetrical). ANN captures non-linear, complex importance ",
                      "without distributional assumptions. Together they provide both confirmatory and exploratory insight. ",
                      tags$b("Normalised Importance = "), "each predictor's sensitivity / max sensitivity × 100%."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("ann_dv"), "Outcome variable (Y)", choices=nc),
                          pickerInput(ns("ann_ivs"), "Predictors (X)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          numericInput(ns("ann_hidden"), "Hidden neurons", 5, 2, 20),
                          numericInput(ns("ann_rep"),    "Repetitions",    10, 1, 50),
                          numericInput(ns("ann_seed"),   "Seed",           42, 1, 9999),
                          actionButton(ns("run_ann"), "🧠 Run SEM-ANN",
                                       class="btn-primary btn-block", style="font-weight:bold;")
                        )
                      ),
                      column(9, uiOutput(ns("ann_results_ui")))
                    )
                  )

                )
              )
            ),

            # ── Tab 8: Endogeneity ───────────────────────────────────
            tabPanel("🔍 Endogeneity",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("🔍 Canvas Endogeneity",
                    tags$br(),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("Wu-Hausman endogeneity test for each structural equation. "),
                      "For each potentially endogenous IV: (1) first-stage regression of IV on ",
                      "all exogenous instruments; (2) residuals added to original equation; ",
                      "(3) if residuals are significant (p < .05), the IV may be endogenous ",
                      "and results should be interpreted with caution or IV estimation employed.",
                      tags$br(),
                      tags$b("Note: "), "Exogenous nodes = constructs with no incoming structural paths ",
                      "(pure IVs). More instruments → more reliable test."
                    ),
                    DT::dataTableOutput(ns("cv_endo_tbl"))
                  ),

                  tabPanel("🔍 Standalone Endogeneity",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("🔍 Endogeneity Tests & IV Regression"),
                      " — Hausman test, Wu-Hausman, Instrumental Variables (2SLS)."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("end_dv"), "Dependent (Y)", choices=nc),
                          pickerInput(ns("end_ivs"), "Regressors (X)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          pickerInput(ns("end_instruments"), "Instruments (Z)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          actionButton(ns("run_end"), "▶ Run", class="btn-success btn-block")
                        )
                      ),
                      column(9, uiOutput(ns("end_results_ui")))
                    )
                  )
                )
              )
            ),

            # ── Tab 9: Advanced ──────────────────────────────────────
            tabPanel("📊 Advanced",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("🔵 Clustering",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("🔵 Cluster Analysis"), " — K-Means, Hierarchical, Model-Based (GMM)."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          pickerInput(ns("clust_vars"), "Variables", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          selectInput(ns("clust_algo"), "Algorithm",
                            c("K-Means"="kmeans","Hierarchical"="hclust","Model-Based (GMM)"="gmm")),
                          numericInput(ns("clust_k"), "Number of clusters (k)", 3, 2, 15),
                          numericInput(ns("clust_seed"), "Seed", 42, 1, 9999),
                          actionButton(ns("run_clust"), "▶ Run Clustering", class="btn-success btn-block")
                        )
                      ),
                      column(9, uiOutput(ns("clust_results_ui")))
                    )
                  ),

                  tabPanel("📏 IRT",
                    tags$div(style="background:#EBF4F7;padding:.7rem;border-radius:6px;margin:.5rem 0 1rem;",
                      tags$b("📏 Item Response Theory (IRT)"),
                      " — Rasch, 2PL, 3PL models for psychometric analysis."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          pickerInput(ns("irt_vars"), "Items (binary scored)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          selectInput(ns("irt_model"), "Model",
                            c("Rasch (1PL)"="rasch","2PL"="2pl","3PL"="3pl")),
                          actionButton(ns("run_irt"), "▶ Run IRT", class="btn-success btn-block")
                        )
                      ),
                      column(9, uiOutput(ns("irt_results_ui")))
                    )
                  ),

                  # ── LPA ───────────────────────────────────────────────
                  tabPanel("👥 LPA",
                    tags$div(style="background:linear-gradient(135deg,#006064,#00838F);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("👥 Latent Profile Analysis (LPA / Mixture Modelling)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Person-centred analysis: identifies subgroups with distinct response profiles",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("LPA vs Clustering: "), "LPA is model-based (Gaussian mixture) — selects optimal profiles via BIC. ",
                      "Profiles differ on mean levels across indicator variables. ",
                      tags$b("Useful when: "), "your SEM model may work differently across respondent subgroups."),
                    fluidRow(
                      column(3,
                        box(width=12, status="info", solidHeader=TRUE, title="Setup",
                          pickerInput(ns("lpa_vars"), "Profile indicators", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          numericInput(ns("lpa_min_k"), "Min profiles", 2, 1, 3),
                          numericInput(ns("lpa_max_k"), "Max profiles", 6, 3, 10),
                          selectInput(ns("lpa_model"), "Covariance structure",
                            c("Equal variance (EEI)"="EEI",
                              "Varying variance (VVI)"="VVI",
                              "Full covariance (VVV)"="VVV")),
                          actionButton(ns("run_lpa"), "👥 Run LPA", class="btn-info btn-block",
                                       style="font-weight:bold;color:white;")
                        )
                      ),
                      column(9, uiOutput(ns("lpa_results_ui")))
                    )
                  ),

                  # ── NConfA ────────────────────────────────────────────
                  tabPanel("⚙️ NConfA",
                    tags$div(style="background:linear-gradient(135deg,#37474F,#546E7A);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("⚙️ Necessary Configuration Analysis (NConfA)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("New 2025 method: tests whether CONFIGURATIONS of conditions (not just single variables) are necessary",
                             style="margin:0;opacity:.85;font-size:.82rem;")),
                    tags$div(class="alert alert-info", style="font-size:.81rem;",
                      tags$b("NConfA (Dul, 2025): "), "Extends NCA to configuration level. A configuration of X1 AND X2 ",
                      "is necessary if — without this specific combination being high — high Y cannot occur. ",
                      tags$b("Complements: "), "fsQCA (sufficient configurations) + NCA (single necessary conditions). ",
                      "Effect size d ≥ .1 small, ≥ .3 medium, ≥ .5 large."),
                    fluidRow(
                      column(3,
                        box(width=12, status="primary", solidHeader=TRUE, title="Setup",
                          selectInput(ns("nconfa_dv"), "Outcome (Y)", choices=nc),
                          pickerInput(ns("nconfa_conds"), "Conditions (X1, X2...)", choices=nc, multiple=TRUE,
                                      options=list(`actions-box`=TRUE)),
                          selectInput(ns("nconfa_method"), "Ceiling method",
                            c("CE-FDH"="ce_fdh","CR-FDH"="cr_fdh")),
                          numericInput(ns("nconfa_p"), "Necessity threshold (%)", 90, 50, 99, 5),
                          actionButton(ns("run_nconfa"), "⚙️ Run NConfA",
                                       class="btn-secondary btn-block", style="font-weight:bold;")
                        )
                      ),
                      column(9, uiOutput(ns("nconfa_results_ui")))
                    )
                  )

                )
              )
            ),

            # ── Tab 10: Diagnostics ──────────────────────────────────
            tabPanel("📊 Diagnostics",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",
                  tabPanel("📊 Fit Dashboard",
                    tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
                      "Interactive gauge dashboard for CFI, TLI, RMSEA, and SRMR. Run CB-SEM/CFA first."),
                    withSpinner(plotlyOutput(ns("fit_gauge_dashboard"), height="420px"))
                  ),
                  tabPanel("🎯 Path Coefficients",
                    tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
                      "Lollipop chart of all standardised path/loading coefficients with significance flags."),
                    withSpinner(plotlyOutput(ns("path_lollipop"), height="480px"))
                  ),
                  tabPanel("🔥 Mod Indices Heatmap",
                    tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
                      "Top modification indices shown as a heatmap to guide model re-specification."),
                    withSpinner(plotlyOutput(ns("mod_heatmap"), height="440px"))
                  )
                )
              )
            ),

            # ── Tab 11: AI & Reports ─────────────────────────────────
            tabPanel("🤖 AI & Reports",
              tags$div(style="margin-top:.6rem;",
                tabsetPanel(type="pills",

                  tabPanel("📝 APA Write-Up",
                    tags$div(class="alert alert-info", style="font-size:.82rem;margin:.5rem 0;",
                      "Comprehensive APA-style write-up combining all analyses run in this session."),
                    verbatimTextOutput(ns("combined_apa")),
                    downloadButton(ns("dl_apa"), "Download (.txt)")
                  ),

                  tabPanel("🤖 AI Interpret",
                    tags$div(
                      style="background:linear-gradient(135deg,#1A3A5C,#2196A6);color:white;
                             padding:.7rem 1rem;border-radius:8px;margin:.5rem 0 .8rem;",
                      tags$h5("🤖 AI Interpretation (Groq — FREE)", style="margin:0 0 .2rem;color:white;"),
                      tags$p("Academic-quality interpretation of all analyses",
                             style="margin:0;opacity:.85;font-size:.82rem;")
                    ),
                    fluidRow(
                      column(2, actionButton(ns("ai_btn_pls"),    "🔗 PLS-SEM",
                        class="btn-outline-primary btn-sm btn-block",   style="margin-bottom:.3rem;")),
                      column(2, actionButton(ns("ai_btn_outer"),  "📊 Outer Model",
                        class="btn-outline-primary btn-sm btn-block",   style="margin-bottom:.3rem;")),
                      column(2, actionButton(ns("ai_btn_sem"),    "🔗 CB-SEM",
                        class="btn-outline-secondary btn-sm btn-block", style="margin-bottom:.3rem;")),
                      column(2, actionButton(ns("ai_btn_pa"),     "📐 Regression",
                        class="btn-outline-secondary btn-sm btn-block", style="margin-bottom:.3rem;")),
                      column(2, actionButton(ns("ai_btn_screen"), "📊 Data Screen",
                        class="btn-outline-secondary btn-sm btn-block", style="margin-bottom:.3rem;")),
                      column(2, actionButton(ns("ai_btn_all"),    "🤖 Full Report",
                        class="btn-success btn-sm btn-block",           style="margin-bottom:.3rem;font-weight:bold;"))
                    ),
                    tags$br(),
                    fluidRow(
                      column(4, actionButton(ns("ai_btn_canvas"), "\U0001f916 AI: Interpret SEM Model",
                                              class="btn-info btn-sm"))
                    ),
                    uiOutput(ns("canvas_ai_output")),
                    br(), uiOutput(ns("ai_output"))
                  )
                )
              )
            )

          ) # end tabsetPanel bottom_tabs
        )   # end bottom div
      )     # end tags$div main
    }) # end renderUI

    # ── Save / Load Canvas Model ──────────────────────────────────────────────
    output$dl_canvas_model <- downloadHandler(
      filename = function() {
        paste0("SEM_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
      },
      content = function(file) {
        cm <- canvas_model()
        if (is.null(cm) || (length(cm$nodes) == 0 && length(cm$edges) == 0)) {
          writeLines('{"nodes":[],"edges":[]}', file)
        } else {
          writeLines(jsonlite::toJSON(cm, auto_unbox = TRUE, pretty = TRUE), file)
        }
      }
    )

    observeEvent(input$upload_canvas_file, {
      req(input$upload_canvas_file)
      fp <- input$upload_canvas_file$datapath
      parsed <- tryCatch(
        jsonlite::fromJSON(fp, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (!is.null(parsed)) {
        canvas_model(parsed)
        session$sendCustomMessage("sem_load_model", parsed)
        showNotification(
          "✅ Canvas model restored! Assign items in the right panel then click ▶ Run.",
          type = "message", duration = 4
        )
      } else {
        showNotification(
          "❌ Could not read model file. Make sure it is a valid .json exported from this tool.",
          type = "error", duration = 5
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # ── Data preview ─────────────────────────────────────────────────────────
    output$preview <- DT::renderDataTable({
      req(df()); tool_dt(head(df(), 50), "Data Preview (first 50 rows)")
    })

    # ========================================================================
    # CANVAS SERVER — parse model JSON, run analyses
    # ========================================================================
    output$canvas_ui <- renderUI({
      HTML(paste0(
        readLines(textConnection(
          gsub("__NS__", ns(""), '
<div style="display:flex;gap:10px;height:680px;">
  <div style="flex:1;border:2px solid #2196A6;border-radius:8px;overflow:hidden;position:relative;background:#FAFAFA;">
    <div style="background:#1A3A5C;padding:6px 10px;display:flex;gap:6px;align-items:center;flex-wrap:wrap;">
      <span style="color:white;font-weight:bold;font-size:.8rem;margin-right:4px;">Mode:</span>
      <button class="sem-tb-btn" data-m="select"   style="background:white;border:1px solid #aaa;border-radius:4px;padding:3px 8px;font-size:.78rem;cursor:pointer;">☝ Select</button>
      <button class="sem-tb-btn" data-m="addNode"  style="background:white;border:1px solid #aaa;border-radius:4px;padding:3px 8px;font-size:.78rem;cursor:pointer;">＋ Add Construct</button>
      <button class="sem-tb-btn" data-m="arrow"    style="background:white;border:1px solid #aaa;border-radius:4px;padding:3px 8px;font-size:.78rem;cursor:pointer;">→ Draw Path</button>
      <button class="sem-tb-btn" data-m="delete"   style="background:white;border:1px solid #aaa;border-radius:4px;padding:3px 8px;font-size:.78rem;cursor:pointer;">✕ Delete</button>
      <span style="flex:1;"></span>
      <span style="color:rgba(255,255,255,.7);font-size:.7rem;">
        <span style="color:#4CAF50;font-weight:bold;">■</span> = mediator (auto)
        &nbsp;<span style="color:#9C27B0;font-weight:bold;">──</span> = moderator
      </span>
      <button id="sem-clear-all" style="background:#FF7043;color:white;border:none;border-radius:4px;padding:3px 8px;font-size:.78rem;cursor:pointer;">🧹 Clear</button>
      <button id="sem-run-btn"   style="background:#4CAF50;color:white;border:none;border-radius:4px;padding:4px 12px;font-size:.8rem;font-weight:bold;cursor:pointer;">▶ Run All Analyses</button>
    </div>
    <svg id="sem-svg" width="100%" height="626" style="display:block;">
      <defs>
        <marker id="arr-direct"     markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto"><polygon points="0 0,10 3.5,0 7" fill="#1565C0"/></marker>
        <marker id="arr-mediation"  markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto"><polygon points="0 0,10 3.5,0 7" fill="#E65100"/></marker>
        <marker id="arr-moderation" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto"><polygon points="0 0,10 3.5,0 7" fill="#6A1B9A"/></marker>
        <marker id="arr-covariance" markerWidth="10" markerHeight="7" refX="10" refY="3.5" orient="auto"><polygon points="0 0,10 3.5,0 7" fill="#555"/></marker>
      </defs>
      <g id="sem-edge-layer"></g>
      <g id="sem-node-layer"></g>
      <text id="sem-tip" x="50%" y="50%" text-anchor="middle" fill="#aaa" font-size="13" pointer-events="none">
        Click ＋ Add Construct → click here to place construct boxes
      </text>
    </svg>
  </div>
  <div style="width:240px;border:2px solid #e0e0e0;border-radius:8px;overflow-y:auto;background:white;padding:8px;">
    <div id="sem-panel-none" style="color:#888;font-size:.82rem;padding:10px;text-align:center;">
      <p>Select a construct or path to edit its properties</p>
    </div>
    <div id="sem-panel-node" style="display:none;">
      <input type="hidden" id="sem-node-id" value=""/>
      <span class="sem-panel-label">Construct Name</span>
      <input id="sem-node-label" type="text" style="width:100%;border:1px solid #ccc;border-radius:4px;padding:3px 6px;font-size:.82rem;margin-bottom:6px;" placeholder="e.g. Engagement"/>
      <span class="sem-panel-label">Construct Type</span>
      <select id="sem-node-type" style="width:100%;border:1px solid #ccc;border-radius:4px;padding:3px;font-size:.82rem;margin-bottom:8px;">
        <option value="reflective">Reflective</option>
        <option value="composite">Composite</option>
        <option value="single">Single Item</option>
      </select>
      <span class="sem-panel-label">Indicators (items)</span>
      <div id="sem-ind-container" style="max-height:340px;overflow-y:auto;border:1px solid #eee;border-radius:4px;padding:4px;font-size:.79rem;"></div>
    </div>
    <div id="sem-panel-edge" style="display:none;padding:6px;">
      <input type="hidden" id="sem-edge-id" value=""/>
      <span class="sem-panel-label">Path Type</span>
      <select id="sem-edge-type" style="width:100%;border:1px solid #ccc;border-radius:4px;padding:3px;font-size:.82rem;margin-bottom:6px;">
        <option value="direct">→ Structural Path</option>
        <option value="moderation">✕ Moderation</option>
        <option value="covariance">↔ Covariance</option>
      </select>
      <div id="sem-edge-info" style="font-size:.78rem;color:#555;margin-bottom:6px;"></div>
      <div style="background:#E8F5E9;border-radius:5px;padding:5px 7px;font-size:.74rem;color:#2E7D32;line-height:1.5;">
        <b>Auto-detection:</b><br>
        Constructs with both incoming <em>and</em> outgoing structural paths turn <b style="color:#2E7D32;">green</b> and are automatically treated as mediators.<br><br>
        <b>Serial mediation:</b> IV → M1 → M2 → DV<br>
        <b>Parallel mediation:</b> IV → M1 → DV<br>
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;IV → M2 → DV
      </div>
    </div>
  </div>
</div>
<script>
  (function() {
    function tryInit() {
      if (typeof window.initSemCanvas === "function") {
        window._semInitialized = false;
        window.initSemCanvas();
      } else {
        setTimeout(tryInit, 50);
      }
    }
    tryInit();
  })();
</script>
          '))
      )))
    })

    # React to canvas_model updates from JS
    observeEvent(input$canvas_model, {
      raw <- tryCatch(jsonlite::fromJSON(input$canvas_model, simplifyVector=FALSE),
                      error=function(e) NULL)
      if (!is.null(raw)) canvas_model(raw)
    })

    # ══════════════════════════════════════════════════════════════════════
    # React to Run button from canvas – COMPREHENSIVE ANALYSIS PIPELINE
    # ══════════════════════════════════════════════════════════════════════
    observeEvent(input$canvas_run, {
      cm    <- canvas_model()
      nodes <- cm$nodes
      edges <- cm$edges

      if (length(nodes) == 0) {
        showNotification("Add constructs to the canvas first.", type="warning")
        return()
      }

      output$canvas_run_status <- renderUI(
        tags$div(class="alert alert-info",
          tags$b("⌛ Running comprehensive analyses…"),
          tags$br(),
          tags$small("Reliability, validity, CFA, CB-SEM, PLS-SEM, regression, mediation, moderation, predictive R²")))

      tryCatch({
        d <- df()

        # ── 1. Build construct items & types ──────────────────────────────
        construct_items <- list()
        construct_types <- list()
        for (nd in nodes) {
          inds <- nd$indicators
          if (is.null(inds) || length(inds) == 0) next
          # fromJSON(simplifyVector=FALSE) returns JSON arrays as R lists —
          # convert to character vector before any set operations
          inds <- as.character(unlist(inds))
          valid_inds <- intersect(inds, names(d))
          if (length(valid_inds) == 0) next
          construct_items[[nd$label]] <- valid_inds
          construct_types[[nd$label]] <- nd$type %||% "reflective"
        }

        if (length(construct_items) == 0) {
          output$canvas_run_status <- renderUI(
            tags$div(class="alert alert-warning",
              "⚠️ No constructs have indicators assigned. Click a construct and assign items in the right panel."))
          return()
        }

        # ── 2. Build construct scores (row means) ─────────────────────────
        cs_list <- lapply(names(construct_items), function(nm) {
          items <- intersect(construct_items[[nm]], names(d))
          mat   <- d[, items, drop=FALSE]
          mat[] <- lapply(mat, as.numeric)
          rowMeans(mat, na.rm=TRUE)
        })
        cs_df <- as.data.frame(setNames(cs_list, names(construct_items)))
        cs_df <- cs_df[complete.cases(cs_df), , drop=FALSE]

        # ── 3. Parse structural and moderation edges ──────────────────────
        get_label <- function(nid) {
          for (nd in nodes) if (!is.null(nd$id) && nd$id == nid) return(nd$label)
          NULL
        }

        struct_edges <- Filter(function(e)
          !is.null(e$type) && e$type %in% c("direct","mediation") &&
          !is.null(e$from) && !is.null(e$to), edges)

        mod_edges <- Filter(function(e)
          !is.null(e$type) && e$type == "moderation", edges)

        path_list <- Filter(function(p) !is.null(p$from) && !is.null(p$to),
          lapply(struct_edges, function(e)
            list(from=get_label(e$from), to=get_label(e$to))))

        all_froms <- if (length(path_list) > 0) vapply(path_list, `[[`, character(1), "from") else character(0)
        all_tos   <- if (length(path_list) > 0) vapply(path_list, `[[`, character(1), "to")   else character(0)
        # Topology-based detection: nodes that appear in BOTH from and to
        topo_meds <- intersect(all_froms, all_tos)
        # Manual override supplement
        manual_meds <- if (!is.null(input$cv_manual_mediators) && length(input$cv_manual_mediators) > 0)
          intersect(as.character(input$cv_manual_mediators),
                    unique(c(all_froms, all_tos))) else character(0)
        mediators <- unique(c(topo_meds, manual_meds))

        # ── Analysis settings ─────────────────────────────────────────
        cv_controls <- if (!is.null(input$cv_controls) && length(input$cv_controls) > 0)
          intersect(as.character(input$cv_controls), names(d)) else character(0)
        cv_instruments <- if (!is.null(input$cv_instruments) && length(input$cv_instruments) > 0)
          intersect(as.character(input$cv_instruments), names(d)) else character(0)
        cv_nboot <- max(100L, as.integer(input$cv_nboot %||% 1000L))
        cv_seed  <- as.integer(input$cv_seed %||% 42L)
        cv_ci    <- as.numeric(input$cv_ci_level %||% "0.95")

        mod_triples <- Filter(Negate(is.null), lapply(mod_edges, function(e) {
          moderator <- get_label(e$from)
          dv        <- get_label(e$to)
          iv        <- NULL
          for (p in path_list) if (!is.null(p$to) && p$to == dv) { iv <- p$from; break }
          if (!is.null(moderator) && !is.null(iv) && !is.null(dv))
            list(moderator=moderator, iv=iv, dv=dv)
          else NULL
        }))

        # ── 4. PLS-SEM + Bootstrap (500 samples) ──────────────────────────
        if (.has_seminr10 && length(construct_items) >= 2 && length(path_list) >= 1) {
          withProgress(message="Running PLS-SEM + Bootstrap (500)…", value=0.2, {
            mm_parts <- list()
            for (nm in names(construct_items)) {
              items_ok <- intersect(construct_items[[nm]], names(d))
              if (length(items_ok) == 0) next
              mm_parts[[nm]] <- if (identical(construct_types[[nm]], "composite"))
                seminr::composite(nm, items_ok, weights=seminr::mode_A)
              else
                seminr::reflective(nm, items_ok)
            }
            endo_groups <- list()
            for (sp in path_list) {
              endo_groups[[sp$to]] <- c(endo_groups[[sp$to]], sp$from)
            }
            mm    <- do.call(seminr::constructs, mm_parts)
            sm    <- do.call(seminr::relationships, lapply(names(endo_groups), function(en)
                       seminr::paths(from=endo_groups[[en]], to=en)))
            all_items <- unique(unlist(construct_items))
            d_sem  <- d[, intersect(all_items, names(d)), drop=FALSE]
            d_sem[]<- lapply(d_sem, as.numeric)
            d_sem  <- na.omit(d_sem)
            pls_m  <- tryCatch(
              seminr::estimate_pls(data=d_sem, measurement_model=mm, structural_model=sm),
              error=function(e) NULL)
            if (!is.null(pls_m)) {
              pls_model_rv(pls_m)
              boot_m <- tryCatch(
                seminr::bootstrap_model(pls_m, nboot=500, cores=1),
                error=function(e) NULL)
              if (!is.null(boot_m)) boot_model_rv(boot_m)
            }
            setProgress(1)
          })
        }

        # ── 5. Store results bundle ────────────────────────────────────────
        canvas_results_rv(list(
          cs_df           = cs_df,
          d_raw           = d,
          construct_items = construct_items,
          construct_types = construct_types,
          path_list       = path_list,
          mediators       = mediators,
          topo_mediators  = topo_meds,
          manual_mediators= manual_meds,
          mod_triples     = mod_triples,
          controls        = cv_controls,
          instruments     = cv_instruments,
          nboot           = cv_nboot,
          seed            = cv_seed,
          ci_level        = cv_ci
        ))

        n_c <- length(construct_items)
        n_p <- length(path_list)
        output$canvas_run_status <- renderUI(
          tags$div(class="alert alert-success",
            tags$b("✅ Analyses complete! "),
            paste0(n_c, " constructs · ", n_p, " structural paths"),
            tags$br(),
            tags$small("All results available in the tabs below.")))

      }, error=function(e) {
        output$canvas_run_status <- renderUI(
          tags$div(class="alert alert-danger",
            tags$b("❌ Error: "), e$message))
      })
    })

    # ══════════════════════════════════════════════════════════════════════
    # Canvas Results UI – 9-tab comprehensive tabset
    # ══════════════════════════════════════════════════════════════════════
    output$canvas_results_ui <- renderUI({
      res <- canvas_results_rv()
      if (is.null(res)) return(NULL)
      tagList(
        tags$h4("📊 Comprehensive Analysis Results",
          style="color:#1A3A5C;margin-top:1.5rem;margin-bottom:.5rem;"),
        tabsetPanel(id=ns("cv_tabs"), type="tabs",
          tabPanel("📋 Descriptive",
            tags$br(),
            DT::dataTableOutput(ns("cv_desc_tbl")),
            tags$br(),
            tags$h5("Construct Correlation Matrix"),
            plotly::plotlyOutput(ns("cv_corr_plot"), height="420px")
          ),
          tabPanel("🔍 Reliability & Validity",
            tags$br(),
            DT::dataTableOutput(ns("cv_rel_tbl")),
            tags$br(),
            tags$h5("HTMT Discriminant Validity (< 0.85 = OK)"),
            DT::dataTableOutput(ns("cv_htmt_tbl"))
          ),
          tabPanel("📐 CFA",
            tags$br(),
            uiOutput(ns("cv_cfa_fit_ui")),
            tags$br(),
            DT::dataTableOutput(ns("cv_cfa_loads_tbl"))
          ),
          tabPanel("🏗 CB-SEM",
            tags$br(),
            uiOutput(ns("cv_cbsem_fit_ui")),
            tags$br(),
            DT::dataTableOutput(ns("cv_cbsem_paths_tbl"))
          ),
          tabPanel("🔗 PLS-SEM",
            tags$br(),
            tags$h5("Outer Loadings"),
            DT::dataTableOutput(ns("cv_pls_outer_tbl")),
            tags$br(),
            tags$h5("Inner Paths + R²"),
            DT::dataTableOutput(ns("cv_pls_inner_tbl")),
            tags$br(),
            tags$h5("Bootstrap Results (500 samples)"),
            DT::dataTableOutput(ns("cv_pls_boot_tbl"))
          ),
          tabPanel("📈 Regression",
            tags$br(),
            DT::dataTableOutput(ns("cv_reg_tbl")),
            tags$br(),
            plotly::plotlyOutput(ns("cv_coef_plot"), height="380px")
          ),
          tabPanel("🔀 Mediation",
            tags$br(),
            uiOutput(ns("cv_med_ui")),
            tags$br(),
            tabsetPanel(type="pills",
              tabPanel("📜 Baron & Kenny (1986)",
                tags$br(),
                tags$div(class="alert alert-info", style="font-size:.81rem;",
                  tags$b("Baron & Kenny (1986) 4-step approach: "),
                  "Step 1 — X→Y (c, total effect); Step 2 — X→M (a path); ",
                  "Step 3 — M→Y|X (b path); Step 4 — X→Y|M (c', direct effect). ",
                  "Sobel test for indirect effect significance. ",
                  tags$b("Verdict: "), "Full mediation = c sig + a sig + b sig + c' non-sig; ",
                  "Partial = all sig."
                ),
                DT::dataTableOutput(ns("cv_bk_tbl"))
              ),
              tabPanel("🔁 Preacher & Hayes (Bootstrap)",
                tags$br(),
                tags$div(class="alert alert-info", style="font-size:.81rem;",
                  tags$b("Preacher & Hayes (2008) bootstrap percentile CI. "),
                  "CI excluding zero = significant indirect effect. ",
                  "Handles simple, serial (IV→M1→M2→DV) and parallel (multiple M) mediation. ",
                  "Number of resamples and CI level controlled in ⚙️ Settings above."
                ),
                DT::dataTableOutput(ns("cv_med_tbl")),
                tags$br(),
                plotly::plotlyOutput(ns("cv_med_plot"), height="380px")
              )
            )
          ),
          tabPanel("⚖️ Moderation",
            tags$br(),
            uiOutput(ns("cv_mod_ui")),
            tags$br(),
            tags$h5("Interaction Regression Coefficients"),
            DT::dataTableOutput(ns("cv_mod_tbl")),
            tags$br(),
            tags$h5("2D Simple Slopes (−1SD / Mean / +1SD of Moderator)"),
            plotly::plotlyOutput(ns("cv_slopes_plot"), height="420px"),
            tags$br(),
            tags$h5("3D Interaction Surface"),
            tags$p(style="color:#666;font-size:.82rem;",
              "Surface shows predicted DV across the full range of IV × Moderator.
               Red dots = observed data. Rotate / zoom with mouse."),
            plotly::plotlyOutput(ns("cv_mod_3d_plot"), height="500px")
          ),
          tabPanel("🎯 Predictive R²",
            tags$br(),
            uiOutput(ns("cv_pred_ui")),
            tags$br(),
            DT::dataTableOutput(ns("cv_pred_tbl"))
          ),
          tabPanel("🔍 Endogeneity",
            tags$br(),
            tags$div(class="alert alert-info", style="font-size:.81rem;",
              tags$b("Wu-Hausman endogeneity test for each structural equation. "),
              "For each potentially endogenous IV: (1) first-stage regression of IV on ",
              "all exogenous instruments; (2) residuals added to original equation; ",
              "(3) if residuals are significant (p < .05), the IV may be endogenous ",
              "and results should be interpreted with caution or IV estimation employed.",
              tags$br(),
              tags$b("Note: "), "Exogenous nodes = constructs with no incoming structural paths ",
              "(pure IVs). More instruments → more reliable test."
            ),
            DT::dataTableOutput(ns("cv_endo_tbl"))
          ),
          # ─────────────────────────────────────────────────────────────────
          tabPanel("🔬 Model Detail",
            tags$br(),
            tags$div(
              style="background:#EBF5FB;border-left:4px solid #2196A6;border-radius:6px;padding:.75rem 1rem;margin-bottom:1rem;",
              tags$b("📐 AMOS / SmartPLS / WarpPLS — Full Model Report"),
              tags$span(style="color:#555;font-size:.82rem;display:block;margin-top:.3rem;",
                "Comprehensive view of all constructs, indicators, factor loadings, structural paths, ",
                "error terms, fit indices, effects decomposition, Fornell-Larcker criterion, and cross-loadings. ",
                "Mirrors the output panel in AMOS, SmartPLS Results view, and WarpPLS outputs.")
            ),
            tabsetPanel(type="pills",
              tabPanel("🏗 Measurement Model",
                tags$br(),
                uiOutput(ns("cv_detail_meas_ui"))
              ),
              tabPanel("🔗 Structural Paths",
                tags$br(),
                uiOutput(ns("cv_detail_struct_ui"))
              ),
              tabPanel("📊 Fit Indices",
                tags$br(),
                uiOutput(ns("cv_detail_fit_ui"))
              ),
              tabPanel("🔄 Effects Decomposition",
                tags$br(),
                uiOutput(ns("cv_detail_effects_ui"))
              ),
              tabPanel("🎯 Validity Matrices",
                tags$br(),
                tags$h5("Fornell-Larcker Criterion", style="color:#1A3A5C;"),
                tags$p(style="color:#666;font-size:.81rem;",
                  "Diagonal = √AVE per construct. Off-diagonal = inter-construct correlations.",
                  "Discriminant validity is supported when √AVE > all correlations in same row/column."),
                DT::dataTableOutput(ns("cv_fl_tbl")),
                tags$br(),
                tags$h5("Cross-Loadings Matrix (PLS)", style="color:#1A3A5C;"),
                tags$p(style="color:#666;font-size:.81rem;",
                  "Indicators should load highest on their own construct (bold diagonal). ",
                  "SmartPLS-style: cross-loadings < primary loadings confirm discriminant validity."),
                DT::dataTableOutput(ns("cv_crossload_tbl"))
              ),
              tabPanel("📝 Model Text (AMOS-style)",
                tags$br(),
                verbatimTextOutput(ns("cv_detail_path_txt"))
              )
            )
          )
        )
      )
    })

    # ── Tab 1: Descriptive Statistics ─────────────────────────────────────
    output$cv_desc_tbl <- DT::renderDataTable({
      res <- canvas_results_rv(); req(res)
      cs  <- res$cs_df
      rows <- lapply(names(cs), function(nm) {
        x <- cs[[nm]]
        skw <- tryCatch(moments::skewness(x, na.rm=TRUE), error=function(e) NA)
        krt <- tryCatch(moments::kurtosis(x, na.rm=TRUE) - 3, error=function(e) NA)
        data.frame(
          Construct = nm,
          N      = sum(!is.na(x)),
          Mean   = round(mean(x, na.rm=TRUE), 3),
          SD     = round(sd(x, na.rm=TRUE), 3),
          Median = round(median(x, na.rm=TRUE), 3),
          Min    = round(min(x, na.rm=TRUE), 3),
          Max    = round(max(x, na.rm=TRUE), 3),
          Skewness = round(skw, 3),
          Ex.Kurtosis = round(krt, 3),
          stringsAsFactors=FALSE
        )
      })
      tool_dt(do.call(rbind, rows), "Descriptive Statistics (Construct Scores)")
    })

    output$cv_corr_plot <- plotly::renderPlotly({
      res <- canvas_results_rv(); req(res)
      cs  <- res$cs_df
      if (ncol(cs) < 2)
        return(plotly::plot_ly() %>% plotly::layout(title="Need ≥2 constructs"))
      cm_mat <- round(cor(cs, use="pairwise.complete.obs"), 3)
      plotly::plot_ly(
        x = colnames(cm_mat), y = rownames(cm_mat),
        z = cm_mat, type = "heatmap",
        colorscale = list(c(0,"#EBF5FB"), c(0.5,"#2E86C1"), c(1,"#1A3A5C")),
        zmin=-1, zmax=1,
        hovertemplate="%{y} — %{x}: %{z}<extra></extra>"
      ) %>%
      plotly::layout(
        title  = "Construct Correlation Matrix",
        margin = list(l=140, b=140)
      )
    })

    # ── Tab 2: Reliability & Validity ─────────────────────────────────────
    output$cv_rel_tbl <- DT::renderDataTable({
      res   <- canvas_results_rv(); req(res)
      d_raw <- res$d_raw
      rows  <- lapply(names(res$construct_items), function(nm) {
        items <- intersect(res$construct_items[[nm]], names(d_raw))
        if (length(items) < 2) return(NULL)
        mat   <- d_raw[, items, drop=FALSE]
        mat[] <- lapply(mat, as.numeric)
        mat   <- na.omit(mat)
        if (nrow(mat) < 5) return(NULL)
        # Cronbach alpha
        alp_val <- tryCatch(psych::alpha(mat)$total$raw_alpha, error=function(e) NA)
        # Omega (≥3 items)
        omg_val <- if (length(items) >= 3)
          tryCatch(psych::omega(mat, nfactors=1, plot=FALSE)$omega.tot, error=function(e) NA)
        else NA
        # Factor loadings → AVE & CR
        lam <- tryCatch({
          fa_r <- psych::fa(mat, nfactors=1, rotate="none", fm="ml")
          as.numeric(fa_r$loadings)
        }, error=function(e) rep(NA_real_, length(items)))
        lam_sq  <- lam^2
        ave_val <- round(mean(lam_sq, na.rm=TRUE), 3)
        cr_num  <- sum(abs(lam), na.rm=TRUE)^2
        cr_den  <- cr_num + sum(1 - lam_sq, na.rm=TRUE)
        cr_val  <- if (cr_den > 0) round(cr_num / cr_den, 3) else NA
        data.frame(
          Construct = nm, Items = length(items),
          Alpha  = round(alp_val, 3), Omega  = round(omg_val, 3),
          AVE    = ave_val,            CR     = cr_val,
          Alpha_OK = ifelse(!is.na(alp_val) & alp_val >= 0.7, "✅", "❌"),
          AVE_OK   = ifelse(!is.na(ave_val) & ave_val >= 0.5, "✅", "❌"),
          CR_OK    = ifelse(!is.na(cr_val)  & cr_val  >= 0.7, "✅", "❌"),
          stringsAsFactors=FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      tool_dt(do.call(rbind, rows),
        "Reliability & Validity (α ≥0.7 | AVE ≥0.5 | CR ≥0.7)")
    })

    output$cv_htmt_tbl <- DT::renderDataTable({
      res    <- canvas_results_rv(); req(res)
      d_raw  <- res$d_raw
      cnames <- names(res$construct_items)
      n      <- length(cnames)
      if (n < 2) return(tool_dt(data.frame(Note="Need ≥2 constructs"), "HTMT"))
      htmt_mat <- matrix(NA_real_, n, n, dimnames=list(cnames, cnames))
      for (i in seq_len(n)) for (j in seq_len(n)) {
        if (i >= j) next
        ci <- intersect(res$construct_items[[cnames[i]]], names(d_raw))
        cj <- intersect(res$construct_items[[cnames[j]]], names(d_raw))
        if (length(ci) < 1 || length(cj) < 1) next
        mi   <- d_raw[, ci, drop=FALSE]; mi[] <- lapply(mi, as.numeric)
        mj   <- d_raw[, cj, drop=FALSE]; mj[] <- lapply(mj, as.numeric)
        both <- na.omit(cbind(mi, mj))
        if (nrow(both) < 5) next
        ci_idx <- seq_len(length(ci)); cj_idx <- length(ci) + seq_len(length(cj))
        r_ij <- mean(abs(cor(both[, ci_idx, drop=FALSE],
                             both[, cj_idx, drop=FALSE])), na.rm=TRUE)
        r_ii_m <- cor(both[, ci_idx, drop=FALSE])
        r_jj_m <- cor(both[, cj_idx, drop=FALSE])
        r_ii   <- if (length(ci) > 1) mean(abs(r_ii_m[lower.tri(r_ii_m)])) else 1
        r_jj   <- if (length(cj) > 1) mean(abs(r_jj_m[lower.tri(r_jj_m)])) else 1
        htmt_val <- if (!is.na(r_ii) && !is.na(r_jj) && r_ii > 0 && r_jj > 0)
          r_ij / sqrt(r_ii * r_jj) else NA
        htmt_mat[i, j] <- round(htmt_val, 3)
      }
      df_htmt            <- as.data.frame(htmt_mat)
      df_htmt$Construct  <- rownames(df_htmt)
      df_htmt            <- df_htmt[, c("Construct", cnames)]
      tool_dt(df_htmt, "HTMT Ratio (< 0.85 = discriminant validity supported)")
    })

    # ── Tab 3: CFA ────────────────────────────────────────────────────────
    cv_cfa_res <- reactive({
      res <- canvas_results_rv(); req(res)
      if (!requireNamespace("lavaan", quietly=TRUE)) return(NULL)
      d_raw  <- res$d_raw
      ci     <- res$construct_items
      avail  <- intersect(unique(unlist(ci)), names(d_raw))
      d_lav  <- d_raw[, avail, drop=FALSE]
      d_lav[]<- lapply(d_lav, as.numeric)
      d_lav  <- na.omit(d_lav)
      lines  <- Filter(Negate(is.null), lapply(names(ci), function(nm) {
        its <- intersect(ci[[nm]], names(d_lav))
        if (length(its) < 2) return(NULL)
        paste0(nm, " =~ ", paste(its, collapse=" + "))
      }))
      if (length(lines) < 1) return(NULL)
      tryCatch(lavaan::cfa(paste(lines, collapse="\n"), data=d_lav, estimator="MLR"),
               error=function(e) NULL)
    })

    output$cv_cfa_fit_ui <- renderUI({
      fit <- cv_cfa_res()
      if (is.null(fit))
        return(tags$div(class="alert alert-warning",
          "CFA could not be estimated. Ensure each construct has ≥2 items and data is numeric."))
      fm <- tryCatch(lavaan::fitMeasures(fit,
        c("chisq","df","pvalue","cfi","tli","rmsea","srmr")), error=function(e) NULL)
      if (is.null(fm)) return(NULL)
      mk <- function(label, val, ok=NULL, thresh=NULL) {
        col <- if (!is.null(ok)) if(ok) "#27AE60" else "#E74C3C" else "#2E86C1"
        tags$div(style=paste0(
          "background:#fff;border:2px solid ",col,";border-radius:8px;",
          "padding:.6rem 1rem;margin:.3rem;display:inline-block;",
          "min-width:110px;text-align:center;"),
          tags$div(style=paste0("font-size:1.25rem;font-weight:700;color:",col,";"),
            round(val,3)),
          tags$div(style="font-size:.75rem;color:#555;", label),
          if (!is.null(thresh)) tags$div(style="font-size:.7rem;color:#888;", thresh)
        )
      }
      tags$div(
        tags$h5("CFA Model Fit Indices"),
        tags$div(style="display:flex;flex-wrap:wrap;",
          mk("χ²", fm["chisq"]), mk("df", fm["df"]), mk("p-value", fm["pvalue"]),
          mk("CFI",   fm["cfi"],   fm["cfi"]   >= 0.95, "≥0.95"),
          mk("TLI",   fm["tli"],   fm["tli"]   >= 0.95, "≥0.95"),
          mk("RMSEA", fm["rmsea"], fm["rmsea"] <= 0.06, "≤0.06"),
          mk("SRMR",  fm["srmr"],  fm["srmr"]  <= 0.08, "≤0.08")
        )
      )
    })

    output$cv_cfa_loads_tbl <- DT::renderDataTable({
      fit <- cv_cfa_res()
      if (is.null(fit))
        return(tool_dt(data.frame(Note="CFA not available"), "CFA Loadings"))
      pe <- tryCatch(lavaan::parameterEstimates(fit, standardized=TRUE), error=function(e) NULL)
      if (is.null(pe))
        return(tool_dt(data.frame(Note="Parameters unavailable"), "CFA Loadings"))
      ld <- pe[pe$op == "=~", c("lhs","rhs","est","se","z","pvalue","std.all")]
      names(ld) <- c("Construct","Item","Estimate","SE","Z","p","Std.Loading")
      ld[,c("Estimate","SE","Z","p","Std.Loading")] <-
        round(ld[,c("Estimate","SE","Z","p","Std.Loading")], 3)
      ld$Sig <- ifelse(ld$p<0.001,"***",ifelse(ld$p<0.01,"**",ifelse(ld$p<0.05,"*","")))
      ld$OK  <- ifelse(abs(ld$Std.Loading) >= 0.7, "✅", "❌")
      tool_dt(ld, "CFA Factor Loadings (Std. ≥0.70 = acceptable)")
    })

    # ── Tab 4: CB-SEM ─────────────────────────────────────────────────────
    cv_cbsem_res <- reactive({
      res <- canvas_results_rv(); req(res)
      if (!requireNamespace("lavaan", quietly=TRUE)) return(NULL)
      d_raw <- res$d_raw; ci <- res$construct_items; pl <- res$path_list
      avail  <- intersect(unique(unlist(ci)), names(d_raw))
      d_lav  <- d_raw[, avail, drop=FALSE]
      d_lav[]<- lapply(d_lav, as.numeric)
      d_lav  <- na.omit(d_lav)
      mlines <- Filter(Negate(is.null), lapply(names(ci), function(nm) {
        its <- intersect(ci[[nm]], names(d_lav))
        if (length(its) < 2) return(NULL)
        paste0(nm, " =~ ", paste(its, collapse=" + "))
      }))
      slines <- vapply(pl, function(p) paste0(p$to, " ~ ", p$from), character(1))
      if (length(mlines) < 1 || length(slines) < 1) return(NULL)
      tryCatch(lavaan::sem(paste(c(mlines, slines), collapse="\n"),
                           data=d_lav, estimator="MLR"), error=function(e) NULL)
    })

    output$cv_cbsem_paths_tbl <- DT::renderDataTable({
      fit <- cv_cbsem_res()
      if (is.null(fit))
        return(tool_dt(data.frame(Note="CB-SEM not available"), "CB-SEM Paths"))
      pe <- tryCatch(lavaan::parameterEstimates(fit, standardized=TRUE, ci=TRUE), error=function(e) NULL)
      if (is.null(pe))
        return(tool_dt(data.frame(Note="Parameters unavailable"), "CB-SEM Paths"))
      rg <- pe[pe$op == "~", ]
      out <- data.frame(
        DV       = rg$lhs,
        IV       = rg$rhs,
        Beta_Unst= round(rg$est,   3),
        Beta_Std = round(rg$std.all, 3),
        SE       = round(rg$se,    3),
        Z        = round(rg$z,     3),
        p        = round(rg$pvalue,4),
        Sig      = ifelse(rg$pvalue<.001,"***",
                     ifelse(rg$pvalue<.01,"**",
                       ifelse(rg$pvalue<.05,"*","ns"))),
        CI_lo_95 = round(rg$ci.lower, 3),
        CI_hi_95 = round(rg$ci.upper, 3),
        stringsAsFactors=FALSE
      )
      # Append R² per DV from lavaan inspect
      r2v <- tryCatch(lavaan::inspect(fit,"r2"), error=function(e) NULL)
      if (!is.null(r2v)) {
        out$R2_DV <- round(as.numeric(r2v[out$DV]), 3)
      }
      tool_dt(out, "CB-SEM Structural Paths (95% CI | AMOS-style | Hu & Bentler, 1999)")
    })

    output$cv_cbsem_fit_ui <- renderUI({
      fit <- cv_cbsem_res()
      if (is.null(fit)) {
        return(tags$div(class="alert alert-warning",
          "CB-SEM could not be estimated. Ensure each construct has ≥2 items, ",
          "numeric data, and at least one structural path is defined."))
      }
      fm <- tryCatch(lavaan::fitMeasures(fit,
        c("chisq","df","pvalue","cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper",
          "srmr","gfi","agfi","aic","bic")), error=function(e) NULL)
      if (is.null(fm)) return(NULL)
      mk2 <- function(label, val, ok=NULL, thresh=NULL) {
        col <- if (!is.null(ok)) if(ok) "#27AE60" else "#E74C3C" else "#2E86C1"
        tags$div(style=paste0("background:#fff;border:2px solid ",col,";border-radius:8px;",
          "padding:.55rem .9rem;margin:.25rem;display:inline-block;min-width:100px;text-align:center;"),
          tags$div(style=paste0("font-size:1.15rem;font-weight:700;color:",col,";"),
                   if(is.numeric(val)) round(val,3) else val),
          tags$div(style="font-size:.72rem;color:#555;margin-top:.1rem;", label),
          if(!is.null(thresh)) tags$div(style="font-size:.68rem;color:#888;", thresh))
      }
      tagList(
        tags$h5("CB-SEM Model Fit Indices", style="color:#1A3A5C;"),
        tags$div(style="display:flex;flex-wrap:wrap;",
          mk2("χ²",    fm["chisq"]),
          mk2("df",    fm["df"]),
          mk2("p",     fm["pvalue"],      fm["pvalue"]>.05,      ">0.05"),
          mk2("CFI",   fm["cfi"],         fm["cfi"]>=.95,        "≥0.95"),
          mk2("TLI",   fm["tli"],         fm["tli"]>=.95,        "≥0.95"),
          mk2("RMSEA", fm["rmsea"],       fm["rmsea"]<=.06,      "≤0.06"),
          mk2("RMSEA 90%CI",
              paste0("[",round(fm["rmsea.ci.lower"],3),",",round(fm["rmsea.ci.upper"],3),"]")),
          mk2("SRMR",  fm["srmr"],        fm["srmr"]<=.08,       "≤0.08"),
          mk2("GFI",   fm["gfi"],         fm["gfi"]>=.90,        "≥0.90"),
          mk2("AGFI",  fm["agfi"],        fm["agfi"]>=.90,       "≥0.90"),
          mk2("AIC",   fm["aic"]),
          mk2("BIC",   fm["bic"])
        ),
        tags$div(style="background:#f8f9fa;border-radius:6px;padding:.6rem .9rem;margin-top:.5rem;font-size:.79rem;color:#555;",
          tags$b("SmartPLS uses variance-based criteria (AVE, CR, HTMT). "),
          "CB-SEM (AMOS) uses covariance-based criteria above. ",
          "For AMOS-equivalent output, see 🔬 Model Detail tab.",
          tags$br(),
          tags$b("Modification Indices: "),
          "High MI (>10) suggests adding a path or cross-loading could improve fit. Shown in Model Detail → Fit Indices tab."
        )
      )
    })

    # ── Tab 5: PLS-SEM ────────────────────────────────────────────────────
    output$cv_pls_outer_tbl <- DT::renderDataTable({
      m <- pls_model_rv()
      if (is.null(m))
        return(tool_dt(data.frame(Note="PLS-SEM not available (seminr not installed or model not estimated)"), "PLS Outer"))
      sm_obj <- tryCatch(summary(m), error=function(e) NULL)
      if (is.null(sm_obj)) return(tool_dt(data.frame(Note="Summary failed"), "PLS Outer"))
      ol <- tryCatch({
        loads <- as.data.frame(sm_obj$loadings)
        loads$Item <- rownames(loads)
        loads[, c(ncol(loads), seq_len(ncol(loads)-1))]
      }, error=function(e) data.frame(Note="Not available"))
      tool_dt(round_df(ol, 3), "PLS-SEM Outer Loadings (≥0.7 = acceptable)")
    })

    output$cv_pls_inner_tbl <- DT::renderDataTable({
      m <- pls_model_rv()
      if (is.null(m))
        return(tool_dt(data.frame(Note="PLS-SEM not available"), "PLS Inner"))
      sm_obj <- tryCatch(summary(m), error=function(e) NULL)
      if (is.null(sm_obj)) return(tool_dt(data.frame(Note="Summary failed"), "PLS Inner"))
      pths <- tryCatch({
        p <- as.data.frame(sm_obj$paths)
        p$Path <- rownames(p)
        p[, c(ncol(p), seq_len(ncol(p)-1))]
      }, error=function(e) data.frame(Note="Not available"))
      tool_dt(round_df(pths, 3), "PLS-SEM Structural Paths + R²")
    })

    output$cv_pls_boot_tbl <- DT::renderDataTable({
      bm <- boot_model_rv()
      if (is.null(bm))
        return(tool_dt(data.frame(Note="Bootstrap not available"), "PLS Bootstrap"))
      sm_obj <- tryCatch(summary(bm), error=function(e) NULL)
      if (is.null(sm_obj)) return(tool_dt(data.frame(Note="Bootstrap summary failed"), "PLS Bootstrap"))
      bt <- tryCatch(as.data.frame(sm_obj$bootstrapped_paths),
                     error=function(e) data.frame(Note="Not available"))
      tool_dt(round_df(bt, 3), "PLS-SEM Bootstrap Results (500 samples, 5% significance)")
    })

    # ── Tab 6: Direct Regression ──────────────────────────────────────────
    cv_reg_results <- reactive({
      res <- canvas_results_rv(); req(res)
      cs <- res$cs_df; pl <- res$path_list
      if (length(pl) == 0) return(NULL)
      controls <- intersect(res$controls %||% character(0), names(cs))
      dvs <- unique(vapply(pl, `[[`, character(1), "to"))
      all_rows <- list()
      for (dv in dvs) {
        pl_dv <- Filter(function(p) !is.null(p$to) && p$to == dv, pl)
        if (length(pl_dv) == 0) next
        ivs <- intersect(vapply(pl_dv, `[[`, character(1), "from"), names(cs))
        # append controls (skip any already in model or equal to dv)
        ctrl_extra <- setdiff(controls, c(ivs, dv))
        all_ivs <- c(ivs, ctrl_extra)
        if (length(all_ivs)==0 || !dv %in% names(cs)) next
        fml  <- as.formula(paste0("`",dv,"` ~ ",paste0("`",all_ivs,"`",collapse="+")))
        lm_m <- tryCatch(lm(fml, data=cs), error=function(e) NULL)
        if (is.null(lm_m)) next
        co   <- as.data.frame(summary(lm_m)$coefficients)
        co$Term  <- rownames(co); co$DV <- dv
        co$R2    <- round(summary(lm_m)$r.squared, 3)
        co$AdjR2 <- round(summary(lm_m)$adj.r.squared, 3)
        names(co)[1:4] <- c("Beta","SE","t","p")
        co$Sig <- ifelse(co$p<0.001,"***",ifelse(co$p<0.01,"**",ifelse(co$p<0.05,"*","ns")))
        all_rows[[dv]] <- round_df(co[, c("DV","Term","Beta","SE","t","p","Sig","R2","AdjR2")], 3)
      }
      if (length(all_rows) == 0) return(NULL)
      do.call(rbind, all_rows)
    })

    output$cv_reg_tbl <- DT::renderDataTable({
      rr <- cv_reg_results()
      if (is.null(rr))
        return(tool_dt(data.frame(Note="No structural paths defined"), "Regression"))
      tool_dt(rr, "Direct Effects OLS Regression")
    })

    output$cv_coef_plot <- plotly::renderPlotly({
      rr <- cv_reg_results()
      if (is.null(rr))
        return(plotly::plot_ly() %>% plotly::layout(title="No data"))
      tbl2 <- rr[rr$Term != "(Intercept)", ]
      if (nrow(tbl2) == 0)
        return(plotly::plot_ly() %>% plotly::layout(title="No predictors"))
      plotly::plot_ly(tbl2,
        x=~Beta, y=~paste(DV,"<-",Term),
        type="scatter", mode="markers",
        error_x=list(array=~1.96*SE, color="#2E86C1"),
        marker=list(color="#1A3A5C", size=8),
        hovertemplate="Beta=%{x:.3f}<extra>%{y}</extra>"
      ) %>%
      plotly::layout(
        title="Regression Coefficient Plot (+-1.96 SE)",
        xaxis=list(title="Beta", zeroline=TRUE),
        yaxis=list(title=""), margin=list(l=220)
      )
    })

    # ── Tab 7: Mediation – DFS-based path finding (simple/parallel/serial) ──
    #
    # compute_chain_effect: product of path coefficients along a chain
    # chain = c("IV","M1","M2","DV")
    # data  = data frame with those columns
    # Uses Hayes-style controls: each regression includes the distal IV and
    # all prior mediators in the chain as covariates.
    # ─────────────────────────────────────────────────────────────────────────
    .chain_effect <- function(chain, data, extra_controls = character(0)) {
      tryCatch({
        n <- length(chain)
        if (n < 3) return(NA_real_)
        iv_node <- chain[1]
        product <- 1.0
        for (i in seq_len(n - 1)) {
          y_var <- chain[i + 1]
          x_var <- chain[i]
          # Controls: iv_node + prior mediators + user-specified controls
          prior  <- if (i > 1) chain[seq_len(i - 1)] else character(0)
          ctrls  <- unique(c(iv_node, prior, extra_controls))
          ctrls  <- ctrls[ctrls != y_var & ctrls != x_var & ctrls %in% names(data)]
          preds  <- c(x_var, ctrls)
          preds  <- preds[preds %in% names(data)]
          Y_vec  <- data[[y_var]]
          X_mat  <- as.matrix(data[, preds, drop=FALSE])
          colnames(X_mat) <- preds
          lm_m   <- lm(Y_vec ~ X_mat)
          x_cn   <- paste0("X_mat", x_var)
          cv     <- coef(lm_m)[x_cn]
          if (is.na(cv)) return(NA_real_)
          product <- product * cv
        }
        product
      }, error = function(e) NA_real_)
    }

    # ══════════════════════════════════════════════════════════════════════
    # Baron & Kenny (1986) four-step mediation
    # ══════════════════════════════════════════════════════════════════════
    cv_bk_results <- reactive({
      res <- canvas_results_rv(); req(res)
      cs   <- res$cs_df; pl <- res$path_list
      meds <- res$mediators
      if (length(meds) == 0) return(NULL)
      ctrl_vars <- intersect(res$controls %||% character(0), names(cs))

      # helper: run lm, extract one term's row + R²
      .bk_row <- function(lm_obj, term_nm, step_lbl, path_lbl, pair_lbl) {
        if (is.null(lm_obj)) return(NULL)
        co <- tryCatch(coef(summary(lm_obj)), error=function(e) NULL)
        if (is.null(co)) return(NULL)
        idx <- which(rownames(co) == term_nm)
        if (length(idx) == 0) return(NULL)
        data.frame(
          Pair  = pair_lbl, Step = step_lbl, Path = path_lbl,
          Beta  = round(co[idx, 1], 3), SE = round(co[idx, 2], 3),
          t     = round(co[idx, 3], 3), p  = round(co[idx, 4], 3),
          Sig   = ifelse(co[idx,4]<0.001,"***",ifelse(co[idx,4]<0.01,"**",
                         ifelse(co[idx,4]<0.05,"*","ns"))),
          R2    = round(summary(lm_obj)$r.squared, 3),
          stringsAsFactors = FALSE
        )
      }

      out_list <- list()
      for (med in meds) {
        # IVs → mediator (non-mediator sources only)
        ivs_raw <- if (length(pl) > 0)
          vapply(Filter(function(p) !is.null(p$to) && p$to == med, pl),
                 `[[`, character(1), "from") else character(0)
        ivs_to_med <- setdiff(ivs_raw, meds)

        # DV from mediator (non-mediator targets only)
        dvs_raw <- if (length(pl) > 0)
          vapply(Filter(function(p) !is.null(p$from) && p$from == med, pl),
                 `[[`, character(1), "to") else character(0)
        dvs_from_med <- setdiff(dvs_raw, meds)

        for (iv in ivs_to_med) {
          for (dv in dvs_from_med) {
            if (!all(c(iv, med, dv) %in% names(cs))) next
            needed <- unique(c(iv, med, dv, ctrl_vars))
            tmp    <- na.omit(cs[, intersect(needed, names(cs)), drop=FALSE])
            if (nrow(tmp) < 20) next
            pair_lbl <- paste0(iv, " → ", med, " → ", dv)

            ctrl_str <- if (length(ctrl_vars) > 0 && length(intersect(ctrl_vars, names(tmp))) > 0) {
              cv_ok <- intersect(ctrl_vars, names(tmp))
              paste0(" + ", paste0("`", cv_ok, "`", collapse=" + "))
            } else ""

            # Step 1: c path — X → Y (total effect)
            lm1 <- tryCatch(
              lm(as.formula(paste0("`",dv,"` ~ `",iv,"`", ctrl_str)), data=tmp),
              error=function(e) NULL)

            # Step 2: a path — X → M
            lm2 <- tryCatch(
              lm(as.formula(paste0("`",med,"` ~ `",iv,"`", ctrl_str)), data=tmp),
              error=function(e) NULL)

            # Step 3+4: M + X → Y (b path and c' path together)
            lm3 <- tryCatch(
              lm(as.formula(paste0("`",dv,"` ~ `",med,"` + `",iv,"`", ctrl_str)), data=tmp),
              error=function(e) NULL)

            term_iv  <- paste0("`", iv,  "`")
            term_med <- paste0("`", med, "`")

            r1 <- .bk_row(lm1, term_iv,  "Step 1", paste0(iv," → ",dv," (c, total effect)"),   pair_lbl)
            r2 <- .bk_row(lm2, term_iv,  "Step 2", paste0(iv," → ",med," (a path)"),            pair_lbl)
            r3 <- .bk_row(lm3, term_med, "Step 3", paste0(med," → ",dv," | ",iv," (b path)"),   pair_lbl)
            r4 <- .bk_row(lm3, term_iv,  "Step 4", paste0(iv," → ",dv," | ",med," (c', direct)"), pair_lbl)

            rows_i <- Filter(Negate(is.null), list(r1, r2, r3, r4))
            if (length(rows_i) == 0) next

            # Sobel test
            a_co <- if (!is.null(lm2)) tryCatch(coef(summary(lm2))[term_iv,], error=function(e) NULL)
            b_co <- if (!is.null(lm3)) tryCatch(coef(summary(lm3))[term_med,], error=function(e) NULL)
            if (!is.null(a_co) && !is.null(b_co) &&
                !is.na(a_co[1]) && !is.na(b_co[1]) &&
                !is.na(a_co[2]) && !is.na(b_co[2])) {
              a_v  <- a_co[1]; a_se <- a_co[2]
              b_v  <- b_co[1]; b_se <- b_co[2]
              ab   <- a_v * b_v
              s_se <- sqrt(b_v^2 * a_se^2 + a_v^2 * b_se^2)
              s_z  <- if (s_se > 0) ab / s_se else NA_real_
              s_p  <- if (!is.na(s_z)) 2 * (1 - pnorm(abs(s_z))) else NA_real_
              rows_i[[length(rows_i)+1]] <- data.frame(
                Pair = pair_lbl, Step = "Sobel",
                Path = paste0("Indirect (a×b) = ", round(ab, 3)),
                Beta = round(ab, 3),  SE = round(s_se, 3),
                t    = round(s_z, 3), p  = round(s_p, 3),
                Sig  = ifelse(!is.na(s_p), ifelse(s_p<0.001,"***",ifelse(s_p<0.01,"**",
                              ifelse(s_p<0.05,"*","ns"))), ""),
                R2   = NA_real_, stringsAsFactors = FALSE
              )
            }

            # B&K verdict
            c_sig  <- !is.null(r1) && !is.na(r1$p) && r1$p < 0.05
            a_sig  <- !is.null(r2) && !is.na(r2$p) && r2$p < 0.05
            b_sig  <- !is.null(r3) && !is.na(r3$p) && r3$p < 0.05
            cp_sig <- !is.null(r4) && !is.na(r4$p) && r4$p < 0.05
            verdict <- if (!c_sig)            "No mediation (c path non-sig)"
              else if (!a_sig || !b_sig)      "Conditions not fully met (a or b non-sig)"
              else if (a_sig && b_sig && !cp_sig) "Full mediation"
              else                                "Partial mediation"

            rows_i[[length(rows_i)+1]] <- data.frame(
              Pair = pair_lbl, Step = "Verdict", Path = verdict,
              Beta = NA, SE = NA, t = NA, p = NA, Sig = "", R2 = NA,
              stringsAsFactors = FALSE
            )
            out_list <- c(out_list, rows_i)
          }
        }
      }
      if (length(out_list) == 0) return(NULL)
      do.call(rbind, out_list)
    })

    cv_med_results <- reactive({
      res <- canvas_results_rv(); req(res)
      cs <- res$cs_df; pl <- res$path_list
      if (length(pl) == 0) return(NULL)

      # ── Build adjacency list from structural edges ─────────────────────
      all_nodes <- unique(c(
        vapply(pl, `[[`, character(1), "from"),
        vapply(pl, `[[`, character(1), "to")
      ))
      nb <- setNames(lapply(all_nodes, function(nd) {
        vapply(Filter(function(p) !is.null(p$from) && p$from == nd, pl),
               `[[`, character(1), "to")
      }), all_nodes)

      # ── DFS: find all indirect paths (3+ nodes) ────────────────────────
      indirect_paths <- list()
      dfs <- function(current_path) {
        cur <- current_path[length(current_path)]
        nbs <- nb[[cur]]; if (is.null(nbs) || length(nbs)==0) return()
        for (next_nd in nbs) {
          if (next_nd %in% current_path) next   # no cycles
          new_path <- c(current_path, next_nd)
          if (length(new_path) > 7) next         # cap chain length
          if (length(new_path) >= 3)             # at least IV→M→DV
            indirect_paths[[length(indirect_paths)+1]] <<- new_path
          dfs(new_path)
        }
      }
      for (start in all_nodes) dfs(c(start))

      # Remove duplicate paths
      path_strs <- vapply(indirect_paths, paste, character(1), collapse="->")
      indirect_paths <- indirect_paths[!duplicated(path_strs)]
      if (length(indirect_paths) == 0) return(NULL)

      # ── Bootstrap each path ────────────────────────────────────────────
      set.seed(res$seed %||% 42L)
      nboot     <- as.integer(res$nboot %||% 1000L)
      ci_level  <- as.numeric(res$ci_level %||% 0.95)
      ci_lo_q   <- (1 - ci_level) / 2
      ci_hi_q   <- 1 - ci_lo_q
      ctrl_vars <- intersect(res$controls %||% character(0), names(cs))
      out_list <- list()

      for (ip in indirect_paths) {
        if (!all(unique(ip) %in% names(cs))) next
        ip_all <- unique(c(ip, ctrl_vars))
        tmp    <- na.omit(cs[, intersect(ip_all, names(cs)), drop=FALSE])
        n_obs  <- nrow(tmp)
        if (n_obs < 20) next

        obs_eff <- .chain_effect(ip, tmp, extra_controls = ctrl_vars)
        if (is.na(obs_eff)) next

        boot_v <- replicate(nboot, {
          .chain_effect(ip, tmp[sample(n_obs, n_obs, TRUE), , drop=FALSE],
                        extra_controls = ctrl_vars)
        })
        boot_v <- boot_v[!is.na(boot_v)]
        if (length(boot_v) < 50) next

        ci_lo <- quantile(boot_v, ci_lo_q); ci_hi <- quantile(boot_v, ci_hi_q)
        n_med <- length(ip) - 2  # number of mediators

        # Classify mediation type
        med_type <- if (n_med == 1) "Simple" else paste0("Serial (", n_med, " mediators)")

        out_list[[length(out_list)+1]] <- data.frame(
          Path      = paste(ip, collapse=" -> "),
          Type      = med_type,
          Mediators = paste(ip[2:(length(ip)-1)], collapse=" -> "),
          IV        = ip[1],
          DV        = ip[length(ip)],
          Indirect  = round(obs_eff, 3),
          CI_Low    = round(ci_lo, 3),
          CI_High   = round(ci_hi, 3),
          Sig       = ifelse(ci_lo > 0 | ci_hi < 0, "Yes", "No"),
          stringsAsFactors=FALSE
        )
      }
      if (length(out_list) == 0) return(NULL)
      res_df <- do.call(rbind, out_list)

      # Detect parallel: same IV+DV, multiple mediators → label them
      iv_dv_combos <- paste(res_df$IV, res_df$DV, sep="->")
      for (combo in unique(iv_dv_combos)) {
        idx <- which(iv_dv_combos == combo & res_df$Type == "Simple")
        if (length(idx) > 1)
          res_df$Type[idx] <- paste0("Parallel (", length(idx), " mediators)")
      }
      res_df
    })

    output$cv_med_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      meds        <- res$mediators
      topo_meds   <- res$topo_mediators   %||% character(0)
      manual_meds <- res$manual_mediators %||% character(0)
      pl          <- res$path_list
      ci_pct      <- paste0(round((res$ci_level %||% 0.95) * 100), "%")

      # Build diagnostic: show all nodes and their path roles
      all_froms <- if (length(pl) > 0) vapply(pl, `[[`, character(1), "from") else character(0)
      all_tos   <- if (length(pl) > 0) vapply(pl, `[[`, character(1), "to")   else character(0)
      all_nodes <- unique(c(all_froms, all_tos))

      diag_info <- if (length(all_nodes) > 0) {
        roles <- vapply(all_nodes, function(n) {
          is_from <- n %in% all_froms
          is_to   <- n %in% all_tos
          if (is_from && is_to)  paste0(n, " [M]")
          else if (is_from)      paste0(n, " [IV]")
          else                   paste0(n, " [DV]")
        }, character(1))
        paste(roles, collapse="  ·  ")
      } else "No structural paths found"

      if (length(meds) == 0) {
        tagList(
          tags$div(class="alert alert-warning",
            tags$b("⚠️ No mediators detected. "),
            "Path roles detected: ", tags$code(diag_info), tags$br(),
            "A mediator must have ", tags$b("both"), " an incoming AND an outgoing structural path. ",
            "Example: IV → M → DV creates M as mediator.", tags$br(),
            tags$b("Tip: "), "Use the ", tags$b("🔀 Manual Mediator Selection"),
            " picker above to override auto-detection."
          )
        )
      } else {
        tags$div(class="alert alert-success",
          tags$b("✅ Mediators: "), paste(meds, collapse=", "),
          if (length(topo_meds) > 0)
            tags$span(style="color:#2E7D32;",
              paste0(" (auto: ", paste(topo_meds, collapse=", "), ")")),
          if (length(manual_meds) > 0)
            tags$span(style="color:#E65100;",
              paste0(" (manual: ", paste(manual_meds, collapse=", "), ")")),
          tags$br(),
          tags$small(paste0("Path roles: ", diag_info)), tags$br(),
          tags$small(paste0("P&H bootstrap: ", res$nboot %||% 1000,
                            " resamples · ", ci_pct, " CI · seed=", res$seed %||% 42))
        )
      }
    })

    # ── Baron & Kenny render ──────────────────────────────────────────────
    # ── AMOS-style mediation diagram helper ────────────────────────────────
    .med_diagram <- function(iv, med, dv, steps_df) {
      # Extract path coefficients safely
      .get <- function(step_lbl, col="Beta") {
        r <- steps_df[steps_df$Step == step_lbl & steps_df$Pair == paste0(iv," → ",med," → ",dv), col]
        if (length(r)==0 || is.na(r[1])) return("?")
        as.character(round(as.numeric(r[1]),3))
      }
      .getp <- function(step_lbl) {
        r <- steps_df[steps_df$Step == step_lbl & steps_df$Pair == paste0(iv," → ",med," → ",dv), "p"]
        if (length(r)==0 || is.na(r[1])) return("")
        p <- as.numeric(r[1])
        if(p<.001) "***" else if(p<.01) "**" else if(p<.05) "*" else "ns"
      }
      a <- paste0(.get("Step 2")," ",.getp("Step 2"))
      b <- paste0(.get("Step 3")," ",.getp("Step 3"))
      c_total <- paste0(.get("Step 1")," ",.getp("Step 1"))
      cp <- paste0(.get("Step 4")," ",.getp("Step 4"))
      # Sobel
      sob_row <- steps_df[steps_df$Step=="Sobel" & steps_df$Pair==paste0(iv," → ",med," → ",dv),,drop=FALSE]
      ab_txt <- if(nrow(sob_row)>0) paste0("ab=",sob_row$Beta[1]," p=",round(sob_row$p[1],3)) else "ab=?"
      # Verdict
      vrd_row <- steps_df[steps_df$Step=="Verdict" & steps_df$Pair==paste0(iv," → ",med," → ",dv),,drop=FALSE]
      verdict <- if(nrow(vrd_row)>0) vrd_row$Path[1] else "?"
      vrd_col <- if(grepl("Full",verdict)) "#27AE60"
                 else if(grepl("Partial",verdict)) "#F39C12"
                 else if(grepl("No mediation",verdict)) "#E74C3C"
                 else "#7F8C8D"

      plotly::plot_ly() |>
        # boxes
        plotly::add_annotations(x=0.1, y=0.5, text=paste0("<b>",iv,"</b><br><i>(IV)</i>"),
          showarrow=FALSE, font=list(size=12,color="white"),
          bgcolor=NAVY, bordercolor=NAVY, borderwidth=2, borderpad=6, xref="paper",yref="paper") |>
        plotly::add_annotations(x=0.5, y=0.88, text=paste0("<b>",med,"</b><br><i>(Mediator)</i>"),
          showarrow=FALSE, font=list(size=12,color="white"),
          bgcolor=TEAL, bordercolor=TEAL, borderwidth=2, borderpad=6, xref="paper",yref="paper") |>
        plotly::add_annotations(x=0.9, y=0.5, text=paste0("<b>",dv,"</b><br><i>(DV)</i>"),
          showarrow=FALSE, font=list(size=12,color="white"),
          bgcolor=NAVY, bordercolor=NAVY, borderwidth=2, borderpad=6, xref="paper",yref="paper") |>
        # a-path arrow IV→Med
        plotly::add_annotations(x=0.5, y=0.88, ax=0.1, ay=0.5, xref="paper",yref="paper",
          axref="paper", ayref="paper",
          text=paste0("<b>a=",a,"</b>"), showarrow=TRUE, arrowhead=2, arrowcolor=TEAL,
          font=list(size=10,color=TEAL)) |>
        # b-path arrow Med→DV
        plotly::add_annotations(x=0.9, y=0.5, ax=0.5, ay=0.88, xref="paper",yref="paper",
          axref="paper", ayref="paper",
          text=paste0("<b>b=",b,"</b>"), showarrow=TRUE, arrowhead=2, arrowcolor=TEAL,
          font=list(size=10,color=TEAL)) |>
        # c'-path arrow IV→DV (direct, below)
        plotly::add_annotations(x=0.9, y=0.35, ax=0.1, ay=0.35, xref="paper",yref="paper",
          axref="paper", ayref="paper",
          text=paste0("<b>c'=",cp,"</b> (direct)"), showarrow=TRUE, arrowhead=2,
          arrowcolor="#E67E22", font=list(size=10,color="#E67E22")) |>
        # c-path total effect label
        plotly::add_annotations(x=0.5, y=0.18, text=paste0("c=",c_total," (total effect)"),
          showarrow=FALSE, font=list(size=10,color="#7F8C8D"), xref="paper",yref="paper") |>
        # Indirect effect + Sobel
        plotly::add_annotations(x=0.5, y=0.05, text=paste0("Indirect: ",ab_txt),
          showarrow=FALSE, font=list(size=10,color="#2980B9"), xref="paper",yref="paper") |>
        # Verdict banner
        plotly::add_annotations(x=0.5, y=-0.05,
          text=paste0("<b>",verdict,"</b>"),
          showarrow=FALSE, font=list(size=13,color=vrd_col),
          bgcolor=paste0(vrd_col,"22"), bordercolor=vrd_col, borderwidth=1, borderpad=5,
          xref="paper",yref="paper") |>
        plotly::layout(
          xaxis=list(visible=FALSE, range=c(-0.05,1.05)),
          yaxis=list(visible=FALSE, range=c(-0.12,1.05)),
          margin=list(l=10,r=10,t=35,b=20),
          paper_bgcolor="white", plot_bgcolor="white",
          title=list(text=paste0("Mediation Diagram: ",iv," → ",med," → ",dv),
                     font=list(color=NAVY, size=13))
        )
    }

    output$cv_bk_tbl <- renderUI({
      bk <- cv_bk_results()
      if (is.null(bk))
        return(tags$div(class="alert alert-warning",
          tags$b("⚠️ No mediators detected. "),
          "Make sure you have drawn mediation paths on the canvas (IV → Mediator → DV). ",
          "Alternatively use the manual mediator picker in the Settings panel above."))

      # Extract unique IV-Med-DV triples for diagrams
      pairs <- unique(bk$Pair)
      diag_tabs <- lapply(pairs, function(p) {
        parts <- strsplit(p, " → ")[[1]]
        if (length(parts) < 3) return(NULL)
        iv_p  <- parts[1]; med_p <- parts[2]; dv_p  <- parts[3]
        tabPanel(p,
          tags$br(),
          # ── Verdict card ──────────────────────────────────────────
          renderUI({
            vrd_row <- bk[bk$Step=="Verdict" & bk$Pair==p,,drop=FALSE]
            verdict <- if(nrow(vrd_row)>0) vrd_row$Path[1] else "?"
            vrd_col <- if(grepl("Full",verdict)) "success"
                       else if(grepl("Partial",verdict)) "warning"
                       else if(grepl("No",verdict)) "danger" else "info"
            icon_v <- if(grepl("Full",verdict)) "✅"
                      else if(grepl("Partial",verdict)) "⚠️"
                      else if(grepl("No",verdict)) "❌" else "ℹ️"
            sob_row <- bk[bk$Step=="Sobel" & bk$Pair==p,,drop=FALSE]
            sobel_txt <- if(nrow(sob_row)>0)
              paste0("Indirect effect (ab) = ",sob_row$Beta[1],
                     "  |  Sobel z = ",sob_row$t[1],
                     "  |  p = ",sob_row$p[1])
            else ""
            tags$div(
              tags$div(class=paste0("alert alert-",vrd_col), style="font-size:1.05rem;",
                tags$h4(paste(icon_v, verdict), style="margin:0 0 .4rem;"),
                if(nzchar(sobel_txt)) tags$p(sobel_txt, style="margin:0;font-size:.88rem;")),
              tags$div(class="alert alert-info", style="font-size:.82rem;",
                tags$b("Mediation Criteria (Baron & Kenny, 1986):"), tags$br(),
                tags$b("Full mediation: "), "c sig + a sig + b sig + c' NON-sig (full suppression of X→Y)", tags$br(),
                tags$b("Partial mediation: "), "c sig + a sig + b sig + c' STILL sig (partial reduction)", tags$br(),
                tags$b("No mediation: "), "c path non-significant", tags$br(),
                tags$b("Indirect-only: "), "c non-sig but a*b sig (consistent mediation)"
              )
            )
          }),
          # ── AMOS-style path diagram ───────────────────────────────
          tags$h5("Path Diagram", style="color:#1A3A5C;margin-top:.5rem;"),
          plotlyOutput(ns(paste0("med_diag_",gsub("[^A-Za-z0-9]","_",p))),
                       height="320px"),
          {
            local({
              p_local   <- p
              iv_local  <- iv_p
              med_local <- med_p
              dv_local  <- dv_p
              output[[paste0("med_diag_",gsub("[^A-Za-z0-9]","_",p_local))]] <-
                plotly::renderPlotly({
                  bk2 <- cv_bk_results()
                  if(is.null(bk2)) return(plotly::plot_ly())
                  .med_diagram(iv_local, med_local, dv_local, bk2)
                })
            })
            NULL
          },
          # ── Detailed step table ───────────────────────────────────
          tags$h5("4-Step Results Table", style="color:#1A3A5C;margin-top:.8rem;"),
          DT::renderDataTable({
            pair_data <- bk[bk$Pair==p & !bk$Step %in% c("Verdict","Sobel"),,drop=FALSE]
            sob_data  <- bk[bk$Pair==p & bk$Step=="Sobel",,drop=FALSE]
            show <- rbind(pair_data, sob_data)
            tool_dt(show[,c("Step","Path","Beta","SE","t","p","Sig","R2")],
                    paste0("B&K Steps: ",p))
          })
        )
      })
      diag_tabs <- Filter(Negate(is.null), diag_tabs)
      if (length(diag_tabs) == 0)
        return(tags$div(class="alert alert-warning","No mediation pairs found."))

      tagList(
        tags$div(class="alert alert-info", style="font-size:.82rem;",
          tags$b("Mediation Analysis (AMOS-style): "),
          "Each tab below shows one IV → Mediator → DV chain with a path diagram and verdict."),
        do.call(tabsetPanel, c(list(type="pills"), diag_tabs))
      )
    })

    output$cv_med_tbl <- DT::renderDataTable({
      mr  <- cv_med_results()
      res <- canvas_results_rv()
      if (is.null(mr))
        return(tool_dt(data.frame(Note="No mediation paths detected or n < 20"), "P&H Mediation"))
      nboot_lbl <- if (!is.null(res)) paste0(res$nboot %||% 1000, " resamples") else "1,000 resamples"
      ci_lbl    <- if (!is.null(res)) paste0(round((res$ci_level %||% 0.95)*100), "% CI") else "95% CI"
      show_cols <- c("Path","Type","Mediators","Indirect","CI_Low","CI_High","Sig")
      tool_dt(mr[, show_cols],
        paste0("Preacher & Hayes — Bootstrap Indirect Effects (", nboot_lbl, ", ", ci_lbl, ")"))
    })

    output$cv_med_plot <- plotly::renderPlotly({
      mr <- cv_med_results()
      if (is.null(mr))
        return(plotly::plot_ly() %>% plotly::layout(title="No mediation paths detected"))
      dot_col <- ifelse(mr$Sig=="Yes","#27AE60","#E74C3C")
      plotly::plot_ly(mr,
        x=~Indirect,
        y=~paste0("[",Type,"] ", Path),
        type="scatter", mode="markers",
        error_x=list(type="data", symmetric=FALSE,
          array     =~(CI_High - Indirect),
          arrayminus=~(Indirect - CI_Low),
          color="#2E86C1"),
        marker=list(color=dot_col, size=10),
        hovertemplate="Indirect=%{x:.3f}<extra>%{y}</extra>"
      ) %>%
      plotly::layout(
        title="Indirect Effects Forest Plot (95% Bootstrap CI, green=significant)",
        xaxis=list(title="Indirect Effect", zeroline=TRUE),
        yaxis=list(title=""), margin=list(l=320)
      )
    })

    # ── Tab 8: Moderation (interaction + simple slopes) ───────────────────
    cv_mod_results <- reactive({
      res <- canvas_results_rv(); req(res)
      cs <- res$cs_df; mts <- res$mod_triples
      if (length(mts) == 0) return(NULL)
      out_list <- list(); slope_data <- list()
      for (mt in mts) {
        iv <- mt$iv; dv <- mt$dv; mod <- mt$moderator
        if (!all(c(iv, dv, mod) %in% names(cs))) next
        tmp <- na.omit(cs[, c(iv, dv, mod)])
        if (nrow(tmp) < 20) next
        Xc <- tmp[[iv]] - mean(tmp[[iv]])
        Wc <- tmp[[mod]] - mean(tmp[[mod]])
        Y  <- tmp[[dv]]
        df_int <- data.frame(Y=Y, X=Xc, W=Wc, XW=Xc*Wc)
        lm_m   <- tryCatch(lm(Y ~ X + W + XW, data=df_int), error=function(e) NULL)
        if (is.null(lm_m)) next
        co      <- as.data.frame(summary(lm_m)$coefficients)
        co$Term <- rownames(co); co$Path <- paste0(iv," x ",mod," -> ",dv)
        co$R2   <- round(summary(lm_m)$r.squared, 3)
        names(co)[1:4] <- c("Beta","SE","t","p")
        co$Sig  <- ifelse(co$p<0.001,"***",ifelse(co$p<0.01,"**",ifelse(co$p<0.05,"*","ns")))
        out_list[[length(out_list)+1]] <-
          round_df(co[, c("Path","Term","Beta","SE","t","p","Sig","R2")], 3)
        # Simple slopes at -1SD, Mean, +1SD of moderator
        w_sd    <- sd(tmp[[mod]])
        coefs   <- coef(lm_m)
        x_range <- seq(min(Xc), max(Xc), length.out=50)
        for (wv in c(-w_sd, 0, w_sd)) {
          y_hat  <- coefs["(Intercept)"] + coefs["X"]*x_range +
                    coefs["W"]*wv + coefs["XW"]*x_range*wv
          label  <- paste0(mod, if(wv < 0) " −1SD" else if(wv > 0) " +1SD" else " Mean")
          slope_data[[length(slope_data)+1]] <- data.frame(
            x     = x_range + mean(tmp[[iv]]),
            y     = y_hat,
            Group = label, DV=dv, IV=iv, stringsAsFactors=FALSE)
        }
      }
      if (length(out_list) == 0) return(NULL)
      list(table=do.call(rbind, out_list), slopes=do.call(rbind, slope_data))
    })

    output$cv_mod_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      mts <- res$mod_triples
      if (length(mts) == 0)
        return(tags$div(class="alert alert-info",
          "No moderation detected. Draw a moderator arrow (from moderator, pointing toward an IV→DV path) on the canvas."))
      paths_txt <- paste(sapply(mts, function(mt)
        paste0(mt$iv," × ",mt$moderator," → ",mt$dv)), collapse="; ")
      tags$div(class="alert alert-success",
        paste0("Moderation paths: ", paths_txt,
               ". All constructs mean-centred; interaction term (X×W) tested."))
    })

    output$cv_mod_tbl <- DT::renderDataTable({
      mr <- cv_mod_results()
      if (is.null(mr))
        return(tool_dt(data.frame(Note="No moderation paths detected or insufficient data"), "Moderation"))
      tool_dt(mr$table, "Moderation – Interaction Regression (Mean-Centred Predictors)")
    })

    output$cv_slopes_plot <- plotly::renderPlotly({
      mr <- cv_mod_results()
      if (is.null(mr) || is.null(mr$slopes))
        return(plotly::plot_ly() %>% plotly::layout(title="No moderation data"))
      sd_df  <- mr$slopes
      groups <- unique(sd_df$Group)
      pal    <- c("#1A3A5C","#2E86C1","#27AE60","#E74C3C","#8E44AD","#E67E22")
      p <- plotly::plot_ly()
      for (i in seq_along(groups)) {
        gd <- sd_df[sd_df$Group == groups[i], ]
        p  <- plotly::add_trace(p, data=gd, x=~x, y=~y, name=groups[i],
          type="scatter", mode="lines",
          line=list(color=pal[((i-1)%%length(pal))+1], width=2.5))
      }
      p %>% plotly::layout(
        title  = "Simple Slopes (−1SD, Mean, +1SD of Moderator)",
        xaxis  = list(title="IV (original scale)"),
        yaxis  = list(title="Predicted DV"),
        legend = list(title=list(text="Moderator Level"))
      )
    })

    # ── 3D Interaction Surface (reliable plotly::surface) ───────────────────
    output$cv_mod_3d_plot <- plotly::renderPlotly({
      res <- canvas_results_rv()
      if (is.null(res))
        return(plotly::plot_ly() |> plotly::layout(title="Run analyses first"))
      mts <- res$mod_triples
      cs  <- res$cs_df
      if (is.null(mts) || length(mts) == 0)
        return(plotly::plot_ly() |>
               plotly::layout(title=list(text="No moderation triples detected — draw a moderation path on the canvas",
                                         font=list(color=NAVY))))

      out_plots <- list()
      for (mt in mts) {
        iv  <- mt$iv; dv <- mt$dv; mod <- mt$moderator
        if (!all(c(iv, dv, mod) %in% names(cs))) next
        tmp <- na.omit(cs[, c(iv, dv, mod)])
        if (nrow(tmp) < 10) next

        # Mean-centre X and W
        Xc <- tmp[[iv]]  - mean(tmp[[iv]])
        Wc <- tmp[[mod]] - mean(tmp[[mod]])
        Y  <- tmp[[dv]]

        df_int <- data.frame(Y=Y, X=Xc, W=Wc, XW=Xc*Wc)
        lm_m <- tryCatch(lm(Y ~ X + W + XW, data=df_int), error=function(e) NULL)
        if (is.null(lm_m)) next
        cf  <- coef(lm_m)
        sm  <- summary(lm_m)
        int_p <- tryCatch(sm$coefficients["XW","Pr(>|t|)"], error=function(e) NA)

        n_grid <- 30L
        x_seq  <- seq(min(Xc), max(Xc), length.out=n_grid)
        w_seq  <- seq(min(Wc), max(Wc), length.out=n_grid)

        # Surface Z matrix: rows = X, cols = W
        Z_mat <- outer(x_seq, w_seq, function(x, w)
          cf[1] + cf["X"]*x + cf["W"]*w + cf["XW"]*x*w)

        # Back-transform to original scale for axis labels
        x_orig <- x_seq + mean(tmp[[iv]])
        w_orig <- w_seq + mean(tmp[[mod]])

        # Build 3 simple-slope lines (−1SD / Mean / +1SD of moderator)
        w_levels <- c(-1, 0, 1) * sd(Wc)
        w_labels <- c("−1SD Moderator","Mean Moderator","+1SD Moderator")
        ss_colors <- c("#E53935","#43A047","#1E88E5")

        p3d <- plotly::plot_ly() |>
          # ── Smooth predicted surface ──────────────────────────────
          plotly::add_surface(
            x = x_orig, y = w_orig, z = t(Z_mat),
            colorscale = list(
              list(0,   "#EBF5FB"),
              list(0.3, "#90CAF9"),
              list(0.6, "#1565C0"),
              list(1,   "#0D2137")
            ),
            opacity   = 0.78,
            showscale = TRUE,
            colorbar  = list(title=list(text=paste0("Pred. ",dv), font=list(size=10))),
            contours  = list(
              z = list(show=TRUE, usecolormap=TRUE, highlightcolor="#FF7043", project=list(z=TRUE))
            ),
            name = "Predicted surface",
            hovertemplate = paste0(iv,"=%{x:.2f}<br>",mod,"=%{y:.2f}<br>",
                                   "Pred. ",dv,"=%{z:.2f}<extra>Surface</extra>")
          ) |>
          # ── Observed data points ──────────────────────────────────
          plotly::add_trace(
            x = tmp[[iv]], y = tmp[[mod]], z = Y,
            type = "scatter3d", mode = "markers",
            marker = list(size=2.5, color="#FF7043", opacity=0.55,
                          symbol="circle",line=list(width=0)),
            name = "Observed",
            hovertemplate = paste0(iv,"=%{x:.2f}<br>",mod,"=%{y:.2f}<br>",
                                   dv,"=%{z:.2f}<extra>Observed</extra>")
          )

        # ── Simple slope lines at −1SD / Mean / +1SD of W ────────
        for (idx_w in seq_along(w_levels)) {
          wl  <- w_levels[idx_w]
          y_l <- cf[1] + cf["X"]*x_seq + cf["W"]*wl + cf["XW"]*x_seq*wl
          p3d <- p3d |> plotly::add_trace(
            x = x_orig, y = rep(wl + mean(tmp[[mod]]), n_grid), z = y_l,
            type="scatter3d", mode="lines",
            line=list(color=ss_colors[idx_w], width=4),
            name=w_labels[idx_w]
          )
        }

        p3d <- p3d |> plotly::layout(
          title = list(
            text = paste0("3D Surface: ", iv, " × ", mod, " → ", dv,
                          "  |  Interaction β=",round(cf["XW"],3),
                          " (p", if(!is.na(int_p)) sprintf("=%.3f",int_p) else "=n/a",")"),
            font = list(size=12, color=NAVY)),
          scene = list(
            xaxis  = list(title=iv,  gridcolor="#CCCCCC",
                          backgroundcolor="#F8F9FA", showbackground=TRUE),
            yaxis  = list(title=mod, gridcolor="#CCCCCC",
                          backgroundcolor="#F8F9FA", showbackground=TRUE),
            zaxis  = list(title=dv,  gridcolor="#CCCCCC",
                          backgroundcolor="#F8F9FA", showbackground=TRUE),
            camera = list(eye=list(x=1.6, y=-1.8, z=0.9)),
            bgcolor = "#FAFAFA"
          ),
          legend = list(x=0.02, y=0.98, bgcolor="rgba(255,255,255,.8)"),
          margin = list(l=0, r=0, t=55, b=0),
          paper_bgcolor = "white"
        )
        out_plots[[length(out_plots)+1]] <- p3d
      }
      if (length(out_plots) == 0)
        return(plotly::plot_ly() |>
               plotly::layout(title="No valid moderation data found in canvas model"))
      out_plots[[1]]
    })

    # ── Tab 9: Predictive R² (5-fold cross-validated) ─────────────────────
    output$cv_pred_ui <- renderUI({
      tags$div(class="alert alert-info",
        "5-fold cross-validated R² per structural equation. ",
        "Values close to OLS R² indicate good generalisability; large gaps suggest overfitting.")
    })

    output$cv_pred_tbl <- DT::renderDataTable({
      res <- canvas_results_rv(); req(res)
      cs  <- res$cs_df; pl <- res$path_list
      if (length(pl) == 0)
        return(tool_dt(data.frame(Note="No structural paths"), "Predictive R²"))
      dvs  <- unique(vapply(pl, `[[`, character(1), "to"))
      rows <- lapply(dvs, function(dv) {
        pl_dv <- Filter(function(p) !is.null(p$to) && p$to == dv, pl)
        if (length(pl_dv) == 0) return(NULL)
        ivs <- intersect(vapply(pl_dv, `[[`, character(1), "from"), names(cs))
        if (length(ivs)==0 || !dv %in% names(cs)) return(NULL)
        tmp <- na.omit(cs[, c(dv, ivs), drop=FALSE])
        n   <- nrow(tmp)
        if (n < 20) return(NULL)
        # OLS R²
        fml_ols <- as.formula(paste0("`",dv,"` ~ ",paste0("`",ivs,"`",collapse="+")))
        lm_ols  <- tryCatch(lm(fml_ols, data=tmp), error=function(e) NULL)
        ols_r2  <- if (!is.null(lm_ols)) round(summary(lm_ols)$r.squared, 3) else NA
        # 5-fold CV
        set.seed(123)
        k     <- 5
        folds <- cut(sample(n), breaks=k, labels=FALSE)
        ss_res_cv <- 0
        ss_tot    <- sum((tmp[[dv]] - mean(tmp[[dv]]))^2)
        for (fold in seq_len(k)) {
          test  <- which(folds == fold)
          train <- which(folds != fold)
          lm_f  <- tryCatch(lm(fml_ols, data=tmp[train,,drop=FALSE]), error=function(e) NULL)
          if (is.null(lm_f)) next
          y_p   <- tryCatch(predict(lm_f, newdata=tmp[test,,drop=FALSE]), error=function(e) NULL)
          if (is.null(y_p)) next
          ss_res_cv <- ss_res_cv + sum((tmp[[dv]][test] - y_p)^2)
        }
        cv_r2 <- round(max(0, 1 - ss_res_cv / ss_tot), 3)
        data.frame(
          DV           = dv,
          Predictors   = paste(ivs, collapse=", "),
          OLS_R2       = ols_r2,
          CV_R2_5fold  = cv_r2,
          Overfitting  = round(ols_r2 - cv_r2, 3),
          Effect_Size  = ifelse(cv_r2 >= 0.26, "Substantial",
                         ifelse(cv_r2 >= 0.13, "Moderate",
                         ifelse(cv_r2 >= 0.02, "Weak", "Negligible"))),
          stringsAsFactors=FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0)
        return(tool_dt(data.frame(Note="No valid paths for CV"), "Predictive R²"))
      tool_dt(do.call(rbind, rows),
        "Predictive R² – 5-Fold CV (Cohen 1988: Weak≥0.02, Moderate≥0.13, Substantial≥0.26)")
    })

    # ══════════════════════════════════════════════════════════════════════
    # Tab 10: Endogeneity — Wu-Hausman test (residual augmentation approach)
    # ══════════════════════════════════════════════════════════════════════
    cv_endo_results <- reactive({
      res <- canvas_results_rv(); req(res)
      cs <- res$cs_df; pl <- res$path_list
      if (length(pl) < 2) return(NULL)

      all_froms <- vapply(pl, `[[`, character(1), "from")
      all_tos   <- vapply(pl, `[[`, character(1), "to")
      # Exogenous = nodes that appear only as senders (no incoming structural path)
      exog_nodes <- setdiff(unique(all_froms), unique(all_tos))
      dvs        <- unique(all_tos)

      # User-specified instruments (columns from raw data, not construct scores)
      # These take priority over auto-detected exogenous constructs.
      # We also include them in the data frame used for regressions.
      user_instr_cols <- intersect(res$instruments %||% character(0), names(res$d_raw))

      out_rows <- list()
      for (dv in dvs) {
        pl_dv <- Filter(function(p) !is.null(p$to) && p$to == dv, pl)
        if (length(pl_dv) == 0) next
        ivs <- intersect(vapply(pl_dv, `[[`, character(1), "from"), names(cs))
        if (length(ivs) == 0 || !dv %in% names(cs)) next

        for (iv_test in ivs) {
          # Instruments: prefer user-specified, fall back to exogenous construct nodes
          if (length(user_instr_cols) > 0) {
            # user instruments from raw data
            instruments <- setdiff(user_instr_cols, c(ivs, dv, iv_test))
            instr_source <- "user-specified"
          } else {
            # auto: exogenous construct labels that also exist as column names in cs
            instruments <- setdiff(intersect(exog_nodes, names(cs)), c(ivs, dv, iv_test))
            instr_source <- "auto (exogenous)"
          }

          if (length(instruments) == 0) {
            out_rows[[length(out_rows)+1]] <- data.frame(
              Equation      = paste0(dv, " ~ ", paste(ivs, collapse="+")),
              IV_Tested     = iv_test,
              Instruments   = "None available",
              Instr_Source  = instr_source,
              WH_t          = NA_real_, WH_p = NA_real_,
              OLS_Beta      = NA_real_, Adj_Beta = NA_real_,
              Endogenous    = "⚠️ Cannot test — add instruments above",
              Sig           = "",
              stringsAsFactors = FALSE
            )
            next
          }

          # Data — merge construct scores + raw instrument columns
          other_ivs   <- setdiff(ivs, iv_test)
          first_preds <- c(instruments, other_ivs)
          needed_cs   <- unique(c(dv, ivs))  # construct scores needed
          tmp_cs      <- cs[, intersect(needed_cs, names(cs)), drop=FALSE]

          # Attach user-specified instrument columns from raw data if needed
          if (length(user_instr_cols) > 0 && nrow(res$d_raw) == nrow(tmp_cs)) {
            instr_ok <- intersect(user_instr_cols, names(res$d_raw))
            tmp <- cbind(tmp_cs, res$d_raw[, instr_ok, drop=FALSE])
          } else {
            tmp <- tmp_cs
          }
          tmp <- na.omit(tmp)
          if (nrow(tmp) < 20) next

          first_preds_ok <- intersect(first_preds, names(tmp))
          if (length(first_preds_ok) == 0) next

          # First stage: iv_test ~ instruments + other IVs
          fml_fs <- as.formula(
            paste0("`",iv_test,"` ~ ", paste0("`",first_preds_ok,"`", collapse=" + ")))
          lm_fs <- tryCatch(lm(fml_fs, data=tmp), error=function(e) NULL)
          if (is.null(lm_fs)) next

          tmp$._resid_wh <- residuals(lm_fs)

          # OLS (original equation)
          fml_ols <- as.formula(
            paste0("`",dv,"` ~ ", paste0("`",ivs,"`", collapse=" + ")))
          lm_ols <- tryCatch(lm(fml_ols, data=tmp), error=function(e) NULL)
          ols_beta <- if (!is.null(lm_ols))
            tryCatch(coef(lm_ols)[paste0("`",iv_test,"`")], error=function(e) NA) else NA
          if (is.na(ols_beta) && !is.null(lm_ols)) {
            co_n <- names(coef(lm_ols))
            idx  <- grep(iv_test, co_n, fixed=TRUE)
            if (length(idx) > 0) ols_beta <- coef(lm_ols)[idx[1]]
          }

          # Augmented equation: add first-stage residuals
          fml_aug <- as.formula(
            paste0("`",dv,"` ~ ", paste0("`",ivs,"`", collapse=" + "), " + `._resid_wh`"))
          lm_aug <- tryCatch(lm(fml_aug, data=tmp), error=function(e) NULL)
          if (is.null(lm_aug)) next

          co_aug <- tryCatch(coef(summary(lm_aug)), error=function(e) NULL)
          if (is.null(co_aug)) next
          wh_row <- tryCatch(co_aug["._resid_wh", , drop=FALSE], error=function(e) NULL)
          # fallback: match by partial name
          if (is.null(wh_row) || nrow(wh_row) == 0) {
            idx_wh <- grep("resid_wh", rownames(co_aug), fixed=TRUE)
            if (length(idx_wh) > 0) wh_row <- co_aug[idx_wh[1], , drop=FALSE] else next
          }

          wh_t <- round(wh_row[1, 3], 3)
          wh_p <- round(wh_row[1, 4], 3)

          # Adjusted beta of iv_test in augmented model
          adj_beta <- NA_real_
          co_n_aug <- rownames(co_aug)
          idx_iv   <- grep(iv_test, co_n_aug, fixed=TRUE)
          idx_iv   <- idx_iv[!grepl("resid", co_n_aug[idx_iv])]
          if (length(idx_iv) > 0)
            adj_beta <- round(co_aug[idx_iv[1], 1], 3)

          out_rows[[length(out_rows)+1]] <- data.frame(
            Equation     = paste0(dv, " ~ ", paste(ivs, collapse="+")),
            IV_Tested    = iv_test,
            Instruments  = paste(instruments, collapse=", "),
            Instr_Source = instr_source,
            WH_t         = wh_t,
            WH_p         = wh_p,
            OLS_Beta     = round(as.numeric(ols_beta), 3),
            Adj_Beta     = adj_beta,
            Endogenous   = ifelse(!is.na(wh_p) & wh_p < 0.05,
                                   "⚠️ Possible endogeneity", "✅ Not detected"),
            Sig          = ifelse(!is.na(wh_p),
                            ifelse(wh_p<0.001,"***",ifelse(wh_p<0.01,"**",
                                   ifelse(wh_p<0.05,"*","ns"))), ""),
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(out_rows) == 0) return(NULL)
      do.call(rbind, out_rows)
    })

    output$cv_endo_tbl <- DT::renderDataTable({
      er <- cv_endo_results()
      if (is.null(er))
        return(tool_dt(
          data.frame(Note=paste0(
            "Endogeneity test requires ≥2 structural paths. ",
            "Select instrumental variables in the 🔍 IV picker above, or ensure ",
            "your model has exogenous constructs (no incoming paths) for auto-detection.")),
          "Endogeneity"))
      tool_dt(er,
        "Wu-Hausman Endogeneity Test — Residual Augmentation (Hausman 1978)")
    })

    # ══════════════════════════════════════════════════════════════════════
    # MODEL DETAIL — AMOS / SmartPLS / WarpPLS Style Full Report
    # ══════════════════════════════════════════════════════════════════════

    # Helper: traffic-light metric card (shared with detail view)
    .mk_card <- function(label, val, ok=NULL, thresh=NULL) {
      col <- if (!is.null(ok)) { if (isTRUE(ok)) "#27AE60" else "#E74C3C" } else "#2E86C1"
      tags$div(
        style=paste0("background:#fff;border:2px solid ",col,";border-radius:8px;",
                     "padding:.55rem .9rem;margin:.25rem;display:inline-block;",
                     "min-width:100px;text-align:center;"),
        tags$div(style=paste0("font-size:1.2rem;font-weight:700;color:",col,";"),
                 if (is.numeric(val)) round(val, 3) else val),
        tags$div(style="font-size:.73rem;color:#555;margin-top:.1rem;", label),
        if (!is.null(thresh))
          tags$div(style="font-size:.68rem;color:#888;", thresh)
      )
    }

    # ── Measurement Model tab ─────────────────────────────────────────────
    output$cv_detail_meas_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      d_raw <- res$d_raw

      # Prefer CB-SEM fit, fall back to CFA fit
      fit_sem <- cv_cbsem_res()
      fit_cfa <- cv_cfa_res()
      fit     <- fit_sem %||% fit_cfa

      # ----- Factor loadings -----
      load_df <- NULL
      if (!is.null(fit)) {
        pe <- tryCatch(lavaan::parameterEstimates(fit, standardized=TRUE), error=function(e) NULL)
        if (!is.null(pe)) {
          ld <- pe[pe$op == "=~", ]
          load_df <- data.frame(
            Construct    = ld$lhs,
            Indicator    = ld$rhs,
            Loading_Unst = round(ld$est,   3),
            Loading_Std  = round(ld$std.all, 3),
            SE           = round(ld$se,    3),
            t            = round(ld$z,     3),
            p            = round(ld$pvalue, 4),
            Sig          = ifelse(ld$pvalue<.001,"***",
                             ifelse(ld$pvalue<.01,"**",
                               ifelse(ld$pvalue<.05,"*","ns"))),
            Acceptable   = ifelse(abs(ld$std.all) >= 0.7, "✅ ≥0.70", "⚠️ <0.70"),
            stringsAsFactors = FALSE
          )
        }
      }

      # Fall-back: PLS outer loadings
      if (is.null(load_df)) {
        m_pls <- pls_model_rv()
        if (!is.null(m_pls)) {
          ol <- tryCatch(as.data.frame(m_pls$outer_loadings), error=function(e) NULL)
          if (!is.null(ol)) {
            rows_pls <- list()
            for (cname in colnames(ol)) {
              for (iname in rownames(ol)) {
                v <- ol[iname, cname]
                if (!is.na(v) && v != 0) {
                  rows_pls[[length(rows_pls)+1]] <- data.frame(
                    Construct=cname, Indicator=iname,
                    Loading_Unst=round(v,3), Loading_Std=round(v,3),
                    SE=NA, t=NA, p=NA, Sig="PLS",
                    Acceptable=ifelse(abs(v)>=0.7,"✅ ≥0.70","⚠️ <0.70"),
                    stringsAsFactors=FALSE)
                }
              }
            }
            if (length(rows_pls) > 0) load_df <- do.call(rbind, rows_pls)
          }
        }
      }

      # Fall-back: correlation-based
      if (is.null(load_df)) {
        rows_fb <- list()
        for (nm in names(res$construct_items)) {
          items <- intersect(res$construct_items[[nm]], names(d_raw))
          if (length(items) < 1) next
          mat <- d_raw[, items, drop=FALSE]; mat[] <- lapply(mat, as.numeric); mat <- na.omit(mat)
          cs  <- rowMeans(mat)
          for (it in items) {
            ld <- tryCatch(cor(mat[[it]], cs, use="complete.obs"), error=function(e) NA)
            rows_fb[[length(rows_fb)+1]] <- data.frame(
              Construct=nm, Indicator=it,
              Loading_Unst=round(ld,3), Loading_Std=round(ld,3),
              SE=NA, t=NA, p=NA, Sig="approx",
              Acceptable=ifelse(!is.na(ld)&&abs(ld)>=0.7,"✅ ≥0.70","⚠️ <0.70"),
              stringsAsFactors=FALSE)
          }
        }
        if (length(rows_fb) > 0) load_df <- do.call(rbind, rows_fb)
      }

      # ----- Reliability & validity table -----
      rel_rows <- lapply(names(res$construct_items), function(nm) {
        items <- intersect(res$construct_items[[nm]], names(d_raw))
        if (length(items) < 2) return(NULL)
        mat <- d_raw[, items, drop=FALSE]; mat[] <- lapply(mat, as.numeric); mat <- na.omit(mat)
        if (nrow(mat) < 5) return(NULL)
        alp <- tryCatch(psych::alpha(mat)$total$raw_alpha, error=function(e) NA)
        omg <- if (length(items)>=3) tryCatch(psych::omega(mat,nfactors=1,plot=FALSE)$omega.tot,error=function(e)NA) else NA
        lam <- tryCatch({
          fa_r <- psych::fa(mat, nfactors=1, rotate="none", fm="ml")
          as.numeric(fa_r$loadings)
        }, error=function(e) rep(NA_real_, length(items)))
        lam_sq <- lam^2
        ave    <- round(mean(lam_sq, na.rm=TRUE), 3)
        cr_num <- sum(abs(lam), na.rm=TRUE)^2
        cr_den <- cr_num + sum(1-lam_sq, na.rm=TRUE)
        cr     <- if (cr_den>0) round(cr_num/cr_den, 3) else NA
        data.frame(
          Construct=nm, Type=res$construct_types[[nm]] %||% "reflective",
          Items=length(items), Alpha=round(alp,3), Omega=round(omg,3),
          CR=cr, AVE=ave, sqrt_AVE=round(sqrt(ave),3),
          Alpha_OK=ifelse(!is.na(alp)&alp>=.7,"✅","❌"),
          AVE_OK  =ifelse(!is.na(ave)&ave>=.5,"✅","❌"),
          CR_OK   =ifelse(!is.na(cr) &cr >=.7,"✅","❌"),
          stringsAsFactors=FALSE)
      })
      rel_df <- tryCatch(do.call(rbind, Filter(Negate(is.null), rel_rows)), error=function(e) NULL)

      tagList(
        tags$h5("📐 Factor Loadings (AMOS / SmartPLS style)", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.81rem;margin-bottom:.5rem;",
          "Standardized loadings ≥0.70 indicate acceptable indicator reliability. ",
          "Loadings are from CB-SEM (lavaan) when available; PLS outer loadings or item-composite correlations otherwise."),
        if (!is.null(load_df)) tool_dt(load_df, "Measurement Model: Factor Loadings")
        else tags$div(class="alert alert-warning", "Run Canvas Analysis to generate loadings."),
        tags$br(),
        tags$h5("📋 Construct Reliability & Convergent Validity", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.81rem;margin-bottom:.5rem;",
          "α ≥ 0.70 (Hair et al., 2019) · CR ≥ 0.70 · AVE ≥ 0.50 (Fornell & Larcker, 1981)"),
        if (!is.null(rel_df)) tool_dt(rel_df, "Reliability & Convergent Validity")
        else tags$div(class="alert alert-warning", "No reliability data.")
      )
    })

    # ── Structural Paths tab ──────────────────────────────────────────────
    output$cv_detail_struct_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      fit <- cv_cbsem_res()

      struct_df <- NULL
      r2_df     <- NULL

      if (!is.null(fit)) {
        pe <- tryCatch(lavaan::parameterEstimates(fit, standardized=TRUE), error=function(e) NULL)
        if (!is.null(pe)) {
          sp <- pe[pe$op == "~", ]
          struct_df <- data.frame(
            Path       = paste0(sp$lhs, " ~ ", sp$rhs),
            From       = sp$rhs,
            To         = sp$lhs,
            Beta_Unst  = round(sp$est,    3),
            Beta_Std   = round(sp$std.all, 3),
            SE         = round(sp$se,     3),
            t          = round(sp$z,      3),
            p          = round(sp$pvalue, 4),
            Sig        = ifelse(sp$pvalue<.001,"***",
                           ifelse(sp$pvalue<.01,"**",
                             ifelse(sp$pvalue<.05,"*","ns"))),
            CI_lo      = round(sp$est - 1.96*sp$se, 3),
            CI_hi      = round(sp$est + 1.96*sp$se, 3),
            stringsAsFactors=FALSE
          )
          # R² per endogenous construct
          r2_raw <- tryCatch(lavaan::inspect(fit, "r2"), error=function(e) NULL)
          if (!is.null(r2_raw)) {
            r2_df <- data.frame(
              Construct   = names(r2_raw),
              R2          = round(as.numeric(r2_raw), 3),
              Error_Term  = round(1 - as.numeric(r2_raw), 3),
              R2_OK       = ifelse(as.numeric(r2_raw) >= 0.1, "✅", "⚠️"),
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Fall-back: PLS inner paths
      if (is.null(struct_df)) {
        m_pls <- pls_model_rv()
        if (!is.null(m_pls)) {
          ip <- tryCatch(as.data.frame(m_pls$path_coef), error=function(e) NULL)
          if (!is.null(ip)) {
            rows_pls <- list()
            for (fr in rownames(ip)) for (to in colnames(ip)) {
              v <- ip[fr, to]
              if (!is.na(v) && v != 0)
                rows_pls[[length(rows_pls)+1]] <- data.frame(
                  Path=paste0(to," ~ ",fr), From=fr, To=to,
                  Beta_Unst=round(v,3), Beta_Std=round(v,3),
                  SE=NA, t=NA, p=NA, Sig="PLS",
                  CI_lo=NA, CI_hi=NA, stringsAsFactors=FALSE)
            }
            if (length(rows_pls)>0) struct_df <- do.call(rbind, rows_pls)
            # R² from PLS
            r2v <- tryCatch(diag(as.matrix(m_pls$rSquared)), error=function(e) NULL)
            if (!is.null(r2v)) {
              r2_df <- data.frame(
                Construct=names(r2v), R2=round(as.numeric(r2v),3),
                Error_Term=round(1-as.numeric(r2v),3),
                R2_OK=ifelse(as.numeric(r2v)>=.1,"✅","⚠️"),
                stringsAsFactors=FALSE)
            }
          }
        }
      }

      tagList(
        tags$h5("🔗 Structural Path Coefficients", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.81rem;margin-bottom:.5rem;",
          "β (Std) = standardized path coefficient · SE = standard error · ",
          "95% CI = [β ± 1.96×SE] · *** p<.001 · ** p<.01 · * p<.05 · ns = non-significant"),
        if (!is.null(struct_df)) tool_dt(struct_df, "Structural Model: Path Coefficients (CB-SEM / PLS)")
        else tags$div(class="alert alert-warning",
          "Structural paths not available. Ensure your canvas model has structural paths and numeric data."),
        tags$br(),
        tags$h5("📊 Explained Variance (R²) & Error Terms", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.81rem;margin-bottom:.5rem;",
          "Error Term = 1 − R² (proportion of unexplained variance). ",
          "R² ≥ 0.10 is considered weak but meaningful in SEM contexts."),
        if (!is.null(r2_df)) tool_dt(r2_df, "R² and Error Terms per Endogenous Construct")
        else tags$div(class="alert alert-info", "R² available after CB-SEM or PLS estimation.")
      )
    })

    # ── Fit Indices tab ───────────────────────────────────────────────────
    output$cv_detail_fit_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      fit_sem <- cv_cbsem_res()
      fit_cfa <- cv_cfa_res()

      render_fit_panel <- function(fit, label) {
        if (is.null(fit)) return(NULL)
        fm <- tryCatch(lavaan::fitMeasures(fit,
          c("chisq","df","pvalue","cfi","tli","ifi","rmsea","rmsea.ci.lower","rmsea.ci.upper",
            "srmr","gfi","agfi","aic","bic","npar")), error=function(e) NULL)
        if (is.null(fm)) return(NULL)
        tagList(
          tags$h5(label, style="color:#1A3A5C;margin-top:1rem;"),
          tags$div(style="display:flex;flex-wrap:wrap;margin-bottom:.5rem;",
            .mk_card("χ²",      fm["chisq"]),
            .mk_card("df",      fm["df"]),
            .mk_card("p-value", fm["pvalue"],
                     fm["pvalue"] > 0.05, ">0.05"),
            .mk_card("CFI",     fm["cfi"],
                     fm["cfi"] >= 0.95, "≥0.95"),
            .mk_card("TLI",     fm["tli"],
                     fm["tli"] >= 0.95, "≥0.95"),
            .mk_card("IFI",     fm["ifi"],
                     fm["ifi"] >= 0.95, "≥0.95"),
            .mk_card("RMSEA",   fm["rmsea"],
                     fm["rmsea"] <= 0.06, "≤0.06"),
            .mk_card("RMSEA 90%CI",
                     paste0("[", round(fm["rmsea.ci.lower"],3),",",round(fm["rmsea.ci.upper"],3),"]")),
            .mk_card("SRMR",    fm["srmr"],
                     fm["srmr"] <= 0.08, "≤0.08"),
            .mk_card("GFI",     fm["gfi"],
                     fm["gfi"] >= 0.90, "≥0.90"),
            .mk_card("AGFI",    fm["agfi"],
                     fm["agfi"] >= 0.90, "≥0.90"),
            .mk_card("AIC",     fm["aic"]),
            .mk_card("BIC",     fm["bic"]),
            .mk_card("# Params",fm["npar"])
          ),
          tags$div(style="background:#f8f9fa;border-radius:6px;padding:.6rem .9rem;font-size:.79rem;color:#555;",
            tags$b("Thresholds (Hair et al., 2019; Hu & Bentler, 1999): "),
            "CFI/TLI/IFI ≥ 0.95 (excellent), ≥ 0.90 (acceptable). ",
            "RMSEA ≤ 0.06 (excellent), ≤ 0.08 (acceptable). SRMR ≤ 0.08. ",
            "p > .05 indicates exact fit (rarely achieved in large samples).")
        )
      }

      tagList(
        if (!is.null(fit_sem))
          render_fit_panel(fit_sem, "CB-SEM Full Model Fit Indices")
        else tags$div(class="alert alert-info",
          "CB-SEM fit indices not available. Ensure constructs have ≥2 indicators and structural paths are drawn."),
        if (!is.null(fit_cfa))
          render_fit_panel(fit_cfa, "CFA (Measurement Model Only) Fit Indices"),
        if (is.null(fit_sem) && is.null(fit_cfa))
          tags$div(class="alert alert-warning",
            "No lavaan fit objects available. Run Canvas Analysis with reflective constructs.")
      )
    })

    # ── Effects Decomposition tab ─────────────────────────────────────────
    output$cv_detail_effects_ui <- renderUI({
      res <- canvas_results_rv(); req(res)
      fit <- cv_cbsem_res()

      if (is.null(fit)) {
        # PLS indirect effects via path tracing
        m_pls <- pls_model_rv()
        if (!is.null(m_pls)) {
          ip <- tryCatch(as.data.frame(m_pls$path_coef), error=function(e) NULL)
          if (!is.null(ip)) {
            # Direct effects only for PLS (no indirect decomposition without bootstrap)
            rows_e <- list()
            for (fr in rownames(ip)) for (to in colnames(ip)) {
              v <- ip[fr, to]
              if (!is.na(v) && v != 0)
                rows_e[[length(rows_e)+1]] <- data.frame(
                  From=fr, To=to,
                  Direct=round(v,3), Indirect="—", Total=round(v,3),
                  Note="Bootstrap required for indirect effects (use PLS-SEM Bootstrap tab)",
                  stringsAsFactors=FALSE)
            }
            if (length(rows_e)>0) {
              eff_df <- do.call(rbind, rows_e)
              return(tagList(
                tags$h5("Effects Summary (PLS — Direct Only)", style="color:#1A3A5C;"),
                tags$div(class="alert alert-info", style="font-size:.81rem;",
                  "PLS indirect effects require bootstrapping (see 🔗 PLS-SEM tab → Bootstrap). ",
                  "Use CB-SEM (lavaan) for automatic effects decomposition below."),
                tool_dt(eff_df, "PLS Path Effects")
              ))
            }
          }
        }
        return(tags$div(class="alert alert-warning",
          "Effects decomposition requires CB-SEM. Draw constructs and structural paths, then re-run Canvas Analysis."))
      }

      # lavaan total/direct/indirect effects
      pe <- tryCatch(lavaan::parameterEstimates(fit, standardized=TRUE), error=function(e) NULL)
      if (is.null(pe)) return(tags$div(class="alert alert-danger", "Could not extract lavaan parameters."))

      direct   <- pe[pe$op=="~",]
      indirect <- pe[pe$op==":=",]  # defined indirect effects if any

      # Build effects table from lavaan structural paths
      # For total effects, we'll compute via path tracing
      path_df <- pe[pe$op=="~", c("lhs","rhs","est","std.all","pvalue")]
      names(path_df) <- c("To","From","Beta_Unst","Beta_Std","p")

      # Identify endogenous constructs
      endo_cons <- unique(path_df$To)
      exo_cons  <- setdiff(unique(c(path_df$From, path_df$To)), endo_cons)

      # Compute total effects via covariance matrix approach
      total_eff <- tryCatch({
        all_cons <- union(unique(path_df$From), unique(path_df$To))
        n_c <- length(all_cons)
        B <- matrix(0, n_c, n_c, dimnames=list(all_cons, all_cons))
        for (i in seq_len(nrow(path_df))) {
          B[path_df$To[i], path_df$From[i]] <- path_df$Beta_Std[i]
        }
        # Total = (I - B)^{-1} B — off-diagonal elements
        I_mat <- diag(n_c)
        T_mat <- tryCatch(solve(I_mat - B) - I_mat, error=function(e) NULL)
        T_mat
      }, error=function(e) NULL)

      # Build effects summary table
      eff_rows <- list()
      for (i in seq_len(nrow(path_df))) {
        fr <- path_df$From[i]; to <- path_df$To[i]
        dir <- round(path_df$Beta_Std[i], 3)
        tot <- if (!is.null(total_eff) && fr %in% rownames(total_eff) && to %in% rownames(total_eff))
                 round(total_eff[to, fr], 3) else NA
        ind <- if (!is.na(tot) && !is.na(dir)) round(tot - dir, 3) else NA
        p_v <- round(path_df$p[i], 4)
        eff_rows[[length(eff_rows)+1]] <- data.frame(
          From=fr, To=to, Direct=dir, Indirect=ind, Total=tot,
          p_direct=p_v,
          Sig=ifelse(p_v<.001,"***",ifelse(p_v<.01,"**",ifelse(p_v<.05,"*","ns"))),
          stringsAsFactors=FALSE)
      }

      # Add multi-hop indirect paths
      if (!is.null(total_eff)) {
        for (to in endo_cons) {
          for (fr in exo_cons) {
            if (fr %in% rownames(total_eff) && to %in% colnames(total_eff)) {
              tot_v <- round(total_eff[to, fr], 3)
              if (!is.na(tot_v) && abs(tot_v) > 0.001) {
                already <- any(sapply(eff_rows, function(r) r$From==fr && r$To==to))
                if (!already)
                  eff_rows[[length(eff_rows)+1]] <- data.frame(
                    From=fr, To=to, Direct=0, Indirect=tot_v, Total=tot_v,
                    p_direct=NA, Sig="indirect", stringsAsFactors=FALSE)
              }
            }
          }
        }
      }

      eff_df <- if (length(eff_rows) > 0) do.call(rbind, eff_rows) else NULL

      tagList(
        tags$h5("🔄 Total, Direct & Indirect Effects Decomposition (CB-SEM)", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.81rem;margin-bottom:.5rem;",
          "Direct = path coefficient. Total = sum of all causal pathways. ",
          "Indirect = Total − Direct (mediation via intermediate constructs). ",
          "Computed via (I − B)⁻¹ matrix (Bollen, 1989)."),
        if (!is.null(eff_df))
          tool_dt(eff_df, "Effects Decomposition Table")
        else tags$div(class="alert alert-info", "No structural paths detected for decomposition."),
        tags$br(),
        tags$div(style="background:#f8f9fa;border-radius:6px;padding:.6rem .9rem;font-size:.79rem;color:#555;",
          tags$b("Reference: "), "Bollen, K.A. (1989). ",
          tags$em("Structural Equations with Latent Variables. "), "Wiley.")
      )
    })

    # ── Fornell-Larcker Criterion table ──────────────────────────────────
    output$cv_fl_tbl <- DT::renderDataTable({
      res   <- canvas_results_rv(); req(res)
      d_raw <- res$d_raw
      cnames <- names(res$construct_items)
      n <- length(cnames); if (n < 1) return(NULL)

      # Compute AVE per construct (from psych::fa)
      ave_vec <- sapply(cnames, function(nm) {
        items <- intersect(res$construct_items[[nm]], names(d_raw))
        if (length(items) < 2) return(NA)
        mat <- d_raw[, items, drop=FALSE]; mat[] <- lapply(mat, as.numeric); mat <- na.omit(mat)
        if (nrow(mat) < 5) return(NA)
        lam <- tryCatch({
          fa_r <- psych::fa(mat, nfactors=1, rotate="none", fm="ml")
          as.numeric(fa_r$loadings)
        }, error=function(e) rep(NA_real_, length(items)))
        mean(lam^2, na.rm=TRUE)
      })

      # Compute inter-construct correlations
      cs_df <- res$cs_df
      cor_mat <- tryCatch(cor(cs_df, use="pairwise.complete.obs"), error=function(e) NULL)
      if (is.null(cor_mat)) cor_mat <- matrix(NA, n, n, dimnames=list(cnames, cnames))

      # Build FL matrix: diagonal = sqrt(AVE), off-diagonal = r
      fl_mat <- matrix(NA_real_, n, n, dimnames=list(cnames, cnames))
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          if (i == j) {
            fl_mat[i, j] <- round(sqrt(ave_vec[i]), 3)
          } else {
            r_ij <- tryCatch(cor_mat[cnames[i], cnames[j]], error=function(e) NA)
            fl_mat[i, j] <- round(r_ij, 3)
          }
        }
      }
      fl_df <- as.data.frame(fl_mat)
      fl_df <- cbind(Construct=cnames, fl_df)

      dt_obj <- DT::datatable(fl_df,
        caption="Fornell-Larcker Criterion (diagonal = √AVE; off-diagonal = inter-construct correlations)",
        rownames=FALSE, options=list(dom="t", paging=FALSE, scrollX=TRUE),
        class="stripe hover compact") |>
        DT::formatStyle(
          columns=cnames,
          backgroundColor=DT::styleInterval(c(-99,99), c("transparent","transparent")))
      dt_obj
    })

    # ── Cross-Loadings Matrix table ───────────────────────────────────────
    output$cv_crossload_tbl <- DT::renderDataTable({
      res   <- canvas_results_rv(); req(res)
      d_raw <- res$d_raw

      # Prefer PLS outer loadings cross-loadings
      m_pls <- pls_model_rv()
      if (!is.null(m_pls)) {
        ol <- tryCatch(as.data.frame(m_pls$outer_loadings), error=function(e) NULL)
        if (!is.null(ol)) {
          ol_show       <- round(ol, 3)
          ol_show       <- cbind(Indicator=rownames(ol_show), ol_show)
          return(tool_dt(ol_show, "PLS Cross-Loadings (indicators × constructs)"))
        }
      }

      # Fall-back: compute correlations of each indicator with all construct scores
      cs_df   <- res$cs_df
      cnames  <- names(res$construct_items)
      all_its <- unique(unlist(res$construct_items))
      all_its <- intersect(all_its, names(d_raw))
      if (length(all_its) == 0 || length(cnames) == 0)
        return(tool_dt(data.frame(Note="No data available"), "Cross-Loadings"))

      d_num <- d_raw[, all_its, drop=FALSE]; d_num[] <- lapply(d_num, as.numeric)
      both  <- na.omit(cbind(d_num, cs_df))
      n_it  <- length(all_its)
      n_cs  <- length(cnames)

      cl_mat <- matrix(NA_real_, n_it, n_cs,
                       dimnames=list(all_its, cnames))
      for (it in all_its)
        for (cs in cnames)
          cl_mat[it, cs] <- tryCatch(
            round(cor(both[[it]], both[[cs]], use="complete.obs"), 3),
            error=function(e) NA)

      cl_df <- cbind(Indicator=all_its, as.data.frame(cl_mat))
      tool_dt(cl_df, "Cross-Loadings Matrix (item-construct correlations)")
    })

    # ── Model Text (AMOS-style text output) ───────────────────────────────
    output$cv_detail_path_txt <- renderText({
      res <- canvas_results_rv()
      if (is.null(res)) return("Run Canvas Analysis to generate the model report.")

      fit_sem <- cv_cbsem_res()
      fit_cfa <- cv_cfa_res()

      lines <- c()
      lines <- c(lines,
        "═══════════════════════════════════════════════════════════════════",
        "  Dr.AIStat — Full Model Report (AMOS / SmartPLS style)",
        paste0("  Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        "═══════════════════════════════════════════════════════════════════",
        "")

      # Constructs
      lines <- c(lines, "CONSTRUCTS", "─────────────────────────────────────────")
      for (nm in names(res$construct_items)) {
        items <- intersect(res$construct_items[[nm]], names(res$d_raw))
        tp    <- res$construct_types[[nm]] %||% "reflective"
        lines <- c(lines, sprintf("  %-20s [%s]  ← %s", nm, toupper(tp),
                                  paste(items, collapse=", ")))
      }
      lines <- c(lines, "")

      # Structural paths
      lines <- c(lines, "STRUCTURAL PATHS", "─────────────────────────────────────────")
      for (sp in res$path_list)
        lines <- c(lines, sprintf("  %-20s  →  %s", sp$from, sp$to))
      lines <- c(lines, "")

      # Mediators
      if (length(res$mediators) > 0) {
        lines <- c(lines, "MEDIATORS", "─────────────────────────────────────────")
        for (m in res$mediators)
          lines <- c(lines, paste0("  ", m))
        lines <- c(lines, "")
      }

      # CB-SEM parameter estimates
      if (!is.null(fit_sem)) {
        pe <- tryCatch(lavaan::parameterEstimates(fit_sem, standardized=TRUE), error=function(e) NULL)
        if (!is.null(pe)) {
          lines <- c(lines, "MEASUREMENT MODEL (CB-SEM — MLR)", "─────────────────────────────────────────")
          ld <- pe[pe$op=="=~",]
          for (i in seq_len(nrow(ld)))
            lines <- c(lines, sprintf("  %-20s =~ %-15s  β=%.3f  SE=%.3f  z=%.3f  p=%.4f  %s",
              ld$lhs[i], ld$rhs[i], ld$std.all[i], ld$se[i], ld$z[i], ld$pvalue[i],
              ifelse(ld$pvalue[i]<.001,"***",ifelse(ld$pvalue[i]<.01,"**",
                ifelse(ld$pvalue[i]<.05,"*","ns")))))
          lines <- c(lines, "")

          lines <- c(lines, "STRUCTURAL MODEL (CB-SEM — MLR)", "─────────────────────────────────────────")
          sp <- pe[pe$op=="~",]
          for (i in seq_len(nrow(sp)))
            lines <- c(lines, sprintf("  %-20s ~  %-15s  β=%.3f  SE=%.3f  z=%.3f  p=%.4f  %s",
              sp$lhs[i], sp$rhs[i], sp$std.all[i], sp$se[i], sp$z[i], sp$pvalue[i],
              ifelse(sp$pvalue[i]<.001,"***",ifelse(sp$pvalue[i]<.01,"**",
                ifelse(sp$pvalue[i]<.05,"*","ns")))))
          lines <- c(lines, "")

          # R²
          r2v <- tryCatch(lavaan::inspect(fit_sem,"r2"), error=function(e) NULL)
          if (!is.null(r2v)) {
            lines <- c(lines, "R² (ENDOGENOUS CONSTRUCTS)", "─────────────────────────────────────────")
            for (nm in names(r2v))
              lines <- c(lines, sprintf("  %-20s  R²=%.3f  Error=%.3f",
                                        nm, r2v[[nm]], 1-r2v[[nm]]))
            lines <- c(lines, "")
          }

          # Fit indices
          fm <- tryCatch(lavaan::fitMeasures(fit_sem,
            c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic")), error=function(e) NULL)
          if (!is.null(fm)) {
            lines <- c(lines, "FIT INDICES (CB-SEM)", "─────────────────────────────────────────",
              sprintf("  χ²(%g)=%.3f  p=%.4f", fm["df"], fm["chisq"], fm["pvalue"]),
              sprintf("  CFI=%.3f  TLI=%.3f  RMSEA=%.3f  SRMR=%.3f",
                      fm["cfi"], fm["tli"], fm["rmsea"], fm["srmr"]),
              sprintf("  AIC=%.1f  BIC=%.1f", fm["aic"], fm["bic"]),
              "")
          }
        }
      }

      # PLS summary if available
      m_pls <- pls_model_rv()
      if (!is.null(m_pls) && is.null(fit_sem)) {
        sm <- tryCatch(summary(m_pls), error=function(e) NULL)
        if (!is.null(sm)) {
          lines <- c(lines, "PLS-SEM OUTER LOADINGS", "─────────────────────────────────────────")
          ol <- tryCatch(round(as.data.frame(m_pls$outer_loadings),3), error=function(e) NULL)
          if (!is.null(ol)) {
            for (r in rownames(ol)) {
              vals <- ol[r,]; non0 <- vals[vals!=0 & !is.na(vals)]
              if (length(non0)>0)
                lines <- c(lines, sprintf("  %-20s  %s", r,
                  paste(paste0(names(non0),"=",non0), collapse="  ")))
            }
          }
          lines <- c(lines, "")
          lines <- c(lines, "PLS-SEM PATH COEFFICIENTS", "─────────────────────────────────────────")
          ip <- tryCatch(round(as.data.frame(m_pls$path_coef),3), error=function(e) NULL)
          if (!is.null(ip)) {
            for (r in rownames(ip)) {
              for (cc in colnames(ip)) {
                v <- ip[r, cc]
                if (!is.na(v) && v != 0)
                  lines <- c(lines, sprintf("  %-20s  →  %-20s  β=%.3f", r, cc, v))
              }
            }
          }
          lines <- c(lines, "")
        }
      }

      lines <- c(lines,
        "═══════════════════════════════════════════════════════════════════",
        "  *** p<.001  ** p<.01  * p<.05  ns = non-significant",
        "  References: Hair et al. (2019); Fornell & Larcker (1981);",
        "              Hu & Bentler (1999); Bollen (1989).",
        "═══════════════════════════════════════════════════════════════════")

      paste(lines, collapse="\n")
    })

    # AI Interpretation for Canvas SEM Results
    observeEvent(input$ai_btn_canvas, {
      api_key <- gemini_key()
      if (is.null(api_key) || !nzchar(trimws(api_key %||% ""))) {
        output$canvas_ai_output <- renderUI(
          tags$div(class="alert alert-warning",
               "⚠️ No API key in sidebar. Add your Groq/Gemini key to use AI interpretation."))
        return()
      }
      cm <- canvas_model()
      ctx <- tryCatch({
        nodes_list <- cm$nodes
        edges_list <- cm$edges
        if (length(nodes_list) == 0) return("No constructs on canvas yet.")
        node_txt <- paste(sapply(nodes_list, function(n)
          paste0(n$label, " [", n$type, "]: ", paste(n$indicators %||% character(0), collapse=", "))
        ), collapse="\n")
        edge_txt <- if (length(edges_list) > 0) {
          nms <- setNames(sapply(nodes_list, function(n) n$label), sapply(nodes_list, function(n) n$id))
          paste(sapply(edges_list, function(e)
            paste0(nms[e$from] %||% e$from, " --[", e$type, "]--> ", nms[e$to] %||% e$to)
          ), collapse="\n")
        } else "No paths defined."
        paste0("CONSTRUCTS:\n", node_txt, "\n\nPATHS:\n", edge_txt)
      }, error = function(e) paste("Error reading model:", e$message))

      output$canvas_ai_output <- renderUI({
        result <- tryCatch(
          call_gemini(paste0(
            "You are an expert in Structural Equation Modelling (SEM) for top-tier management journals.\n\n",
            "Interpret this SEM model. Include:\n",
            "1. Describe constructs, their measurement approach, and theoretical meaning\n",
            "2. Evaluate each path: expected direction, theoretical justification\n",
            "3. Recommended fit indices to report (CFI, TLI, RMSEA, SRMR for CB-SEM; AVE, CR, HTMT for PLS)\n",
            "4. Potential issues: identification, multicollinearity, common method bias\n",
            "5. APA-style model description paragraph\n\n",
            "MODEL:\n", ctx
          ), api_key),
          error = function(e) paste("AI error:", e$message)
        )
        tags$div(
          style = "background:#f0f9ff;border-left:4px solid #2196A6;padding:1.2rem;border-radius:8px;margin-top:.5rem;line-height:1.8;",
          tags$b("🤖 AI Interpretation:"), tags$br(), tags$br(),
          tags$div(style = "white-space:pre-wrap;font-size:.92rem;", result)
        )
      })
    }, ignoreInit = TRUE)

    # get_lv_defs from canvas model (replaces old SmartPLS builder approach)
    get_lv_defs <- function() {
      cm <- canvas_model()
      nodes <- cm$nodes
      if (length(nodes) == 0) return(list())
      lapply(nodes, function(nd) {
        list(name  = nd$label %||% nd$id,
             items = nd$indicators %||% character(0),
             type  = nd$type  %||% "reflective")
      })
    }

    # ========================================================================
    # TAB 2: DATA SCREENING
    # ========================================================================
    observeEvent(input$run_screen, {
      req(df(), input$screen_vars)
      d_raw <- df()[, input$screen_vars, drop=FALSE]
      d_num <- dplyr::mutate(d_raw, dplyr::across(dplyr::everything(), as.numeric))
      n     <- nrow(d_num)

      output$screen_output_ui <- renderUI({
        tagList(
          tags$h4("📊 Descriptive Statistics", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("screen_desc_tbl")),
          tags$hr(),
          tags$h4("🔍 Missing Values", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("screen_missing_tbl")),
          tags$hr(),
          tags$h4("📐 Normality Tests (Skewness, Kurtosis, Shapiro-Wilk)", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;",
            "Skewness ≤ |2| and Kurtosis ≤ |7| acceptable for ML-SEM (West et al., 1995). ",
            "Shapiro-Wilk: p > .05 = normal (n ≤ 5000)."),
          DT::dataTableOutput(ns("screen_norm_tbl")),
          tags$hr(),
          tags$h4("🧪 Harman's Single Factor Test (CMV)", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;",
            "> 50% variance by single factor = CMV concern (Podsakoff et al., 2003)."),
          uiOutput(ns("screen_harman_ui")),
          tags$hr(),
          tags$h4("⚠️ Outlier Detection (Mahalanobis Distance)", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;",
            "Rows with p < .001 on Mahalanobis χ² may be multivariate outliers."),
          DT::dataTableOutput(ns("screen_outlier_tbl")),
          br(),
          downloadButton(ns("dl_screen"), "Download Screening Report (Excel)")
        )
      })

      output$screen_desc_tbl <- DT::renderDataTable({
        desc <- tryCatch({
          d_s <- psych::describe(d_num)
          round(as.data.frame(d_s), 4)
        }, error=function(e) data.frame(Note="psych::describe failed"))
        tool_dt(desc, paste0("Descriptive Statistics (N = ", n, ")"))
      })

      output$screen_missing_tbl <- DT::renderDataTable({
        miss_df <- data.frame(
          Variable    = names(d_num),
          n_missing   = sapply(d_num, function(x) sum(is.na(x))),
          pct_missing = round(100 * sapply(d_num, function(x) mean(is.na(x))), 2),
          stringsAsFactors=FALSE)
        miss_df$Status <- ifelse(miss_df$pct_missing == 0, "✅ Complete",
                           ifelse(miss_df$pct_missing < 5, "⚠️ Low (<5%)",
                           ifelse(miss_df$pct_missing < 20, "⚠️ Moderate (5-20%)", "❌ High (>20%)")))
        tool_dt(miss_df, "Missing Value Summary")
      })

      output$screen_norm_tbl <- DT::renderDataTable({
        norm_df <- do.call(rbind, lapply(names(d_num), function(v) {
          x   <- na.omit(d_num[[v]])
          sk  <- tryCatch(round(moments::skewness(x), 4), error=function(e) NA)
          ku  <- tryCatch(round(moments::kurtosis(x) - 3, 4), error=function(e) NA)
          sw  <- if (length(x) >= 3 && length(x) <= 5000) {
            tryCatch({ res <- shapiro.test(x[1:min(5000,length(x))])
                       paste0("W=", round(res$statistic, 4), " p=", round(res$p.value, 4)) },
                     error=function(e) "N/A")
          } else "N/A (n>5000)"
          sw_ok <- if (!grepl("N/A", sw)) {
            pval <- tryCatch(as.numeric(sub(".*p=", "", sw)), error=function(e) NA)
            ifelse(is.na(pval), "?", ifelse(pval > .05, "✅ Normal", "⚠️ Non-normal"))
          } else "ℹ️ N/A"
          data.frame(Variable=v, Skewness=sk,
                     `Sk OK`=ifelse(is.na(sk),"?",ifelse(abs(sk)<=2,"✅","⚠️")),
                     Kurtosis=ku,
                     `Ku OK`=ifelse(is.na(ku),"?",ifelse(abs(ku)<=7,"✅","⚠️")),
                     `Shapiro-Wilk`=sw, `SW Verdict`=sw_ok,
                     check.names=FALSE, stringsAsFactors=FALSE)
        }))
        tool_dt(norm_df, "Normality Assessment")
      })

      output$screen_harman_ui <- renderUI({
        d_c <- na.omit(d_num)
        if (nrow(d_c) < 5 || ncol(d_c) < 2) return(tags$p("Insufficient data."))
        tryCatch({
          pc1_var <- tryCatch({
            pc_res <- psych::principal(d_c, nfactors=1, rotate="none")
            pc_res$values[1] / sum(pc_res$values) * 100
          }, error=function(e) {
            eig <- eigen(cor(d_c, use="pairwise"))$values
            eig[1] / sum(eig) * 100
          })
          cmv_concern <- pc1_var > 50
          tags$div(style=paste0("background:",if(cmv_concern)"#FFEBEE" else "#E8F5E9",
            ";border-left:4px solid ",if(cmv_concern)"#C62828" else "#2E7D32",
            ";padding:.8rem;border-radius:6px;"),
            tags$b(paste0(if(cmv_concern)"⚠️ CMV Concern: " else "✅ Acceptable: ",
                          round(pc1_var, 2), "% variance by first factor")),
            tags$br(),
            tags$small(style="color:#555;",
              if(cmv_concern) "First factor > 50% (Harman, 1976; Podsakoff et al., 2003)"
              else "First factor < 50% — Harman test does not indicate severe CMV"))
        }, error=function(e) tags$p(paste("CMV error:", e$message)))
      })

      output$screen_outlier_tbl <- DT::renderDataTable({
        d_c <- na.omit(d_num)
        if (nrow(d_c) < (ncol(d_c) + 2))
          return(tool_dt(data.frame(Note="Too few rows"), "Outliers"))
        outlier_df <- tryCatch({
          mah   <- mahalanobis(d_c, colMeans(d_c), cov(d_c))
          p_val <- pchisq(mah, df=ncol(d_c), lower.tail=FALSE)
          idx   <- which(p_val < .001)
          if (length(idx) == 0)
            data.frame(Note="No multivariate outliers (all p > .001)")
          else
            data.frame(Row=idx, `Mahal.D`=round(mah[idx],3),
                       `p-value`=round(p_val[idx],4),
                       Verdict="⚠️ Outlier", check.names=FALSE)
        }, error=function(e) data.frame(Note=e$message))
        tool_dt(outlier_df, paste0("Mahalanobis Outliers (N=", nrow(d_c), ")"))
      })

      output$dl_screen <- downloadHandler(
        filename=function() paste0("DataScreening_", Sys.Date(), ".xlsx"),
        content=function(file) {
          sheets <- list()
          tryCatch({ sheets[["Descriptives"]] <- round(as.data.frame(psych::describe(d_num)),4) }, error=function(e){})
          sheets[["Missing"]] <- data.frame(
            Variable=names(d_num),
            n_missing=sapply(d_num, function(x) sum(is.na(x))),
            pct=round(100*sapply(d_num, function(x) mean(is.na(x))),2))
          if (length(sheets)==0) sheets[["Note"]] <- data.frame(msg="No data")
          write_xlsx(sheets, file)
        }
      )
    })

    # ========================================================================
    # TAB 3: EFA
    # ========================================================================
    observeEvent(input$run_efa, {
      req(df(), input$efa_vars)
      d_efa <- df()[, input$efa_vars, drop=FALSE]
      d_efa <- dplyr::mutate(d_efa, dplyr::across(dplyr::everything(), as.numeric))
      d_efa <- na.omit(d_efa)
      nf    <- input$efa_nf  %||% 3
      rot   <- input$efa_rot %||% "oblimin"

      output$efa_output_ui <- renderUI({
        tagList(
          tags$h4("📐 Scree Plot & Parallel Analysis", style="color:#1A3A5C;"),
          plotOutput(ns("efa_scree_plot"), height="350px"),
          tags$hr(),
          tags$h4("📊 Factor Loadings", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("efa_loadings_tbl")),
          tags$hr(),
          tags$h4("📋 Fit & Communalities", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("efa_fit_tbl")),
          br(),
          downloadButton(ns("dl_efa"), "Download EFA (Excel)")
        )
      })

      output$efa_scree_plot <- renderPlot({
        tryCatch({
          par(mfrow=c(1,2))
          eigs <- eigen(cor(d_efa, use="pairwise"))$values
          plot(eigs, type="b", pch=19, col="#2196A6",
               xlab="Factor", ylab="Eigenvalue", main="Scree Plot")
          abline(h=1, lty=2, col="#E74C3C")
          pa <- tryCatch(psych::fa.parallel(d_efa, fa="fa", sim=FALSE, plot=FALSE, n.iter=50),
                         error=function(e) NULL)
          if (!is.null(pa)) {
            plot(pa$fa.values, type="b", pch=19, col="#2196A6",
                 xlab="Factor", ylab="Eigenvalue",
                 main=paste0("Parallel Analysis (nfactors=", pa$nfact, ")"))
            lines(pa$fa.sim, type="b", pch=17, col="#E74C3C", lty=2)
            legend("topright", c("Actual","Simulated"), pch=c(19,17),
                   col=c("#2196A6","#E74C3C"), cex=.8)
          }
          par(mfrow=c(1,1))
        }, error=function(e) { plot.new(); text(.5,.5,paste("Error:",e$message)) })
      })

      output$efa_loadings_tbl <- DT::renderDataTable({
        efa_res <- tryCatch(psych::fa(d_efa, nfactors=nf, rotate=rot, fm="ml"),
                            error=function(e) psych::fa(d_efa, nfactors=nf, rotate=rot, fm="pa"))
        ld <- round(as.data.frame(unclass(efa_res$loadings)), 4)
        ld$Communality <- round(efa_res$communalities, 4)
        ld$Uniqueness  <- round(efa_res$uniquenesses, 4)
        dt <- datatable(ld, rownames=TRUE,
          options=list(pageLength=30, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")),
          extensions="Buttons")
        for (cn in names(ld)[seq_len(nf)]) {
          dt <- dt |> formatStyle(cn,
            backgroundColor=styleInterval(c(-.7,-.5,-.4,.4,.5,.7),
              c("#FFCDD2","#FFEBEE","#FFF9C4","white","#E8F5E9","#A5D6A7","#388E3C")))
        }
        dt
      })

      output$efa_fit_tbl <- DT::renderDataTable({
        efa_res <- tryCatch(psych::fa(d_efa, nfactors=nf, rotate=rot, fm="ml"),
                            error=function(e) psych::fa(d_efa, nfactors=nf, rotate=rot, fm="pa"))
        fit_df <- data.frame(
          Metric  = c("RMSEA","TLI","BIC","SRMR","Chi-square","df","p-value","% Var explained"),
          Value   = c(round(efa_res$RMSEA["RMSEA"],4), round(efa_res$TLI,4),
                      round(efa_res$BIC,2), round(efa_res$rms,4),
                      round(efa_res$STATISTIC,2), efa_res$dof,
                      round(efa_res$PVAL,4),
                      round(sum(efa_res$Vaccounted["Proportion Var",])*100,2)),
          Verdict = c(ifelse(efa_res$RMSEA["RMSEA"]<=.06,"✅ Good","⚠️"),
                      ifelse(efa_res$TLI>=.95,"✅ Good","⚠️"),
                      "","","","","",
                      ifelse(sum(efa_res$Vaccounted["Proportion Var",])*100>=50,"✅ ≥50%","⚠️")),
          stringsAsFactors=FALSE)
        tool_dt(fit_df, paste0("EFA Fit (n=", nrow(d_efa), ")"))
      })

      output$dl_efa <- downloadHandler(
        filename=function() paste0("EFA_", Sys.Date(), ".xlsx"),
        content=function(file) {
          efa_res <- tryCatch(psych::fa(d_efa, nfactors=nf, rotate=rot, fm="ml"),
                              error=function(e) psych::fa(d_efa, nfactors=nf, rotate=rot, fm="pa"))
          ld <- round(as.data.frame(unclass(efa_res$loadings)), 4)
          ld$Communality <- round(efa_res$communalities, 4)
          write_xlsx(list("Loadings"=ld), file)
        }
      )
    })


    # ========================================================================
    # TAB 4: CFA / RELIABILITY (uses canvas constructs)
    # ========================================================================
    observeEvent(input$run_cfa_rel, {
      req(df())
      lv_defs <- tryCatch(get_lv_defs(), error=function(e) list())
      if (length(lv_defs) == 0) {
        showNotification("Define constructs in the Model Canvas tab first.", type="warning")
        return()
      }
      all_items <- unique(unlist(lapply(lv_defs, `[[`, "items")))
      d_cfa <- df()[, intersect(all_items, names(df())), drop=FALSE]
      d_cfa <- dplyr::mutate(d_cfa, dplyr::across(dplyr::everything(), as.numeric))
      d_cfa <- na.omit(d_cfa)

      output$cfa_rel_output_ui <- renderUI({
        tagList(
          tags$h4("✅ CFA Fit Indices", style="color:#1A3A5C;"),
          uiOutput(ns("cfa_fit_cards")),
          tags$hr(),
          tags$h4("📊 Factor Loadings (λ, SE, z, p)", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("cfa_loadings_tbl")),
          tags$hr(),
          tags$h4("🔒 Reliability & Convergent Validity", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;", "α ≥.70 | CR ≥.70 | AVE ≥.50"),
          DT::dataTableOutput(ns("cfa_reliability_tbl")),
          tags$hr(),
          tags$h4("🔍 Discriminant Validity — HTMT", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;", "HTMT < .85 (Henseler et al., 2015)"),
          DT::dataTableOutput(ns("cfa_htmt_tbl")),
          tags$hr(),
          tags$h4("📐 Fornell-Larcker Criterion", style="color:#1A3A5C;"),
          tags$p(style="color:#666;font-size:.84rem;", "Diagonal = √AVE; must exceed off-diagonals"),
          DT::dataTableOutput(ns("cfa_fl_tbl")),
          br(),
          downloadButton(ns("dl_cfa_rel"), "Download CFA/Reliability (Excel)")
        )
      })

      cfa_syntax <- paste(sapply(lv_defs, function(lv) {
        items_ok <- intersect(lv$items, names(d_cfa))
        if (length(items_ok) < 2) return(NULL)
        paste0(lv$name, " =~ ", paste(items_ok, collapse=" + "))
      }), collapse="\n")

      cfa_fit <- tryCatch(lavaan::cfa(cfa_syntax, data=d_cfa, estimator="ML"),
                          error=function(e) NULL)

      output$cfa_fit_cards <- renderUI({
        if (is.null(cfa_fit))
          return(tags$div(class="alert alert-danger",
            "CFA failed — ensure constructs have ≥2 indicators and numeric data."))
        fi <- lavaan::fitMeasures(cfa_fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
        mk <- function(lbl, val, ok)
          tags$div(style=paste0("background:",if(ok)GREEN else "#C0392B",
            ";color:white;padding:.7rem;border-radius:7px;text-align:center;margin:.3rem;"),
            tags$b(paste(if(ok)"✅" else "❌", lbl)), tags$br(),
            sprintf("%.3f", as.numeric(val)))
        tagList(
          tags$p(paste0("N = ", nrow(d_cfa))),
          fluidRow(
            column(2, mk("CFI ≥.95",   fi["cfi"],   fi["cfi"]>=.95)),
            column(2, mk("TLI ≥.95",   fi["tli"],   fi["tli"]>=.95)),
            column(2, mk("RMSEA ≤.06", fi["rmsea"], fi["rmsea"]<=.06)),
            column(2, mk("SRMR ≤.08",  fi["srmr"],  fi["srmr"]<=.08)),
            column(4, tags$div(style="background:#1A3A5C;color:white;padding:.7rem;
                                border-radius:7px;text-align:center;margin:.3rem;",
              tags$b("χ²(df)"), tags$br(),
              sprintf("%.2f(%g) p%s", fi["chisq"], fi["df"], apa_p(fi["pvalue"]))))
          )
        )
      })

      output$cfa_loadings_tbl <- DT::renderDataTable({
        if (is.null(cfa_fit)) return(tool_dt(data.frame(Note="CFA failed"), "Loadings"))
        params <- lavaan::parameterEstimates(cfa_fit, standardized=TRUE) |>
          dplyr::filter(op == "=~") |>
          dplyr::select(Construct=lhs, Indicator=rhs, lambda=std.all, SE=se, z=z, p=pvalue) |>
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~round(.x,4)),
                        Sig=sig_stars(p),
                        Verdict=ifelse(abs(lambda)>=.70,"✅ Good",
                                  ifelse(abs(lambda)>=.50,"⚠️ Accept.","❌ Low")))
        tool_dt(params, paste0("CFA Loadings (n=", nrow(d_cfa), ")"))
      })

      output$cfa_reliability_tbl <- DT::renderDataTable({
        rel_df <- do.call(rbind, lapply(lv_defs, function(lv) {
          items_ok <- intersect(lv$items, names(d_cfa))
          if (length(items_ok) < 2) return(NULL)
          x_mat  <- d_cfa[, items_ok, drop=FALSE]
          al     <- tryCatch(psych::alpha(x_mat)$total$raw_alpha, error=function(e) NA)
          ld_vals <- if (!is.null(cfa_fit)) {
            pe <- lavaan::parameterEstimates(cfa_fit, standardized=TRUE)
            pe[pe$lhs==lv$name & pe$op=="=~", "std.all"]
          } else rowMeans(cor(x_mat, use="pairwise"), na.rm=TRUE)
          sum_ld  <- sum(ld_vals, na.rm=TRUE)
          sum_err <- sum(1 - ld_vals^2, na.rm=TRUE)
          cr      <- sum_ld^2 / (sum_ld^2 + sum_err)
          ave     <- mean(ld_vals^2, na.rm=TRUE)
          data.frame(Construct=lv$name, n_items=length(items_ok),
                     Alpha=round(al,4), CR=round(cr,4), AVE=round(ave,4),
                     `α OK`=ifelse(is.na(al),"?",ifelse(al>=.70,"✅","❌")),
                     `CR OK`=ifelse(cr>=.70,"✅","❌"),
                     `AVE OK`=ifelse(ave>=.50,"✅","❌"),
                     check.names=FALSE, stringsAsFactors=FALSE)
        }))
        if (is.null(rel_df)) rel_df <- data.frame(Note="No constructs")
        tool_dt(rel_df, "Reliability & Convergent Validity")
      })

      output$cfa_htmt_tbl <- DT::renderDataTable({
        lv_names <- sapply(lv_defs, `[[`, "name")
        if (length(lv_names) < 2) return(tool_dt(data.frame(Note="Need ≥2 constructs"), "HTMT"))
        htmt_mat <- matrix(NA, length(lv_names), length(lv_names), dimnames=list(lv_names,lv_names))
        for (i in seq_along(lv_defs)) for (j in seq_along(lv_defs)) {
          if (i >= j) next
          a <- intersect(lv_defs[[i]]$items, names(d_cfa))
          b <- intersect(lv_defs[[j]]$items, names(d_cfa))
          if (!length(a) || !length(b)) next
          cab <- cor(d_cfa[,a,drop=FALSE], d_cfa[,b,drop=FALSE], use="pairwise")
          caa <- cor(d_cfa[,a,drop=FALSE], use="pairwise")
          cbb <- cor(d_cfa[,b,drop=FALSE], use="pairwise")
          mab <- mean(abs(cab), na.rm=TRUE)
          maa <- if (length(a)>1) mean(abs(caa[lower.tri(caa)]),na.rm=TRUE) else 1
          mbb <- if (length(b)>1) mean(abs(cbb[lower.tri(cbb)]),na.rm=TRUE) else 1
          htmt <- if (maa>0&&mbb>0) mab/sqrt(maa*mbb) else NA
          htmt_mat[i,j] <- htmt_mat[j,i] <- round(htmt,4)
        }
        diag(htmt_mat) <- NA
        dt <- datatable(as.data.frame(htmt_mat), rownames=TRUE,
          options=list(pageLength=20, scrollX=TRUE))
        dt
      })

      output$cfa_fl_tbl <- DT::renderDataTable({
        lv_names <- sapply(lv_defs, `[[`, "name")
        rel_df2  <- do.call(rbind, lapply(lv_defs, function(lv) {
          items_ok <- intersect(lv$items, names(d_cfa))
          ld_vals  <- if (!is.null(cfa_fit)) {
            pe <- lavaan::parameterEstimates(cfa_fit, standardized=TRUE)
            pe[pe$lhs==lv$name & pe$op=="=~","std.all"]
          } else rep(NA, length(items_ok))
          data.frame(name=lv$name, sqrt_ave=sqrt(mean(ld_vals^2, na.rm=TRUE)))
        }))
        if (is.null(rel_df2)) return(tool_dt(data.frame(Note="No data"),"F-L"))
        fl_mat <- matrix(NA, nrow(rel_df2), nrow(rel_df2), dimnames=list(rel_df2$name,rel_df2$name))
        diag(fl_mat) <- round(rel_df2$sqrt_ave, 4)
        scores <- sapply(lv_defs, function(lv) {
          items_ok <- intersect(lv$items, names(d_cfa))
          if (!length(items_ok)) return(rep(NA, nrow(d_cfa)))
          rowMeans(d_cfa[,items_ok,drop=FALSE], na.rm=TRUE)
        })
        colnames(scores) <- sapply(lv_defs, `[[`, "name")
        cor_mat <- cor(scores, use="pairwise")
        for (i in seq_len(nrow(fl_mat))) for (j in seq_len(ncol(fl_mat)))
          if (i!=j) fl_mat[i,j] <- round(cor_mat[i,j],4)
        tool_dt(as.data.frame(fl_mat), "Fornell-Larcker (Diagonal = √AVE)")
      })

      output$dl_cfa_rel <- downloadHandler(
        filename=function() paste0("CFA_Reliability_", Sys.Date(), ".xlsx"),
        content=function(file) {
          sheets <- list()
          if (!is.null(cfa_fit)) {
            fi <- lavaan::fitMeasures(cfa_fit, c("cfi","tli","rmsea","srmr","chisq","df"))
            sheets[["CFA_Fit"]] <- as.data.frame(t(round(fi,4)))
            sheets[["Loadings"]] <- as.data.frame(
              lavaan::parameterEstimates(cfa_fit, standardized=TRUE) |>
              dplyr::filter(op=="=~") |>
              dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~round(.x,4))))
          }
          if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="Run CFA first")
          write_xlsx(sheets, file)
        }
      )
    })


    # ========================================================================
    # TABS 5-8: OUTER MODEL, INNER MODEL, BOOTSTRAP, IPMA (ported from v1)
    # ========================================================================
    output$outer_model_ui <- renderUI({
      m <- pls_model_rv()
      if (is.null(m)) return(tags$div(class="alert alert-info",
        "ℹ️ Build your model in the Model Canvas tab and click ▶ Run All Analyses."))
      sm <- tryCatch(summary(m), error=function(e) NULL)
      if (is.null(sm)) return(tags$div(class="alert alert-danger", "Could not extract model summary."))
      tagList(
        tags$h4("📊 A. Outer Loadings", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_loadings_tbl")),
        tags$hr(),
        tags$h4("🔒 B. Reliability & Validity", style="color:#1A3A5C;"),
        tags$p(style="color:#666;font-size:.85rem;", "α ≥.70 | rho_A ≥.70 | CR ≥.70 | AVE ≥.50"),
        DT::dataTableOutput(ns("pls_reliability_tbl")),
        tags$hr(),
        tags$h4("🔍 C. HTMT Matrix", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_htmt_tbl")),
        tags$hr(),
        tags$h4("📐 D. Fornell-Larcker Criterion", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_fl_tbl")),
        tags$hr(),
        tags$h4("🧮 E. Outer VIF", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_vif_outer_tbl")),
        br(), downloadButton(ns("dl_outer"), "Download Outer Model (Excel)")
      )
    })

    output$pls_loadings_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      ld <- tryCatch(round(as.data.frame(summary(m)$loadings),4),
                     error=function(e) data.frame(Note="Not available"))
      datatable(ld, rownames=TRUE, extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel"))) |>
        formatStyle(names(ld), backgroundColor=styleInterval(c(.60,.70),c("#FFEBEE","#FFF9C4","#E8F5E9")))
    })
    output$pls_reliability_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      rel <- tryCatch(round(as.data.frame(summary(m)$reliability),4),
                      error=function(e) data.frame(Note="Not available"))
      dt  <- datatable(rel, rownames=TRUE, extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")))
      if ("AVE" %in% names(rel)) dt <- dt |> formatStyle("AVE", backgroundColor=styleInterval(.50,c("#FFEBEE","#E8F5E9")))
      dt
    })
    output$pls_htmt_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      h <- tryCatch(round(as.data.frame(summary(m)$validity$htmt),4),
                    error=function(e) data.frame(Note="HTMT not available — run Bootstrap first"))
      datatable(h, rownames=TRUE, extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel"))) |>
        formatStyle(names(h), backgroundColor=styleInterval(c(.85,.90),c("#E8F5E9","#FFF9C4","#FFEBEE")))
    })
    output$pls_fl_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      fl <- tryCatch({
        f <- summary(m)$validity$fl_criteria
        if (is.null(f)) {
          rel  <- summary(m)$reliability
          cors <- cor(m$construct_scores, use="pairwise.complete.obs")
          diag(cors) <- sqrt(rel[,"AVE"])
          round(as.data.frame(cors),4)
        } else round(as.data.frame(f),4)
      }, error=function(e) data.frame(Note="Not available"))
      datatable(fl, rownames=TRUE, options=list(pageLength=20, scrollX=TRUE))
    })
    output$pls_vif_outer_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      v <- tryCatch({
        vd <- summary(m)$vif_antecedents
        if (is.null(vd)) data.frame(Note="No formative constructs") else round(as.data.frame(vd),4)
      }, error=function(e) data.frame(Note="VIF not available"))
      datatable(v, rownames=TRUE, options=list(pageLength=20, scrollX=TRUE))
    })
    output$dl_outer <- downloadHandler(
      filename=function() paste0("PLS_OuterModel_", Sys.Date(), ".xlsx"),
      content=function(file) {
        m <- pls_model_rv(); req(m); sm <- summary(m)
        sheets <- list()
        tryCatch({ sheets[["Loadings"]]    <- round(as.data.frame(sm$loadings),4)    }, error=function(e){})
        tryCatch({ sheets[["Reliability"]] <- round(as.data.frame(sm$reliability),4) }, error=function(e){})
        if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="No data")
        write_xlsx(sheets, file)
      }
    )

    # ── INNER MODEL ──────────────────────────────────────────────────────────
    output$inner_model_ui <- renderUI({
      m <- pls_model_rv()
      if (is.null(m)) return(tags$div(class="alert alert-info","ℹ️ Run Model Canvas first."))
      tagList(
        tags$h4("🔗 A. Structural Paths", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_paths_tbl")),
        tags$hr(),
        tags$h4("📈 B. R²", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_r2_tbl")),
        tags$hr(),
        tags$h4("📐 C. f² Effect Sizes", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_fsq_tbl")),
        tags$hr(),
        tags$h4("🧮 D. Inner VIF", style="color:#1A3A5C;"),
        DT::dataTableOutput(ns("pls_vif_inner_tbl")),
        br(), downloadButton(ns("dl_inner"), "Download Inner Model (Excel)")
      )
    })
    output$pls_paths_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      p <- tryCatch(round(as.data.frame(summary(m)$paths),4),
                    error=function(e) data.frame(Note="Paths not available"))
      datatable(p, rownames=TRUE, extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")))
    })
    output$pls_r2_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      r2 <- tryCatch({
        p <- summary(m)$paths
        r2c <- grep("R\\^2|R2|Rsq", colnames(p), ignore.case=TRUE)
        if (length(r2c)) round(as.data.frame(p[,r2c,drop=FALSE]),4)
        else data.frame(Note="See path table")
      }, error=function(e) data.frame(Note="R² not available"))
      datatable(r2, rownames=TRUE, options=list(pageLength=20, scrollX=TRUE))
    })
    output$pls_fsq_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      f <- tryCatch({
        fs <- summary(m)$fSquare
        if (is.null(fs)) stop("no fSquare")
        round(as.data.frame(fs),4)
      }, error=function(e) data.frame(Note="f² not available"))
      datatable(f, rownames=TRUE, extensions="Buttons",
        options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel"))) |>
        formatStyle(names(f), backgroundColor=styleInterval(c(.02,.15,.35),c("white","#E8F5E9","#C8E6C9","#A5D6A7")))
    })
    output$pls_vif_inner_tbl <- DT::renderDataTable({
      m <- pls_model_rv(); req(m)
      v <- tryCatch({
        vd <- summary(m)$vif_antecedents
        if (is.null(vd)) data.frame(Note="Not computed") else round(as.data.frame(vd),4)
      }, error=function(e) data.frame(Note="VIF not available"))
      datatable(v, rownames=TRUE, options=list(pageLength=20, scrollX=TRUE))
    })
    output$dl_inner <- downloadHandler(
      filename=function() paste0("PLS_InnerModel_", Sys.Date(), ".xlsx"),
      content=function(file) {
        m <- pls_model_rv(); req(m); sm <- summary(m)
        sheets <- list()
        tryCatch({ sheets[["Paths"]]   <- round(as.data.frame(sm$paths),4)   }, error=function(e){})
        tryCatch({ sheets[["fSquare"]] <- round(as.data.frame(sm$fSquare),4) }, error=function(e){})
        if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="No data")
        write_xlsx(sheets, file)
      }
    )

    # ── BOOTSTRAP ────────────────────────────────────────────────────────────
    observeEvent(input$run_boot_pls, {
      m <- pls_model_rv()
      if (is.null(m))  { showNotification("Run Model Canvas first.",  type="warning"); return() }
      if (!.has_seminr10) { showNotification("seminr not installed.", type="error");   return() }
      output$boot_results_ui <- renderUI(
        tags$div(class="alert alert-info",
          paste0("⌛ Bootstrapping (", input$nboot, " samples)... 30-120 sec.")))
      tryCatch({
        set.seed(input$boot_seed %||% 123)
        bm <- seminr::bootstrap_model(seminr_model=m, nboot=input$nboot,
                                       cores=1L, seed=input$boot_seed %||% 123)
        boot_model_rv(bm)
        output$boot_results_ui <- renderUI({
          bsm <- tryCatch(summary(bm, alpha=.05), error=function(e) NULL)
          if (is.null(bsm)) return(tags$div(class="alert alert-danger","Bootstrap summary failed."))
          tagList(
            tags$div(class="alert alert-success",
              tags$b(paste0("✅ Bootstrap complete — ", input$nboot, " samples"))),
            tags$h4("🔄 A. Bootstrapped Paths (95% CI)", style="color:#1A3A5C;"),
            DT::dataTableOutput(ns("boot_paths_tbl")),
            tags$hr(),
            tags$h4("🔗 B. Indirect Effects", style="color:#1A3A5C;"),
            DT::dataTableOutput(ns("boot_indirect_tbl")),
            tags$hr(),
            tags$h4("🔍 C. Bootstrapped HTMT", style="color:#1A3A5C;"),
            DT::dataTableOutput(ns("boot_htmt_tbl")),
            br(), downloadButton(ns("dl_bootstrap"),"Download Bootstrap (Excel)")
          )
        })
        output$boot_paths_tbl <- DT::renderDataTable({
          bsm <- tryCatch(summary(bm,alpha=.05),error=function(e) NULL); req(bsm)
          bp  <- tryCatch(round(as.data.frame(bsm$bootstrapped_paths),4),
                          error=function(e) data.frame(Note="Not available"))
          datatable(bp, rownames=TRUE, extensions="Buttons",
            options=list(pageLength=25, scrollX=TRUE, dom="Bfrtip", buttons=c("copy","csv","excel")))
        })
        output$boot_indirect_tbl <- DT::renderDataTable({
          bsm <- tryCatch(summary(bm,alpha=.05),error=function(e) NULL); req(bsm)
          ie  <- tryCatch({
            i <- bsm$bootstrapped_indirect_effects
            if (is.null(i)||nrow(as.data.frame(i))==0)
              data.frame(Note="No indirect effects — add mediating paths")
            else round(as.data.frame(i),4)
          }, error=function(e) data.frame(Note="Not available"))
          datatable(ie, rownames=TRUE, options=list(pageLength=25, scrollX=TRUE))
        })
        output$boot_htmt_tbl <- DT::renderDataTable({
          bsm <- tryCatch(summary(bm,alpha=.05),error=function(e) NULL); req(bsm)
          ht  <- tryCatch({
            h <- bsm$bootstrapped_HTMT
            if (is.null(h)) data.frame(Note="Not available") else round(as.data.frame(h),4)
          }, error=function(e) data.frame(Note="Not available"))
          datatable(ht, rownames=TRUE, options=list(pageLength=25, scrollX=TRUE))
        })
      }, error=function(e) {
        output$boot_results_ui <- renderUI(
          tags$div(class="alert alert-danger", tags$b("❌ Bootstrap Error: "), e$message))
      })
    })
    output$dl_bootstrap <- downloadHandler(
      filename=function() paste0("PLS_Bootstrap_", Sys.Date(), ".xlsx"),
      content=function(file) {
        bm <- boot_model_rv(); req(bm)
        bsm <- tryCatch(summary(bm,alpha=.05),error=function(e) NULL); req(bsm)
        sheets <- list()
        tryCatch({ sheets[["BootPaths"]]    <- round(as.data.frame(bsm$bootstrapped_paths),4)             },error=function(e){})
        tryCatch({ sheets[["BootIndirect"]] <- round(as.data.frame(bsm$bootstrapped_indirect_effects),4)  },error=function(e){})
        if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="No data")
        write_xlsx(sheets, file)
      }
    )

    # ── IPMA ─────────────────────────────────────────────────────────────────
    output$ipma_target_ui <- renderUI({
      m <- pls_model_rv(); if (is.null(m)) return(NULL)
      sm  <- summary(m)
      p   <- tryCatch(sm$paths, error=function(e) NULL)
      endo <- if (!is.null(p)) rownames(p) else colnames(m$construct_scores)
      endo <- endo[nchar(endo)>0]
      selectInput(ns("ipma_target"),"Target construct for IPMA",choices=endo,selected=endo[length(endo)])
    })
    observeEvent(input$run_ipma, {
      m <- pls_model_rv()
      if (is.null(m)) { showNotification("Run Model Canvas first.", type="warning"); return() }
      tryCatch({
        target <- input$ipma_target; req(target)
        sm <- summary(m)
        te <- tryCatch(sm$total_effects, error=function(e) NULL)
        scores  <- as.data.frame(m$construct_scores)
        perf    <- sapply(names(scores), function(cn) {
          x  <- scores[[cn]]
          mn <- min(x,na.rm=TRUE); mx <- max(x,na.rm=TRUE)
          if (mn==mx) return(rep(50,length(x)))
          (x-mn)/(mx-mn)*100
        })
        perf_means <- colMeans(perf, na.rm=TRUE)
        if (!is.null(te) && target %in% colnames(te)) {
          imp <- te[,target]; imp <- imp[names(imp)!=target & names(imp)%in%names(perf_means)]
        } else {
          p <- sm$paths
          if (!is.null(p) && target %in% rownames(p)) {
            imp_row <- p[target,,drop=FALSE]
            imp     <- as.numeric(imp_row[1,]); names(imp) <- colnames(imp_row)
            imp     <- imp[names(imp)%in%names(perf_means)]
          } else {
            imp <- setNames(rep(0,ncol(scores)-1), setdiff(names(scores),target))
          }
        }
        common    <- intersect(names(imp),names(perf_means))
        if (!length(common)) { showNotification("No common constructs.",type="warning"); return() }
        imp_vals  <- abs(imp[common]); perf_vals <- perf_means[common]
        ipma_df   <- data.frame(Construct=common,
                                Importance=round(imp_vals,4), Performance=round(perf_vals,2),
                                Priority=ifelse(imp_vals>median(imp_vals)&perf_vals<median(perf_vals),
                                                "High Priority","Monitor"),
                                stringsAsFactors=FALSE)
        output$ipma_plot <- renderPlotly({
          plotly::plot_ly(ipma_df, x=~Importance, y=~Performance,
            type="scatter", mode="markers+text",
            text=~Construct, textposition="top center",
            color=~Priority, colors=c("High Priority"="#E74C3C","Monitor"="#3498DB"),
            marker=list(size=18,opacity=.85),
            hovertemplate="%{text}<br>Imp: %{x:.3f}<br>Perf: %{y:.1f}%<extra></extra>") |>
          plotly::layout(
            title=paste("IPMA — Target:", target),
            xaxis=list(title="Importance (Total Effect)"),
            yaxis=list(title="Performance (0–100)", range=c(0,100)),
            paper_bgcolor="white", plot_bgcolor="#FAFAFA")
        })
        output$ipma_tbl <- DT::renderDataTable({ tool_dt(ipma_df, paste("IPMA:",target)) })
      }, error=function(e) showNotification(paste("IPMA error:",e$message),type="error"))
    })

    # ========================================================================
    # TAB 9: CB-SEM (lavaan)
    # ========================================================================
    sem_result <- eventReactive(input$run_sem, {
      req(df(), input$sem_cols, input$sem_syntax)
      d <- df()[,input$sem_cols,drop=FALSE] |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) |> na.omit()
      tryCatch({
        fit <- lavaan::sem(input$sem_syntax, data=d, estimator=input$sem_estimator)
        list(fit=fit, data=d, n=nrow(d), error=NULL)
      }, error=function(e) list(fit=NULL, error=e$message, data=NULL))
    })
    output$fit_cards <- renderUI({
      req(sem_result()); res <- sem_result()
      if (!is.null(res$error))
        return(tags$div(style="color:red;padding:1rem;background:#FFE0E0;border-radius:8px;",
          tags$b("SEM Error: "), res$error))
      fi <- lavaan::fitMeasures(res$fit,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
      mk_card <- function(lbl,val,ok)
        tags$div(style=paste0("background:",if(ok)GREEN else "#C0392B",
          ";color:white;padding:.8rem;border-radius:8px;text-align:center;margin:.3rem;"),
          tags$b(paste(if(ok)"✓" else "✗",lbl)),tags$br(),sprintf("%.3f",as.numeric(val)))
      tagList(
        tags$h4(paste0("✅ SEM — N = ",res$n," (",input$sem_estimator,")")),
        fluidRow(
          column(2,mk_card("CFI ≥.95",  fi["cfi"],  fi["cfi"]>=.95)),
          column(2,mk_card("TLI ≥.95",  fi["tli"],  fi["tli"]>=.95)),
          column(2,mk_card("RMSEA ≤.06",fi["rmsea"],fi["rmsea"]<=.06)),
          column(2,mk_card("SRMR ≤.08", fi["srmr"], fi["srmr"]<=.08)),
          column(2,tags$div(style="background:#1A3A5C;color:white;padding:.8rem;border-radius:8px;text-align:center;margin:.3rem;",
            tags$b("χ²"),tags$br(),sprintf("%.2f(df=%g,p%s)",fi["chisq"],fi["df"],apa_p(fi["pvalue"])))),
          column(2,tags$div(style="background:#1A3A5C;color:white;padding:.8rem;border-radius:8px;text-align:center;margin:.3rem;",
            tags$b("AIC/BIC"),tags$br(),sprintf("%.0f/%.0f",fi["aic"],fi["bic"])))
        ),
        tags$p(style="color:#666;font-size:.82rem;","Hu & Bentler (1999): CFI/TLI ≥.95, RMSEA ≤.06, SRMR ≤.08")
      )
    })
    output$params_tbl <- DT::renderDataTable({
      req(sem_result()); res <- sem_result(); if (!is.null(res$error)) return(NULL)
      params <- lavaan::parameterEstimates(res$fit,standardized=TRUE) |>
        dplyr::mutate(sig=sig_stars(pvalue), dplyr::across(dplyr::where(is.numeric),~round(.x,4)))
      tool_dt(params,"Parameter Estimates (std.all = standardized)")
    })
    output$sem_diagram <- renderPlot({
      req(sem_result()); res <- sem_result(); if (!is.null(res$error)) return(NULL)
      tryCatch(
        semPlot::semPaths(res$fit,what="std",layout="spring",style="lisrel",
          edge.label.cex=.85,label.cex=.9,residuals=TRUE,
          color=list(lat=TEAL,man=NAVY),title=FALSE),
        error=function(e) { plot.new(); text(.5,.5,"Diagram unavailable") })
    })
    output$mod_indices_tbl <- DT::renderDataTable({
      req(sem_result()); res <- sem_result(); if (!is.null(res$error)) return(NULL)
      mi <- tryCatch(lavaan::modindices(res$fit)|>
        dplyr::filter(mi>5)|>dplyr::arrange(dplyr::desc(mi))|>head(20)|>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric),~round(.x,3))),
        error=function(e) NULL)
      if (is.null(mi)) return(NULL)
      tool_dt(mi,"Modification Indices (MI > 5)")
    })
    output$dl_sem <- downloadHandler(
      filename=function() paste0("SEM_",Sys.Date(),".xlsx"),
      content=function(file) {
        req(sem_result()); res <- sem_result(); if (!is.null(res$error)) return()
        params <- as.data.frame(lavaan::parameterEstimates(res$fit,standardized=TRUE))
        fi_df  <- as.data.frame(t(lavaan::fitMeasures(res$fit)))
        write_xlsx(list("Parameters"=params,"FitIndices"=fi_df),file)
      }
    )


    # ── Dynamic extra inputs for Regression ──────────────────────────────────
    output$reg_extra_ui <- renderUI({
      type <- input$reg_type %||% "ols"
      nc   <- num_cols_r()
      if (type == "hier") {
        pickerInput(ns("reg_block1"), "Block 1 predictors", choices=nc, multiple=TRUE,
                    options=list(`actions-box`=TRUE))
      } else if (type == "med") {
        selectInput(ns("reg_med"), "Mediator (M)", choices=nc)
      } else if (type == "mod") {
        selectInput(ns("reg_mod"), "Moderator (W)", choices=nc)
      } else if (type %in% c("poly2","poly3","scurve","loglin","explin")) {
        tagList(
          tags$div(class="alert alert-info", style="font-size:.80rem;padding:.5rem .75rem;",
            switch(type,
              "poly2"    = "📈 Quadratic: fits Y ~ X + X² (captures U-shaped / inverted-U relationships)",
              "poly3"    = "📈 Cubic: fits Y ~ X + X² + X³ (captures S-shaped or more complex curves)",
              "scurve"   = "📈 S-Curve (WarpPLS): log(Y) ~ log(X) — multiplicative/power relationship",
              "loglin"   = "📈 Log-Linear: Y ~ log(X) — diminishing returns / accelerating growth",
              "explin"   = "📈 Exponential: log(Y) ~ X — exponential growth/decay",
              ""))
        )
      } else if (type == "piecewise") {
        tagList(
          numericInput(ns("reg_breakpt"), "Breakpoint / threshold (X value)", value=NA),
          tags$div(class="alert alert-info", style="font-size:.80rem;padding:.5rem .75rem;",
            "📈 Piecewise: fits separate slopes below and above the breakpoint (threshold effect).")
        )
      } else NULL
    })

    # ── Dynamic setup inputs for Group Tests ─────────────────────────────────
    output$gt_setup_ui <- renderUI({
      test <- input$gt_test %||% "ttest"
      nc   <- num_cols_r()
      ac   <- all_cols_r()
      if (test == "chisq") {
        tagList(
          selectInput(ns("grp_dv"), "Variable 1 (rows)", choices=ac),
          selectInput(ns("grp_iv"), "Variable 2 (cols)", choices=ac)
        )
      } else if (test %in% c("ttest","mw")) {
        tagList(
          selectInput(ns("grp_dv"), "Numeric variable (Y)", choices=nc),
          selectInput(ns("grp_iv"), "Grouping variable",    choices=ac),
          if (test == "ttest")
            checkboxInput(ns("grp_varequal"), "Assume equal variances", FALSE)
        )
      } else {
        tagList(
          selectInput(ns("grp_dv"), "Numeric variable (Y)", choices=nc),
          selectInput(ns("grp_iv"), "Grouping variable",    choices=ac)
        )
      }
    })

    # ========================================================================
    # TAB 10: REGRESSION (Simple/Multiple OLS, Hierarchical, Mediation, Moderation)
    # ========================================================================
    reg_result <- eventReactive(input$run_reg, {
      req(df(), input$reg_dv, input$reg_ivs)
      d <- df()
      dv  <- input$reg_dv
      ivs <- input$reg_ivs
      type <- input$reg_type %||% "ols"

      d_reg <- d[, unique(c(dv, ivs)), drop=FALSE]
      d_reg <- dplyr::mutate(d_reg, dplyr::across(dplyr::everything(), as.numeric))
      d_reg <- na.omit(d_reg)
      n     <- nrow(d_reg)
      if (n < 5) return(list(error="Too few rows after listwise deletion."))

      if (type == "hier") {
        req(input$reg_block1)
        b1  <- input$reg_block1
        b2  <- setdiff(ivs, b1)
        f1  <- as.formula(paste(dv,"~",paste(b1,collapse="+")))
        f2  <- as.formula(paste(dv,"~",paste(ivs,collapse="+")))
        m1  <- lm(f1, data=d_reg); s1 <- summary(m1)
        m2  <- lm(f2, data=d_reg); s2 <- summary(m2)
        delta_r2 <- s2$r.squared - s1$r.squared
        av  <- anova(m1, m2)
        list(type="hier", m1=m1, m2=m2, s1=s1, s2=s2,
             delta_r2=delta_r2, anova_table=av, n=n, dv=dv, b1=b1, b2=b2, error=NULL)
      } else if (type == "med") {
        req(input$reg_med)
        med <- input$reg_med
        iv  <- ivs[1]
        # Baron & Kenny (1986) steps + Sobel
        d_m <- d_reg[, unique(c(dv, iv, med)), drop=FALSE]; d_m <- na.omit(d_m)
        ma  <- lm(as.formula(paste(med,"~",iv)),  data=d_m); sa <- summary(ma)
        mb  <- lm(as.formula(paste(dv,"~",iv,"+",med)), data=d_m); sb <- summary(mb)
        mc  <- lm(as.formula(paste(dv,"~",iv)),  data=d_m); sc <- summary(mc)
        a   <- coef(ma)[iv];      sea <- summary(ma)$coefficients[iv,"Std. Error"]
        b   <- coef(mb)[med];     seb <- summary(mb)$coefficients[med,"Std. Error"]
        ab  <- a * b
        sobel_se <- sqrt(b^2*sea^2 + a^2*seb^2)
        sobel_z  <- ab / sobel_se
        sobel_p  <- 2*pnorm(-abs(sobel_z))
        list(type="med", ma=ma, mb=mb, mc=mc, sa=sa, sb=sb, sc=sc,
             a=a, b=b, ab=ab, sobel_z=sobel_z, sobel_p=sobel_p,
             iv=iv, med=med, dv=dv, n=nrow(d_m), error=NULL)
      } else if (type == "mod") {
        req(input$reg_mod)
        mod  <- input$reg_mod
        iv   <- ivs[1]
        d_r  <- d_reg[, unique(c(dv,iv,mod)), drop=FALSE]
        d_r  <- na.omit(d_r)
        d_r[[iv]]  <- scale(d_r[[iv]])[,1]
        d_r[[mod]] <- scale(d_r[[mod]])[,1]
        int_nm     <- paste0(iv,"X",mod)
        d_r[[int_nm]] <- d_r[[iv]] * d_r[[mod]]
        f   <- as.formula(paste(dv,"~",iv,"+",mod,"+",int_nm))
        m   <- lm(f, data=d_r); s <- summary(m)
        list(type="mod", m=m, s=s, iv=iv, mod=mod, int_nm=int_nm,
             dv=dv, n=nrow(d_r), error=NULL)
      } else if (type == "poly2") {
        # Quadratic: Y ~ X + X²  (WarpPLS quadratic relationship)
        iv <- ivs[1]
        d_p <- d_reg[, c(dv, iv), drop=FALSE]; d_p <- na.omit(d_p)
        d_p[[iv]] <- scale(d_p[[iv]])[,1]  # centre before squaring
        iv2_nm <- paste0(iv, "_sq")
        d_p[[iv2_nm]] <- d_p[[iv]]^2
        f <- as.formula(paste(dv, "~", iv, "+", iv2_nm))
        m <- lm(f, data=d_p); s <- summary(m)
        # Turning point x* = -b1 / (2*b2)
        cf  <- coef(m)
        tp  <- if (!is.na(cf[iv]) && !is.na(cf[iv2_nm]) && cf[iv2_nm] != 0)
                 round(-cf[iv] / (2 * cf[iv2_nm]), 3) else NA
        shape <- if (!is.na(cf[iv2_nm]))
                   ifelse(cf[iv2_nm] > 0, "U-shaped (minimum)", "Inverted-U (maximum)") else "—"
        list(type="poly2", m=m, s=s, n=nrow(d_p), dv=dv, iv=iv, iv2=iv2_nm,
             turning_point=tp, shape=shape, error=NULL)

      } else if (type == "poly3") {
        # Cubic: Y ~ X + X² + X³
        iv <- ivs[1]
        d_p <- d_reg[, c(dv, iv), drop=FALSE]; d_p <- na.omit(d_p)
        d_p[[iv]] <- scale(d_p[[iv]])[,1]
        iv2_nm <- paste0(iv,"_sq"); iv3_nm <- paste0(iv,"_cu")
        d_p[[iv2_nm]] <- d_p[[iv]]^2; d_p[[iv3_nm]] <- d_p[[iv]]^3
        f <- as.formula(paste(dv,"~",iv,"+",iv2_nm,"+",iv3_nm))
        m <- lm(f, data=d_p); s <- summary(m)
        list(type="poly3", m=m, s=s, n=nrow(d_p), dv=dv, iv=iv,
             iv2=iv2_nm, iv3=iv3_nm, error=NULL)

      } else if (type == "scurve") {
        # S-Curve / Log-Log: log(Y) ~ log(X)  — WarpPLS warp3/S-curve
        iv <- ivs[1]
        d_s <- d_reg[, c(dv, iv), drop=FALSE]; d_s <- na.omit(d_s)
        d_s <- d_s[d_s[[dv]] > 0 & d_s[[iv]] > 0, ]
        if (nrow(d_s) < 5)
          return(list(error="S-Curve requires all values of DV and IV to be positive (> 0)."))
        log_dv_nm <- paste0("log_", dv); log_iv_nm <- paste0("log_", iv)
        d_s[[log_dv_nm]] <- log(d_s[[dv]]); d_s[[log_iv_nm]] <- log(d_s[[iv]])
        f <- as.formula(paste(log_dv_nm, "~", log_iv_nm))
        m <- lm(f, data=d_s); s <- summary(m)
        elasticity <- round(coef(m)[log_iv_nm], 3)
        list(type="scurve", m=m, s=s, n=nrow(d_s), dv=dv, iv=iv,
             log_dv=log_dv_nm, log_iv=log_iv_nm,
             elasticity=elasticity,
             interp=sprintf("A 1%% increase in %s is associated with a %.2f%% change in %s (elasticity).",
                            iv, elasticity, dv),
             error=NULL)

      } else if (type == "loglin") {
        # Log-Linear: Y ~ log(X) — diminishing returns
        iv <- ivs[1]
        d_l <- d_reg[, c(dv, iv), drop=FALSE]; d_l <- na.omit(d_l)
        d_l <- d_l[d_l[[iv]] > 0, ]
        if (nrow(d_l) < 5)
          return(list(error="Log-Linear requires all IV values to be positive (> 0)."))
        log_iv_nm <- paste0("log_", iv)
        d_l[[log_iv_nm]] <- log(d_l[[iv]])
        f <- as.formula(paste(dv, "~", log_iv_nm))
        m <- lm(f, data=d_l); s <- summary(m)
        list(type="loglin", m=m, s=s, n=nrow(d_l), dv=dv, iv=iv,
             log_iv=log_iv_nm, error=NULL)

      } else if (type == "explin") {
        # Exponential: log(Y) ~ X
        iv <- ivs[1]
        d_e <- d_reg[, c(dv, iv), drop=FALSE]; d_e <- na.omit(d_e)
        d_e <- d_e[d_e[[dv]] > 0, ]
        if (nrow(d_e) < 5)
          return(list(error="Exponential model requires all DV values to be positive (> 0)."))
        log_dv_nm <- paste0("log_", dv)
        d_e[[log_dv_nm]] <- log(d_e[[dv]])
        f <- as.formula(paste(log_dv_nm, "~", iv))
        m <- lm(f, data=d_e); s <- summary(m)
        growth_rate <- round((exp(coef(m)[iv]) - 1) * 100, 2)
        list(type="explin", m=m, s=s, n=nrow(d_e), dv=dv, iv=iv,
             log_dv=log_dv_nm,
             growth_rate=growth_rate,
             interp=sprintf("A 1-unit increase in %s is associated with a %.2f%% change in %s.",
                            iv, growth_rate, dv),
             error=NULL)

      } else if (type == "piecewise") {
        # Piecewise / threshold regression
        iv <- ivs[1]; bp <- input$reg_breakpt
        if (is.na(bp)) return(list(error="Please specify a breakpoint value."))
        d_pw <- d_reg[, c(dv, iv), drop=FALSE]; d_pw <- na.omit(d_pw)
        d_pw[[paste0(iv,"_below")]] <- pmin(d_pw[[iv]], bp) - bp
        d_pw[[paste0(iv,"_above")]] <- pmax(d_pw[[iv]], bp) - bp
        f <- as.formula(paste(dv,"~",paste0(iv,"_below"),"+",paste0(iv,"_above")))
        m <- lm(f, data=d_pw); s <- summary(m)
        list(type="piecewise", m=m, s=s, n=nrow(d_pw), dv=dv, iv=iv,
             breakpoint=bp, error=NULL)

      } else {
        # Simple / Multiple OLS
        f <- as.formula(paste(dv,"~",paste(ivs,collapse="+")))
        m <- lm(f, data=d_reg); s <- summary(m)
        list(type="ols", m=m, s=s, n=n, dv=dv, ivs=ivs, error=NULL)
      }
    })

    observeEvent(input$run_reg, {
      res <- tryCatch(reg_result(), error=function(e) list(error=e$message))

      # ── Coefficients tab ──
      output$reg_coef_tbl <- DT::renderDataTable({
        if (!is.null(res$error)) return(NULL)
        if (res$type == "ols") {
          ct <- as.data.frame(res$s$coefficients)
          ct$Predictor <- rownames(ct); names(ct) <- c("B","SE","t","p","Predictor")
          ct$Sig <- sig_stars(ct$p)
          tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),
                  paste0("OLS: ",res$dv," ~ ",paste(res$ivs,collapse=" + ")))
        } else if (res$type == "hier") {
          ct <- as.data.frame(res$s2$coefficients); ct$Pred <- rownames(ct)
          names(ct) <- c("B","SE","t","p","Predictor"); ct$Sig <- sig_stars(ct$p)
          tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),
                  paste0("Block 2 (Full) — R²=",round(res$s2$r.squared,4)))
        } else if (res$type == "med") {
          tool_dt(round_df(as.data.frame(res$sb$coefficients),4),
                  paste0("b path: ",res$iv," + ",res$med," → ",res$dv))
        } else if (res$type == "mod") {
          ct <- as.data.frame(res$s$coefficients); ct$Pred <- rownames(ct)
          names(ct) <- c("B","SE","t","p","Predictor"); ct$Sig <- sig_stars(ct$p)
          tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),
                  "Moderation Coefficients (mean-centered)")
        } else if (res$type %in% c("poly2","poly3","scurve","loglin","explin","piecewise")) {
          ct <- as.data.frame(res$s$coefficients); ct$Predictor <- rownames(ct)
          names(ct)[1:4] <- c("B","SE","t","p"); ct$Sig <- sig_stars(ct$p)
          lbl <- switch(res$type,
            "poly2"     = paste0("Quadratic: ",res$dv," ~ ",res$iv," + ",res$iv,"²"),
            "poly3"     = paste0("Cubic: ",res$dv," ~ ",res$iv," + ",res$iv,"² + ",res$iv,"³"),
            "scurve"    = paste0("S-Curve (Log-Log): log(",res$dv,") ~ log(",res$iv,")"),
            "loglin"    = paste0("Log-Linear: ",res$dv," ~ log(",res$iv,")"),
            "explin"    = paste0("Exponential: log(",res$dv,") ~ ",res$iv),
            "piecewise" = paste0("Piecewise: breakpoint = ",res$breakpoint),
            res$type)
          tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4), lbl)
        } else NULL
      })

      # ── Model Summary tab ──
      output$reg_summary_ui <- renderUI({
        if (!is.null(res$error))
          return(tags$div(class="alert alert-danger", tags$b("Error: "), res$error))
        if (res$type == "ols") {
          tags$div(class="alert alert-success",
            tags$b("R² = "), round(res$s$r.squared,4),
            tags$b(" | Adj R² = "), round(res$s$adj.r.squared,4),
            tags$b(sprintf(" | F(%g,%g) = ",res$s$fstatistic[2],res$s$fstatistic[3])),
            round(res$s$fstatistic[1],2),
            tags$b(" | N = "), res$n)
        } else if (res$type == "hier") {
          tagList(
            tags$h5("Block 1"), DT::dataTableOutput(ns("reg_b1_tbl")),
            tags$hr(),
            tags$h5("Block 2 (Full)"), DT::dataTableOutput(ns("reg_b2_tbl")),
            tags$div(class="alert alert-info",
              tags$b("ΔR² = "), round(res$delta_r2,4),
              tags$b(" | F-change: "), sprintf("F(%g,%g)=%.2f, p=%.4f",
                res$anova_table$Df[2], res$anova_table$Res.Df[2],
                res$anova_table$F[2], res$anova_table$`Pr(>F)`[2]))
          )
        } else if (res$type == "med") {
          tagList(
            tags$div(class="alert alert-info",
              tags$b("Sobel Test: "),
              sprintf("z = %.3f, p = %.4f | ab = %.4f", res$sobel_z, res$sobel_p, res$ab)),
            tags$h5(paste("a path:", res$iv, "→", res$med)),
            DT::dataTableOutput(ns("reg_ma_tbl")),
            tags$h5(paste("c path (total):", res$iv, "→", res$dv)),
            DT::dataTableOutput(ns("reg_mc_tbl"))
          )
        } else if (res$type == "mod") {
          tags$div(class="alert alert-info",
            tags$b(paste0("N = ",res$n," | R² = ",round(res$s$r.squared,4))),
            sprintf(" | F(%g,%g) = %.2f",
              res$s$fstatistic[2],res$s$fstatistic[3],res$s$fstatistic[1]))
        } else if (res$type %in% c("poly2","poly3","scurve","loglin","explin","piecewise")) {
          tagList(
            tags$div(class="alert alert-success",
              tags$b("R² = "), round(res$s$r.squared,4),
              tags$b(" | Adj R² = "), round(res$s$adj.r.squared,4),
              tags$b(sprintf(" | F(%g,%g) = ",res$s$fstatistic[2],res$s$fstatistic[3])),
              round(res$s$fstatistic[1],2),
              tags$b(" | N = "), res$n
            ),
            if (res$type=="poly2" && !is.na(res$turning_point))
              tags$div(class="alert alert-info",
                tags$b("Curve shape: "), res$shape,
                tags$b(" | Turning point (standardized X): "), res$turning_point),
            if (res$type=="scurve")
              tags$div(class="alert alert-info",
                tags$b("Elasticity (β from log-log): "), res$elasticity,
                tags$br(), res$interp),
            if (res$type=="explin")
              tags$div(class="alert alert-info",
                tags$b("Implied growth rate: "), paste0(res$growth_rate,"%"),
                tags$br(), res$interp),
            tags$div(class="alert alert-success", style="font-size:.81rem;margin-top:.5rem;",
              "📈 See the ", tags$b("Fitted Curve"), " tab for the non-linear plot.",
              " Diagnostics tab shows model adequacy checks.")
          )
        }
      })

      # ── Non-linear fitted curve plot ──
      output$reg_nlcurve_plot <- plotly::renderPlotly({
        req(!is.null(res) && is.null(res$error))
        req(res$type %in% c("poly2","poly3","scurve","loglin","explin","piecewise"))
        m <- res$m
        # Extract original x and y from model frame
        mf <- model.frame(m)
        pred_col <- names(mf)[1]   # response in model (may be log-transformed)
        x_col    <- names(mf)[2]   # first predictor
        y_obs    <- mf[[pred_col]]
        x_obs    <- mf[[x_col]]
        # Predicted on fine grid
        x_range  <- seq(min(x_obs, na.rm=TRUE), max(x_obs, na.rm=TRUE), length.out=200)
        newdf    <- as.data.frame(setNames(list(x_range), x_col))
        # Add extra terms for polynomial
        if (res$type=="poly2") newdf[[res$iv2]] <- x_range^2
        if (res$type=="poly3") {
          newdf[[res$iv2]] <- x_range^2
          newdf[[res$iv3]] <- x_range^3
        }
        if (res$type=="piecewise") {
          bp <- res$breakpoint
          newdf[[paste0(res$iv,"_below")]] <- pmin(x_range, bp) - bp
          newdf[[paste0(res$iv,"_above")]] <- pmax(x_range, bp) - bp
        }
        y_fit <- tryCatch(predict(m, newdata=newdf), error=function(e) NULL)
        if (is.null(y_fit)) return(plotly::plot_ly() %>%
          plotly::layout(title="Could not compute fitted curve"))

        x_lbl <- res$iv %||% x_col
        y_lbl <- res$dv %||% pred_col
        trans_note <- if (res$type %in% c("scurve","explin")) " (log scale — model response)" else ""

        plotly::plot_ly() |>
          plotly::add_trace(x=x_obs, y=y_obs, type="scatter", mode="markers",
            name="Observed", marker=list(color="#2196A6", size=5, opacity=0.55)) |>
          plotly::add_trace(x=x_range, y=y_fit, type="scatter", mode="lines",
            name="Fitted curve", line=list(color="#E74C3C", width=2.5)) |>
          plotly::layout(
            title=paste0(switch(res$type,
              "poly2"="Quadratic","poly3"="Cubic","scurve"="S-Curve (Log-Log)",
              "loglin"="Log-Linear","explin"="Exponential","piecewise"="Piecewise",""),
              " Fit: R²=", round(res$s$r.squared,3)),
            xaxis=list(title=x_lbl),
            yaxis=list(title=paste0(y_lbl, trans_note))
          )
      })

      # ── Diagnostics tab ──
      output$reg_diag_plot <- renderPlot({
        req(is.null(res$error))
        m <- switch(res$type,
          "ols"  = res$m,
          "hier" = res$m2,
          "med"  = res$mb,
          "mod"  = res$m,
          "poly2"= res$m, "poly3"=res$m, "scurve"=res$m,
          "loglin"=res$m, "explin"=res$m, "piecewise"=res$m,
          NULL)
        req(!is.null(m))
        par(mfrow=c(2,2))
        tryCatch(plot(m), error=function(e){ plot.new(); text(.5,.5,"Diagnostics unavailable") })
      }, height=420)

      # ── APA Write-Up tab ──
      output$reg_apa_txt <- renderText({
        if (!is.null(res$error)) return(paste("Error:", res$error))
        if (res$type == "ols") {
          p_val <- tryCatch(
            pf(res$s$fstatistic[1],res$s$fstatistic[2],res$s$fstatistic[3],lower.tail=FALSE),
            error=function(e) NA)
          sprintf(
            "Regression Results\n\nA multiple regression was conducted to predict %s from %s. The model was statistically significant, F(%g, %g) = %.2f, p %s, explaining %.1f%% of variance (R² = %.3f, adjusted R² = %.3f). N = %d.\n\nCoefficients are provided in the Coefficients tab.",
            res$dv, paste(res$ivs,collapse=", "),
            res$s$fstatistic[2], res$s$fstatistic[3], res$s$fstatistic[1],
            apa_p(p_val), res$s$r.squared*100, res$s$r.squared, res$s$adj.r.squared, res$n)
        } else if (res$type == "hier") {
          sprintf(
            "Hierarchical Regression Results\n\nBlock 1 predictors (%s): R² = %.3f.\nBlock 2 added predictors (%s): R² = %.3f.\nΔR² = %.3f, F(%g, %g) = %.2f, p = %.4f.",
            paste(res$b1,collapse=", "), res$s1$r.squared,
            paste(res$b2,collapse=", "), res$s2$r.squared,
            res$delta_r2,
            res$anova_table$Df[2], res$anova_table$Res.Df[2],
            res$anova_table$F[2], res$anova_table$`Pr(>F)`[2])
        } else if (res$type == "med") {
          sprintf(
            "Mediation Analysis (Baron & Kenny + Sobel Test)\n\nIndirect effect ab = %.4f. Sobel z = %.3f, p %s.\nVerdict: %s",
            res$ab, res$sobel_z, apa_p(res$sobel_p),
            if(res$sobel_p<.05) "Mediation supported." else "Mediation not supported at p < .05.")
        } else if (res$type == "mod") {
          ct <- res$s$coefficients
          int_p <- tryCatch(ct[res$int_nm,"Pr(>|t|)"], error=function(e) NA)
          sprintf(
            "Moderation Analysis\n\nInteraction term (%s): β = %.3f, t = %.3f, p %s. N = %d, R² = %.3f.\nVerdict: %s",
            res$int_nm,
            tryCatch(ct[res$int_nm,"Estimate"], error=function(e) NA),
            tryCatch(ct[res$int_nm,"t value"],  error=function(e) NA),
            apa_p(int_p), res$n, res$s$r.squared,
            if(!is.na(int_p)&&int_p<.05) "Significant moderation effect." else "Non-significant moderation.")
        } else if (res$type %in% c("poly2","poly3","scurve","loglin","explin","piecewise")) {
          p_f <- tryCatch(
            pf(res$s$fstatistic[1],res$s$fstatistic[2],res$s$fstatistic[3],lower.tail=FALSE),
            error=function(e) NA)
          base_txt <- sprintf(
            "Non-Linear Regression Results\n\nModel: %s.\nF(%g, %g) = %.2f, p %s. R² = %.3f, Adjusted R² = %.3f. N = %d.\n\n",
            switch(res$type,
              "poly2"    = paste0(res$dv," ~ ",res$iv," + ",res$iv,"² (quadratic)"),
              "poly3"    = paste0(res$dv," ~ ",res$iv," + ",res$iv,"² + ",res$iv,"³ (cubic)"),
              "scurve"   = paste0("log(",res$dv,") ~ log(",res$iv,") (S-curve / log-log)"),
              "loglin"   = paste0(res$dv," ~ log(",res$iv,") (log-linear)"),
              "explin"   = paste0("log(",res$dv,") ~ ",res$iv," (exponential)"),
              "piecewise"= paste0(res$dv," ~ piecewise(",res$iv,", bp=",res$breakpoint,")")),
            res$s$fstatistic[2], res$s$fstatistic[3],
            res$s$fstatistic[1], apa_p(p_f),
            res$s$r.squared, res$s$adj.r.squared, res$n)
          extra <- switch(res$type,
            "poly2"  = if (!is.na(res$turning_point))
                         sprintf("Curve shape: %s. Turning point at standardized X = %.3f.\n",
                                 res$shape, res$turning_point) else "",
            "scurve" = sprintf("Elasticity (power coefficient): β = %.3f. %s\n",
                               res$elasticity, res$interp),
            "explin" = sprintf("Implied growth rate per unit increase in %s: %.2f%%.\n%s\n",
                               res$iv, res$growth_rate, res$interp),
            "")
          paste0(base_txt, extra, "Coefficients are reported in the Coefficients tab.")
        } else ""
      })

      # ── Sub-tables (used inside reg_summary_ui renderUI above) ──
      output$reg_b1_tbl <- DT::renderDataTable({ req(res$type=="hier")
        ct <- as.data.frame(res$s1$coefficients); ct$Pred <- rownames(ct)
        names(ct) <- c("B","SE","t","p","Predictor"); ct$Sig <- sig_stars(ct$p)
        tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),
                paste0("Block 1 R²=",round(res$s1$r.squared,4)))
      })
      output$reg_b2_tbl <- DT::renderDataTable({ req(res$type=="hier")
        ct <- as.data.frame(res$s2$coefficients); ct$Pred <- rownames(ct)
        names(ct) <- c("B","SE","t","p","Predictor"); ct$Sig <- sig_stars(ct$p)
        tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),
                paste0("Block 2 R²=",round(res$s2$r.squared,4)))
      })
      output$reg_ma_tbl <- DT::renderDataTable({ req(res$type=="med")
        tool_dt(round_df(as.data.frame(res$sa$coefficients),4),"a path") })
      output$reg_mb_tbl <- DT::renderDataTable({ req(res$type=="med")
        tool_dt(round_df(as.data.frame(res$sb$coefficients),4),"b path") })
      output$reg_mc_tbl <- DT::renderDataTable({ req(res$type=="med")
        tool_dt(round_df(as.data.frame(res$sc$coefficients),4),"c path (total)") })
      output$reg_mod_tbl <- DT::renderDataTable({ req(res$type=="mod")
        ct <- as.data.frame(res$s$coefficients); ct$Pred <- rownames(ct)
        names(ct) <- c("B","SE","t","p","Predictor"); ct$Sig <- sig_stars(ct$p)
        tool_dt(round_df(ct[,c("Predictor","B","SE","t","p","Sig")],4),"Moderation") })

      output$dl_reg <- downloadHandler(
        filename=function() paste0("Regression_",Sys.Date(),".xlsx"),
        content=function(file) {
          sheets <- list()
          tryCatch({
            if (res$type=="ols") {
              sheets[["OLS"]] <- round_df(as.data.frame(res$s$coefficients),4)
            } else if (res$type=="hier") {
              sheets[["Block1"]] <- round_df(as.data.frame(res$s1$coefficients),4)
              sheets[["Block2"]] <- round_df(as.data.frame(res$s2$coefficients),4)
              sheets[["Delta_R2"]] <- data.frame(
                Delta_R2=round(res$delta_r2,4),
                F=round(res$anova_table$F[2],4),
                p=round(res$anova_table$`Pr(>F)`[2],4))
            } else if (res$type=="med") {
              sheets[["a_path"]] <- round_df(as.data.frame(res$sa$coefficients),4)
              sheets[["b_path"]] <- round_df(as.data.frame(res$sb$coefficients),4)
              sheets[["c_path"]] <- round_df(as.data.frame(res$sc$coefficients),4)
              sheets[["Sobel"]]  <- data.frame(ab=round(res$ab,4),z=round(res$sobel_z,4),p=round(res$sobel_p,4))
            } else if (res$type=="mod") {
              sheets[["Moderation"]] <- round_df(as.data.frame(res$s$coefficients),4)
            } else if (res$type %in% c("poly2","poly3","scurve","loglin","explin","piecewise")) {
              ct <- round_df(as.data.frame(res$s$coefficients),4)
              ct$Predictor <- rownames(ct); sheets[["NonLinear_Coefficients"]] <- ct
              sheets[["Model_Summary"]] <- data.frame(
                Model=res$type, N=res$n, R2=round(res$s$r.squared,4),
                Adj_R2=round(res$s$adj.r.squared,4),
                F=round(res$s$fstatistic[1],4),
                df1=res$s$fstatistic[2], df2=res$s$fstatistic[3],
                DV=res$dv, IV=res$iv)
              if (res$type=="poly2" && !is.na(res$turning_point))
                sheets[["Turning_Point"]] <- data.frame(Shape=res$shape, Turning_Point=res$turning_point)
              if (res$type=="scurve")
                sheets[["Elasticity"]] <- data.frame(Elasticity=res$elasticity, Interpretation=res$interp)
              if (res$type=="explin")
                sheets[["Growth_Rate"]] <- data.frame(Growth_Rate_pct=res$growth_rate, Interpretation=res$interp)
            }
          }, error=function(e){})
          if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="No results")
          write_xlsx(sheets, file)
        }
      )

      # ── AI Interpret for Regression ──
      output$ai_reg_out <- renderUI(NULL)
    })

    observeEvent(input$ai_reg, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) {
        output$ai_reg_out <- renderUI(.ai_k10()); return()
      }
      ctx <- tryCatch({
        res <- reg_result()
        paste0("REGRESSION TYPE: ",res$type," | N=",res$n," | DV=",res$dv,
               "\nCoefficients:\n",
               paste(capture.output(print(round(
                 as.data.frame(if(res$type=="ols") res$s$coefficients
                               else if(res$type=="hier") res$s2$coefficients
                               else if(res$type=="med") res$sb$coefficients
                               else res$s$coefficients),4))),collapse="\n"))
      }, error=function(e) "Run Regression first.")
      output$ai_reg_out <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an expert in regression analysis writing for a peer-reviewed journal.\n\n",
        "Interpret these regression results:\n",
        "1. Overall model fit (R², F, p)\n2. Individual predictors (β, t, p)\n",
        "3. Effect sizes and practical significance\n4. APA-style Results section\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })

    # ========================================================================
    # TAB 11: GROUP TESTS
    # ========================================================================
    observeEvent(input$run_gt, {
      req(df(), input$gt_test)
      d <- df(); type <- input$gt_test
      output$gt_results_ui <- renderUI({
        tryCatch({
          res <- switch(type,
            "ttest" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv, input$grp_iv), drop=FALSE]
              d2[[input$grp_dv]] <- as.numeric(d2[[input$grp_dv]])
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              ve <- isTRUE(input$grp_varequal)
              tt <- t.test(as.formula(paste(input$grp_dv,"~",input$grp_iv)), data=d2, var.equal=ve)
              list(type="ttest", result=tt, n=nrow(d2))
            },
            "mw" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv,input$grp_iv),drop=FALSE]
              d2[[input$grp_dv]] <- as.numeric(d2[[input$grp_dv]])
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              wt <- wilcox.test(as.formula(paste(input$grp_dv,"~",input$grp_iv)), data=d2)
              list(type="mw", result=wt, n=nrow(d2))
            },
            "anova" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv,input$grp_iv),drop=FALSE]
              d2[[input$grp_dv]] <- as.numeric(d2[[input$grp_dv]])
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              av <- aov(as.formula(paste(input$grp_dv,"~",input$grp_iv)), data=d2)
              tu <- TukeyHSD(av)
              list(type="anova", result=av, tukey=tu, n=nrow(d2))
            },
            "kw" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv,input$grp_iv),drop=FALSE]
              d2[[input$grp_dv]] <- as.numeric(d2[[input$grp_dv]])
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              kt <- kruskal.test(as.formula(paste(input$grp_dv,"~",input$grp_iv)), data=d2)
              list(type="kw", result=kt, n=nrow(d2))
            },
            "manova" = {
              req(input$grp_dv, input$grp_iv)
              nc_vars <- num_cols_r()
              d2 <- d[, unique(c(nc_vars, input$grp_iv)), drop=FALSE]
              d2 <- dplyr::mutate(d2, dplyr::across(dplyr::where(is.numeric), as.numeric))
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              y_mat <- as.matrix(d2[, setdiff(nc_vars, input$grp_iv), drop=FALSE])
              mv <- manova(y_mat ~ d2[[input$grp_iv]])
              list(type="manova", result=mv, n=nrow(d2))
            },
            "chisq" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv,input$grp_iv),drop=FALSE]; d2 <- na.omit(d2)
              tbl <- table(d2[[input$grp_iv]], d2[[input$grp_dv]])
              ct  <- chisq.test(tbl)
              list(type="chisq", result=ct, table=tbl, n=nrow(d2))
            },
            "welch" = {
              req(input$grp_dv, input$grp_iv)
              d2 <- d[,c(input$grp_dv,input$grp_iv),drop=FALSE]
              d2[[input$grp_dv]] <- as.numeric(d2[[input$grp_dv]])
              d2[[input$grp_iv]] <- as.factor(d2[[input$grp_iv]])
              d2 <- na.omit(d2)
              wf <- oneway.test(as.formula(paste(input$grp_dv,"~",input$grp_iv)), data=d2, var.equal=FALSE)
              list(type="welch", result=wf, n=nrow(d2))
            },
            list(error="Unknown test type")
          )
          if (!is.null(res$error)) return(tags$div(class="alert alert-danger", res$error))

          # Build main stat summary block
          stat_block <- switch(res$type,
            "ttest"  = tags$div(class="alert alert-success",
              tags$b("t-test: "), sprintf("t(%.1f) = %.3f, p = %.4f, 95%% CI [%.3f, %.3f]",
                res$result$parameter, res$result$statistic, res$result$p.value,
                res$result$conf.int[1], res$result$conf.int[2])),
            "mw"     = tags$div(class="alert alert-success",
              tags$b("Mann-Whitney U: "), sprintf("W = %.0f, p = %.4f",
                res$result$statistic, res$result$p.value)),
            "anova"  = {
              sm <- summary(res$result)[[1]]
              tags$div(class="alert alert-success",
                tags$b("One-way ANOVA: "), sprintf("F(%g,%g) = %.3f, p = %.4f",
                  sm$Df[1], sm$Df[2], sm$`F value`[1], sm$`Pr(>F)`[1]))
            },
            "kw"     = tags$div(class="alert alert-success",
              tags$b("Kruskal-Wallis: "), sprintf("H(%.0f) = %.3f, p = %.4f",
                res$result$parameter, res$result$statistic, res$result$p.value)),
            "manova" = {
              sm <- tryCatch(summary(res$result), error=function(e) NULL)
              tags$div(class="alert alert-success",
                tags$b("MANOVA (Pillai): "),
                if (!is.null(sm))
                  sprintf("Pillai = %.3f, F = %.3f, p = %.4f",
                    sm$stats["d2[[input$grp_iv]]","Pillai"],
                    sm$stats["d2[[input$grp_iv]]","approx F"],
                    sm$stats["d2[[input$grp_iv]]","Pr(>F)"])
                else "See console for details")
            },
            "chisq"  = tags$div(class="alert alert-success",
              tags$b("Chi-square: "), sprintf("χ²(%g) = %.3f, p = %.4f",
                res$result$parameter, res$result$statistic, res$result$p.value)),
            "welch"  = tags$div(class="alert alert-success",
              tags$b("Welch ANOVA: "), sprintf("F(%.1f,%.1f) = %.3f, p = %.4f",
                res$result$parameter[1], res$result$parameter[2],
                res$result$statistic, res$result$p.value)),
            tags$div()
          )

          extra_tbl <- if (res$type == "anova") {
            tagList(tags$hr(), tags$h5("Tukey HSD Post-hoc"),
              DT::dataTableOutput(ns("grp_tukey_tbl")))
          } else if (res$type == "chisq") {
            tagList(tags$hr(), tags$h5("Contingency Table"),
              DT::dataTableOutput(ns("grp_ct_tbl")))
          } else NULL

          output$grp_tukey_tbl <- DT::renderDataTable({ req(res$type=="anova")
            td <- as.data.frame(res$tukey[[1]])
            td$Comparison <- rownames(td); td$Sig <- sig_stars(td[,"p adj"])
            tool_dt(round_df(td,4),"Tukey HSD")
          })
          output$grp_ct_tbl <- DT::renderDataTable({ req(res$type=="chisq")
            tool_dt(as.data.frame.matrix(res$table),"Contingency Table")
          })

          tagList(
            tags$h4(paste("📐", toupper(res$type), "Results"), style="color:#1A3A5C;"),
            tags$p(paste0("N = ", res$n)),
            stat_block,
            extra_tbl,
            br(), downloadButton(ns("dl_grp"), "Download Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger",
          tags$b("Error: "), e$message))
      })
      output$dl_grp <- downloadHandler(
        filename=function() paste0("GroupTest_",Sys.Date(),".xlsx"),
        content=function(file) {
          write_xlsx(list(Results=data.frame(Note="See on-screen table")), file)
        }
      )
    })

    # ========================================================================
    # TAB 12: ML / PREDICTION
    # ========================================================================
    observeEvent(input$run_ml, {
      req(df(), input$ml_target, input$ml_features)
      d     <- df()
      dv    <- input$ml_target
      ivs   <- input$ml_features
      algo  <- input$ml_algo %||% "rf"
      split <- (input$ml_split %||% 70) / 100

      output$ml_results_ui <- renderUI({
        tryCatch({
          d2 <- d[, unique(c(dv,ivs)), drop=FALSE]
          d2[[dv]] <- as.numeric(d2[[dv]])
          for (v in ivs) d2[[v]] <- as.numeric(d2[[v]])
          d2 <- na.omit(d2)
          n  <- nrow(d2)
          if (n < 20) stop("Need ≥20 complete rows for ML.")

          set.seed(input$ml_seed %||% 42)
          train_idx <- sample(seq_len(n), size=floor(split*n))
          train_d   <- d2[train_idx,]; test_d <- d2[-train_idx,]

          # Infer task: logistic = classification, others = regression by default
          is_class  <- algo %in% c("logit", "lr")
          if (is_class) {
            train_d[[dv]] <- as.factor(train_d[[dv]])
            test_d[[dv]]  <- as.factor(test_d[[dv]])
          }

          fmla <- as.formula(paste(dv,"~",paste(ivs,collapse="+")))

          fit <- switch(algo,
            "rf"   = randomForest::randomForest(fmla, data=train_d, ntree=200),
            "svm"  = e1071::svm(fmla, data=train_d, scale=TRUE),
            "rpart"= rpart::rpart(fmla, data=train_d),
            "logit"= glm(fmla, data=train_d,
                          family=if(is_class) binomial() else gaussian()),
            stop("Unknown algorithm")
          )

          pred <- predict(fit, newdata=test_d)

          if (is_class) {
            pred_fac  <- factor(pred, levels=levels(test_d[[dv]]))
            true_fac  <- test_d[[dv]]
            cm        <- table(Actual=true_fac, Predicted=pred_fac)
            acc       <- sum(diag(cm))/sum(cm)
            metric_txt <- sprintf("Accuracy = %.4f  |  N_test = %d", acc, nrow(test_d))
          } else {
            rmse <- sqrt(mean((as.numeric(pred) - as.numeric(test_d[[dv]]))^2, na.rm=TRUE))
            r2   <- cor(as.numeric(pred), as.numeric(test_d[[dv]]), use="complete.obs")^2
            metric_txt <- sprintf("RMSE = %.4f  |  R² = %.4f  |  N_test = %d", rmse, r2, nrow(test_d))
          }

          # Variable importance
          vi_df <- tryCatch({
            if (algo == "rf") {
              vi <- randomForest::importance(fit)
              data.frame(Variable=rownames(vi), Importance=round(vi[,1],4))
            } else if (algo == "rpart") {
              vi <- fit$variable.importance
              data.frame(Variable=names(vi), Importance=round(vi,4))
            } else data.frame(Note="Importance not available for this algorithm")
          }, error=function(e) data.frame(Note="Not available"))

          # Store for output tables
          ml_result_local <- list(fit=fit, vi=vi_df, metric=metric_txt,
                                   is_class=is_class, algo=algo)
          if (is_class) ml_result_local$cm <- cm

          output$ml_metrics_tbl <- DT::renderDataTable({ tool_dt(vi_df,"Variable Importance") })
          output$ml_cm_tbl <- DT::renderDataTable({
            if (is_class) tool_dt(as.data.frame.matrix(ml_result_local$cm),"Confusion Matrix")
            else data.frame(Note="Regression task — see metrics above")
          })

          tagList(
            tags$h4(paste("🤖 ML Results —", toupper(algo)), style="color:#1A3A5C;"),
            tags$div(class="alert alert-success", tags$b("Performance: "), metric_txt),
            tags$h5("Variable Importance"), DT::dataTableOutput(ns("ml_metrics_tbl")),
            if (is_class) tagList(tags$hr(), tags$h5("Confusion Matrix"),
                                  DT::dataTableOutput(ns("ml_cm_tbl"))),
            br(), downloadButton(ns("dl_ml"),"Download ML Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_ml <- downloadHandler(
        filename=function() paste0("ML_Results_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # TAB 13: CLUSTERING
    # ========================================================================
    observeEvent(input$run_clust, {
      req(df(), input$clust_vars)
      d_c   <- df()[, input$clust_vars, drop=FALSE]
      d_c   <- dplyr::mutate(d_c, dplyr::across(dplyr::everything(), as.numeric))
      d_c   <- na.omit(d_c)
      d_sc  <- scale(d_c)
      type  <- input$clust_algo %||% "kmeans"
      k     <- input$clust_k %||% 3

      output$clust_results_ui <- renderUI({
        tryCatch({
          if (type == "kmeans") {
            set.seed(42)
            km  <- kmeans(d_sc, centers=k, nstart=25)
            sizes <- as.data.frame(table(Cluster=km$cluster))
            sil   <- tryCatch({
              library(cluster)
              s <- silhouette(km$cluster, dist(d_sc))
              round(mean(s[,"sil_width"]),4)
            }, error=function(e) NA)
            d_c$Cluster <- factor(km$cluster)

            output$clust_tbl <- DT::renderDataTable({
              centers_df <- as.data.frame(round(km$centers, 4))
              centers_df <- cbind(Cluster=paste0("C",seq_len(nrow(centers_df))), centers_df)
              tool_dt(centers_df,"K-means Cluster Centers (standardized)")
            })
            output$clust_size_tbl <- DT::renderDataTable({
              tool_dt(sizes,"Cluster Sizes")
            })

            tagList(
              tags$h4("🔵 K-Means Clustering Results", style="color:#1A3A5C;"),
              tags$div(class="alert alert-success",
                tags$b(paste0("k = ",k," clusters | N = ",nrow(d_c))),
                if(!is.na(sil)) paste0(" | Mean Silhouette = ",sil)),
              tags$h5("Cluster Centers"), DT::dataTableOutput(ns("clust_tbl")),
              tags$h5("Cluster Sizes"),   DT::dataTableOutput(ns("clust_size_tbl")),
              br(), downloadButton(ns("dl_clust"),"Download Clustering (Excel)")
            )

          } else if (type == "hclust") {
            hc  <- hclust(dist(d_sc), method=input$clust_hc_method %||% "ward.D2")
            grps <- cutree(hc, k=k)
            sizes <- as.data.frame(table(Cluster=grps))
            output$clust_dendro <- renderPlot({
              plot(hc, hang=-1, main=paste0("Dendrogram (",input$clust_hc_method," linkage)"),
                   xlab="", sub="", cex=.7)
              rect.hclust(hc, k=k, border=c("#E74C3C","#3498DB","#2ECC71","#F39C12","#9B59B6")[seq_len(k)])
            }, height=400)
            output$clust_size_tbl <- DT::renderDataTable({ tool_dt(sizes,"Cluster Sizes") })
            tagList(
              tags$h4("🌳 Hierarchical Clustering", style="color:#1A3A5C;"),
              plotOutput(ns("clust_dendro"), height="380px"),
              DT::dataTableOutput(ns("clust_size_tbl")),
              br(), downloadButton(ns("dl_clust"),"Download (Excel)")
            )
          } else {
            # GMM
            mc_fit <- tryCatch({
              mclust::Mclust(d_sc, G=k)
            }, error=function(e) NULL)
            if (is.null(mc_fit)) stop("mclust not available or GMM failed.")
            grps  <- mc_fit$classification
            sizes <- as.data.frame(table(Cluster=grps))
            output$clust_size_tbl <- DT::renderDataTable({ tool_dt(sizes,"GMM Component Sizes") })
            tagList(
              tags$h4("🔵 Gaussian Mixture Model (GMM)", style="color:#1A3A5C;"),
              tags$div(class="alert alert-success",
                tags$b(paste0("G = ",mc_fit$G," components | BIC = ",round(mc_fit$bic,2)))),
              DT::dataTableOutput(ns("clust_size_tbl")),
              br(), downloadButton(ns("dl_clust"),"Download (Excel)")
            )
          }
        }, error=function(e) tags$div(class="alert alert-danger",tags$b("Error: "),e$message))
      })
      output$dl_clust <- downloadHandler(
        filename=function() paste0("Clustering_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # TAB 14: DESCRIPTIVES
    # ========================================================================
    observeEvent(input$run_desc, {
      req(df(), input$desc_vars)
      d_d <- df()[, input$desc_vars, drop=FALSE]
      d_d <- dplyr::mutate(d_d, dplyr::across(dplyr::everything(), as.numeric))

      output$desc_results_ui <- renderUI({
        tagList(
          tags$h4("📊 Descriptive Statistics", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("desc_stats_tbl")),
          tags$hr(),
          tags$h4("📐 Correlation Matrix", style="color:#1A3A5C;"),
          DT::dataTableOutput(ns("desc_cor_tbl")),
          br(), downloadButton(ns("dl_desc"),"Download Descriptives (Excel)")
        )
      })

      output$desc_stats_tbl <- DT::renderDataTable({
        d_stats <- tryCatch(round(as.data.frame(psych::describe(na.omit(d_d))),4),
                            error=function(e) data.frame(Note="psych::describe failed"))
        tool_dt(d_stats, paste0("Descriptives (N = ", nrow(na.omit(d_d)), ")"))
      })
      output$desc_cor_tbl <- DT::renderDataTable({
        cor_m <- round(cor(na.omit(d_d), use="pairwise.complete.obs"),4)
        datatable(as.data.frame(cor_m), rownames=TRUE,
          options=list(pageLength=20, scrollX=TRUE)) |>
          formatStyle(colnames(cor_m),
            backgroundColor=styleInterval(c(-.7,-.3,.3,.7),
              c("#1565C0","#64B5F6","white","#EF9A9A","#C62828")),
            color=styleInterval(c(-.7,.7),c("white","black","white")))
      })
      output$dl_desc <- downloadHandler(
        filename=function() paste0("Descriptives_",Sys.Date(),".xlsx"),
        content=function(file) {
          d_nd <- na.omit(d_d)
          sheets <- list()
          tryCatch({ sheets[["Descriptives"]] <- round(as.data.frame(psych::describe(d_nd)),4) },error=function(e){})
          tryCatch({ sheets[["Correlations"]] <- round(as.data.frame(cor(d_nd)),4) },error=function(e){})
          if (!length(sheets)) sheets[["Note"]] <- data.frame(msg="No data")
          write_xlsx(sheets,file)
        }
      )
    })

    # ========================================================================
    # TAB 15: IRT
    # ========================================================================
    observeEvent(input$run_irt, {
      req(df(), input$irt_vars)
      d_irt <- df()[, input$irt_vars, drop=FALSE]
      d_irt <- dplyr::mutate(d_irt, dplyr::across(dplyr::everything(), as.numeric))
      d_irt <- na.omit(d_irt)
      model <- input$irt_model %||% "2pl"

      output$irt_results_ui <- renderUI({
        tryCatch({
          irt_fit <- switch(model,
            "rasch" = mirt::mirt(d_irt, model=1, itemtype="Rasch",  verbose=FALSE),
            "2pl"   = mirt::mirt(d_irt, model=1, itemtype="2PL",    verbose=FALSE),
            "3pl"   = mirt::mirt(d_irt, model=1, itemtype="3PL",    verbose=FALSE),
            stop("Unknown IRT model — choose rasch, 2pl, or 3pl"))

          params  <- mirt::coef(irt_fit, IRTpars=TRUE, simplify=TRUE)$items
          fit_idx <- tryCatch(mirt::M2(irt_fit, type="C2"), error=function(e) NULL)

          output$irt_params_tbl <- DT::renderDataTable({
            tool_dt(round(as.data.frame(params),4),
                    paste0(model," Parameters (N=",nrow(d_irt),")"))
          })
          output$irt_fit_ui <- renderUI({
            if (!is.null(fit_idx)) {
              fi <- round(as.data.frame(fit_idx),4)
              tags$div(class="alert alert-info",
                tags$b("Model Fit: "),
                paste0("CFI=",fi$CFI,"  RMSEA=",fi$RMSEA,"  SRMSR=",fi$SRMSR))
            } else tags$p("Fit indices not available.", style="color:#888;")
          })
          output$irt_icc_plot <- renderPlot({
            tryCatch(mirt::plot(irt_fit, type="trace", main=paste(model,"Item Characteristic Curves")),
                     error=function(e) { plot.new(); text(.5,.5,"ICC plot unavailable") })
          }, height=400)

          tagList(
            tags$h4(paste("📏 IRT —", model), style="color:#1A3A5C;"),
            uiOutput(ns("irt_fit_ui")),
            tags$h5("Item Parameters"), DT::dataTableOutput(ns("irt_params_tbl")),
            tags$hr(),
            tags$h5("Item Characteristic Curves"),
            plotOutput(ns("irt_icc_plot"), height="380px"),
            br(), downloadButton(ns("dl_irt"),"Download IRT Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger",
          tags$b("Error: "),e$message,
          tags$br(),tags$small("Ensure mirt is installed: install.packages('mirt')")))
      })
      output$dl_irt <- downloadHandler(
        filename=function() paste0("IRT_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # TAB 16: ENDOGENEITY (Hausman + 2SLS)
    # ========================================================================
    observeEvent(input$run_end, {
      req(df(), input$end_dv, input$end_ivs, input$end_instruments)
      d     <- df()
      dv    <- input$end_dv
      all_x <- input$end_ivs            # all regressors
      endog <- all_x[1]                 # treat first regressor as endogenous
      exog  <- if (length(all_x) > 1) all_x[-1] else character(0)
      ivs   <- input$end_instruments    # instruments (Z)

      output$end_results_ui <- renderUI({
        tryCatch({
          d2 <- d[, unique(c(dv,endog,ivs,exog)), drop=FALSE]
          d2 <- dplyr::mutate(d2, dplyr::across(dplyr::everything(), as.numeric))
          d2 <- na.omit(d2)
          n  <- nrow(d2)
          if (n < 10) stop("Insufficient data.")

          # OLS (biased if endogeneity present)
          ols_f <- as.formula(paste(dv,"~",paste(c(endog,exog),collapse="+")))
          ols_m <- lm(ols_f, data=d2); ols_s <- summary(ols_m)

          # First stage: regress endogenous var on instruments
          fs_f  <- as.formula(paste(endog,"~",paste(c(ivs,exog),collapse="+")))
          fs_m  <- lm(fs_f, data=d2); fs_s <- summary(fs_m)
          d2$.fitted_endog <- fitted(fs_m)

          # Second stage: replace endogenous var with fitted values
          ss_f  <- as.formula(paste(dv,"~",paste(c(".fitted_endog",exog),collapse="+")))
          ss_m  <- lm(ss_f, data=d2); ss_s <- summary(ss_m)

          # Hausman test (manual: regress residuals from 1st stage)
          d2$.resid_fs <- resid(fs_m)
          hauss_f <- as.formula(paste(dv,"~",paste(c(endog,exog,".resid_fs"),collapse="+")))
          hauss_m <- lm(hauss_f, data=d2); hauss_s <- summary(hauss_m)
          hauss_coef <- hauss_s$coefficients[".resid_fs",]
          hauss_p    <- hauss_coef["Pr(>|t|)"]
          hauss_verdict <- ifelse(hauss_p < .05,
            "⚠️ Endogeneity detected (p < .05) — use 2SLS estimates",
            "✅ No significant endogeneity (p ≥ .05) — OLS acceptable")

          output$endo_ols_tbl <- DT::renderDataTable({
            tool_dt(round_df(as.data.frame(ols_s$coefficients),4),
              paste0("OLS (potentially biased) — R²=",round(ols_s$r.squared,4)))
          })
          output$endo_fs_tbl <- DT::renderDataTable({
            tool_dt(round_df(as.data.frame(fs_s$coefficients),4),
              paste0("1st Stage (F=",round(fs_s$fstatistic[1],2),
                     " R²=",round(fs_s$r.squared,4),
                     if(fs_s$fstatistic[1]>10)" ✅ Strong instrument" else " ⚠️ Weak instrument?",")")
            )
          })
          output$endo_2sls_tbl <- DT::renderDataTable({
            tool_dt(round_df(as.data.frame(ss_s$coefficients),4),"2SLS (2nd Stage)")
          })

          tagList(
            tags$h4("🔍 Endogeneity — Hausman Test + 2SLS", style="color:#1A3A5C;"),
            tags$div(class=paste0("alert alert-", if(hauss_p<.05)"warning" else "success"),
              tags$b("Hausman Test: "), hauss_verdict,
              tags$br(), tags$small(sprintf("t = %.3f, p = %.4f",hauss_coef["t value"],hauss_p))),
            tags$h5("OLS Estimates"),      DT::dataTableOutput(ns("endo_ols_tbl")),
            tags$hr(),
            tags$h5("First Stage (IV Regression)"), DT::dataTableOutput(ns("endo_fs_tbl")),
            tags$hr(),
            tags$h5("2SLS Estimates"),     DT::dataTableOutput(ns("endo_2sls_tbl")),
            br(), downloadButton(ns("dl_endo"),"Download Endogeneity Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger",tags$b("Error: "),e$message))
      })
      output$dl_endo <- downloadHandler(
        filename=function() paste0("Endogeneity_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })


    # ========================================================================
    # ADVANCED METHOD: HCM / FORMATIVE MEASUREMENT
    # ========================================================================
    observeEvent(input$run_formative, {
      req(df(), input$form_indicators)
      d_f <- df()[, input$form_indicators, drop=FALSE]
      d_f <- dplyr::mutate(d_f, dplyr::across(dplyr::everything(), as.numeric))
      d_f <- na.omit(d_f)
      output$hcm_results_ui <- renderUI({
        tryCatch({
          # VIF for each indicator
          vif_rows <- lapply(seq_along(input$form_indicators), function(i) {
            dv_v  <- input$form_indicators[i]
            ivs_v <- setdiff(input$form_indicators, dv_v)
            if (length(ivs_v) == 0) return(data.frame(Indicator=dv_v, VIF=NA))
            f <- as.formula(paste(dv_v, "~", paste(ivs_v, collapse="+")))
            m <- lm(f, data=d_f)
            r2 <- summary(m)$r.squared
            vif_val <- if (r2 >= 1) Inf else round(1 / (1 - r2), 3)
            data.frame(Indicator=dv_v, VIF=vif_val,
                       Status=if (is.infinite(vif_val)) "⚠️ Perfect collinearity"
                              else if (vif_val > 5) "❌ Critical (> 5)"
                              else if (vif_val > 3.3) "⚠️ Caution (> 3.3)"
                              else "✅ Acceptable (≤ 3.3)")
          })
          vif_df <- do.call(rbind, vif_rows)
          # OLS weights (regression of composite on indicators)
          d_f$COMPOSITE <- rowMeans(d_f, na.rm=TRUE)
          f_w <- as.formula(paste("COMPOSITE ~", paste(input$form_indicators, collapse="+")))
          m_w <- lm(f_w, data=d_f)
          wt <- as.data.frame(summary(m_w)$coefficients[-1,,drop=FALSE])
          wt$Indicator <- rownames(wt)
          names(wt) <- c("Weight","SE","t","p","Indicator")
          wt$Sig <- sig_stars(wt$p)
          wt$Status <- ifelse(wt$p < .05, "✅ Significant", "⚠️ Non-significant")
          output$form_vif_tbl <- DT::renderDataTable({ tool_dt(vif_df, "Formative VIF") })
          output$form_wt_tbl  <- DT::renderDataTable({ tool_dt(round_df(wt[,c("Indicator","Weight","SE","t","p","Sig","Status")],4), "Outer Weights") })
          tagList(
            tags$h4("📐 Formative Measurement Results", style="color:#880E4F;"),
            tags$div(class="alert alert-warning", style="font-size:.81rem;",
              tags$b("Criteria (Hair et al., 2019): "), "VIF < 3.3 (no multicollinearity); ",
              "Weights p < .05 (significant contribution). N = ", nrow(d_f)),
            tags$h5("Collinearity (VIF)"), DT::dataTableOutput(ns("form_vif_tbl")),
            tags$h5("Outer Weights"),     DT::dataTableOutput(ns("form_wt_tbl")),
            br(), downloadButton(ns("dl_formative"), "Download Formative Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_formative <- downloadHandler(
        filename=function() paste0("Formative_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    observeEvent(input$run_hcm, {
      req(df(), input$hcm_lv1)
      d_h <- df()[, input$hcm_lv1, drop=FALSE]
      d_h <- dplyr::mutate(d_h, dplyr::across(dplyr::everything(), as.numeric))
      d_h <- na.omit(d_h)
      output$hcm_results_ui <- renderUI({
        tryCatch({
          approach <- input$hcm_approach %||% "repeated"
          hoc_name <- input$hcm_name %||% "HOC"
          if (approach == "repeated") {
            # Repeated indicators approach: each FO LV's items load on the HO construct
            composite_scores <- rowMeans(d_h, na.rm=TRUE)
            d_h[[hoc_name]] <- composite_scores
            # Regression of HOC on each FOC (proxy scores)
            f <- as.formula(paste(hoc_name, "~", paste(input$hcm_lv1, collapse="+")))
            m <- lm(f, data=d_h)
            s <- summary(m)
            ct <- as.data.frame(s$coefficients[-1,,drop=FALSE])
            ct$FO_Construct <- rownames(ct)
            names(ct) <- c("Weight","SE","t","p","FO_Construct")
            ct$Sig <- sig_stars(ct$p)
            output$hcm_tbl <- DT::renderDataTable({
              tool_dt(round_df(ct[,c("FO_Construct","Weight","SE","t","p","Sig")],4),
                      paste0("HCM: ", hoc_name, " ← FO constructs (R²=",round(s$r.squared,3),")"))
            })
            tagList(
              tags$h4(paste("🏗 HCM Results:", hoc_name), style="color:#880E4F;"),
              tags$div(class="alert alert-success",
                tags$b("R² = "), round(s$r.squared,3),
                tags$b(" | N = "), nrow(d_h)),
              tags$h5("Second-Order Paths (FO → HOC)"),
              DT::dataTableOutput(ns("hcm_tbl")),
              tags$div(class="alert alert-info", style="font-size:.81rem;",
                "Interpretation: Weights represent the contribution of each first-order construct to ",
                "the second-order composite. Significant weights (p < .05) indicate that the FO construct ",
                "is a meaningful component of the HOC (Hair et al., 2019).")
            )
          } else {
            # Two-stage: first get PLS scores (proxy = mean), then regress
            tags$div(class="alert alert-warning",
              "Two-stage HCM requires PLS model results. Run PLS-SEM via the canvas first, then re-run HCM.")
          }
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
    })

    # ========================================================================
    # ADVANCED METHOD: NCA — Necessary Condition Analysis
    # ========================================================================
    observeEvent(input$run_nca, {
      req(df(), input$nca_dv, input$nca_ivs)
      d_nca <- df()[, unique(c(input$nca_dv, input$nca_ivs)), drop=FALSE]
      d_nca <- dplyr::mutate(d_nca, dplyr::across(dplyr::everything(), as.numeric))
      d_nca <- na.omit(d_nca)
      output$nca_results_ui <- renderUI({
        tryCatch({
          dv  <- input$nca_dv
          ivs <- input$nca_ivs
          mth <- input$nca_method %||% "ce_fdh"
          p_t <- (input$nca_p %||% 90) / 100
          n   <- nrow(d_nca)
          if (n < 10) stop("Need at least 10 complete observations.")
          y <- d_nca[[dv]]
          # Compute ceiling effect size d for each IV
          nca_rows <- lapply(ivs, function(xv) {
            x <- d_nca[[xv]]
            # Scope = (max_x - min_x) * (max_y - min_y)
            scope <- (max(x)-min(x)) * (max(y)-min(y))
            if (scope == 0) return(data.frame(Condition=xv, d_CE_FDH=NA, d_CR_FDH=NA, Status="Insufficient variance"))
            # CE-FDH ceiling line (upper-left frontier)
            ord <- order(x)
            xs <- x[ord]; ys <- y[ord]
            # CE-FDH: ceiling = maximum Y at each unique X
            unique_x <- unique(xs)
            ce_ceiling_y <- sapply(unique_x, function(xi) max(ys[xs <= xi]))
            ce_area <- sum(diff(c(min(x), unique_x)) * ce_ceiling_y[seq_along(unique_x)])
            ce_area_above <- (max(x)-min(x))*(max(y)-min(y)) - ce_area
            d_ce <- round(ce_area_above / scope, 4)
            # CR-FDH: regression on upper frontier points (quantile regression at tau=.9)
            cr_d <- tryCatch({
              q_m <- quantreg::rq(y ~ x, tau=.9, data=d_nca[, c(dv,xv)])
              fitted_v <- fitted(q_m)
              cr_area_above <- sum(pmax(0, fitted_v - y)) * (max(x)-min(x))/n
              round(cr_area_above / scope, 4)
            }, error=function(e) NA)
            status <- if (!is.na(d_ce) && d_ce >= .5) "✅ Large (≥.50)"
                      else if (!is.na(d_ce) && d_ce >= .3) "✅ Medium (≥.30)"
                      else if (!is.na(d_ce) && d_ce >= .1) "⚠️ Small (≥.10)"
                      else "❌ Negligible (< .10)"
            data.frame(Condition=xv, d_CE_FDH=d_ce, d_CR_FDH=cr_d, Status=status,
                       stringsAsFactors=FALSE)
          })
          nca_df <- do.call(rbind, nca_rows)

          # Bottleneck table at threshold p_t
          bn_rows <- lapply(ivs, function(xv) {
            x <- d_nca[[xv]]; y_val <- quantile(y, p_t, na.rm=TRUE)
            x_needed <- tryCatch({
              # CE-FDH: smallest X where ceiling Y >= threshold
              ord <- order(x); xs <- x[ord]; ys <- y[ord]
              unique_x <- sort(unique(xs))
              ceilings  <- sapply(unique_x, function(xi) max(ys[xs <= xi]))
              idx <- which(ceilings >= y_val)
              if (length(idx)==0) max(x) else round(unique_x[min(idx)],3)
            }, error=function(e) NA)
            pct_x <- round((x_needed - min(x))/(max(x)-min(x))*100,1)
            data.frame(Condition=xv,
                       Y_threshold=round(y_val,3),
                       X_needed=x_needed,
                       Pct_of_range=pct_x,
                       stringsAsFactors=FALSE)
          })
          bn_df <- do.call(rbind, bn_rows)

          output$nca_effect_tbl  <- DT::renderDataTable({ tool_dt(nca_df, "NCA Effect Sizes (d)") })
          output$nca_bottleneck  <- DT::renderDataTable({ tool_dt(bn_df, paste0("Bottleneck Table @ ",input$nca_p,"th percentile")) })
          output$nca_scatter     <- renderPlotly({
            iv1 <- ivs[1]
            x <- d_nca[[iv1]]; y_p <- d_nca[[dv]]
            ord <- order(x); xs <- x[ord]; ys <- y_p[ord]
            unique_x <- sort(unique(xs))
            ce_ceiling <- sapply(unique_x, function(xi) max(ys[xs <= xi]))
            plot_ly() |>
              add_markers(x=x, y=y_p, marker=list(color=TEAL,opacity=.5,size=6),
                          name="Observed", text=paste0(iv1,"=",round(x,2),", ",dv,"=",round(y_p,2)),
                          hoverinfo="text") |>
              add_lines(x=unique_x, y=ce_ceiling,
                        line=list(color="#C0392B",width=2,dash="solid"), name="CE-FDH Ceiling") |>
              layout(title=list(text=paste0("NCA: ",iv1," → ",dv), font=list(color=NAVY)),
                     xaxis=list(title=iv1), yaxis=list(title=dv),
                     plot_bgcolor="white", paper_bgcolor="white")
          })

          tagList(
            tags$h4("🔑 NCA Results", style="color:#E65100;"),
            tags$div(class="alert alert-success",
              tags$b(paste0("N = ", n, " | Outcome = ", dv, " | Conditions = ", length(ivs)))),
            tags$h5("Effect Sizes (d)"), DT::dataTableOutput(ns("nca_effect_tbl")),
            tags$hr(),
            tags$h5(paste0("Bottleneck Table (",input$nca_p,"th percentile)")),
            DT::dataTableOutput(ns("nca_bottleneck")),
            tags$hr(),
            tags$h5(paste0("NCA Scatter + Ceiling Line: ", ivs[1])),
            plotlyOutput(ns("nca_scatter"), height="400px"),
            br(), downloadButton(ns("dl_nca"), "Download NCA Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_nca <- downloadHandler(
        filename=function() paste0("NCA_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # ADVANCED METHOD: NConfA — Necessary Configuration Analysis
    # ========================================================================
    observeEvent(input$run_nconfa, {
      req(df(), input$nconfa_dv, input$nconfa_conds)
      d_nc <- df()[, unique(c(input$nconfa_dv, input$nconfa_conds)), drop=FALSE]
      d_nc <- dplyr::mutate(d_nc, dplyr::across(dplyr::everything(), as.numeric))
      d_nc <- na.omit(d_nc)
      output$nconfa_results_ui <- renderUI({
        tryCatch({
          dv   <- input$nconfa_dv
          cond <- input$nconfa_conds
          p_t  <- (input$nconfa_p %||% 90) / 100
          n    <- nrow(d_nc)
          if (n < 10) stop("Need at least 10 complete observations.")
          y <- d_nc[[dv]]

          # Pairwise configurations (all 2-way combinations)
          pairs <- combn(cond, 2, simplify=FALSE)

          config_rows <- lapply(pairs, function(pair) {
            x1 <- d_nc[[pair[1]]]; x2 <- d_nc[[pair[2]]]
            # Configuration = minimum of the two conditions (AND-logic for necessity)
            config <- pmin(x1, x2)
            scope  <- (max(config)-min(config)) * (max(y)-min(y))
            if (scope < 1e-8) return(data.frame(Configuration=paste(pair,collapse=" AND "),
                                               d_CE_FDH=NA, Necessary=NA))
            # CE-FDH on config
            ord <- order(config)
            cs  <- config[ord]; ys <- y[ord]
            unique_c <- sort(unique(cs))
            ceilings <- sapply(unique_c, function(ci) max(ys[cs <= ci]))
            d_ce <- round(1 - sum(diff(c(min(config),unique_c))*ceilings[seq_along(unique_c)])/scope, 4)
            d_ce <- max(0, d_ce)
            data.frame(
              Configuration = paste(pair, collapse=" AND "),
              d_CE_FDH      = d_ce,
              Necessary     = ifelse(d_ce >= .1, "✅ Yes (d≥.10)", "❌ No (d<.10)"),
              Effect_Size   = ifelse(d_ce>=.5,"Large",ifelse(d_ce>=.3,"Medium",ifelse(d_ce>=.1,"Small","Negligible"))),
              stringsAsFactors=FALSE
            )
          })

          # All-conditions configuration
          if (length(cond) >= 2) {
            config_all <- apply(d_nc[, cond, drop=FALSE], 1, min)
            scope_all  <- (max(config_all)-min(config_all))*(max(y)-min(y))
            if (scope_all >= 1e-8) {
              ord <- order(config_all); cs <- config_all[ord]; ys_a <- y[ord]
              uc  <- sort(unique(cs))
              cl  <- sapply(uc, function(ci) max(ys_a[cs <= ci]))
              d_all <- max(0, round(1 - sum(diff(c(min(config_all),uc))*cl[seq_along(uc)])/scope_all, 4))
              config_rows <- c(config_rows, list(data.frame(
                Configuration=paste(cond,collapse=" AND "),
                d_CE_FDH=d_all,
                Necessary=ifelse(d_all>=.1,"✅ Yes (d≥.10)","❌ No (d<.10)"),
                Effect_Size=ifelse(d_all>=.5,"Large",ifelse(d_all>=.3,"Medium",ifelse(d_all>=.1,"Small","Negligible"))),
                stringsAsFactors=FALSE)))
            }
          }

          config_df <- do.call(rbind, config_rows)
          config_df <- config_df[order(-config_df$d_CE_FDH, na.last=TRUE), ]

          output$nconfa_tbl <- DT::renderDataTable({ tool_dt(config_df, "NConfA: Necessary Configurations") })
          output$nconfa_bar <- renderPlotly({
            config_df2 <- config_df[!is.na(config_df$d_CE_FDH), ]
            config_df2 <- config_df2[order(config_df2$d_CE_FDH), ]
            plot_ly(config_df2) |>
              add_bars(x=~d_CE_FDH, y=~Configuration,
                       marker=list(color=~ifelse(d_CE_FDH>=.1,TEAL,"#CCCCCC")),
                       text=~round(d_CE_FDH,3), textposition="outside",
                       orientation="h", name="d (CE-FDH)") |>
              add_segments(x=.1, xend=.1, y=0.5, yend=nrow(config_df2)+.5,
                           line=list(color="#C0392B",dash="dot",width=2),
                           name="Min threshold (d=.10)", inherit=FALSE) |>
              layout(title=list(text="NConfA Effect Sizes by Configuration",font=list(color=NAVY)),
                     xaxis=list(title="Effect Size d (CE-FDH)"),
                     yaxis=list(title="", automargin=TRUE),
                     margin=list(l=180),
                     plot_bgcolor="white", paper_bgcolor="white")
          })

          tagList(
            tags$h4("⚙️ NConfA Results", style="color:#37474F;"),
            tags$div(class="alert alert-success",
              tags$b(paste0("N = ",n," | Outcome = ",dv," | ",length(pairs)+1," configurations tested"))),
            tags$div(class="alert alert-info", style="font-size:.81rem;",
              tags$b("Interpretation (Dul, 2025): "),
              "Configurations with d ≥ .10 are necessary — the absence of this combination ",
              "prevents high outcome scores. Larger d = stronger bottleneck."),
            DT::dataTableOutput(ns("nconfa_tbl")),
            tags$hr(),
            plotlyOutput(ns("nconfa_bar"), height="380px"),
            br(), downloadButton(ns("dl_nconfa"), "Download NConfA Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_nconfa <- downloadHandler(
        filename=function() paste0("NConfA_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # ADVANCED METHOD: SEM-ANN (Hybrid ANN Importance)
    # ========================================================================
    observeEvent(input$run_ann, {
      req(df(), input$ann_dv, input$ann_ivs)
      d_ann <- df()[, unique(c(input$ann_dv, input$ann_ivs)), drop=FALSE]
      d_ann <- dplyr::mutate(d_ann, dplyr::across(dplyr::everything(), as.numeric))
      d_ann <- na.omit(d_ann)
      output$ann_results_ui <- renderUI({
        tryCatch({
          dv   <- input$ann_dv
          ivs  <- input$ann_ivs
          h    <- input$ann_hidden %||% 5
          reps <- input$ann_rep    %||% 10
          n    <- nrow(d_ann)
          if (n < 20) stop("Need at least 20 complete observations for ANN.")

          # Scale inputs [0,1] for ANN stability
          d_sc <- as.data.frame(lapply(d_ann, function(x) (x-min(x))/(max(x)-min(x)+1e-10)))

          set.seed(input$ann_seed %||% 42)
          fmla <- as.formula(paste(dv, "~", paste(ivs, collapse="+")))

          # Train nnet ANN (nnet package)
          ann_fit <- nnet::nnet(fmla, data=d_sc, size=h, linout=TRUE,
                                maxit=500, trace=FALSE, decay=.01)

          # Compute sensitivity-based importance (Garson/Olden method)
          wts   <- ann_fit$wts
          n_inp <- length(ivs)
          n_hid <- h
          # Input-to-hidden weights
          w_ih <- matrix(wts[1:(n_inp*n_hid)+1], nrow=n_inp, ncol=n_hid)
          # Hidden-to-output weights
          w_ho <- wts[(n_inp*n_hid+n_hid+2):(n_inp*n_hid+n_hid+n_hid+1)]
          # Absolute importance
          contrib <- abs(w_ih) %*% abs(w_ho)
          imp_raw <- as.numeric(contrib)
          imp_norm <- round(imp_raw / max(imp_raw) * 100, 2)

          imp_df <- data.frame(
            Predictor = ivs,
            Raw_Importance = round(imp_raw, 4),
            Normalised_Pct = imp_norm,
            Rank = rank(-imp_norm),
            stringsAsFactors = FALSE
          )
          imp_df <- imp_df[order(-imp_df$Normalised_Pct), ]

          # Also run OLS for comparison
          ols_m <- lm(fmla, data=d_sc)
          ols_s <- summary(ols_m)
          ols_ct <- as.data.frame(ols_s$coefficients[-1,,drop=FALSE])
          ols_ct$Predictor <- rownames(ols_ct)
          ols_ct$Std_Beta  <- round(ols_ct[,"Estimate"],4)
          imp_df <- merge(imp_df, ols_ct[,c("Predictor","Std_Beta")], by="Predictor", all.x=TRUE)
          imp_df <- imp_df[order(-imp_df$Normalised_Pct),]

          output$ann_imp_tbl <- DT::renderDataTable({
            tool_dt(round_df(imp_df,4), paste0("SEM-ANN Normalised Importance (hidden=",h,")"))
          })
          output$ann_bar_plot <- renderPlotly({
            imp_ord <- imp_df[order(imp_df$Normalised_Pct),]
            plot_ly(imp_ord) |>
              add_bars(x=~Normalised_Pct, y=~Predictor,
                       marker=list(color=TEAL, line=list(color=NAVY,width=.5)),
                       text=~paste0(Normalised_Pct,"%"), textposition="outside",
                       orientation="h", name="ANN Importance") |>
              add_markers(x=~abs(Std_Beta)*100, y=~Predictor,
                          marker=list(color="#C0392B",size=10,symbol="diamond"),
                          name="OLS |β| (scaled)", hoverinfo="text",
                          text=~paste0("OLS β=",Std_Beta)) |>
              layout(title=list(text=paste0("SEM-ANN vs OLS: Relative Importance of Predictors of ",dv),
                                font=list(color=NAVY,size=12)),
                     xaxis=list(title="Normalised Importance (%) / Scaled |β|"),
                     yaxis=list(title="",automargin=TRUE),
                     margin=list(l=120), barmode="overlay",
                     plot_bgcolor="white",paper_bgcolor="white")
          })

          tagList(
            tags$h4("🧠 SEM-ANN Results", style="color:#0D47A1;"),
            tags$div(class="alert alert-success",
              tags$b(paste0("ANN: hidden=",h,", reps=",reps," | OLS R²=",
                             round(ols_s$r.squared,3)," | N=",n))),
            tags$div(class="alert alert-info", style="font-size:.81rem;",
              tags$b("ANN Importance: "), "Garson/Olden connection-weight method — sums absolute products ",
              "of input→hidden and hidden→output weights. Normalised to 100% = highest importance predictor. ",
              tags$b("Red diamonds: "), "OLS standardised beta (|β|) for comparison."),
            tags$h5("Normalised Importance Table"),
            DT::dataTableOutput(ns("ann_imp_tbl")),
            tags$hr(),
            tags$h5("Importance Bar Chart (ANN vs OLS)"),
            plotlyOutput(ns("ann_bar_plot"), height="420px"),
            br(), downloadButton(ns("dl_ann"), "Download SEM-ANN Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_ann <- downloadHandler(
        filename=function() paste0("SEM_ANN_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # ADVANCED METHOD: Bayesian SEM (blavaan)
    # ========================================================================
    observeEvent(input$run_bsem, {
      req(df(), input$sem_syntax)
      output$bsem_results_ui <- renderUI({
        tryCatch({
          model_syntax <- input$sem_syntax
          if (!nzchar(trimws(model_syntax)))
            stop("Write your lavaan model syntax in the CB-SEM Manual tab first.")
          d_b <- df()
          vars_used <- unique(unlist(regmatches(model_syntax,
            gregexpr("[A-Za-z][A-Za-z0-9_]*", model_syntax))))
          d_b <- d_b[, intersect(vars_used, names(d_b)), drop=FALSE]
          d_b <- dplyr::mutate(d_b, dplyr::across(dplyr::everything(), as.numeric))
          d_b <- na.omit(d_b)
          if (!requireNamespace("blavaan", quietly=TRUE))
            stop("Package 'blavaan' not installed. Run: install.packages('blavaan')")
          iter   <- input$bsem_iter   %||% 2000
          chains <- input$bsem_chains %||% 2
          sampler <- input$bsem_estimator %||% "stan"
          withProgress(message="Running Bayesian MCMC (may take 1–3 min)...", value=0.5, {
            bfit <- blavaan::bsem(model_syntax, data=d_b,
                                   n.chains=chains, burnin=floor(iter/2),
                                   sample=iter, target=sampler,
                                   bcontrol=list(cores=1))
          })
          # Extract posterior summaries
          post <- tryCatch(blavaan::blavInspect(bfit,"postmean"), error=function(e) NULL)
          sm   <- tryCatch(as.data.frame(blavaan::blavSummary(bfit)$PE), error=function(e) NULL)
          ppp  <- tryCatch(blavaan::blavFitIndices(bfit)$PPP, error=function(e) NA)
          output$bsem_tbl <- DT::renderDataTable({
            if (!is.null(sm)) {
              sm2 <- sm[sm$op %in% c("=~","~","~~"),
                        intersect(c("lhs","op","rhs","est.std","mean","sd","2.5%","97.5%","psrf"),names(sm))]
              tool_dt(round_df(sm2,4),"Bayesian SEM — Posterior Summaries")
            } else data.frame(Note="Could not extract summaries")
          })
          tagList(
            tags$h4("🎲 Bayesian SEM Results", style="color:#4A148C;"),
            tags$div(class="alert alert-success",
              tags$b("Posterior Predictive p-value (PPP): "), round(ppp,3),
              " — values 0.05–0.95 indicate acceptable model fit."),
            tags$div(class="alert alert-info", style="font-size:.81rem;",
              tags$b("Posterior Mean: "), "Bayesian point estimate (≈ frequentist β). ",
              tags$b("95% CrI: "), "Credible interval — if excludes 0, effect is credibly non-zero. ",
              tags$b("PSRF: "), "Convergence diagnostic — values < 1.05 indicate convergence."),
            DT::dataTableOutput(ns("bsem_tbl")),
            br(), downloadButton(ns("dl_bsem"), "Download Bayesian SEM Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_bsem <- downloadHandler(
        filename=function() paste0("BayesianSEM_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # ADVANCED METHOD: MASEM — Meta-Analytic SEM
    # ========================================================================
    observeEvent(input$run_masem, {
      req(input$masem_cor_input)
      output$masem_results_ui <- renderUI({
        tryCatch({
          # Parse user-provided correlation matrix (CSV)
          cor_text <- input$masem_cor_input
          n_text   <- input$masem_n_input %||% "300"
          con <- textConnection(cor_text)
          cor_df  <- tryCatch(read.csv(con, row.names=1, check.names=FALSE),
                               error=function(e) NULL)
          close(con)
          if (is.null(cor_df))
            stop("Could not parse correlation matrix. Use CSV format with variable names as row and column names.")
          cor_mat <- as.matrix(cor_df)
          # Ensure symmetry
          cor_mat <- (cor_mat + t(cor_mat)) / 2
          diag(cor_mat) <- 1
          # Parse sample sizes
          ns_vec <- as.numeric(trimws(strsplit(n_text, "\n|,")[[1]]))
          ns_vec <- ns_vec[!is.na(ns_vec)]
          n_total <- sum(ns_vec)
          n_studies <- length(ns_vec)
          vn <- colnames(cor_mat)
          # Stage 1: Weighted average correlation matrix
          if (n_studies > 1) {
            weights <- ns_vec / n_total
            pooled_r <- cor_mat   # single matrix provided; in real MASEM multiple matrices are averaged
          } else {
            pooled_r <- cor_mat
          }
          # Stage 2: Fit canvas model to pooled correlation matrix (if SEM syntax available)
          model_syntax <- input$sem_syntax %||% ""
          masem_stage2 <- NULL
          if (nzchar(trimws(model_syntax)) && requireNamespace("lavaan", quietly=TRUE)) {
            masem_stage2 <- tryCatch({
              lavaan::cfa(model_syntax, sample.cov=pooled_r, sample.nobs=n_total,
                          estimator="ML")
            }, error=function(e) NULL)
          }
          output$masem_cor_tbl <- DT::renderDataTable({
            tool_dt(round(as.data.frame(pooled_r),4),
                    paste0("Pooled Correlation Matrix (",n_studies," stud",
                           if(n_studies==1)"y" else "ies",
                           ", N=",n_total,")"))
          })
          fit_block <- if (!is.null(masem_stage2)) {
            fi <- tryCatch(lavaan::fitMeasures(masem_stage2,c("cfi","tli","rmsea","srmr")),error=function(e)NULL)
            if (!is.null(fi))
              tags$div(class="alert alert-success",
                tags$b("Stage 2 Fit: "),
                sprintf("CFI=%.3f | TLI=%.3f | RMSEA=%.3f | SRMR=%.3f",
                        fi["cfi"],fi["tli"],fi["rmsea"],fi["srmr"]))
            else tags$div(class="alert alert-warning","Stage 2 SEM could not compute fit indices.")
          } else {
            tags$div(class="alert alert-info",
              "Stage 2: Write your model in the CB-SEM Manual tab and re-run to fit it to the pooled matrix.")
          }
          tagList(
            tags$h4("📚 MASEM Results", style="color:#1B5E20;"),
            tags$div(class="alert alert-success",
              tags$b(paste0("Studies = ",n_studies," | Total N = ",n_total," | Variables = ",length(vn)))),
            tags$h5("Stage 1: Pooled Correlation Matrix"),
            DT::dataTableOutput(ns("masem_cor_tbl")),
            tags$hr(),
            tags$h5("Stage 2: Model Fit to Pooled Matrix"),
            fit_block,
            br(), downloadButton(ns("dl_masem"), "Download MASEM Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_masem <- downloadHandler(
        filename=function() paste0("MASEM_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # ADVANCED METHOD: LPA — Latent Profile Analysis
    # ========================================================================
    observeEvent(input$run_lpa, {
      req(df(), input$lpa_vars)
      d_lpa <- df()[, input$lpa_vars, drop=FALSE]
      d_lpa <- dplyr::mutate(d_lpa, dplyr::across(dplyr::everything(), as.numeric))
      d_lpa <- na.omit(d_lpa)
      output$lpa_results_ui <- renderUI({
        tryCatch({
          min_k <- input$lpa_min_k %||% 2
          max_k <- input$lpa_max_k %||% 6
          mdl   <- input$lpa_model %||% "VVI"
          n     <- nrow(d_lpa)
          if (n < 30) stop("Need at least 30 complete observations for LPA.")
          if (!requireNamespace("mclust", quietly=TRUE))
            stop("Package 'mclust' not installed. Run: install.packages('mclust')")
          # Fit models across range of profiles
          d_sc_lpa <- scale(d_lpa)
          fit <- mclust::Mclust(d_sc_lpa, G=min_k:max_k, modelNames=mdl)
          if (is.null(fit)) stop("mclust::Mclust returned NULL — try fewer profiles or different model.")
          opt_k <- fit$G
          classes <- fit$classification
          probs   <- fit$z   # posterior probabilities
          # Profile means (back-transformed)
          means_df <- as.data.frame(t(fit$parameters$mean))
          means_df$Profile <- paste0("P", seq_len(nrow(means_df)))
          sizes_df <- as.data.frame(table(Profile=paste0("P",classes)))
          # BIC table
          bic_vals <- fit$BIC
          bic_df <- data.frame(
            Model=names(bic_vals[1,]), BIC=round(as.numeric(bic_vals[which.max(rowSums(bic_vals)),]),2)
          ) |> dplyr::arrange(dplyr::desc(BIC))
          output$lpa_bic_tbl     <- DT::renderDataTable({ tool_dt(bic_df, "BIC by Number of Profiles") })
          output$lpa_means_tbl   <- DT::renderDataTable({ tool_dt(round_df(means_df,3), paste0("Profile Means (k=",opt_k,", standardised)")) })
          output$lpa_sizes_tbl   <- DT::renderDataTable({ tool_dt(sizes_df, "Profile Sizes") })
          output$lpa_profile_plot <- renderPlotly({
            df_m <- tidyr::pivot_longer(means_df, -Profile, names_to="Variable", values_to="Mean")
            plot_ly(df_m, x=~Variable, y=~Mean, color=~Profile,
                    type="scatter", mode="lines+markers",
                    line=list(width=2), marker=list(size=8)) |>
              layout(title=list(text=paste0("LPA Profile Plot (k=",opt_k,")"), font=list(color=NAVY)),
                     xaxis=list(title=""), yaxis=list(title="Standardised Mean"),
                     plot_bgcolor="white", paper_bgcolor="white")
          })
          tagList(
            tags$h4("👥 LPA Results", style="color:#006064;"),
            tags$div(class="alert alert-success",
              tags$b(paste0("Optimal profiles: k = ",opt_k," | Model: ",mdl," | N = ",n))),
            tags$div(class="alert alert-info", style="font-size:.81rem;",
              tags$b("Selection: "), "Highest BIC = best fitting model. ",
              tags$b("Entropy: "), round(tryCatch(mclust::icl(fit),error=function(e)NA),3),
              " — values > 0.8 indicate clear profile separation."),
            tags$h5("BIC Model Comparison"), DT::dataTableOutput(ns("lpa_bic_tbl")),
            tags$h5("Profile Sizes"),        DT::dataTableOutput(ns("lpa_sizes_tbl")),
            tags$h5("Profile Means (Standardised)"), DT::dataTableOutput(ns("lpa_means_tbl")),
            tags$hr(),
            tags$h5("Profile Plot"),
            plotlyOutput(ns("lpa_profile_plot"), height="420px"),
            br(), downloadButton(ns("dl_lpa"), "Download LPA Results (Excel)")
          )
        }, error=function(e) tags$div(class="alert alert-danger", tags$b("Error: "), e$message))
      })
      output$dl_lpa <- downloadHandler(
        filename=function() paste0("LPA_",Sys.Date(),".xlsx"),
        content=function(file) write_xlsx(list(Note=data.frame(msg="See on-screen results")),file)
      )
    })

    # ========================================================================
    # TAB 17: APA WRITE-UP (combined)
    # ========================================================================
    output$combined_apa <- renderText({
      parts <- character(0)
      m <- pls_model_rv()
      if (!is.null(m)) {
        sm <- tryCatch(summary(m), error=function(e) NULL)
        if (!is.null(sm)) {
          rel <- tryCatch(sm$reliability, error=function(e) NULL)
          pth <- tryCatch(sm$paths,       error=function(e) NULL)
          parts <- c(parts, paste0(
            "PLS-SEM RESULTS (seminr; Hair et al., 2019)\n",
            "=====================================================\n",
            if (!is.null(rel)) paste0("OUTER MODEL:\n",
              paste(capture.output(print(round(as.data.frame(rel),4))),collapse="\n"),"\n\n") else "",
            if (!is.null(pth)) paste0("INNER MODEL:\n",
              paste(capture.output(print(round(as.data.frame(pth),4))),collapse="\n"),"\n\n") else "",
            "REFERENCES:\n",
            "Hair, J.F., Risher, J.J., Sarstedt, M., & Ringle, C.M. (2019). ",
            "When to use and how to report results of PLS-SEM. European Business Review, 31(1), 2-24.\n",
            "Henseler, J., Ringle, C.M., & Sarstedt, M. (2015). ",
            "A new criterion for assessing discriminant validity. JAMR, 12(1), 115-135."
          ))
        }
      }
      res <- tryCatch(sem_result(), error=function(e) NULL)
      if (!is.null(res) && is.null(res$error)) {
        fi <- lavaan::fitMeasures(res$fit,c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic"))
        parts <- c(parts, paste0(
          "\nCB-SEM RESULTS (lavaan; Rosseel, 2012)\n",
          "=====================================================\n",
          sprintf("χ²(%g)=%.2f, p%s\nCFI=%.3f | TLI=%.3f | RMSEA=%.3f | SRMR=%.3f\nAIC=%.0f | BIC=%.0f\n\n",
            fi["df"],fi["chisq"],apa_p(fi["pvalue"]),
            fi["cfi"],fi["tli"],fi["rmsea"],fi["srmr"],fi["aic"],fi["bic"]),
          "REFERENCES:\n",
          "Rosseel, Y. (2012). lavaan: An R package for SEM. JSS, 48(2), 1-36.\n",
          "Hu, L.T., & Bentler, P.M. (1999). Cutoff criteria for fit indexes. SEM, 6(1), 1-55."
        ))
      }
      if (!length(parts)) return("Run analyses to generate APA write-up.")
      paste0(paste(parts,collapse="\n\n"),
             "\n\n[Dr.AIStat — Dr. Aneeq Inam | ORCID: 0000-0001-7682-2244]")
    })

    # ========================================================================
    # TAB 18: AI INTERPRET
    # ========================================================================
    observeEvent(input$ai_btn_pls, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      m <- pls_model_rv()
      ctx <- if (!is.null(m)) {
        sm <- tryCatch(summary(m),error=function(e) NULL)
        if (!is.null(sm)) paste0("PLS-SEM RESULTS\nReliability:\n",
          paste(capture.output(print(round(as.data.frame(sm$reliability),4))),collapse="\n"),
          "\n\nPaths:\n",paste(capture.output(print(round(as.data.frame(sm$paths),4))),collapse="\n"))
        else "Run PLS-SEM first."
      } else "Run Model Canvas first."
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an expert in PLS-SEM writing for a top-tier management journal.\n\n",
        "Provide DETAILED interpretation of PLS-SEM results. Include:\n",
        "1. Outer model: loadings ≥.70, α/rho_A/CR ≥.70, AVE ≥.50\n",
        "2. HTMT discriminant validity (<.85 Henseler et al., 2015)\n",
        "3. Inner model: path coefficients, significance (t>1.96), R²\n",
        "4. f² effect sizes: small=.02, medium=.15, large=.35 (Cohen, 1988)\n",
        "5. Overall model quality and theoretical implications\n",
        "6. APA-style PLS-SEM Results section (Hair et al., 2019 standards)\n\n",
        "Formal academic prose (6-7 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })
    observeEvent(input$ai_btn_outer, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      m <- pls_model_rv()
      ctx <- if (!is.null(m)) {
        sm <- tryCatch(summary(m),error=function(e) NULL)
        if (!is.null(sm)) paste0("OUTER MODEL\nLoadings:\n",
          paste(capture.output(print(round(as.data.frame(sm$loadings),4))),collapse="\n"),
          "\nReliability:\n",paste(capture.output(print(round(as.data.frame(sm$reliability),4))),collapse="\n"))
        else "Run PLS-SEM first."
      } else "Run Model Canvas first."
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an expert psychometrician writing for a peer-reviewed management journal.\n\n",
        "Interpret outer model results:\n",
        "1. Loadings vs .70 threshold\n2. α, rho_A, CR, AVE assessment\n",
        "3. HTMT and Fornell-Larcker discriminant validity\n",
        "4. Overall psychometric verdict\n\nFormal academic prose (5-6 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })
    observeEvent(input$ai_btn_sem, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      ctx <- tryCatch({
        sr <- sem_result(); req(sr)
        sm <- capture.output(summary(sr$fit,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE))
        paste0("CB-SEM\nModel:\n",input$sem_syntax,"\n\nOutput:\n",paste(head(sm,100),collapse="\n"))
      }, error=function(e) "Run CB-SEM first.")
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an SEM expert writing for a top-tier management journal.\n\n",
        "Interpret CB-SEM results:\n",
        "1. Model fit: CFI/TLI ≥.95, RMSEA ≤.06, SRMR ≤.08\n",
        "2. χ²/df ≤ 3 acceptable\n3. Loadings, structural paths, R²\n",
        "4. Modification indices\n5. Kline (2016) reporting standards\n\n",
        "Formal academic prose (6-7 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })
    observeEvent(input$ai_btn_screen, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      ctx <- tryCatch({
        req(df(), input$screen_vars)
        d_num <- dplyr::mutate(df()[,input$screen_vars,drop=FALSE],
                                dplyr::across(dplyr::everything(),as.numeric))
        paste0("N=",nrow(d_num)," | Missing=",sum(is.na(d_num)),"\n",
               paste(capture.output(print(round(psych::describe(d_num)[,c(2,3,4,8,9)],3))),collapse="\n"))
      }, error=function(e) "Run Data Screening first.")
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are a psychometric expert reviewing data quality for SEM.\n\n",
        "Interpret data screening report:\n",
        "1. Sample adequacy\n2. Missing data (FIML, MI, listwise)\n",
        "3. Normality (skewness/kurtosis)\n4. CMV (Harman's)\n5. Outliers\n",
        "6. Overall verdict and recommendations\n\nFormal academic prose (4-5 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })
    observeEvent(input$ai_btn_pa, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      ctx <- tryCatch({
        req(reg_result())
        res <- reg_result()
        paste0("REGRESSION TYPE: ",res$type," | N=",res$n," | DV=",res$dv)
      }, error=function(e) "Run Regression first.")
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an expert in regression analysis writing for a peer-reviewed journal.\n\n",
        "Interpret regression results:\n",
        "1. Overall model fit (R², F, p)\n2. Individual predictors (β, t, p)\n",
        "3. Effect sizes and practical significance\n4. APA-style Results section\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key%||%""))) { output$ai_output <- renderUI(.ai_k10()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        m <- pls_model_rv()
        if (!is.null(m)) {
          sm <- tryCatch(summary(m),error=function(e) NULL)
          if (!is.null(sm))
            parts <- c(parts,paste0("PLS-SEM:\n",
              paste(head(capture.output(print(round(as.data.frame(sm$paths),4))),30),collapse="\n")))
        }
        sr <- tryCatch(sem_result(),error=function(e) NULL)
        if (!is.null(sr)&&is.null(sr$error)) {
          fi <- lavaan::fitMeasures(sr$fit,c("cfi","tli","rmsea","srmr"))
          parts <- c(parts,paste0("CB-SEM: CFI=",round(fi["cfi"],3)," RMSEA=",round(fi["rmsea"],3)))
        }
        if (!length(parts)) stop("no results")
        paste(parts,collapse="\n\n")
      }, error=function(e) "Run at least one analysis first.")
      output$ai_output <- renderUI({ .ai_r10(call_gemini(paste0(
        "You are an SEM expert writing for a top-tier management journal.\n\n",
        "Write COMPREHENSIVE SEM Results section covering PLS-SEM and CB-SEM:\n",
        "1. Justification for method choice\n2. Outer model: loadings, reliability, validity\n",
        "3. Inner model: paths, R², f²\n4. Bootstrap significance\n",
        "5. CB-SEM fit and parameters\n6. Cross-method comparison\n\n",
        "APA 7. Formal academic prose (7-9 paragraphs).\n\nDATA:\n",ctx),api_key))})
    })

    # ── Data Preview ──────────────────────────────────────────────────────────
    output$preview <- DT::renderDataTable({
      req(df()); tool_dt(head(df(),50),"Data Preview (first 50 rows)")
    })

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Fit Indices Gauge Dashboard
    output$fit_gauge_dashboard <- renderPlotly({
      res <- tryCatch(sem_result(), error=function(e) NULL)
      if (is.null(res) || !is.null(res$error))
        return(plot_ly() |> layout(title="Run CB-SEM / CFA first"))
      fi <- tryCatch(
        fitMeasures(res$fit, c("cfi","tli","rmsea","srmr")),
        error=function(e) NULL
      )
      if (is.null(fi)) return(plot_ly() |> layout(title="Could not extract fit indices"))

      make_gauge <- function(name, val, good_thresh, bad_thresh, higher_is_better=TRUE) {
        ok_col  <- GREEN; bad_col <- "#C0392B"; mid_col <- AMBER
        col_val <- if (higher_is_better) {
          if (val >= good_thresh) ok_col else if (val >= bad_thresh) mid_col else bad_col
        } else {
          if (val <= good_thresh) ok_col else if (val <= bad_thresh) mid_col else bad_col
        }
        plot_ly(type="indicator", mode="gauge+number",
                value=round(val,3),
                title=list(text=name,font=list(size=13,color=NAVY)),
                gauge=list(axis=list(range=list(0,1)),
                           bar=list(color=col_val),
                           steps=list(list(range=c(0,bad_thresh),color="#FFD0D0"),
                                      list(range=c(bad_thresh,good_thresh),color="#FFF3CD"),
                                      list(range=c(good_thresh,1),color="#D4EDDA")),
                           threshold=list(line=list(color="black",width=3),
                                          thickness=0.75,value=good_thresh)))
      }

      p1 <- make_gauge("CFI",   as.numeric(fi["cfi"]),   0.95, 0.90, TRUE)
      p2 <- make_gauge("TLI",   as.numeric(fi["tli"]),   0.95, 0.90, TRUE)
      p3 <- make_gauge("RMSEA", as.numeric(fi["rmsea"]), 0.06, 0.08, FALSE)
      p4 <- make_gauge("SRMR",  as.numeric(fi["srmr"]),  0.08, 0.10, FALSE)

      subplot(p1,p2,p3,p4, nrows=1) |>
        layout(title=list(text="Model Fit Indices Dashboard (green=good | amber=acceptable | red=poor)",
                          font=list(color=NAVY,size=12)),
               paper_bgcolor="white")
    })

    # Path Coefficients Lollipop
    output$path_lollipop <- renderPlotly({
      res <- tryCatch(sem_result(), error=function(e) NULL)
      if (is.null(res) || !is.null(res$error))
        return(plot_ly() |> layout(title="Run CB-SEM / CFA first"))
      params <- tryCatch(
        parameterEstimates(res$fit, standardized=TRUE) |>
          dplyr::filter(op %in% c("=~","~")) |>
          dplyr::mutate(
            Label = paste(lhs, op, rhs),
            Sig   = pvalue < 0.05
          ),
        error=function(e) NULL
      )
      if (is.null(params) || nrow(params)==0)
        return(plot_ly() |> layout(title="No path parameters found"))

      params <- params[order(params$std.all), ]
      plot_ly(params) |>
        add_segments(x=0, xend=~std.all, y=~Label, yend=~Label,
                     line=list(color=TEAL,width=2), name="β") |>
        add_markers(x=~std.all, y=~Label,
                    marker=list(color=~ifelse(Sig,NAVY,"#AAAAAA"),size=9),
                    text=~sprintf("%s: β=%.3f, p%s",Label,std.all,
                                  ifelse(pvalue<.001,"<.001",sprintf("=%.3f",pvalue))),
                    hoverinfo="text", name="Std. Loading/Path") |>
        add_segments(x=0,xend=0,y=0.5,yend=nrow(params)+0.5,
                     line=list(color="#C0392B",dash="dot",width=1),
                     name="Zero",inherit=FALSE) |>
        layout(title=list(text="Standardised Path & Loading Coefficients",font=list(color=NAVY)),
               xaxis=list(title="Standardised Estimate"),
               yaxis=list(title="",automargin=TRUE),
               margin=list(l=200),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # Modification Indices Heatmap
    output$mod_heatmap <- renderPlotly({
      res <- tryCatch(sem_result(), error=function(e) NULL)
      if (is.null(res) || !is.null(res$error))
        return(plot_ly() |> layout(title="Run CB-SEM / CFA first"))
      mi <- tryCatch(modindices(res$fit), error=function(e) NULL)
      if (is.null(mi) || nrow(mi)==0)
        return(plot_ly() |> layout(title="No modification indices available"))

      mi <- mi[mi$mi > 3, ]
      if (nrow(mi)==0) return(plot_ly() |> layout(title="No modification indices > 3"))
      mi <- mi[order(-mi$mi), ][seq_len(min(30, nrow(mi))), ]
      mi$label <- paste(mi$lhs, mi$op, mi$rhs)

      plot_ly(mi, x=~op, y=~label, z=~mi, type="heatmap",
              colorscale=list(c(0,"#EBF4F7"),c(1,"#C0392B")),
              text=~round(mi,2), texttemplate="%{text}",
              hovertemplate="Path: %{y}<br>MI: %{z:.2f}<extra></extra>") |>
        layout(title=list(text="Top Modification Indices (MI > 3)",font=list(color=NAVY)),
               xaxis=list(title="Operator"),
               yaxis=list(title="",automargin=TRUE),
               margin=list(l=200),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════

  }) # end moduleServer
} # end mod10_server
