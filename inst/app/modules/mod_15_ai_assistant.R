# =============================================================
# mod_15_ai_assistant.R  — Dr.AIStat AI Research Assistant
# Dr. Aneeq Inam | ORCID: 0000-0001-7682-2244
# =============================================================
#
# Five intelligent tools for researchers:
#   1. 🔍 Smart Data Profiler   — instant quality + analysis recommendations
#   2. 🗺️ Variable Auto-Mapper  — AI assigns variable roles from column names
#   3. 📝 APA Report Builder    — publication-ready APA 7th edition write-ups
#   4. 🎯 Research Roadmap      — full analysis pipeline planner for your study
#   5. 🔋 Power Analysis        — sample size & statistical power calculator
# =============================================================

# ── Helper: green/amber/red badge ───────────────────────────
badge <- function(label, color = "success") {
  tags$span(class = paste0("label label-", color),
            style = "font-size:.72rem; margin-left:4px;", label)
}

# ── Helper: result card ─────────────────────────────────────
result_card <- function(..., bg = "#f8f9fa", border = "#dee2e6") {
  tags$div(
    style = sprintf(
      paste0("background:%s; border:1px solid %s; border-radius:8px;",
             " padding:14px 18px; margin-bottom:12px;"), bg, border),
    ...
  )
}

# =============================================================
#  UI
# =============================================================
mod15_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        box(
          width = 12, solidHeader = TRUE, status = "primary",
          title = tags$span(
            tags$b("🤖 AI Research Assistant"),
            tags$span(" — Smart profiling · Variable mapping · APA writing · Roadmap · Power",
                      style = "font-size:.82rem; color:#7fb3c8; margin-left:8px;")
          ),

          # ── Banner ──────────────────────────────────────────
          tags$div(
            style = paste0(
              "background:linear-gradient(135deg,#1A3A5C 0%,#2196A6 60%,#16a085 100%);",
              "border-radius:10px; padding:14px 20px; margin-bottom:14px;",
              "display:flex; align-items:center; gap:14px;"
            ),
            tags$div(style="font-size:2.2rem;line-height:1;", "🤖"),
            tags$div(
              tags$h5(style="color:white;margin:0 0 3px;font-size:1rem;font-weight:bold;",
                      "Dr.AIStat AI Research Assistant"),
              tags$p(style="color:#a8d4e4;margin:0;font-size:.78rem;",
                     "Five AI-powered tools. Profiler and Power Analysis work without an API key.",
                     " Variable Mapper, APA Builder, and Research Roadmap need your FREE Groq key (sidebar).")
            )
          ),

          tabBox(
            width = 12, id = ns("tabs"),

            # ══════════════════════════════════════════════════
            # Tab 1 — Smart Data Profiler
            # ══════════════════════════════════════════════════
            tabPanel(
              "🔍 Smart Data Profiler",
              br(),
              fluidRow(
                column(4,
                  tags$div(
                    style = "background:#f0f7ff;border:1px solid #cce0f5;border-radius:8px;padding:16px;",
                    tags$h6(style="color:#1A3A5C;font-weight:bold;margin-top:0;",
                            "📁 Upload or Use Global Dataset"),
                    uiOutput(ns("sdp_global_banner")),
                    fileInput(ns("sdp_file"), label=NULL,
                              accept = c(".csv",".xlsx",".xls",".tsv",".rds"),
                              buttonLabel = "📁 Browse", placeholder = "No file selected"),
                    hr(style="border-color:#dde; margin:8px 0;"),
                    numericInput(ns("sdp_preview_n"), "Preview rows", value=6, min=3, max=20),
                    checkboxInput(ns("sdp_include_cat"), "Include categorical columns in profile", value=TRUE),
                    br(),
                    actionButton(ns("run_sdp"), "🔍 Profile My Data",
                                 class="btn-primary btn-block",
                                 style="font-weight:bold;"),
                    br(),
                    actionButton(ns("ai_sdp"), "🤖 AI Analysis Recommendations",
                                 class="btn-success btn-block"),
                    br(),
                    tags$p(style="color:#888;font-size:.72rem;",
                           "💡 Profile works instantly (no API key). Click 'AI Recommendations' to get smart analysis suggestions powered by Groq.")
                  )
                ),
                column(8,
                  uiOutput(ns("sdp_results_ui"))
                )
              )
            ),

            # ══════════════════════════════════════════════════
            # Tab 2 — Variable Auto-Mapper
            # ══════════════════════════════════════════════════
            tabPanel(
              "🗺️ Variable Auto-Mapper",
              br(),
              tags$div(
                style = paste0("background:linear-gradient(135deg,#fff8f0,#fff3e6);",
                               "border:1px solid #ffd9b3;border-radius:10px;",
                               "padding:12px 18px;margin-bottom:12px;"),
                tags$p(style="margin:0;color:#7a4200;font-size:.82rem;",
                       HTML("🗺️ Paste your column names, describe your study, select the target analysis — ",
                            "the AI instantly assigns each variable to its correct role ",
                            "(DV, IV, mediator, moderator, covariate) with rationale."))
              ),
              fluidRow(
                column(4,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;", "📋 Column Names"),
                    tags$p(style="color:#999;font-size:.73rem;margin:0 0 4px;",
                           "One per line, or comma-separated:"),
                    textAreaInput(ns("vam_cols"), label=NULL, rows=9, width="100%",
                                  placeholder="job_satisfaction\nwork_engagement\ntransformational_leadership\npsychological_safety\nage\ngender\ntenure"),
                    br(),
                    actionButton(ns("sdp_to_vam"), "⬇ Import from Profiler",
                                 class="btn-default btn-sm btn-block",
                                 style="font-size:.75rem;")
                  )
                ),
                column(4,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;", "📖 Study Description"),
                    textAreaInput(ns("vam_study"), label=NULL, rows=4, width="100%",
                                  placeholder="e.g. I am examining how transformational leadership affects job satisfaction, with work engagement as mediator and tenure as a covariate."),
                    selectInput(ns("vam_analysis"), "🎯 Target Analysis",
                                choices=c(
                                  "Mediation Analysis (PLS-SEM / PROCESS)"  = "mediation",
                                  "Moderation / Interaction"                 = "moderation",
                                  "Full SEM (CB-SEM / PLS-SEM)"              = "sem",
                                  "Multiple / Hierarchical Regression"       = "regression",
                                  "CFA / Measurement Model"                  = "cfa",
                                  "EFA (Exploratory)"                        = "efa",
                                  "ANOVA / Group Comparison"                 = "anova",
                                  "Machine Learning Classification"          = "ml",
                                  "Panel Data / Fixed Effects"               = "panel",
                                  "Survival Analysis"                        = "survival",
                                  "fsQCA / Boolean"                          = "fsqca",
                                  "Bibliometric / Scientometric"             = "bibliometric"
                                ), width="100%"),
                    selectInput(ns("vam_scale"), "📊 Measurement Scale",
                                choices=c("Likert 5-point"="likert5",
                                          "Likert 7-point"="likert7",
                                          "Continuous/Ratio"="continuous",
                                          "Binary/Dummy"="binary",
                                          "Mixed"="mixed"),
                                width="100%"),
                    actionButton(ns("run_vam"), "🗺️ Map Variables with AI",
                                 class="btn-warning btn-block",
                                 style="font-weight:bold;color:#fff;")
                  )
                ),
                column(4,
                  uiOutput(ns("vam_results_ui"))
                )
              )
            ),

            # ══════════════════════════════════════════════════
            # Tab 3 — APA Report Builder
            # ══════════════════════════════════════════════════
            tabPanel(
              "📝 APA Report Builder",
              br(),
              tags$div(
                style = paste0("background:linear-gradient(135deg,#f0fff4,#e6f9ee);",
                               "border:1px solid #9fe0b0;border-radius:10px;",
                               "padding:12px 18px;margin-bottom:12px;"),
                tags$p(style="margin:0;color:#1a5c35;font-size:.82rem;",
                       HTML("📝 Paste your raw statistical output, describe your study context, ",
                            "and receive a fully formatted <b>APA 7th edition</b> Methods + Results section ",
                            "— ready to drop directly into your manuscript."))
              ),
              fluidRow(
                column(6,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;", "📋 Study Details"),
                    fluidRow(
                      column(6, textInput(ns("apa_n"), "Sample Size (N)", placeholder="e.g. 312")),
                      column(6, selectInput(ns("apa_analysis"), "Analysis Type",
                                           choices=c(
                                             "SEM / Path Analysis"        = "sem",
                                             "PLS-SEM"                    = "plssem",
                                             "Mediation Analysis"         = "mediation",
                                             "Moderation Analysis"        = "moderation",
                                             "Multiple Regression"        = "regression",
                                             "CFA / Measurement Model"    = "cfa",
                                             "EFA"                        = "efa",
                                             "ANOVA / t-test"             = "anova",
                                             "Machine Learning"           = "ml",
                                             "Panel Data"                 = "panel",
                                             "fsQCA"                      = "fsqca",
                                             "Bibliometric Analysis"      = "bibliometric",
                                             "Survival Analysis"          = "survival"
                                           )))
                    ),
                    textInput(ns("apa_dv"),      "Dependent Variable(s)",
                              placeholder="e.g. Job Satisfaction, Engagement"),
                    textInput(ns("apa_iv"),      "Independent Variable(s)",
                              placeholder="e.g. Transformational Leadership, Safety Climate"),
                    textInput(ns("apa_control"), "Control Variables (optional)",
                              placeholder="e.g. Age, Gender, Tenure"),
                    textInput(ns("apa_software"), "Software & Package",
                              placeholder="e.g. R (lavaan 0.6-17), SmartPLS 4.0"),
                    checkboxGroupInput(ns("apa_sections"), "Include sections:",
                                       choices = c("Participants & Procedure" = "participants",
                                                   "Measures"                 = "measures",
                                                   "Analytical Strategy"      = "strategy",
                                                   "Results"                  = "results",
                                                   "Limitations Note"         = "limitations"),
                                       selected = c("strategy","results"), inline=TRUE)
                  )
                ),
                column(6,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;",
                            "📊 Paste Statistical Output"),
                    tags$p(style="color:#999;font-size:.73rem;margin:0 0 4px;",
                           "Copy-paste key statistics — fit indices, path coefficients, CIs, R², etc.:"),
                    textAreaInput(ns("apa_stats"), label=NULL, rows=10, width="100%",
                                  placeholder=paste0(
                                    "e.g.\nModel fit: CFI = 0.96, RMSEA = 0.052, SRMR = 0.048\n\n",
                                    "H1: Leadership → Satisfaction: β = 0.42, SE = 0.08, t = 5.25, p < .001\n",
                                    "     95% CI [0.27, 0.57]\n\n",
                                    "Indirect effect (Bootstrap N=5000):\n",
                                    "  β = 0.18, 95% BootCI [0.09, 0.29], p < .01\n\n",
                                    "R² Satisfaction = 0.38, R² Engagement = 0.29"
                                  )),
                    br(),
                    actionButton(ns("run_apa"), "📝 Generate APA Write-Up",
                                 class="btn-success btn-block", style="font-weight:bold;")
                  )
                )
              ),
              br(),
              uiOutput(ns("apa_results_ui"))
            ),

            # ══════════════════════════════════════════════════
            # Tab 4 — Research Roadmap
            # ══════════════════════════════════════════════════
            tabPanel(
              "🎯 Research Roadmap",
              br(),
              tags$div(
                style = paste0("background:linear-gradient(135deg,#f5f0ff,#ede6ff);",
                               "border:1px solid #c4a8ff;border-radius:10px;",
                               "padding:12px 18px;margin-bottom:12px;"),
                tags$p(style="margin:0;color:#3d1a8a;font-size:.82rem;",
                       HTML("🎯 Describe your research and let Dr.AIStat plan your <b>complete analysis journey</b> — ",
                            "from data preparation through hypothesis testing to robustness checks, ",
                            "with exact Dr.AIStat module references at every step."))
              ),
              fluidRow(
                column(4,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;",
                            "🔬 Research Details"),
                    textAreaInput(ns("rr_questions"), "Research Questions / Hypotheses", rows=5,
                                  width="100%",
                                  placeholder=paste0("e.g.\nH1: Transformational leadership positively affects ",
                                                     "organizational commitment.\n",
                                                     "H2: Psychological safety mediates H1.\n",
                                                     "H3: Gender moderates the leadership–commitment link.")),
                    selectInput(ns("rr_design"), "Study Design",
                                choices=c("Cross-sectional survey"      = "crosssectional",
                                          "Longitudinal / panel"        = "longitudinal",
                                          "Experimental / quasi-exp."   = "experimental",
                                          "Comparative / multi-group"   = "multigroup",
                                          "Mixed methods"               = "mixed",
                                          "Bibliometric / lit. review"  = "bibliometric"),
                                width="100%"),
                    selectInput(ns("rr_field"), "Research Field",
                                choices=c("Human Resource Management" = "hrm",
                                          "Organizational Behaviour"   = "ob",
                                          "Strategic Management"       = "strategy",
                                          "Marketing / Consumer"       = "marketing",
                                          "Healthcare Management"      = "healthcare",
                                          "Education Research"         = "education",
                                          "Finance / Economics"        = "finance",
                                          "Information Systems"        = "is",
                                          "General Management"         = "management"),
                                width="100%"),
                    fluidRow(
                      column(6, textInput(ns("rr_n"), "Sample Size", placeholder="e.g. 280")),
                      column(6, selectInput(ns("rr_journal"), "Journal Target",
                                           choices=c("SSCI Q1 / ABS 4*" = "q1",
                                                     "SSCI Q2 / ABS 3"  = "q2",
                                                     "Scopus Q1-Q2"     = "scopus",
                                                     "Conference"       = "conference"),
                                           width="100%"))
                    ),
                    actionButton(ns("run_rr"), "🎯 Plan My Research Journey",
                                 class="btn-primary btn-block", style="font-weight:bold;")
                  )
                ),
                column(8,
                  uiOutput(ns("rr_results_ui"))
                )
              )
            ),

            # ══════════════════════════════════════════════════
            # Tab 5 — Power Analysis
            # ══════════════════════════════════════════════════
            tabPanel(
              "🔋 Power Analysis",
              br(),
              tags$div(
                style = paste0("background:linear-gradient(135deg,#fffde7,#fff8e1);",
                               "border:1px solid #ffe082;border-radius:10px;",
                               "padding:12px 18px;margin-bottom:12px;"),
                tags$p(style="margin:0;color:#5f4100;font-size:.82rem;",
                       HTML("🔋 Calculate the <b>minimum sample size</b> required to detect your effect ",
                            "at a given power level (typically 80%), or compute the actual power your ",
                            "current sample achieves. Uses the <code>pwr</code> package. No API key needed."))
              ),
              fluidRow(
                column(4,
                  tags$div(
                    style="background:#fafafa;border:1px solid #ddd;border-radius:8px;padding:14px;",
                    tags$h6(style="font-weight:bold;color:#1A3A5C;margin-top:0;", "⚙️ Parameters"),
                    selectInput(ns("pwr_test"), "Statistical Test",
                                choices=c(
                                  "One-sample t-test"                       = "t_one",
                                  "Independent samples t-test"              = "t_two",
                                  "Paired t-test"                           = "t_paired",
                                  "One-way ANOVA (F-test)"                  = "anova",
                                  "Simple / Multiple Regression (F²)"       = "reg",
                                  "Chi-square (goodness-of-fit)"            = "chisq",
                                  "Pearson Correlation"                     = "corr",
                                  "Logistic Regression (approximate)"       = "logistic",
                                  "SEM / Path Analysis (RMSEA approach)"    = "sem"
                                ), width="100%"),
                    selectInput(ns("pwr_solve"), "Solve for:",
                                choices=c("Sample Size (N)" = "n",
                                          "Power (1−β)"     = "power"),
                                width="100%"),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'power'", ns("pwr_solve")),
                      numericInput(ns("pwr_n"), "Your Sample Size (N)", value=200, min=10, max=10000)
                    ),
                    selectInput(ns("pwr_effect"), "Expected Effect Size",
                                choices=c("Small (d=0.2 / f=0.1 / f²=0.02)"  = "small",
                                          "Medium (d=0.5 / f=0.25 / f²=0.15)" = "medium",
                                          "Large (d=0.8 / f=0.4 / f²=0.35)"  = "large",
                                          "Custom (enter below)"               = "custom"),
                                width="100%"),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'custom'", ns("pwr_effect")),
                      numericInput(ns("pwr_effect_val"), "Effect size value", value=0.3, min=0.01, step=0.01)
                    ),
                    numericInput(ns("pwr_alpha"), "Significance level (α)", value=0.05, min=0.001, max=0.10, step=0.01),
                    conditionalPanel(
                      condition = sprintf("input['%s'] == 'n'", ns("pwr_solve")),
                      sliderInput(ns("pwr_target"), "Target Power (1−β)", min=0.60, max=0.99, value=0.80, step=0.01)
                    ),
                    conditionalPanel(
                      condition = sprintf("['anova','reg'].includes(input['%s'])", ns("pwr_test")),
                      numericInput(ns("pwr_k"), "No. of groups / predictors (k)", value=3, min=2, max=20)
                    ),
                    br(),
                    actionButton(ns("run_pwr"), "🔋 Calculate Power",
                                 class="btn-warning btn-block", style="font-weight:bold;color:#fff;")
                  )
                ),
                column(8,
                  uiOutput(ns("pwr_results_ui"))
                )
              )
            )  # end Power Analysis tab

          )  # end tabBox
        )  # end box
      )  # end column
    )  # end fluidRow
  )
}

# =============================================================
#  SERVER
# =============================================================
mod15_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Internal reactive storage ──────────────────────────
    sdp_data_rv    <- reactiveVal(NULL)  # profiler data
    sdp_profile_rv <- reactiveVal(NULL)  # computed profile
    vam_result_rv  <- reactiveVal(NULL)  # variable mapping
    apa_result_rv  <- reactiveVal(NULL)  # APA write-up
    rr_result_rv   <- reactiveVal(NULL)  # roadmap
    pwr_result_rv  <- reactiveVal(NULL)  # power result

    # ══════════════════════════════════════════════════════
    # TAB 1: SMART DATA PROFILER
    # ══════════════════════════════════════════════════════

    # Global data banner
    output$sdp_global_banner <- renderUI({
      gd <- global_shared_data()
      if (is.null(gd)) return(NULL)
      tags$div(
        style = paste0("background:#e6f4ea;border:1px solid #81c784;border-radius:6px;",
                       "padding:7px 12px;margin-bottom:8px;",
                       "display:flex;align-items:center;justify-content:space-between;"),
        tags$span(style="color:#1b5e20;font-size:.78rem;",
                  sprintf("📋 %s  (%d×%d)", global_shared_name(), nrow(gd), ncol(gd))),
        actionButton(ns("sdp_use_global"), "⬇ Use",
                     class="btn-success btn-xs", style="padding:2px 8px;font-size:.72rem;")
      )
    })

    observeEvent(input$sdp_use_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) sdp_data_rv(gd)
    })
    observeEvent(input$sdp_file, {
      req(input$sdp_file)
      tryCatch({
        sdp_data_rv(read_uploaded(input$sdp_file$datapath, input$sdp_file$name))
      }, error = function(e) showNotification(paste("Load error:", e$message), type="error"))
    })
    # Import columns to Variable Mapper
    observeEvent(input$sdp_to_vam, {
      d <- sdp_data_rv()
      if (!is.null(d)) {
        nm <- paste(names(d), collapse="\n")
        updateTextAreaInput(session, "vam_cols", value=nm)
        showNotification("Column names copied to Variable Mapper.", type="message", duration=3)
      }
    })

    # Run profiler (no API key)
    observeEvent(input$run_sdp, {
      d <- sdp_data_rv()
      req(d)
      withProgress(message="Profiling data...", {
        profile <- lapply(names(d), function(cn) {
          x     <- d[[cn]]
          dtype <- if (is.numeric(x)) "Numeric" else if (is.factor(x) || is.character(x)) "Categorical" else class(x)[1]
          n_obs   <- length(x)
          n_miss  <- sum(is.na(x))
          pct_miss <- round(100 * n_miss / n_obs, 1)
          n_uniq  <- length(unique(na.omit(x)))
          if (is.numeric(x)) {
            list(column=cn, type=dtype, n=n_obs, missing=n_miss, pct_missing=pct_miss,
                 unique=n_uniq,
                 mean=round(mean(x,na.rm=TRUE),3), sd=round(sd(x,na.rm=TRUE),3),
                 min=round(min(x,na.rm=TRUE),3), max=round(max(x,na.rm=TRUE),3),
                 skew=round(moments::skewness(x,na.rm=TRUE),3),
                 kurt=round(moments::kurtosis(x,na.rm=TRUE),3))
          } else {
            top_val <- if (n_uniq > 0) names(sort(table(na.omit(x)), decreasing=TRUE)[1]) else NA
            list(column=cn, type=dtype, n=n_obs, missing=n_miss, pct_missing=pct_miss,
                 unique=n_uniq,
                 mean=NA, sd=NA, min=NA, max=NA,
                 skew=NA, kurt=NA)
          }
        })
        sdp_profile_rv(do.call(rbind, lapply(profile, as.data.frame)))
      })
    })

    # Render profiler results
    output$sdp_results_ui <- renderUI({
      prof <- sdp_profile_rv()
      d    <- sdp_data_rv()
      if (is.null(d) && is.null(prof)) {
        return(tags$div(style="text-align:center;padding:40px;color:#aaa;",
                        tags$p("📁 Upload a file or use the global dataset, then click 'Profile My Data'")))
      }
      if (!is.null(d) && is.null(prof)) {
        n_miss  <- sum(is.na(d))
        n_total <- prod(dim(d))
        return(result_card(
          tags$p(style="color:#1A3A5C;font-weight:bold;", "Dataset loaded — click 🔍 Profile My Data"),
          tags$p(sprintf("%d rows × %d cols | Missing values: %d (%.1f%%)",
                         nrow(d), ncol(d), n_miss, 100*n_miss/n_total))
        ))
      }
      req(prof)
      n_num  <- sum(prof$type=="Numeric")
      n_cat  <- sum(prof$type=="Categorical")
      n_miss_cols <- sum(prof$missing > 0)
      pct_miss_overall <- round(mean(prof$pct_missing), 1)
      high_miss <- prof$column[prof$pct_missing > 20]

      tagList(
        # KPI bar
        fluidRow(
          column(3, tags$div(style="text-align:center;background:#e3f2fd;border-radius:8px;padding:10px;",
                             tags$h4(style="margin:0;color:#1565c0;",nrow(d)), tags$p(style="margin:0;font-size:.75rem;","Rows"))),
          column(3, tags$div(style="text-align:center;background:#e8f5e9;border-radius:8px;padding:10px;",
                             tags$h4(style="margin:0;color:#2e7d32;",ncol(d)), tags$p(style="margin:0;font-size:.75rem;","Columns"))),
          column(3, tags$div(style="text-align:center;background:#fce4ec;border-radius:8px;padding:10px;",
                             tags$h4(style="margin:0;color:#c62828;",n_miss_cols), tags$p(style="margin:0;font-size:.75rem;","Cols with missing"))),
          column(3, tags$div(style="text-align:center;background:#fff3e0;border-radius:8px;padding:10px;",
                             tags$h4(style="margin:0;color:#e65100;",paste0(pct_miss_overall,"%")), tags$p(style="margin:0;font-size:.75rem;","Avg % missing")))
        ),
        br(),
        if (length(high_miss) > 0)
          tags$div(class="alert alert-warning", style="font-size:.8rem;",
                   tags$b("⚠️ High missing data (>20%): "),
                   paste(high_miss, collapse=", ")),
        tabBox(width=12, side="left",
          tabPanel("📋 Column Profile",
            DT::dataTableOutput(ns("sdp_profile_tbl"))
          ),
          tabPanel("📊 Missing Data",
            plotlyOutput(ns("sdp_missing_plot"), height="300px")
          ),
          tabPanel("📈 Distributions",
            uiOutput(ns("sdp_dist_ui"))
          ),
          tabPanel("🔗 Correlations",
            plotlyOutput(ns("sdp_corr_plot"), height="350px")
          )
        ),
        br(),
        uiOutput(ns("sdp_ai_results_ui"))
      )
    })

    output$sdp_profile_tbl <- DT::renderDataTable({
      prof <- sdp_profile_rv(); req(prof)
      DT::datatable(prof, rownames=FALSE, options=list(pageLength=15,scrollX=TRUE),
                    class="compact stripe") |>
        DT::formatStyle("pct_missing",
                        background = DT::styleInterval(c(5,20), c("#d4edda","#fff3cd","#f8d7da")),
                        color = DT::styleInterval(c(5,20), c("#155724","#856404","#721c24")))
    })

    output$sdp_missing_plot <- renderPlotly({
      prof <- sdp_profile_rv(); req(prof)
      df_miss <- prof[order(-prof$pct_missing), ][1:min(20,nrow(prof)), ]
      plot_ly(df_miss, x=~pct_missing, y=~reorder(column, pct_missing), type="bar",
              orientation="h",
              marker=list(color=ifelse(df_miss$pct_missing>20,"#e74c3c",
                                       ifelse(df_miss$pct_missing>5,"#f39c12","#2ecc71")))) |>
        layout(title="Missing Data by Column (%)",
               xaxis=list(title="% Missing"), yaxis=list(title=""),
               margin=list(l=140))
    })

    output$sdp_dist_ui <- renderUI({
      prof <- sdp_profile_rv(); req(prof)
      num_cols_p <- prof$column[prof$type=="Numeric"]
      if (length(num_cols_p)==0) return(tags$p("No numeric columns."))
      tagList(
        tags$p(style="font-size:.78rem;color:#888;",
               sprintf("Distribution plots for %d numeric column(s)", length(num_cols_p))),
        plotlyOutput(ns("sdp_dist_plot"), height=paste0(max(200, 80*ceiling(length(num_cols_p)/3)), "px"))
      )
    })

    output$sdp_dist_plot <- renderPlotly({
      d <- sdp_data_rv(); prof <- sdp_profile_rv(); req(d, prof)
      num_cols_p <- prof$column[prof$type=="Numeric"]
      num_cols_p <- head(num_cols_p, 9)
      n <- length(num_cols_p)
      ncols_grid <- min(3, n); nrows_grid <- ceiling(n/ncols_grid)
      fig <- plotly::subplot(
        lapply(seq_along(num_cols_p), function(i) {
          x <- na.omit(d[[num_cols_p[i]]])
          plot_ly(x=x, type="histogram", name=num_cols_p[i],
                  marker=list(color="#2196A6", line=list(color="#fff",width=0.5))) |>
            layout(xaxis=list(title=num_cols_p[i]), yaxis=list(title="Count"),
                   showlegend=FALSE)
        }),
        nrows=nrows_grid, shareX=FALSE, shareY=FALSE, titleX=TRUE, titleY=TRUE
      ) |> layout(title=list(text="Variable Distributions", font=list(size=13)))
      fig
    })

    output$sdp_corr_plot <- renderPlotly({
      d <- sdp_data_rv(); req(d)
      num_d <- d[, sapply(d, is.numeric), drop=FALSE]
      num_d <- num_d[, colSums(!is.na(num_d)) > 3, drop=FALSE]
      if (ncol(num_d) < 2) return(plot_ly() |> layout(title="Need ≥2 numeric cols"))
      cm <- round(cor(num_d, use="pairwise.complete.obs"), 2)
      plot_ly(x=colnames(cm), y=rownames(cm), z=cm, type="heatmap",
              colorscale="RdBu", zmin=-1, zmax=1,
              text=cm, texttemplate="%{text}") |>
        layout(title="Correlation Matrix", margin=list(l=100, b=100))
    })

    # AI recommendations
    output$sdp_ai_results_ui <- renderUI({ uiOutput(ns("sdp_ai_box")) })
    sdp_ai_rv <- reactiveVal(NULL)

    observeEvent(input$ai_sdp, {
      d <- sdp_data_rv(); req(d)
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) {
        showNotification("Please enter your Groq API key in the sidebar.", type="warning", duration=5)
        return()
      }
      prof <- sdp_profile_rv()
      col_summary <- if (!is.null(prof)) {
        paste(apply(prof, 1, function(r) {
          sprintf("%s [%s, N=%s, miss=%.0f%%]", r["column"], r["type"], r["n"], as.numeric(r["pct_missing"]))
        }), collapse="; ")
      } else {
        paste(names(d), collapse=", ")
      }
      prompt <- paste0(
        "You are an expert research methods consultant specializing in quantitative social science.\n",
        "A researcher has a dataset with the following variables:\n",
        col_summary, "\n\n",
        "Dataset: ", nrow(d), " rows × ", ncol(d), " columns\n\n",
        "Your task: Provide intelligent analysis recommendations.\n",
        "Respond ONLY with a JSON object in this exact format:\n",
        '{"data_quality": "Brief assessment of data quality in 2 sentences.",\n',
        ' "top_analyses": [\n',
        '   {"rank": 1, "analysis": "Name of analysis", "module": "Dr.AIStat module name",\n',
        '    "rationale": "Why this analysis fits this data in 1-2 sentences.",\n',
        '    "suitability": 95},\n',
        '   {"rank": 2, ...}, {"rank": 3, ...}, {"rank": 4, ...}, {"rank": 5, ...}\n',
        ' ],\n',
        ' "cautions": ["caution 1", "caution 2"],\n',
        ' "sample_size_note": "Assessment of whether N is adequate."}\n',
        "Return ONLY the JSON, no other text."
      )
      withProgress(message="AI is analysing your data...", {
        raw <- tryCatch(
          call_gemini(prompt, api_key, model="llama-3.3-70b-versatile"),
          error = function(e) paste("Error:", e$message)
        )
      })
      sdp_ai_rv(raw)
    })

    output$sdp_ai_box <- renderUI({
      raw <- sdp_ai_rv(); if (is.null(raw)) return(NULL)
      # Try parse JSON
      parsed <- tryCatch({
        js <- regmatches(raw, regexpr("\\{[\\s\\S]+\\}", raw, perl=TRUE))
        if (length(js)==0) stop("no JSON")
        jsonlite::fromJSON(js)
      }, error=function(e) NULL)

      if (is.null(parsed)) {
        return(result_card(tags$h6("🤖 AI Analysis Recommendations"), tags$pre(style="white-space:pre-wrap;font-size:.8rem;",raw)))
      }

      suitability_color <- function(s) {
        if (s >= 85) "#28a745" else if (s >= 65) "#ffc107" else "#dc3545"
      }

      analyses_cards <- if (!is.null(parsed$top_analyses)) {
        lapply(seq_len(nrow(parsed$top_analyses)), function(i) {
          r <- parsed$top_analyses[i,]
          suit <- as.integer(r$suitability %||% 70)
          tags$div(
            style=sprintf(paste0("border-left:4px solid %s;padding:8px 12px;",
                                 "background:#fafafa;border-radius:0 6px 6px 0;margin-bottom:8px;"),
                          suitability_color(suit)),
            tags$div(style="display:flex;justify-content:space-between;align-items:flex-start;",
              tags$div(
                tags$span(style="font-weight:bold;color:#1A3A5C;font-size:.88rem;",
                          sprintf("#%d  %s", i, r$analysis)),
                tags$br(),
                tags$span(style="font-size:.75rem;color:#2196A6;", r$module)
              ),
              tags$span(style=sprintf("font-size:1.1rem;font-weight:bold;color:%s;", suitability_color(suit)),
                        sprintf("%d%%", suit))
            ),
            tags$p(style="font-size:.79rem;color:#555;margin:5px 0 0;", r$rationale)
          )
        })
      } else list()

      tagList(
        result_card(bg="#f8f9ff", border="#c5cae9",
          tags$h6(style="color:#1A3A5C;font-weight:bold;", "🤖 AI Analysis Recommendations"),
          tags$p(style="font-size:.82rem;color:#444;",
                 tags$b("Data Quality: "), parsed$data_quality %||% ""),
          tags$p(style="font-size:.82rem;color:#444;",
                 tags$b("Sample Size: "), parsed$sample_size_note %||% ""),
          if (length(parsed$cautions) > 0)
            tags$div(class="alert alert-warning", style="font-size:.78rem;padding:6px 10px;",
                     tags$b("⚠️ Cautions: "),
                     paste(parsed$cautions, collapse=" | ")),
          tags$h6(style="color:#1A3A5C;margin-top:12px;", "📊 Top Recommended Analyses:"),
          do.call(tagList, analyses_cards)
        )
      )
    })

    # ══════════════════════════════════════════════════════
    # TAB 2: VARIABLE AUTO-MAPPER
    # ══════════════════════════════════════════════════════

    observeEvent(input$run_vam, {
      cols_text <- trimws(input$vam_cols %||% "")
      study     <- trimws(input$vam_study %||% "")
      if (!nzchar(cols_text)) {
        showNotification("Please paste your column names first.", type="warning"); return()
      }
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) {
        showNotification("Please enter your Groq API key in the sidebar.", type="warning"); return()
      }
      # Normalise input
      cols <- trimws(unlist(strsplit(cols_text, "[,\n]+")))
      cols <- cols[nzchar(cols)]
      analysis_label <- switch(input$vam_analysis,
        mediation="Mediation Analysis", moderation="Moderation Analysis",
        sem="Full SEM", regression="Multiple / Hierarchical Regression",
        cfa="CFA", efa="EFA", anova="ANOVA / Group Comparison",
        ml="Machine Learning", panel="Panel Data", survival="Survival Analysis",
        fsqca="fsQCA", bibliometric="Bibliometric Analysis", input$vam_analysis)

      prompt <- paste0(
        "You are an expert in quantitative research methods.\n",
        "A researcher wants to run: ", analysis_label, "\n",
        "Measurement scale: ", input$vam_scale, "\n",
        "Study description: ", study, "\n",
        "Column names: ", paste(cols, collapse=", "), "\n\n",
        "Assign each variable to the correct role for this analysis.\n",
        "Respond ONLY with this exact JSON format:\n",
        '{"mappings": [\n',
        '  {"variable":"col_name","role":"Dependent Variable","color":"danger","rationale":"why"},\n',
        '  {"variable":"col_name","role":"Independent Variable","color":"primary","rationale":"why"},\n',
        '  ...\n',
        '],\n',
        '"model_formula": "DV ~ IV1 + IV2 (mediator: M) [moderator: W]",\n',
        '"key_note": "One important methodological note for this analysis.",\n',
        '"suggested_software": "R package or tool recommendation"}\n',
        "\nRole options: Dependent Variable, Independent Variable, Mediator, Moderator, ",
        "Covariate/Control, ID/Group Variable, Time Variable, Instrument, Ignore.\n",
        "Color options: danger (red=DV), primary (blue=IV), warning (amber=mediator/moderator), ",
        "success (green=control), default (grey=ignore).\n",
        "Return ONLY the JSON."
      )
      withProgress(message="AI is mapping your variables...", {
        raw <- tryCatch(call_gemini(prompt, api_key, model="llama-3.3-70b-versatile"),
                        error=function(e) paste("Error:", e$message))
      })
      vam_result_rv(raw)
    })

    output$vam_results_ui <- renderUI({
      raw <- vam_result_rv()
      if (is.null(raw)) {
        return(tags$div(style="padding:30px;text-align:center;color:#aaa;",
                        "Mapping results will appear here after you click 'Map Variables with AI'"))
      }
      parsed <- tryCatch({
        js <- regmatches(raw, regexpr("\\{[\\s\\S]+\\}", raw, perl=TRUE))
        if (length(js)==0) stop("no JSON")
        jsonlite::fromJSON(js)
      }, error=function(e) NULL)

      if (is.null(parsed)) return(result_card(tags$pre(style="white-space:pre-wrap;font-size:.78rem;",raw)))

      mapping_rows <- if (!is.null(parsed$mappings)) {
        lapply(seq_len(nrow(parsed$mappings)), function(i) {
          m <- parsed$mappings[i,]
          tags$div(style="display:flex;align-items:flex-start;margin-bottom:6px;gap:8px;",
            tags$span(class=paste0("label label-", m$color %||% "default"),
                      style="min-width:130px;text-align:center;padding:4px 8px;font-size:.74rem;",
                      m$role %||% ""),
            tags$div(
              tags$b(style="font-size:.82rem;", m$variable %||% ""),
              tags$p(style="font-size:.72rem;color:#666;margin:0;", m$rationale %||% "")
            )
          )
        })
      } else list()

      result_card(bg="#f8fff8", border="#a5d6a7",
        tags$h6(style="color:#1A3A5C;font-weight:bold;", "🗺️ Variable Role Assignments"),
        do.call(tagList, mapping_rows),
        if (!is.null(parsed$model_formula)) tagList(
          hr(style="margin:10px 0;"),
          tags$p(style="font-size:.82rem;",
                 tags$b("📐 Model formula: "),
                 tags$code(parsed$model_formula))
        ),
        if (!is.null(parsed$key_note)) tags$div(
          style="background:#fffde7;border-left:3px solid #f9a825;padding:6px 10px;border-radius:0 5px 5px 0;margin-top:8px;",
          tags$p(style="font-size:.78rem;margin:0;color:#5f4100;",
                 tags$b("💡 Note: "), parsed$key_note)
        ),
        if (!is.null(parsed$suggested_software)) tags$p(
          style="font-size:.75rem;color:#666;margin-top:6px;",
          tags$b("🔧 Suggested software: "), parsed$suggested_software
        )
      )
    })

    # ══════════════════════════════════════════════════════
    # TAB 3: APA REPORT BUILDER
    # ══════════════════════════════════════════════════════

    observeEvent(input$run_apa, {
      stats_text <- trimws(input$apa_stats %||% "")
      if (!nzchar(stats_text)) {
        showNotification("Please paste your statistical output first.", type="warning"); return()
      }
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) {
        showNotification("Please enter your Groq API key in the sidebar.", type="warning"); return()
      }
      sections <- paste(input$apa_sections %||% c("strategy","results"), collapse=", ")
      prompt <- paste0(
        "You are an expert academic writing assistant specialising in APA 7th edition.\n",
        "Write a formal, publication-quality APA 7th edition results section for the following study.\n\n",
        "STUDY DETAILS:\n",
        "- Sample size (N): ", input$apa_n %||% "not specified", "\n",
        "- Analysis type: ", input$apa_analysis, "\n",
        "- Dependent variable(s): ", input$apa_dv %||% "not specified", "\n",
        "- Independent variable(s): ", input$apa_iv %||% "not specified", "\n",
        "- Control variables: ", input$apa_control %||% "none", "\n",
        "- Software: ", input$apa_software %||% "R", "\n",
        "- Sections requested: ", sections, "\n\n",
        "STATISTICAL OUTPUT TO REPORT:\n",
        stats_text, "\n\n",
        "INSTRUCTIONS:\n",
        "1. Write in formal academic third-person prose (no bullet points in the output).\n",
        "2. Report all statistics in exact APA 7th edition format: β = .42, SE = .08, t(310) = 5.25, p < .001.\n",
        "3. Use italics notation: *β*, *SE*, *t*, *p*, *F*, *df*, *R*², *N*.\n",
        "4. Include bootstrapped confidence intervals where relevant.\n",
        "5. Start Results with 'Preliminary analyses confirmed...' or 'Prior to main analyses...'.\n",
        "6. Be specific — do not leave placeholders like [value].\n",
        "7. End with a brief summary sentence of key findings.\n\n",
        "Write the complete text now:"
      )
      withProgress(message="AI is writing your APA report...", {
        result <- tryCatch(call_gemini(prompt, api_key, model="llama-3.3-70b-versatile"),
                           error=function(e) paste("Error:", e$message))
      })
      apa_result_rv(result)
    })

    output$apa_results_ui <- renderUI({
      result <- apa_result_rv()
      if (is.null(result)) return(NULL)
      tagList(
        result_card(bg="#f0fff4", border="#81c784",
          tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
            tags$h6(style="margin:0;font-weight:bold;color:#1A3A5C;", "📝 APA 7th Edition Write-Up"),
            tags$div(
              actionButton(ns("apa_copy_btn"), "📋 Copy Text",
                           class="btn-default btn-sm", style="margin-right:4px;"),
              downloadButton(ns("apa_download"), "📥 Download .txt",
                             class="btn-primary btn-sm")
            )
          ),
          tags$div(
            style=paste0("background:white;border:1px solid #ddd;border-radius:6px;",
                         "padding:16px;font-size:.87rem;line-height:1.7;",
                         "font-family:'Times New Roman',Times,serif;"),
            HTML(gsub("\n", "<br>", htmltools::htmlEscape(result)))
          )
        )
      )
    })

    output$apa_download <- downloadHandler(
      filename = function() paste0("APA_Report_", Sys.Date(), ".txt"),
      content  = function(file) writeLines(apa_result_rv() %||% "", file)
    )

    observeEvent(input$apa_copy_btn, {
      result <- apa_result_rv()
      if (!is.null(result)) {
        session$sendCustomMessage("clipboard_copy", result)
        showNotification("Text copied to clipboard!", type="message", duration=2)
      }
    })

    # ══════════════════════════════════════════════════════
    # TAB 4: RESEARCH ROADMAP
    # ══════════════════════════════════════════════════════

    observeEvent(input$run_rr, {
      questions <- trimws(input$rr_questions %||% "")
      if (!nzchar(questions)) {
        showNotification("Please describe your research questions first.", type="warning"); return()
      }
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) {
        showNotification("Please enter your Groq API key in the sidebar.", type="warning"); return()
      }
      design_label <- switch(input$rr_design,
        crosssectional="Cross-sectional survey", longitudinal="Longitudinal panel",
        experimental="Experimental/quasi-experimental", multigroup="Multi-group comparative",
        mixed="Mixed methods", bibliometric="Bibliometric/systematic review",
        input$rr_design)
      journal_label <- switch(input$rr_journal,
        q1="SSCI Q1/ABS 4* (top-tier)", q2="SSCI Q2/ABS 3 (high-quality)",
        scopus="Scopus Q1-Q2", conference="Conference paper", input$rr_journal)

      prompt <- paste0(
        "You are an expert research methods consultant for business and management research.\n",
        "Create a complete, practical analysis roadmap for this study.\n\n",
        "STUDY PROFILE:\n",
        "- Research questions/hypotheses: ", questions, "\n",
        "- Study design: ", design_label, "\n",
        "- Research field: ", input$rr_field, "\n",
        "- Sample size (N): ", input$rr_n %||% "not specified", "\n",
        "- Target journal: ", journal_label, "\n\n",
        "Respond ONLY with this JSON format:\n",
        '{"study_title": "Short descriptive title for this study",\n',
        ' "phases": [\n',
        '   {"phase": 1, "name": "Phase name", "icon": "emoji",\n',
        '    "steps": [\n',
        '      {"step": "Step title",\n',
        '       "module": "Dr.AIStat module name (e.g. Module 1: Descriptive Stats)",\n',
        '       "priority": "Essential",\n',
        '       "effort": "15 min",\n',
        '       "rationale": "Why this step matters in 1 sentence."},\n',
        '      ...\n',
        '    ]},\n',
        '   ...\n',
        ' ],\n',
        ' "key_risks": ["risk 1", "risk 2", "risk 3"],\n',
        ' "reviewer_tips": ["tip for reviewers 1", "tip 2"],\n',
        ' "total_estimated_time": "X-Y hours"}\n\n',
        "Create 4-5 phases (e.g. Data Prep, Measurement, Structural Testing, Robustness, Reporting).\n",
        "Reference actual Dr.AIStat modules where applicable:\n",
        "Module 1: Descriptive Stats, Module 2: Measurement/CFA/EFA, Module 3: Group Tests,\n",
        "Module 4: Regression, Module 5: ML Classification, Module 6: Clustering,\n",
        "Module 10: SEM Canvas (PLS-SEM, CB-SEM, Mediation, Moderation),\n",
        "Module 11: CoMe Analysis, Module 12: Bibliometric, Module 13: fsQCA,\n",
        "Module 14: STATA-Style Methods, Module 15: AI Research Assistant\n",
        "Return ONLY the JSON."
      )
      withProgress(message="AI is planning your research journey...", {
        raw <- tryCatch(call_gemini(prompt, api_key, model="llama-3.3-70b-versatile"),
                        error=function(e) paste("Error:", e$message))
      })
      rr_result_rv(raw)
    })

    output$rr_results_ui <- renderUI({
      raw <- rr_result_rv()
      if (is.null(raw)) {
        return(tags$div(style="padding:40px;text-align:center;color:#aaa;",
                        "Your research roadmap will appear here."))
      }
      parsed <- tryCatch({
        js <- regmatches(raw, regexpr("\\{[\\s\\S]+\\}", raw, perl=TRUE))
        if (length(js)==0) stop("no JSON")
        jsonlite::fromJSON(js)
      }, error=function(e) NULL)

      if (is.null(parsed)) return(result_card(tags$pre(style="white-space:pre-wrap;font-size:.78rem;",raw)))

      priority_color <- function(p) {
        switch(tolower(trimws(p %||% "")),
          essential   = "#e74c3c",
          recommended = "#f39c12",
          optional    = "#3498db",
          "#666"
        )
      }

      phase_cards <- if (!is.null(parsed$phases)) {
        lapply(seq_along(parsed$phases), function(i) {
          ph    <- parsed$phases[[i]]
          steps <- if (is.data.frame(ph$steps)) ph$steps else as.data.frame(ph$steps)
          phase_color <- c("#1A3A5C","#2196A6","#16a085","#8e44ad","#c0392b")[((i-1)%%5)+1]
          tags$div(
            style=sprintf(paste0("border-left:5px solid %s;background:#fff;",
                                 "border-radius:0 8px 8px 0;margin-bottom:14px;",
                                 "box-shadow:0 1px 4px rgba(0,0,0,.08);overflow:hidden;"), phase_color),
            # Phase header
            tags$div(
              style=sprintf("background:%s;padding:8px 14px;", phase_color),
              tags$h6(style="color:white;margin:0;font-size:.9rem;",
                      sprintf("%s  Phase %d: %s",
                              ph$icon %||% "📌",
                              ph$phase %||% i,
                              ph$name %||% ""))
            ),
            # Steps
            tags$div(style="padding:10px 14px;",
              do.call(tagList, lapply(seq_len(nrow(steps)), function(j) {
                s <- steps[j,]
                tags$div(style="border-bottom:1px solid #f0f0f0;padding:6px 0;",
                  tags$div(style="display:flex;justify-content:space-between;align-items:flex-start;",
                    tags$div(
                      tags$span(style=sprintf("color:%s;font-weight:bold;font-size:.83rem;",
                                              priority_color(s$priority)),
                                sprintf("▶ %s", s$step %||% "")),
                      tags$br(),
                      tags$span(style="font-size:.73rem;color:#2196A6;font-style:italic;",
                                s$module %||% "")
                    ),
                    tags$div(style="text-align:right;flex-shrink:0;margin-left:8px;",
                      tags$span(class=paste0("label label-",
                                             switch(tolower(trimws(s$priority %||% "")),
                                                    essential="danger", recommended="warning", "default")),
                                style="font-size:.66rem;",
                                s$priority %||% ""),
                      tags$br(),
                      tags$span(style="font-size:.68rem;color:#999;", s$effort %||% "")
                    )
                  ),
                  tags$p(style="font-size:.75rem;color:#666;margin:2px 0 0 12px;",
                         s$rationale %||% "")
                )
              }))
            )
          )
        })
      } else list()

      tagList(
        result_card(bg="#f9f0ff", border="#c4a8ff",
          tags$h5(style="color:#3d1a8a;margin-top:0;",
                  paste0("🎯 ", parsed$study_title %||% "Research Analysis Roadmap")),
          if (!is.null(parsed$total_estimated_time))
            tags$p(style="color:#666;font-size:.8rem;",
                   tags$b("⏱ Estimated time: "), parsed$total_estimated_time),
          tags$h6(style="color:#1A3A5C;margin-bottom:8px;", "📋 Analysis Phases"),
          do.call(tagList, phase_cards),
          if (!is.null(parsed$key_risks) && length(parsed$key_risks) > 0)
            tags$div(style="background:#fde8e8;border-radius:8px;padding:10px 14px;margin-top:6px;",
              tags$h6(style="color:#c0392b;margin:0 0 6px;", "⚠️ Key Risks to Address"),
              tags$ul(style="margin:0;padding-left:18px;",
                      lapply(parsed$key_risks, function(r) tags$li(style="font-size:.78rem;", r)))
            ),
          if (!is.null(parsed$reviewer_tips) && length(parsed$reviewer_tips) > 0)
            tags$div(style="background:#e8f4e8;border-radius:8px;padding:10px 14px;margin-top:8px;",
              tags$h6(style="color:#1a5c35;margin:0 0 6px;", "✅ Reviewer Tips"),
              tags$ul(style="margin:0;padding-left:18px;",
                      lapply(parsed$reviewer_tips, function(t) tags$li(style="font-size:.78rem;", t)))
            )
        )
      )
    })

    # ══════════════════════════════════════════════════════
    # TAB 5: POWER ANALYSIS
    # ══════════════════════════════════════════════════════

    observeEvent(input$run_pwr, {
      tryCatch({
        if (!requireNamespace("pwr", quietly=TRUE)) {
          install.packages("pwr", repos="https://cloud.r-project.org", quiet=TRUE)
        }
        library(pwr)

        solve_for <- input$pwr_solve
        alpha     <- input$pwr_alpha %||% 0.05
        k         <- max(2L, as.integer(input$pwr_k %||% 3))
        target_pwr <- input$pwr_target %||% 0.80
        user_n     <- as.integer(input$pwr_n %||% 200)

        # Effect size
        es_val <- switch(input$pwr_effect %||% "medium",
          small  = list(d=0.2, f=0.10, f2=0.02, r=0.10, w=0.10),
          medium = list(d=0.5, f=0.25, f2=0.15, r=0.30, w=0.30),
          large  = list(d=0.8, f=0.40, f2=0.35, r=0.50, w=0.50),
          custom = {
            v <- input$pwr_effect_val %||% 0.3
            list(d=v, f=v, f2=v, r=v, w=v)
          }
        )

        test <- input$pwr_test %||% "t_two"
        res  <- switch(test,
          t_one    = if (solve_for=="n")
                       pwr.t.test(d=es_val$d, sig.level=alpha, power=target_pwr, type="one.sample")
                     else
                       pwr.t.test(d=es_val$d, sig.level=alpha, n=user_n, type="one.sample"),
          t_two    = if (solve_for=="n")
                       pwr.t.test(d=es_val$d, sig.level=alpha, power=target_pwr, type="two.sample")
                     else
                       pwr.t.test(d=es_val$d, sig.level=alpha, n=user_n, type="two.sample"),
          t_paired = if (solve_for=="n")
                       pwr.t.test(d=es_val$d, sig.level=alpha, power=target_pwr, type="paired")
                     else
                       pwr.t.test(d=es_val$d, sig.level=alpha, n=user_n, type="paired"),
          anova    = if (solve_for=="n")
                       pwr.anova.test(k=k, f=es_val$f, sig.level=alpha, power=target_pwr)
                     else
                       pwr.anova.test(k=k, f=es_val$f, sig.level=alpha, n=user_n),
          reg      = if (solve_for=="n")
                       pwr.f2.test(u=k, f2=es_val$f2, sig.level=alpha, power=target_pwr)
                     else
                       pwr.f2.test(u=k, f2=es_val$f2, sig.level=alpha),
          chisq    = if (solve_for=="n")
                       pwr.chisq.test(w=es_val$w, df=k-1, sig.level=alpha, power=target_pwr)
                     else
                       pwr.chisq.test(w=es_val$w, df=k-1, sig.level=alpha, N=user_n),
          corr     = if (solve_for=="n")
                       pwr.r.test(r=es_val$r, sig.level=alpha, power=target_pwr)
                     else
                       pwr.r.test(r=es_val$r, sig.level=alpha, n=user_n),
          {
            # logistic / sem — use Cohen's f² approximation
            if (solve_for=="n")
              pwr.f2.test(u=max(1L,k-1L), f2=es_val$f2, sig.level=alpha, power=target_pwr)
            else
              pwr.f2.test(u=max(1L,k-1L), f2=es_val$f2, sig.level=alpha)
          }
        )

        # Extract results
        n_result    <- if (!is.null(res$n))  ceiling(res$n)  else if (!is.null(res$N)) ceiling(res$N) else NA
        pwr_result  <- round(res$power %||% NA, 3)
        effect_used <- es_val$d %||% es_val$f %||% es_val$f2

        # Power curve data
        n_range <- seq(20, max(500, (n_result %||% 200)*1.5), by=10)
        curve_pwr <- tryCatch({
          sapply(n_range, function(n_i) {
            r2 <- tryCatch(switch(test,
              t_one    = pwr.t.test(d=es_val$d, sig.level=alpha, n=n_i, type="one.sample"),
              t_two    = pwr.t.test(d=es_val$d, sig.level=alpha, n=n_i, type="two.sample"),
              t_paired = pwr.t.test(d=es_val$d, sig.level=alpha, n=n_i, type="paired"),
              anova    = pwr.anova.test(k=k, f=es_val$f, sig.level=alpha, n=n_i),
              reg      = pwr.f2.test(u=k, f2=es_val$f2, sig.level=alpha, v=n_i-k-1),
              corr     = pwr.r.test(r=es_val$r, sig.level=alpha, n=n_i),
              pwr.t.test(d=es_val$d, sig.level=alpha, n=n_i, type="two.sample")
            ), error=function(e) NULL)
            if (is.null(r2)) NA else round(r2$power, 3)
          })
        }, error=function(e) rep(NA, length(n_range)))

        pwr_result_rv(list(
          n=n_result, power=pwr_result, alpha=alpha,
          solve_for=solve_for, test=test,
          n_range=n_range, curve_pwr=curve_pwr,
          effect_val=effect_used,
          effect_label=input$pwr_effect,
          raw=res
        ))
      }, error=function(e) {
        showNotification(paste("Power analysis error:", e$message), type="error", duration=8)
      })
    })

    output$pwr_results_ui <- renderUI({
      r <- pwr_result_rv()
      if (is.null(r)) {
        return(tags$div(style="padding:40px;text-align:center;color:#aaa;",
                        "Power analysis results will appear here."))
      }
      adequacy <- if (!is.na(r$power)) {
        if (r$power >= 0.80) list(label="Adequate Power ≥ 80%", color="#28a745")
        else if (r$power >= 0.70) list(label="Marginal Power (70–79%)", color="#ffc107")
        else list(label="Insufficient Power < 70%", color="#dc3545")
      } else list(label="Power computed", color="#6c757d")

      tagList(
        fluidRow(
          column(4, tags$div(style=sprintf("background:%s;color:white;border-radius:8px;padding:12px;text-align:center;", adequacy$color),
                             tags$h3(style="margin:0;", if (r$solve_for=="n") paste0("N = ", r$n %||% "—") else paste0(round((r$power %||% 0)*100), "%")),
                             tags$p(style="margin:0;font-size:.78rem;", if (r$solve_for=="n") "Minimum sample size" else "Achieved power"))),
          column(4, tags$div(style="background:#e3f2fd;border-radius:8px;padding:12px;text-align:center;",
                             tags$h4(style="margin:0;color:#1565c0;", paste0(round((r$power %||% 0)*100), "%")),
                             tags$p(style="margin:0;font-size:.78rem;", "Statistical Power (1−β)"))),
          column(4, tags$div(style="background:#f3e5f5;border-radius:8px;padding:12px;text-align:center;",
                             tags$h4(style="margin:0;color:#6a1b9a;", adequacy$label),
                             tags$p(style="margin:0;font-size:.78rem;", sprintf("α = %.3f | effect = %s", r$alpha, r$effect_label))))
        ),
        br(),
        tags$div(style="background:white;border:1px solid #ddd;border-radius:8px;padding:12px;",
          tags$h6(style="color:#1A3A5C;margin-top:0;", "📈 Power Curve"),
          plotlyOutput(ns("pwr_curve_plot"), height="280px")
        ),
        br(),
        if (!is.na(r$n) && r$solve_for == "n")
          result_card(bg="#e8f5e9", border="#81c784",
            tags$p(style="font-size:.82rem;margin:0;",
                   tags$b("📝 APA Reporting: "),
                   sprintf("A priori power analysis (α = %.2f, power = %.2f, effect size = %s) ",
                           r$alpha, r$power %||% 0.80, r$effect_label),
                   sprintf("indicated a minimum sample size of N = %d.", r$n %||% 0))
          )
      )
    })

    output$pwr_curve_plot <- renderPlotly({
      r <- pwr_result_rv(); req(r)
      df_curve <- data.frame(n=r$n_range, power=r$curve_pwr)
      df_curve <- df_curve[!is.na(df_curve$power), ]
      if (nrow(df_curve)==0) return(plot_ly() |> layout(title="Insufficient data for curve"))
      plot_ly(df_curve, x=~n, y=~power, type="scatter", mode="lines",
              line=list(color="#2196A6", width=2.5),
              name="Power") |>
        add_segments(x=min(df_curve$n), xend=max(df_curve$n),
                     y=0.80, yend=0.80,
                     line=list(color="#e74c3c", dash="dash", width=1.5),
                     name="80% threshold") |>
        layout(
          xaxis=list(title="Sample Size (N)"),
          yaxis=list(title="Power (1−β)", range=c(0,1)),
          hovermode="x unified",
          showlegend=TRUE
        )
    })

  })
}
