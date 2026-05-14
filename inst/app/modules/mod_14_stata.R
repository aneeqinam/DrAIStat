# =============================================================
# mod_14_stata.R — STATA-Style Econometric Methods
# Panel Data · Survival · Limited DV · Quantile Regression
# Causal Inference (DiD, RDD) · Time Series (ARIMA/VAR) · IV/2SLS
#
# Dr. Aneeq Inam —
# =============================================================

mod14_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon  = "📈",
      title = "STATA-Style Econometric Methods",
      subtitle = "Panel Data (xtreg) · Survival (stcox/sts) · Tobit/Probit/Logit · Quantile Regression (qreg) · DiD · RDD · IV/2SLS · ARIMA/VAR"
    ),
    uiOutput(ns("global_data_banner")),
    fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV / Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui"))
  )
}

mod14_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Data ───────────────────────────────────────────────────
    data_rv <- reactiveVal(NULL)

    output$global_data_banner <- renderUI({
      gd <- global_shared_data()
      if (is.null(gd)) return(NULL)
      tags$div(
        style = paste0("background:#e6f4ea; border:1px solid #81c784; border-radius:7px;",
                       " padding:8px 14px; margin-bottom:8px;",
                       " display:flex; align-items:center; justify-content:space-between;"),
        tags$span(style="color:#1b5e20; font-size:.82rem;",
          tags$b("📋 Global dataset ready: "),
          sprintf("%s  (%d rows × %d cols)", global_shared_name(), nrow(gd), ncol(gd))
        ),
        actionButton(ns("load_global"), "⬇ Use This Dataset",
                     class="btn-success btn-sm",
                     style="padding:3px 10px; font-size:.78rem;")
      )
    })

    observeEvent(input$load_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) {
        data_rv(gd)
        showNotification(paste0("Using: ", global_shared_name()), type="message", duration=3)
      }
    })

    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        withProgress(message = "Loading data...", {
          data_rv(read_uploaded(input$file$datapath, input$file$name))
        })
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })

    output$file_info <- renderUI({
      d <- data_rv(); if (is.null(d)) return(NULL)
      tags$div(style = "background:#f5f5f5;padding:10px;border-radius:4px;",
        tags$strong(paste0("Loaded: ", nrow(d), " rows × ", ncol(d),
          " columns | Variables: ", paste(head(names(d), 8), collapse = ", "),
          if (ncol(d) > 8) "…" else "")))
    })

    all_cols <- reactive({ req(data_rv()); names(data_rv()) })
    num_cols <- reactive({ req(data_rv()); numeric_cols(data_rv()) })

    # ══════════════════════════════════════════════════════════
    #  MAIN UI
    # ══════════════════════════════════════════════════════════
    output$main_ui <- renderUI({
      req(data_rv())
      tabBox(width = 12, title = "STATA-Style Econometric Methods",

        # ── 1. Panel Data ─────────────────────────────────────
        tabPanel("🗂 Panel Data",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("xtset id time"), " + ", code("xtreg y x*, fe/re"),
            " — Fixed Effects removes entity-level unobservables; Hausman test selects FE vs RE."),
          fluidRow(
            column(3, pickerInput(ns("pd_dv"),   "Dependent Variable (Y)", choices = num_cols())),
            column(3, pickerInput(ns("pd_ivs"),  "Independent Variables",  choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, pickerInput(ns("pd_id"),   "Entity ID (unit/firm/person)", choices = all_cols())),
            column(3, pickerInput(ns("pd_time"), "Time Variable",          choices = all_cols()))
          ),
          fluidRow(
            column(3, selectInput(ns("pd_model"), "Model Type",
              choices = c("Fixed Effects (FE)" = "within", "Random Effects (RE)" = "random",
                "Both + Hausman Test" = "both", "Pooled OLS" = "pooling",
                "Between Effects" = "between", "First Differences" = "fd"))),
            column(3, selectInput(ns("pd_se"), "Standard Errors",
              choices = c("Standard" = "std", "Arellano (cluster-robust)" = "arellano",
                "White HC1" = "white1", "White HC2" = "white2"))),
            column(3, br(), actionButton(ns("run_pd"), "▶ Run Panel Analysis",
              class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("pd_results_ui"))
        ),

        # ── 2. Survival Analysis ──────────────────────────────
        tabPanel("⏱ Survival Analysis",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("stset time, failure(event)"), " · ",
            code("sts graph"), " (Kaplan-Meier) · ", code("stcox covariates"), " (Cox PH)"),
          fluidRow(
            column(3, pickerInput(ns("sa_time"),  "Time Variable (duration)",           choices = num_cols())),
            column(3, pickerInput(ns("sa_event"), "Event Indicator (1=event, 0=cens.)", choices = all_cols())),
            column(3, pickerInput(ns("sa_group"), "Group Variable (KM stratification)", choices = c("None", all_cols()))),
            column(3, pickerInput(ns("sa_covs"),  "Cox PH Covariates",                  choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(3, selectInput(ns("sa_type"), "Analysis",
              choices = c("Kaplan-Meier" = "km", "Cox Proportional Hazards" = "cox", "Both" = "both"))),
            column(3, br(), actionButton(ns("run_sa"), "▶ Run Survival Analysis",
              class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("sa_results_ui"))
        ),

        # ── 3. Limited Dependent Variables ────────────────────
        tabPanel("🎯 Limited DV",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("probit"), " · ", code("logit"), " · ",
            code("tobit"), " · ", code("ologit"), " · ", code("mlogit"),
            " — Includes Average Marginal Effects (AME) like STATA's ", code("margins, dydx(*)")),
          fluidRow(
            column(3, pickerInput(ns("ldv_dv"),  "Dependent Variable (Y)", choices = all_cols())),
            column(3, pickerInput(ns("ldv_ivs"), "Independent Variables",  choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, selectInput(ns("ldv_type"), "Model Type",
              choices = c("Logit (binary DV)" = "logit", "Probit (binary DV)" = "probit",
                "Tobit (censored DV)" = "tobit", "Ordered Logit (ordinal DV)" = "ologit",
                "Multinomial Logit (nominal DV)" = "mlogit"))),
            column(3, numericInput(ns("ldv_tobit_lo"), "Tobit lower limit (censoring)", value = 0))
          ),
          fluidRow(
            column(4, checkboxInput(ns("ldv_margins"), "Compute Average Marginal Effects (AME)", value = TRUE)),
            column(4, br(), actionButton(ns("run_ldv"), "▶ Run Model", class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("ldv_results_ui"))
        ),

        # ── 4. Quantile Regression ────────────────────────────
        tabPanel("📐 Quantile Regression",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("qreg y x*, quantile(.25/.50/.75)"),
            " — Estimates effects at multiple quantiles of Y, not just the conditional mean."),
          fluidRow(
            column(3, pickerInput(ns("qr_dv"),  "Dependent Variable (Y)", choices = num_cols())),
            column(5, pickerInput(ns("qr_ivs"), "Independent Variables",  choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, sliderInput(ns("qr_taus"), "Quantile range",
              min = 0.05, max = 0.95, value = c(0.25, 0.75), step = 0.05))
          ),
          fluidRow(
            column(4, checkboxInput(ns("qr_all"), "Estimate all 9 quantiles (0.1–0.9)", value = FALSE)),
            column(4, br(), actionButton(ns("run_qr"), "▶ Run Quantile Regression",
              class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("qr_results_ui"))
        ),

        # ── 5. Causal Inference ───────────────────────────────
        tabPanel("⚖ Causal Inference",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "),
            code("diff outcome, treated(D) period(T)"), " (DiD) · ",
            code("rdrobust outcome running, c(threshold)"), " (RDD)"),
          fluidRow(
            column(3, selectInput(ns("ci_method"), "Method",
              choices = c("Difference-in-Differences (DiD)" = "did",
                "Regression Discontinuity (RDD)" = "rdd"))),
            column(9, uiOutput(ns("ci_inputs_ui")))
          ),
          fluidRow(column(3, br(),
            actionButton(ns("run_ci"), "▶ Run Analysis", class = "btn-primary btn-block"))),
          hr(),
          uiOutput(ns("ci_results_ui"))
        ),

        # ── 6. Time Series ────────────────────────────────────
        tabPanel("📉 Time Series",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("dfuller/pperron"), " (unit root) · ",
            code("arima"), " · ", code("var"), " (VAR) · ", code("irf"), " (Impulse Response)"),
          fluidRow(
            column(3, pickerInput(ns("ts_var"), "Primary Variable", choices = num_cols())),
            column(3, pickerInput(ns("ts_vars_multi"), "Variables for VAR", choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, selectInput(ns("ts_type"), "Analysis",
              choices = c("Unit Root Tests (ADF+PP)" = "unitroot",
                "ARIMA (auto-select)" = "arima",
                "ARIMA (manual orders)" = "arima_manual",
                "VAR (Vector Autoregression)" = "var",
                "Impulse Response Functions" = "irf"))),
            column(3, numericInput(ns("ts_freq"), "Frequency (12=monthly, 4=quarterly)", value = 12, min = 1))
          ),
          fluidRow(
            column(2, numericInput(ns("ts_p"), "AR(p)", value = 1, min = 0)),
            column(2, numericInput(ns("ts_d"), "I(d)",  value = 1, min = 0)),
            column(2, numericInput(ns("ts_q"), "MA(q)", value = 1, min = 0)),
            column(2, numericInput(ns("ts_lag"), "Max Lags (VAR)", value = 4, min = 1)),
            column(4, br(), actionButton(ns("run_ts"), "▶ Run Time Series",
              class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("ts_results_ui"))
        ),

        # ── 7. IV / 2SLS ──────────────────────────────────────
        tabPanel("🔧 IV / 2SLS",
          tags$div(class = "alert alert-info", style = "font-size:.80rem;margin-bottom:.8rem;",
            tags$b("STATA: "), code("ivregress 2sls y exog (endog = instruments)"),
            " — Wu-Hausman test for endogeneity, weak-instrument F-test, Sargan over-ID test."),
          fluidRow(
            column(3, pickerInput(ns("iv_dv"),    "Dependent Variable (Y)", choices = num_cols())),
            column(3, pickerInput(ns("iv_exog"),  "Exogenous Regressors",   choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, pickerInput(ns("iv_endog"), "Endogenous Regressor(s)", choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(3, pickerInput(ns("iv_instr"), "Instruments (excluded)", choices = num_cols(),
              multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(3, selectInput(ns("iv_method"), "Estimator",
              choices = c("2SLS" = "2sls", "GMM" = "gmm", "LIML" = "liml"))),
            column(3, br(), actionButton(ns("run_iv"), "▶ Run IV Estimation",
              class = "btn-primary btn-block"))
          ),
          hr(),
          uiOutput(ns("iv_results_ui"))
        ),

        # ── 8. AI Interpret ───────────────────────────────────
        tabPanel("🤖 AI Interpret",
          tags$div(style = "padding:.8rem 0;",
            tags$div(
              style = "background:#EFF8FF;border-left:4px solid #2196A6;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;font-size:.85rem;",
              tags$b("How to use:"), " Run any analysis, then click its AI button for a detailed academic interpretation.", tags$br(),
              tags$a("🔑 FREE Groq key → console.groq.com/keys",
                href = "https://console.groq.com/keys", target = "_blank",
                style = "color:#2196A6;font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_pd"),  "🤖 Interpret Panel Data",      class = "btn-default btn-block")),
              column(4, actionButton(ns("ai_sa"),  "🤖 Interpret Survival",         class = "btn-default btn-block")),
              column(4, actionButton(ns("ai_ldv"), "🤖 Interpret Limited DV",       class = "btn-default btn-block"))
            ),
            tags$br(),
            fluidRow(
              column(4, actionButton(ns("ai_qr"),  "🤖 Interpret Quantile Reg",    class = "btn-default btn-block")),
              column(4, actionButton(ns("ai_ci"),  "🤖 Interpret Causal Inference", class = "btn-default btn-block")),
              column(4, actionButton(ns("ai_ts"),  "🤖 Interpret Time Series",     class = "btn-default btn-block"))
            ),
            tags$br(),
            fluidRow(
              column(4, actionButton(ns("ai_iv"),  "🤖 Interpret IV / 2SLS",       class = "btn-default btn-block"))
            ),
            hr(),
            uiOutput(ns("ai_status_ui")),
            withSpinner(verbatimTextOutput(ns("ai_out")))
          )
        ),

        # ── 9. Download ───────────────────────────────────────
        tabPanel("💾 Download",
          tags$div(style = "padding:1.5rem 0;",
            tags$p(style = "color:#555;font-size:.86rem;",
              "Download all STATA-style results as an Excel workbook."),
            downloadButton(ns("dl_excel"), "📥 Download All Results (Excel)",
              class = "btn-success btn-lg")
          )
        )

      ) # /tabBox
    }) # /main_ui


    # ══════════════════════════════════════════════════════════
    #  PANEL DATA
    # ══════════════════════════════════════════════════════════
    pd_res <- reactiveVal(NULL)

    observeEvent(input$run_pd, {
      req(input$pd_dv, input$pd_ivs, input$pd_id, input$pd_time)
      d <- data_rv()
      withProgress(message = "Running panel analysis...", {
        tryCatch({
          if (!requireNamespace("plm",    quietly = TRUE)) install.packages("plm")
          if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
          library(plm); library(lmtest)

          fmla   <- as.formula(paste(input$pd_dv, "~", paste(input$pd_ivs, collapse = "+")))
          pdata  <- plm::pdata.frame(d, index = c(input$pd_id, input$pd_time))
          se_tp  <- input$pd_se

          run_plm <- function(mtype) {
            m  <- plm::plm(fmla, data = pdata, model = mtype)
            vc <- switch(se_tp,
              arellano = plm::vcovHC(m, type = "arellano", cluster = "group"),
              white1   = plm::vcovHC(m, type = "HC1"),
              white2   = plm::vcovHC(m, type = "HC2"),
              NULL)
            list(model = m, vcov = vc)
          }

          if (input$pd_model == "both") {
            fe  <- run_plm("within"); re <- run_plm("random")
            ht  <- tryCatch(plm::phtest(fe$model, re$model), error = function(e) NULL)
            pd_res(list(type = "both", fe = fe, re = re, hausman = ht,
              dv = input$pd_dv, ivs = input$pd_ivs))
          } else {
            res <- run_plm(input$pd_model)
            pd_res(list(type = input$pd_model, model = res$model, vcov = res$vcov,
              dv = input$pd_dv, ivs = input$pd_ivs))
          }
          showNotification("✅ Panel analysis complete!", type = "message")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      })
    })

    output$pd_results_ui <- renderUI({
      res <- pd_res()
      if (is.null(res)) return(tags$p(style = "color:#888;", "Run panel analysis to see results."))
      tagList(
        fluidRow(
          column(12, h5("📋 Coefficient Table")),
          column(12, DT::dataTableOutput(ns("pd_coef_tbl")))
        ),
        hr(),
        fluidRow(
          column(6, h5("📊 Model Summary"), verbatimTextOutput(ns("pd_summary"))),
          column(6, h5("📈 Coefficient Plot"), withSpinner(plotlyOutput(ns("pd_coef_plot"), height = "350px")))
        ),
        if (!is.null(res$hausman)) tagList(
          hr(),
          fluidRow(column(12, h5("🔍 Hausman Test (FE vs RE)"), verbatimTextOutput(ns("pd_hausman"))))
        )
      )
    })

    output$pd_coef_tbl <- DT::renderDataTable({
      res <- pd_res(); req(res)
      make_df <- function(m, vc, nm) {
        ct <- if (!is.null(vc)) lmtest::coeftest(m, vcov = vc) else lmtest::coeftest(m)
        data.frame(Model = nm, Variable = rownames(ct),
          Coefficient = round(ct[,1],4), Std_Error = round(ct[,2],4),
          t_stat = round(ct[,3],3), p_value = round(ct[,4],4),
          Sig = ifelse(ct[,4]<.001,"***",ifelse(ct[,4]<.01,"**",ifelse(ct[,4]<.05,"*",ifelse(ct[,4]<.1,".","")))),
          stringsAsFactors = FALSE)
      }
      if (res$type == "both") {
        out <- rbind(make_df(res$fe$model, res$fe$vcov, "FE"),
                     make_df(res$re$model, res$re$vcov, "RE"))
      } else {
        out <- make_df(res$model, res$vcov, toupper(res$type))
      }
      tool_dt(out, "Panel Data Results")
    })

    output$pd_summary <- renderPrint({
      res <- pd_res(); req(res)
      if (res$type == "both") {
        cat("=== FIXED EFFECTS ===\n"); print(summary(res$fe$model))
        cat("\n=== RANDOM EFFECTS ===\n"); print(summary(res$re$model))
      } else print(summary(res$model))
    })

    output$pd_hausman <- renderPrint({
      res <- pd_res(); req(res, res$hausman)
      print(res$hausman)
      p <- res$hausman$p.value
      cat("\n─── Interpretation ───\n")
      cat(sprintf("Chi² = %.3f, p = %.4f\n", res$hausman$statistic, p))
      if (p < 0.05) cat("✅ H0 rejected → Fixed Effects preferred.\n")
      else          cat("ℹ️  Fail to reject H0 → Random Effects is efficient.\n")
    })

    output$pd_coef_plot <- renderPlotly({
      res <- pd_res(); req(res)
      get_ci <- function(m, vc) {
        ct <- if (!is.null(vc)) lmtest::coeftest(m, vcov = vc) else lmtest::coeftest(m)
        data.frame(var = rownames(ct), coef = ct[,1], se = ct[,2],
          lo = ct[,1]-1.96*ct[,2], hi = ct[,1]+1.96*ct[,2], stringsAsFactors = FALSE)
      }
      if (res$type == "both") {
        fe_d <- get_ci(res$fe$model, res$fe$vcov); fe_d$model <- "FE"
        re_d <- get_ci(res$re$model, res$re$vcov); re_d$model <- "RE"
        df <- rbind(fe_d, re_d)
      } else {
        df <- get_ci(res$model, res$vcov); df$model <- toupper(res$type)
      }
      plotly::plot_ly(df, x = ~coef, y = ~var, color = ~model,
        colors = c("#2196A6","#E74C3C"), type = "scatter", mode = "markers",
        marker = list(size = 10),
        error_x = list(type="data", symmetric=FALSE, array=~(hi-coef), arrayminus=~(coef-lo))) |>
        plotly::add_segments(x=0,xend=0,y=0,yend=nrow(df)+1,line=list(color="gray",dash="dot"),showlegend=FALSE) |>
        plotly::layout(title = "Coefficients ± 95% CI",
          xaxis = list(title = "Coefficient"), yaxis = list(title = "", autorange = "reversed"))
    })


    # ══════════════════════════════════════════════════════════
    #  SURVIVAL ANALYSIS
    # ══════════════════════════════════════════════════════════
    sa_res <- reactiveVal(NULL)

    observeEvent(input$run_sa, {
      req(input$sa_time, input$sa_event)
      d <- data_rv()
      withProgress(message = "Running survival analysis...", {
        tryCatch({
          if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
          t_var <- input$sa_time; e_var <- input$sa_event
          g_var <- if (input$sa_group == "None") NULL else input$sa_group
          covs  <- input$sa_covs
          d[[e_var]] <- as.numeric(d[[e_var]])
          surv_obj <- survival::Surv(d[[t_var]], d[[e_var]])

          km_fit <- NULL; cox_fit <- NULL
          if (input$sa_type %in% c("km","both")) {
            fmla <- if (!is.null(g_var)) as.formula(paste("surv_obj ~", g_var)) else surv_obj ~ 1
            km_fit <- survival::survfit(fmla, data = d)
          }
          if (input$sa_type %in% c("cox","both") && length(covs) > 0) {
            cox_fit <- survival::coxph(
              as.formula(paste("surv_obj ~", paste(covs, collapse = "+"))), data = d)
          }
          sa_res(list(km = km_fit, cox = cox_fit, d = d,
            t_var = t_var, e_var = e_var, g_var = g_var, covs = covs, type = input$sa_type))
          showNotification("✅ Survival analysis complete!", type = "message")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      })
    })

    output$sa_results_ui <- renderUI({
      res <- sa_res()
      if (is.null(res)) return(tags$p(style = "color:#888;", "Run survival analysis to see results."))
      tagList(
        if (!is.null(res$km)) tagList(
          fluidRow(column(12, h5("📈 Kaplan-Meier Survival Curve"),
            withSpinner(plotlyOutput(ns("sa_km_plot"), height = "420px")))),
          hr(),
          fluidRow(column(12, h5("📋 KM Survival Table"), DT::dataTableOutput(ns("sa_km_tbl"))))
        ),
        if (!is.null(res$cox)) tagList(
          hr(),
          fluidRow(
            column(7, h5("🔬 Cox PH Coefficients"), DT::dataTableOutput(ns("sa_cox_tbl"))),
            column(5, h5("Forest Plot"), withSpinner(plotlyOutput(ns("sa_forest_plot"), height = "350px")))
          ),
          hr(),
          fluidRow(column(12, h5("📄 Cox PH Summary"), verbatimTextOutput(ns("sa_cox_summary"))))
        )
      )
    })

    output$sa_km_plot <- renderPlotly({
      res <- sa_res(); req(res, res$km)
      sm  <- summary(res$km)
      df  <- data.frame(time = sm$time, surv = sm$surv, lower = sm$lower, upper = sm$upper,
        strata = if (is.null(sm$strata)) "Overall" else as.character(sm$strata))
      plotly::plot_ly(df, x = ~time, y = ~surv, color = ~strata,
        type = "scatter", mode = "lines", line = list(width = 2.5)) |>
        plotly::add_ribbons(ymin = ~lower, ymax = ~upper, opacity = 0.15, showlegend = FALSE) |>
        plotly::layout(title = "Kaplan-Meier Survival Curve",
          xaxis = list(title = "Time"),
          yaxis = list(title = "Survival Probability", range = c(0, 1)),
          hovermode = "x unified")
    })

    output$sa_km_tbl <- DT::renderDataTable({
      res <- sa_res(); req(res, res$km)
      km_times <- tryCatch(quantile(res$km$time, probs = seq(0.1, 1, 0.1), na.rm = TRUE),
        error = function(e) NULL)
      sm  <- if (!is.null(km_times)) summary(res$km, times = km_times) else summary(res$km)
      df  <- data.frame(Time = round(sm$time,2), Survival = round(sm$surv,4),
        Std_Err = round(sm$std.err,4), CI_Lower = round(sm$lower,4), CI_Upper = round(sm$upper,4),
        n_risk = sm$n.risk, n_event = sm$n.event)
      if (!is.null(sm$strata)) df$Group <- as.character(sm$strata)
      tool_dt(df, "Kaplan-Meier Survival Table")
    })

    output$sa_cox_tbl <- DT::renderDataTable({
      res <- sa_res(); req(res, res$cox)
      sm <- summary(res$cox); cf <- sm$coefficients; ci <- sm$conf.int
      out <- data.frame(Variable = rownames(cf),
        log_HR = round(cf[,"coef"],4), HR = round(exp(cf[,"coef"]),4),
        SE = round(cf[,"se(coef)"],4), z = round(cf[,"z"],3),
        p_value = round(cf[,"Pr(>|z|)"],4),
        Sig = ifelse(cf[,"Pr(>|z|)"]<.001,"***",ifelse(cf[,"Pr(>|z|)"]<.01,"**",
          ifelse(cf[,"Pr(>|z|)"]<.05,"*",ifelse(cf[,"Pr(>|z|)"]<.1,".","")))),
        HR_CI_lo = round(ci[,"lower .95"],4), HR_CI_hi = round(ci[,"upper .95"],4))
      tool_dt(out, "Cox PH Hazard Ratios (95% CI)")
    })

    output$sa_forest_plot <- renderPlotly({
      res <- sa_res(); req(res, res$cox)
      sm <- summary(res$cox); cf <- sm$coefficients; ci <- sm$conf.int
      df <- data.frame(var = rownames(cf), hr = exp(cf[,1]),
        lo = ci[,"lower .95"], hi = ci[,"upper .95"], p = cf[,"Pr(>|z|)"])
      df$color <- ifelse(df$p < 0.05, "Significant", "NS")
      plotly::plot_ly(df, x = ~hr, y = ~var, color = ~color,
        colors = c(Significant = "#E74C3C", NS = "#95A5A6"),
        type = "scatter", mode = "markers", marker = list(size = 12, symbol = "diamond"),
        error_x = list(type="data", symmetric=FALSE, array=~(hi-hr), arrayminus=~(hr-lo))) |>
        plotly::add_segments(x=1,xend=1,y=0,yend=nrow(df)+1,line=list(color="gray",dash="dot"),showlegend=FALSE) |>
        plotly::layout(title = "Forest Plot — Hazard Ratios",
          xaxis = list(title = "Hazard Ratio (HR)", type = "log"),
          yaxis = list(title = "", autorange = "reversed"))
    })

    output$sa_cox_summary <- renderPrint({
      res <- sa_res(); req(res, res$cox); print(summary(res$cox))
    })


    # ══════════════════════════════════════════════════════════
    #  LIMITED DEPENDENT VARIABLES
    # ══════════════════════════════════════════════════════════
    ldv_res <- reactiveVal(NULL)

    observeEvent(input$run_ldv, {
      req(input$ldv_dv, input$ldv_ivs)
      d <- data_rv()
      withProgress(message = "Running LDV model...", {
        tryCatch({
          dv <- input$ldv_dv; ivs <- input$ldv_ivs; type <- input$ldv_type
          fmla <- as.formula(paste(dv, "~", paste(ivs, collapse = "+")))
          result <- list(type = type, dv = dv, ivs = ivs)

          if (type == "logit") {
            d[[dv]] <- as.numeric(as.factor(d[[dv]])) - 1
            m <- glm(fmla, data = d, family = binomial(link = "logit"))
            result$model <- m
          } else if (type == "probit") {
            d[[dv]] <- as.numeric(as.factor(d[[dv]])) - 1
            m <- glm(fmla, data = d, family = binomial(link = "probit"))
            result$model <- m
          } else if (type == "tobit") {
            if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
            m <- AER::tobit(fmla, data = d, left = input$ldv_tobit_lo)
            result$model <- m
          } else if (type == "ologit") {
            if (!requireNamespace("MASS",   quietly = TRUE)) install.packages("MASS")
            if (!requireNamespace("lmtest", quietly = TRUE)) install.packages("lmtest")
            d[[dv]] <- factor(d[[dv]], ordered = TRUE)
            m <- MASS::polr(fmla, data = d, method = "logistic", Hess = TRUE)
            result$model <- m
          } else if (type == "mlogit") {
            if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")
            d[[dv]] <- factor(d[[dv]])
            m <- nnet::multinom(fmla, data = d, trace = FALSE)
            result$model <- m
          }

          # Average marginal effects via margins package
          if (input$ldv_margins && type %in% c("logit","probit") &&
              requireNamespace("margins", quietly = TRUE)) {
            result$ame <- tryCatch(summary(margins::margins(m)), error = function(e) NULL)
          }

          ldv_res(result)
          showNotification("✅ Model complete!", type = "message")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      })
    })

    output$ldv_results_ui <- renderUI({
      res <- ldv_res()
      if (is.null(res)) return(tags$p(style = "color:#888;", "Run a model to see results."))
      tagList(
        fluidRow(
          column(7, h5("📋 Coefficients"), DT::dataTableOutput(ns("ldv_coef_tbl"))),
          column(5, h5("📈 Effects Plot"), withSpinner(plotlyOutput(ns("ldv_effects_plot"), height = "350px")))
        ),
        if (!is.null(res$ame)) tagList(
          hr(),
          fluidRow(column(12, h5("📊 Average Marginal Effects (STATA: margins, dydx(*))"),
            DT::dataTableOutput(ns("ldv_ame_tbl"))))
        ),
        hr(),
        fluidRow(column(12, h5("📄 Model Summary"), verbatimTextOutput(ns("ldv_summary"))))
      )
    })

    output$ldv_coef_tbl <- DT::renderDataTable({
      res <- ldv_res(); req(res)
      tryCatch({
        m <- res$model; type <- res$type
        if (type %in% c("logit","probit")) {
          sm <- summary(m)$coefficients
          out <- data.frame(Variable = rownames(sm), Coefficient = round(sm[,1],4),
            Std_Error = round(sm[,2],4), z_stat = round(sm[,3],3),
            p_value = round(sm[,4],4),
            Sig = ifelse(sm[,4]<.001,"***",ifelse(sm[,4]<.01,"**",ifelse(sm[,4]<.05,"*",ifelse(sm[,4]<.1,".","")))),
            stringsAsFactors = FALSE)
          if (type == "logit") out$Odds_Ratio <- round(exp(sm[,1]),4)
        } else if (type == "tobit") {
          sm <- summary(m)$coefficients
          out <- data.frame(Variable = rownames(sm), Coefficient = round(sm[,1],4),
            Std_Error = round(sm[,2],4), t_stat = round(sm[,3],3),
            p_value = round(sm[,4],4),
            Sig = ifelse(sm[,4]<.001,"***",ifelse(sm[,4]<.01,"**",ifelse(sm[,4]<.05,"*",ifelse(sm[,4]<.1,".","")))),
            stringsAsFactors = FALSE)
        } else if (type == "ologit") {
          ct <- tryCatch(lmtest::coeftest(m), error = function(e) NULL)
          if (!is.null(ct)) {
            out <- data.frame(Variable = rownames(ct), Coef = round(ct[,1],4),
              SE = round(ct[,2],4), z = round(ct[,3],3), p = round(ct[,4],4),
              Sig = ifelse(ct[,4]<.001,"***",ifelse(ct[,4]<.01,"**",ifelse(ct[,4]<.05,"*",ifelse(ct[,4]<.1,".","")))),
              OR = round(exp(ct[,1]),4), stringsAsFactors = FALSE)
          } else out <- as.data.frame(summary(m)$coefficients)
        } else {
          sm <- summary(m)$coefficients
          out <- as.data.frame(round(sm, 4))
          out$Variable <- rownames(sm)
        }
        tool_dt(out, paste(toupper(type), "Coefficients"))
      }, error = function(e) data.frame(Error = e$message))
    }, server = FALSE)

    output$ldv_ame_tbl <- DT::renderDataTable({
      res <- ldv_res(); req(res, res$ame)
      ame <- as.data.frame(res$ame)
      cols <- intersect(c("factor","AME","SE","z","p","lower","upper"), names(ame))
      tool_dt(round(ame[, cols, drop = FALSE], 4), "Average Marginal Effects (AME)")
    })

    output$ldv_effects_plot <- renderPlotly({
      res <- ldv_res(); req(res)
      tryCatch({
        m <- res$model; type <- res$type
        if (type %in% c("logit","probit","tobit")) {
          sm <- summary(m)$coefficients
          vars <- setdiff(rownames(sm), "(Intercept)")
          sm <- sm[vars,,drop=FALSE]
          df <- data.frame(var = vars, coef = sm[,1], se = sm[,2],
            lo = sm[,1]-1.96*sm[,2], hi = sm[,1]+1.96*sm[,2])
          if (type == "logit") {
            df$coef_e <- exp(df$coef); df$lo_e <- exp(df$lo); df$hi_e <- exp(df$hi)
            plotly::plot_ly(df, x = ~coef_e, y = ~var, type = "scatter", mode = "markers",
              marker = list(size=10, color="#2196A6"),
              error_x = list(type="data", symmetric=FALSE, array=~(hi_e-coef_e), arrayminus=~(coef_e-lo_e))) |>
              plotly::add_segments(x=1,xend=1,y=0,yend=nrow(df)+1,line=list(color="gray",dash="dot"),showlegend=FALSE) |>
              plotly::layout(title="Odds Ratios (95% CI)",
                xaxis=list(title="Odds Ratio",type="log"), yaxis=list(title="",autorange="reversed"))
          } else {
            plotly::plot_ly(df, x = ~coef, y = ~var, type = "scatter", mode = "markers",
              marker = list(size=10, color="#2196A6"),
              error_x = list(type="data", symmetric=FALSE, array=~(hi-coef), arrayminus=~(coef-lo))) |>
              plotly::add_segments(x=0,xend=0,y=0,yend=nrow(df)+1,line=list(color="gray",dash="dot"),showlegend=FALSE) |>
              plotly::layout(title="Coefficients (95% CI)",
                xaxis=list(title="Coefficient"), yaxis=list(title="",autorange="reversed"))
          }
        } else {
          plotly::plot_ly() |> plotly::layout(title = "Plot not available for this model type.")
        }
      }, error = function(e) plotly::plot_ly() |> plotly::layout(title = paste("Error:", e$message)))
    })

    output$ldv_summary <- renderPrint({
      res <- ldv_res(); req(res); print(summary(res$model))
    })


    # ══════════════════════════════════════════════════════════
    #  QUANTILE REGRESSION
    # ══════════════════════════════════════════════════════════
    qr_res <- reactiveVal(NULL)

    observeEvent(input$run_qr, {
      req(input$qr_dv, input$qr_ivs)
      d <- data_rv()
      withProgress(message = "Running quantile regression...", {
        tryCatch({
          if (!requireNamespace("quantreg", quietly = TRUE)) install.packages("quantreg")
          fmla <- as.formula(paste(input$qr_dv, "~", paste(input$qr_ivs, collapse = "+")))
          taus <- if (input$qr_all) seq(0.1, 0.9, 0.1) else {
            lo <- input$qr_taus[1]; hi <- input$qr_taus[2]
            sort(unique(c(lo, 0.5, hi)))
          }
          models <- lapply(taus, function(tau) quantreg::rq(fmla, data = d, tau = tau))
          names(models) <- paste0("Q", taus*100)
          qr_res(list(models = models, taus = taus, dv = input$qr_dv, ivs = input$qr_ivs, d = d))
          showNotification("✅ Quantile regression complete!", type = "message")
        }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
      })
    })

    output$qr_results_ui <- renderUI({
      res <- qr_res()
      if (is.null(res)) return(tags$p(style = "color:#888;", "Run quantile regression to see results."))
      tagList(
        fluidRow(column(12, h5("📋 Coefficients Across Quantiles"), DT::dataTableOutput(ns("qr_coef_tbl")))),
        hr(),
        fluidRow(
          column(7, h5("📈 Quantile Process Plot (1st IV vs OLS)"),
            withSpinner(plotlyOutput(ns("qr_process_plot"), height = "400px"))),
          column(5, h5("📄 Summary"), verbatimTextOutput(ns("qr_summary")))
        )
      )
    })

    output$qr_coef_tbl <- DT::renderDataTable({
      res <- qr_res(); req(res)
      rows <- lapply(seq_along(res$models), function(i) {
        tau <- res$taus[i]; m <- res$models[[i]]
        sm  <- tryCatch(summary(m, se = "boot")$coefficients, error = function(e) NULL)
        if (is.null(sm)) return(NULL)
        data.frame(Quantile = tau, Variable = rownames(sm),
          Coefficient = round(sm[,1],4), Lower_CI = round(sm[,2],4),
          Upper_CI = round(sm[,3],4), p_value = round(sm[,4],4),
          Sig = ifelse(sm[,4]<.001,"***",ifelse(sm[,4]<.01,"**",ifelse(sm[,4]<.05,"*",ifelse(sm[,4]<.1,".","")))),
          stringsAsFactors = FALSE)
      })
      tool_dt(do.call(rbind, Filter(Negate(is.null), rows)), "Quantile Regression Coefficients")
    })

    output$qr_process_plot <- renderPlotly({
      res <- qr_res(); req(res)
      iv_show <- res$ivs[1]; taus <- res$taus
      rows <- lapply(seq_along(res$models), function(i) {
        sm <- tryCatch(summary(res$models[[i]], se = "boot")$coefficients, error = function(e) NULL)
        if (is.null(sm) || !iv_show %in% rownames(sm)) return(NULL)
        data.frame(tau = taus[i], coef = sm[iv_show,"coefficients"],
          lo = sm[iv_show,"lower bd"], hi = sm[iv_show,"upper bd"])
      })
      df <- do.call(rbind, Filter(Negate(is.null), rows))
      req(nrow(df) > 0)
      ols_coef <- tryCatch(
        coef(lm(as.formula(paste(res$dv,"~",paste(res$ivs,collapse="+"))), data=res$d))[[iv_show]],
        error = function(e) NA)
      p <- plotly::plot_ly(df, x = ~tau, y = ~coef, type = "scatter", mode = "lines+markers",
        line = list(color="#2196A6", width=2), marker = list(size=8, color="#2196A6"), name = iv_show) |>
        plotly::add_ribbons(ymin = ~lo, ymax = ~hi, fillcolor = "rgba(33,150,166,0.15)",
          line = list(width=0), name = "95% CI")
      if (!is.na(ols_coef))
        p <- plotly::add_segments(p, y = ols_coef, yend = ols_coef,
          x = min(taus), xend = max(taus), line = list(color="red",dash="dash"), name = "OLS")
      plotly::layout(p, title = paste("Quantile Process:", iv_show),
        xaxis = list(title = "Quantile (τ)"), yaxis = list(title = "Coefficient"))
    })

    output$qr_summary <- renderPrint({
      res <- qr_res(); req(res)
      cat("Quantile Regression | DV:", res$dv, "| IVs:", paste(res$ivs, collapse=", "), "\n")
      for (i in seq_along(res$models)) {
        cat(sprintf("\n--- τ = %.2f ---\n", res$taus[i]))
        tryCatch(print(summary(res$models[[i]])), error = function(e) cat(e$message, "\n"))
      }
    })


    # ══════════════════════════════════════════════════════════
    #  CAUSAL INFERENCE — DiD / RDD
    # ══════════════════════════════════════════════════════════
    output$ci_inputs_ui <- renderUI({
      method <- input$ci_method; ncols <- num_cols(); acols <- all_cols()
      if (method == "did") {
        fluidRow(
          column(3, pickerInput(ns("did_outcome"), "Outcome Variable",          choices = ncols)),
          column(3, pickerInput(ns("did_treat"),   "Treatment Indicator (0/1)",  choices = acols)),
          column(3, pickerInput(ns("did_post"),    "Post-Period Indicator (0/1)", choices = acols)),
          column(3, pickerInput(ns("did_covs"),    "Additional Covariates",       choices = ncols,
            multiple = TRUE, options = list(`actions-box` = TRUE)))
        )
      } else {
        fluidRow(
          column(3, pickerInput(ns("rdd_outcome"), "Outcome Variable",  choices = ncols)),
          column(3, pickerInput(ns("rdd_running"), "Running Variable",  choices = ncols)),
          column(3, numericInput(ns("rdd_cutoff"), "Cutoff / Threshold", value = 0)),
          column(3, selectInput(ns("rdd_kernel"),  "Kernel",
            choices = c("Triangular"="triangular","Epanechnikov"="epanechnikov","Uniform"="uniform")))
        )
      }
    })

    ci_res <- reactiveVal(NULL)

    observeEvent(input$run_ci, {
      req(input$ci_method)
      d <- data_rv()
      withProgress(message = "Running causal analysis...", {
        tryCatch({
          method <- input$ci_method
          if (method == "did") {
            req(input$did_outcome, input$did_treat, input$did_post)
            y <- input$did_outcome; treat <- input$did_treat; post <- input$did_post
            d[[treat]] <- as.numeric(d[[treat]]); d[[post]] <- as.numeric(d[[post]])
            d[["DID_interaction"]] <- d[[treat]] * d[[post]]
            extra_covs <- input$did_covs
            ivs_did <- c(treat, post, "DID_interaction",
              if (!is.null(extra_covs) && length(extra_covs) > 0) extra_covs)
            m  <- lm(as.formula(paste(y,"~",paste(ivs_did,collapse="+"))), data = d)
            ci_res(list(type="did", model=m, y=y, treat=treat, post=post, d=d))
          } else {
            req(input$rdd_outcome, input$rdd_running)
            if (!requireNamespace("rdrobust", quietly=TRUE)) install.packages("rdrobust")
            y_v <- d[[input$rdd_outcome]]; x_v <- d[[input$rdd_running]]
            rdd_m <- rdrobust::rdrobust(y_v, x_v, c = input$rdd_cutoff, kernel = input$rdd_kernel)
            ci_res(list(type="rdd", model=rdd_m, y_name=input$rdd_outcome,
              x_name=input$rdd_running, cutoff=input$rdd_cutoff, d=d))
          }
          showNotification("✅ Causal analysis complete!", type="message")
        }, error = function(e) showNotification(paste("Error:", e$message), type="error"))
      })
    })

    output$ci_results_ui <- renderUI({
      res <- ci_res()
      if (is.null(res)) return(tags$p(style="color:#888;","Run analysis to see results."))
      if (res$type == "did") {
        tagList(
          fluidRow(
            column(7, h5("📋 DiD Regression Results"), DT::dataTableOutput(ns("did_coef_tbl"))),
            column(5, h5("📈 Group Means by Period"), withSpinner(plotlyOutput(ns("did_plot"), height="350px")))
          ),
          hr(),
          fluidRow(column(12, h5("📄 Full Summary"), verbatimTextOutput(ns("did_summary"))))
        )
      } else {
        tagList(
          fluidRow(
            column(7, h5("📋 RDD Estimates"), DT::dataTableOutput(ns("rdd_coef_tbl"))),
            column(5, h5("📈 RDD Plot"), withSpinner(plotlyOutput(ns("rdd_plot"), height="350px")))
          ),
          hr(),
          fluidRow(column(12, h5("📄 RDD Summary"), verbatimTextOutput(ns("rdd_summary"))))
        )
      }
    })

    output$did_coef_tbl <- DT::renderDataTable({
      res <- ci_res(); req(res, res$type=="did")
      sm <- summary(res$model)$coefficients
      out <- data.frame(Variable=rownames(sm), Estimate=round(sm[,1],4),
        Std_Error=round(sm[,2],4), t_stat=round(sm[,3],3), p_value=round(sm[,4],4),
        Sig=ifelse(sm[,4]<.001,"***",ifelse(sm[,4]<.01,"**",ifelse(sm[,4]<.05,"*",ifelse(sm[,4]<.1,".","")))),
        stringsAsFactors=FALSE)
      out$Note <- ifelse(out$Variable=="DID_interaction","← ATT (causal estimate)","")
      tool_dt(out, "DiD Results — ATT = DID_interaction coefficient")
    })

    output$did_plot <- renderPlotly({
      res <- ci_res(); req(res, res$type=="did")
      d <- res$d; y <- res$y; treat <- res$treat; post <- res$post
      d$grp_lbl <- paste0(ifelse(d[[treat]]==1,"Treated","Control")," | ",
        ifelse(d[[post]]==1,"Post","Pre"))
      d$treat_g  <- ifelse(d[[treat]]==1,"Treated","Control")
      d$period   <- as.numeric(d[[post]])
      means <- aggregate(d[[y]] ~ d$treat_g + d$period, FUN=mean, na.rm=TRUE)
      names(means) <- c("group","period","y")
      plotly::plot_ly(means, x=~period, y=~y, color=~group,
        colors=c(Control="#95A5A6", Treated="#E74C3C"),
        type="scatter", mode="lines+markers", marker=list(size=12), line=list(width=2)) |>
        plotly::layout(title="DiD: Group Means by Period",
          xaxis=list(title="Period", tickvals=c(0,1), ticktext=c("Pre","Post")),
          yaxis=list(title=y))
    })

    output$did_summary <- renderPrint({
      res <- ci_res(); req(res, res$type=="did")
      sm <- summary(res$model); print(sm)
      cfs <- coef(res$model)
      if ("DID_interaction" %in% names(cfs)) {
        att <- cfs["DID_interaction"]; ci <- confint(res$model)["DID_interaction",]
        cat(sprintf("\n─── ATT = %.4f | 95%% CI: [%.4f, %.4f] ───\n", att, ci[1], ci[2]))
        cat(sprintf("R² = %.4f | Adj R² = %.4f\n", sm$r.squared, sm$adj.r.squared))
      }
    })

    output$rdd_coef_tbl <- DT::renderDataTable({
      res <- ci_res(); req(res, res$type=="rdd")
      m <- res$model
      out <- data.frame(Bandwidth=round(m$bws[1,1],4),
        Obs_Left=m$N_h[1], Obs_Right=m$N_h[2],
        Estimate=round(m$coef[1],4), Std_Error=round(m$se[1],4),
        CI_Lower=round(m$ci[1,1],4), CI_Upper=round(m$ci[1,2],4),
        p_value=round(m$pv[1],4),
        Sig=ifelse(m$pv[1]<.001,"***",ifelse(m$pv[1]<.01,"**",ifelse(m$pv[1]<.05,"*",""))))
      tool_dt(out, "RDD Estimate (rdrobust)")
    })

    output$rdd_plot <- renderPlotly({
      res <- ci_res(); req(res, res$type=="rdd")
      d <- res$d; xn <- res$x_name; yn <- res$y_name; cut <- res$cutoff
      d$side <- ifelse(d[[xn]] < cut, "Left","Right")
      bk_l <- seq(min(d[[xn]][d$side=="Left"],na.rm=TRUE), cut, length.out=11)
      bk_r <- seq(cut, max(d[[xn]][d$side=="Right"],na.rm=TRUE), length.out=11)
      d$bin <- cut(d[[xn]], breaks=c(bk_l, bk_r[-1]), include.lowest=TRUE)
      ms <- aggregate(cbind(d[[yn]], d[[xn]]) ~ d$bin + d$side, FUN=mean, na.rm=TRUE)
      names(ms) <- c("bin","side","y","x")
      plotly::plot_ly(ms, x=~x, y=~y, color=~side,
        colors=c(Left="#2196A6",Right="#E74C3C"),
        type="scatter", mode="markers", marker=list(size=9)) |>
        plotly::add_segments(x=cut,xend=cut,y=min(ms$y,na.rm=TRUE),yend=max(ms$y,na.rm=TRUE),
          line=list(color="black",dash="dash",width=2), showlegend=FALSE) |>
        plotly::layout(title=paste("RDD Plot (cutoff =",cut,")"),
          xaxis=list(title=xn), yaxis=list(title=yn))
    })

    output$rdd_summary <- renderPrint({
      res <- ci_res(); req(res, res$type=="rdd"); print(summary(res$model))
    })


    # ══════════════════════════════════════════════════════════
    #  TIME SERIES
    # ══════════════════════════════════════════════════════════
    ts_res <- reactiveVal(NULL)

    observeEvent(input$run_ts, {
      req(input$ts_var)
      d <- data_rv()
      withProgress(message = "Running time series analysis...", {
        tryCatch({
          type <- input$ts_type; freq <- input$ts_freq
          y <- ts(d[[input$ts_var]], frequency = freq)
          result <- list(type=type, var=input$ts_var, y=y)

          if (type == "unitroot") {
            if (!requireNamespace("tseries", quietly=TRUE)) install.packages("tseries")
            result$adf  <- tseries::adf.test(y)
            result$pp   <- tseries::pp.test(y)
            result$kpss <- tryCatch(tseries::kpss.test(y), error=function(e) NULL)

          } else if (type %in% c("arima","arima_manual")) {
            if (!requireNamespace("forecast", quietly=TRUE)) install.packages("forecast")
            m <- if (type=="arima") forecast::auto.arima(y) else
              forecast::Arima(y, order=c(input$ts_p, input$ts_d, input$ts_q))
            result$model <- m
            result$forecast <- forecast::forecast(m, h=12)

          } else if (type %in% c("var","irf")) {
            req(input$ts_vars_multi)
            if (!requireNamespace("vars", quietly=TRUE)) install.packages("vars")
            vs <- input$ts_vars_multi
            mts_d <- ts(d[,vs,drop=FALSE], frequency=freq)
            lag_sel <- vars::VARselect(mts_d, lag.max=input$ts_lag, type="const")
            opt_lag <- as.integer(lag_sel$selection["AIC(n)"])
            opt_lag <- max(1L, opt_lag)   # ensure at least lag 1
            var_m   <- vars::VAR(mts_d, p=opt_lag, type="const")
            result$var_model <- var_m; result$lag_sel <- lag_sel; result$vars_sel <- vs
            if (type=="irf") result$irf <- vars::irf(var_m, n.ahead=10, boot=TRUE)
          }

          ts_res(result)
          showNotification("✅ Time series analysis complete!", type="message")
        }, error=function(e) showNotification(paste("Error:", e$message), type="error"))
      })
    })

    output$ts_results_ui <- renderUI({
      res <- ts_res()
      if (is.null(res)) return(tags$p(style="color:#888;","Run time series analysis to see results."))
      type <- res$type
      if (type == "unitroot") {
        tagList(
          fluidRow(column(12, h5("🔍 Unit Root Tests"), DT::dataTableOutput(ns("ts_ur_tbl")))),
          hr(),
          fluidRow(column(12, h5("📈 Series"), withSpinner(plotlyOutput(ns("ts_series_plot"), height="280px"))))
        )
      } else if (type %in% c("arima","arima_manual")) {
        tagList(
          fluidRow(
            column(7, h5("📋 ARIMA Model"), verbatimTextOutput(ns("ts_arima_summary"))),
            column(5, h5("📈 Forecast"), withSpinner(plotlyOutput(ns("ts_fc_plot"), height="350px")))
          ),
          hr(),
          fluidRow(column(12, h5("📋 Forecast Table"), DT::dataTableOutput(ns("ts_fc_tbl"))))
        )
      } else if (type %in% c("var","irf")) {
        tagList(
          fluidRow(column(12, h5("📄 VAR Summary"), verbatimTextOutput(ns("ts_var_summary")))),
          hr(),
          fluidRow(column(12, h5("📈 VAR Series"),  withSpinner(plotlyOutput(ns("ts_var_plot"), height="320px")))),
          if (type=="irf") tagList(
            hr(),
            fluidRow(column(12, h5("📈 Impulse Response"), withSpinner(plotlyOutput(ns("ts_irf_plot"), height="380px"))))
          )
        )
      }
    })

    output$ts_ur_tbl <- DT::renderDataTable({
      res <- ts_res(); req(res, res$type=="unitroot")
      rows <- list(
        data.frame(Test="ADF (H0: unit root)", Stat=round(res$adf$statistic,4),
          p=round(res$adf$p.value,4),
          Conclusion=ifelse(res$adf$p.value<0.05,"Stationary","Unit root"), stringsAsFactors=FALSE),
        data.frame(Test="PP (H0: unit root)", Stat=round(res$pp$statistic,4),
          p=round(res$pp$p.value,4),
          Conclusion=ifelse(res$pp$p.value<0.05,"Stationary","Unit root"), stringsAsFactors=FALSE)
      )
      if (!is.null(res$kpss))
        rows[[3]] <- data.frame(Test="KPSS (H0: stationary)", Stat=round(res$kpss$statistic,4),
          p=round(res$kpss$p.value,4),
          Conclusion=ifelse(res$kpss$p.value>0.05,"Stationary","Non-stationary"), stringsAsFactors=FALSE)
      tool_dt(do.call(rbind, rows), "Unit Root Test Results")
    })

    output$ts_series_plot <- renderPlotly({
      res <- ts_res(); req(res)
      t <- as.numeric(time(res$y)); y <- as.numeric(res$y)
      plotly::plot_ly(x=t, y=y, type="scatter", mode="lines",
        line=list(color="#2196A6", width=1.5)) |>
        plotly::layout(title=paste("Series:", res$var),
          xaxis=list(title="Time"), yaxis=list(title=res$var))
    })

    output$ts_arima_summary <- renderPrint({
      res <- ts_res(); req(res, res$model); print(res$model)
      cat("\n--- Ljung-Box Test ---\n")
      tryCatch(print(Box.test(residuals(res$model), lag=10, type="Ljung")), error=function(e) cat("N/A\n"))
    })

    output$ts_fc_plot <- renderPlotly({
      res <- ts_res(); req(res, res$forecast)
      fc <- res$forecast
      at <- as.numeric(time(res$y)); ft <- as.numeric(time(fc$mean))
      plotly::plot_ly() |>
        plotly::add_lines(x=at, y=as.numeric(res$y), name="Actual", line=list(color="#2196A6")) |>
        plotly::add_lines(x=ft, y=as.numeric(fc$mean), name="Forecast", line=list(color="#E74C3C",dash="dash")) |>
        plotly::add_ribbons(x=ft, ymin=as.numeric(fc$lower[,"95%"]), ymax=as.numeric(fc$upper[,"95%"]),
          fillcolor="rgba(231,76,60,0.15)", line=list(width=0), name="95% CI") |>
        plotly::layout(title="ARIMA Forecast",
          xaxis=list(title="Time"), yaxis=list(title=res$var))
    })

    output$ts_fc_tbl <- DT::renderDataTable({
      res <- ts_res(); req(res, res$forecast); fc <- res$forecast
      tool_dt(data.frame(Period=seq_along(fc$mean),
        Forecast=round(as.numeric(fc$mean),4),
        Lo80=round(as.numeric(fc$lower[,"80%"]),4), Hi80=round(as.numeric(fc$upper[,"80%"]),4),
        Lo95=round(as.numeric(fc$lower[,"95%"]),4), Hi95=round(as.numeric(fc$upper[,"95%"]),4)),
        "12-Period Forecast")
    })

    output$ts_var_summary <- renderPrint({
      res <- ts_res(); req(res, res$var_model)
      cat("=== VAR Model | Lag:", res$var_model$p, "(AIC) | Variables:",
        paste(res$vars_sel, collapse=", "), "===\n\n")
      print(summary(res$var_model))
    })

    output$ts_var_plot <- renderPlotly({
      res <- ts_res(); req(res, res$var_model)
      df <- data.frame(as.matrix(res$var_model$y)); df$t <- seq_len(nrow(df))
      cols <- c("#2196A6","#E74C3C","#27AE60","#F39C12","#8E44AD")
      p <- plotly::plot_ly()
      for (i in seq_along(res$vars_sel))
        p <- plotly::add_lines(p, data=df, x=~t,
          y=as.formula(paste0("~",res$vars_sel[i])), name=res$vars_sel[i],
          line=list(color=cols[((i-1)%%5)+1], width=1.5))
      plotly::layout(p, title="VAR Series", xaxis=list(title="Time"), yaxis=list(title="Value"))
    })

    output$ts_irf_plot <- renderPlotly({
      res <- ts_res(); req(res, res$irf)
      vs <- res$vars_sel; irf <- res$irf
      imp <- vs[1]; resp <- vs[min(2, length(vs))]
      iv <- irf$irf[[imp]][,resp]; lo <- irf$Lower[[imp]][,resp]; hi <- irf$Upper[[imp]][,resp]
      periods <- 0:(length(iv)-1)
      plotly::plot_ly(x=periods, y=iv, type="scatter", mode="lines+markers",
        line=list(color="#2196A6",width=2), name=paste(imp,"→",resp)) |>
        plotly::add_ribbons(ymin=lo, ymax=hi, fillcolor="rgba(33,150,166,0.15)",
          line=list(width=0), name="95% CI") |>
        plotly::add_segments(y=0,yend=0,x=0,xend=max(periods),
          line=list(color="gray",dash="dot"), showlegend=FALSE) |>
        plotly::layout(title=paste("IRF:", imp, "→", resp),
          xaxis=list(title="Periods"), yaxis=list(title="Response"))
    })


    # ══════════════════════════════════════════════════════════
    #  IV / 2SLS
    # ══════════════════════════════════════════════════════════
    iv_res <- reactiveVal(NULL)

    observeEvent(input$run_iv, {
      req(input$iv_dv, input$iv_endog, input$iv_instr)
      d <- data_rv()
      withProgress(message = "Running IV estimation...", {
        tryCatch({
          if (!requireNamespace("AER", quietly=TRUE)) install.packages("AER")
          dv <- input$iv_dv
          exog  <- if (!is.null(input$iv_exog)  && length(input$iv_exog)>0)  input$iv_exog  else character(0)
          endog <- input$iv_endog; instr <- input$iv_instr
          all_reg  <- c(exog, endog)
          all_inst <- c(exog, instr)
          if (length(all_reg) == 0 || length(all_inst) == 0)
            stop("Please select at least one regressor and one instrument.")
          fmla <- as.formula(paste(dv,"~",paste(all_reg,collapse="+"),"|",paste(all_inst,collapse="+")))
          m  <- AER::ivreg(fmla, data=d)
          sm <- summary(m, diagnostics=TRUE)
          ols_m <- lm(as.formula(paste(dv,"~",paste(all_reg,collapse="+"))), data=d)
          iv_res(list(model=m, summary=sm, ols=ols_m, dv=dv, endog=endog, exog=exog, instr=instr))
          showNotification("✅ IV estimation complete!", type="message")
        }, error=function(e) showNotification(paste("Error:", e$message), type="error"))
      })
    })

    output$iv_results_ui <- renderUI({
      res <- iv_res()
      if (is.null(res)) return(tags$p(style="color:#888;","Run IV estimation to see results."))
      tagList(
        fluidRow(
          column(7, h5("📋 2SLS vs OLS Coefficients"), DT::dataTableOutput(ns("iv_coef_tbl"))),
          column(5, h5("📈 Comparison Plot"), withSpinner(plotlyOutput(ns("iv_compare_plot"), height="350px")))
        ),
        hr(),
        fluidRow(column(12, h5("🔍 Diagnostic Tests (Wu-Hausman · Weak Instruments · Sargan)"),
          DT::dataTableOutput(ns("iv_diag_tbl")))),
        hr(),
        fluidRow(column(12, h5("📄 Full Summary"), verbatimTextOutput(ns("iv_summary"))))
      )
    })

    output$iv_coef_tbl <- DT::renderDataTable({
      res <- iv_res(); req(res)
      make_df <- function(sm, method) {
        data.frame(Method=method, Variable=rownames(sm),
          Estimate=round(sm[,1],4), Std_Error=round(sm[,2],4),
          t_stat=round(sm[,3],3), p_value=round(sm[,4],4),
          Sig=ifelse(sm[,4]<.001,"***",ifelse(sm[,4]<.01,"**",ifelse(sm[,4]<.05,"*",ifelse(sm[,4]<.1,".","")))),
          stringsAsFactors=FALSE)
      }
      out <- rbind(make_df(res$summary$coefficients,"2SLS"),
                   make_df(summary(res$ols)$coefficients,"OLS"))
      tool_dt(out, "2SLS vs OLS Comparison")
    })

    output$iv_diag_tbl <- DT::renderDataTable({
      res <- iv_res(); req(res)
      diag <- tryCatch(res$summary$diagnostics, error=function(e) NULL)
      if (is.null(diag)) return(data.frame(Note="Diagnostics not available"))
      concl <- c(
        ifelse(diag[1,"p-value"]<0.05,"Endogeneity present → IV preferred","No endogeneity detected"),
        ifelse(diag[2,"p-value"]>0.05,"⚠️ Weak instruments","Instruments are strong")
      )
      if (nrow(diag)>=3) concl <- c(concl,
        ifelse(diag[3,"p-value"]>0.05,"Over-ID ok (instruments valid)","Over-ID test failed"))
      out <- data.frame(Test=rownames(diag),
        Statistic=round(diag[,"statistic"],4),
        df1=diag[,"df1"], df2=diag[,"df2"],
        p_value=round(diag[,"p-value"],4),
        Conclusion=concl, stringsAsFactors=FALSE)
      tool_dt(out, "IV Diagnostic Tests")
    })

    output$iv_compare_plot <- renderPlotly({
      res <- iv_res(); req(res)
      build_df <- function(sm, method) {
        vars <- setdiff(rownames(sm),"(Intercept)")
        sm <- sm[vars,,drop=FALSE]
        data.frame(var=vars, coef=sm[,1], se=sm[,2], method=method, stringsAsFactors=FALSE)
      }
      df <- rbind(build_df(res$summary$coefficients,"2SLS"),
                  build_df(summary(res$ols)$coefficients,"OLS"))
      df$lo <- df$coef-1.96*df$se; df$hi <- df$coef+1.96*df$se
      df$rank <- as.numeric(factor(df$var))
      df$y_pos <- df$rank + ifelse(df$method=="2SLS",-0.2,0.2)
      plotly::plot_ly(df, x=~coef, y=~y_pos, color=~method,
        colors=c("2SLS"="#E74C3C","OLS"="#2196A6"),
        type="scatter", mode="markers", marker=list(size=10),
        text=~paste(var,"(",method,")"), hoverinfo="text+x",
        error_x=list(type="data",symmetric=FALSE,array=~(hi-coef),arrayminus=~(coef-lo))) |>
        plotly::add_segments(x=0,xend=0,y=0,yend=max(df$y_pos)+0.5,
          line=list(color="gray",dash="dot"),showlegend=FALSE) |>
        plotly::layout(title="2SLS vs OLS (95% CI)",
          xaxis=list(title="Coefficient"),
          yaxis=list(title="",tickvals=seq_along(levels(factor(df$var))),
            ticktext=levels(factor(df$var))))
    })

    output$iv_summary <- renderPrint({
      res <- iv_res(); req(res); print(res$summary)
    })


    # ══════════════════════════════════════════════════════════
    #  AI INTERPRETATION
    # ══════════════════════════════════════════════════════════
    ai_text_rv <- reactiveVal("")

    run_ai <- function(label, text_fn) {
      key <- gemini_key()
      if (nchar(trimws(key)) < 5) {
        showNotification("Please enter a Groq API key in the sidebar.", type="warning"); return()
      }
      txt <- tryCatch(paste(capture.output(text_fn()), collapse="\n"),
        error=function(e) paste("Error:", e$message))
      prompt <- paste0(
        "You are an expert econometrician and statistician. Provide a detailed academic interpretation of the following ",
        label, " results. Cover: (1) statistical significance and effect sizes, ",
        "(2) economic/practical implications, (3) key assumptions and their satisfaction, ",
        "(4) limitations and future research directions. Use APA 7th edition style.\n\n", txt)
      withProgress(message="🤖 AI interpreting...", {
        tryCatch({
          resp <- httr::POST(
            "https://api.groq.com/openai/v1/chat/completions",
            httr::add_headers(Authorization=paste("Bearer", key), `Content-Type`="application/json"),
            body=jsonlite::toJSON(list(
              model="llama-3.3-70b-versatile",
              messages=list(list(role="user", content=prompt)),
              max_tokens=1200L, temperature=0.3), auto_unbox=TRUE), encode="raw")
          out <- jsonlite::fromJSON(httr::content(resp,"text",encoding="UTF-8"))
          ai_text_rv(out$choices$message$content[1])
        }, error=function(e) ai_text_rv(paste("Error:", e$message)))
      })
    }

    observeEvent(input$ai_pd,  {
      req(pd_res())
      run_ai("Panel Data", function() {
        if (pd_res()$type=="both") {
          print(summary(pd_res()$fe$model)); print(summary(pd_res()$re$model))
          if (!is.null(pd_res()$hausman)) print(pd_res()$hausman)
        } else print(summary(pd_res()$model))
      })
    })
    observeEvent(input$ai_sa,  {
      req(sa_res())
      run_ai("Survival Analysis", function() {
        if (!is.null(sa_res()$km))  print(sa_res()$km)
        if (!is.null(sa_res()$cox)) print(summary(sa_res()$cox))
      })
    })
    observeEvent(input$ai_ldv, {
      req(ldv_res())
      run_ai("Limited Dependent Variable", function() print(summary(ldv_res()$model)))
    })
    observeEvent(input$ai_qr,  {
      req(qr_res())
      run_ai("Quantile Regression", function() {
        for (i in seq_along(qr_res()$models)) {
          cat(sprintf("τ=%.2f:\n", qr_res()$taus[i]))
          print(summary(qr_res()$models[[i]]))
        }
      })
    })
    observeEvent(input$ai_ci,  {
      req(ci_res())
      run_ai("Causal Inference", function() {
        if (ci_res()$type=="did") print(summary(ci_res()$model))
        else print(summary(ci_res()$model))
      })
    })
    observeEvent(input$ai_ts,  {
      req(ts_res())
      run_ai("Time Series", function() {
        type <- ts_res()$type
        if (type=="unitroot") { print(ts_res()$adf); print(ts_res()$pp) }
        else if (type %in% c("arima","arima_manual")) print(ts_res()$model)
        else print(summary(ts_res()$var_model))
      })
    })
    observeEvent(input$ai_iv,  {
      req(iv_res())
      run_ai("IV/2SLS", function() print(iv_res()$summary))
    })

    output$ai_status_ui <- renderUI({
      txt <- ai_text_rv()
      if (nchar(txt)==0) return(NULL)
      tags$div(style="background:#EFF8FF;border:1px solid #2196A6;border-radius:6px;padding:.5rem;margin-bottom:.5rem;font-size:.75rem;color:#2196A6;",
        "✅ AI interpretation ready")
    })
    output$ai_out <- renderPrint({
      txt <- ai_text_rv()
      if (nchar(txt)==0) cat("Run an analysis and click an AI button above.")
      else cat(txt)
    })


    # ══════════════════════════════════════════════════════════
    #  DOWNLOAD
    # ══════════════════════════════════════════════════════════
    output$dl_excel <- downloadHandler(
      filename = function() paste0("DrAIStat_STATA_", format(Sys.time(),"%Y%m%d_%H%M"), ".xlsx"),
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx")
        wb <- openxlsx::createWorkbook()

        safe_sheet <- function(nm, df) {
          tryCatch({
            if (!is.null(df) && nrow(df) > 0) {
              openxlsx::addWorksheet(wb, nm)
              openxlsx::writeData(wb, nm, df)
            }
          }, error=function(e) NULL)
        }

        # Panel Data
        if (!is.null(pd_res())) {
          res <- pd_res()
          mk <- function(m,vc,nm) {
            ct <- if(!is.null(vc)) lmtest::coeftest(m,vcov=vc) else lmtest::coeftest(m)
            data.frame(Model=nm,Variable=rownames(ct),Coef=round(ct[,1],4),
              SE=round(ct[,2],4),t=round(ct[,3],3),p=round(ct[,4],4))
          }
          df_pd <- if(res$type=="both")
            rbind(mk(res$fe$model,res$fe$vcov,"FE"), mk(res$re$model,res$re$vcov,"RE"))
          else mk(res$model,res$vcov,toupper(res$type))
          safe_sheet("Panel_Data", df_pd)
        }

        # Survival
        if (!is.null(sa_res()) && !is.null(sa_res()$cox)) {
          sm <- summary(sa_res()$cox)$coefficients
          safe_sheet("Survival_Cox", data.frame(Variable=rownames(sm),
            logHR=round(sm[,1],4),HR=round(exp(sm[,1]),4),SE=round(sm[,2],4),p=round(sm[,5],4)))
        }

        # LDV
        if (!is.null(ldv_res())) tryCatch({
          sm <- summary(ldv_res()$model)$coefficients
          safe_sheet("Limited_DV", data.frame(Variable=rownames(sm),
            Coef=round(sm[,1],4),SE=round(sm[,2],4),p=round(sm[,ncol(sm)],4)))
        }, error=function(e) NULL)

        # QR
        if (!is.null(qr_res())) {
          rows <- lapply(seq_along(qr_res()$models), function(i) {
            sm <- tryCatch(summary(qr_res()$models[[i]],se="boot")$coefficients, error=function(e) NULL)
            if (is.null(sm)) return(NULL)
            data.frame(Q=qr_res()$taus[i],Variable=rownames(sm),
              Coef=round(sm[,1],4),p=round(sm[,4],4))
          })
          safe_sheet("Quantile_Reg", do.call(rbind, Filter(Negate(is.null), rows)))
        }

        # DiD
        if (!is.null(ci_res()) && ci_res()$type=="did") {
          sm <- summary(ci_res()$model)$coefficients
          safe_sheet("DiD", data.frame(Variable=rownames(sm),
            Estimate=round(sm[,1],4),SE=round(sm[,2],4),p=round(sm[,4],4)))
        }

        # IV
        if (!is.null(iv_res())) {
          sm <- iv_res()$summary$coefficients
          safe_sheet("IV_2SLS", data.frame(Variable=rownames(sm),
            Estimate=round(sm[,1],4),SE=round(sm[,2],4),p=round(sm[,4],4)))
        }

        openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
      }
    )

  }) # /moduleServer
} # /mod14_server
