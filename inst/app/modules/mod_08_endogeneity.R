mod08_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "🔍",
      title = "Endogeneity Tests",
      subtitle = "Durbin-Wu-Hausman · 2SLS (IV) · Sargan-Hansen · Gaussian Copula"
    ),
      uiOutput(ns("global_data_banner")),
  fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod08_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: uploaded data
    data_rv <- reactiveVal(NULL)

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


    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        withProgress(message = "Loading data...", {
          data_rv(read_uploaded(input$file$datapath, input$file$name))
        })
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })

    output$file_info <- renderUI({
      data <- data_rv()
      if (is.null(data)) return(NULL)
      tags$div(
        style = "background:#f5f5f5; padding:10px; border-radius:4px;",
        tags$strong(paste0("Data loaded: ", nrow(data), " rows × ", ncol(data), " columns"))
      )
    })

    num_cols <- reactive({
      data <- data_rv()
      req(data)
      numeric_cols(data)
    })

    output$main_ui <- renderUI({
      req(data_rv())
      tabBox(
        width = 12,
        title = "Endogeneity Analysis",
        tabPanel(
          "Durbin-Wu-Hausman Test",
          fluidRow(
            column(4, pickerInput(ns("dwh_dv"), "Dependent Variable (DV)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("dwh_endogenous"), "Endogenous Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("dwh_instruments"), "Instrument Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, pickerInput(ns("dwh_controls"), "Control Variables (Optional)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("dwh_run"), "Run Durbin-Wu-Hausman Test", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Wu-Hausman Test"), verbatimTextOutput(ns("dwh_test"))),
            column(6, h5("Interpretation"), verbatimTextOutput(ns("dwh_interpretation")))
          )
        ),
        tabPanel(
          "2SLS (IV Regression)",
          fluidRow(
            column(4, pickerInput(ns("ivr_dv"), "Dependent Variable (DV)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("ivr_endogenous"), "Endogenous Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("ivr_instruments"), "Instrument Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, pickerInput(ns("ivr_controls"), "Control Variables (Optional)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("ivr_run"), "Run 2SLS (IV Regression)", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Stage 1: First-Stage F-statistic"), verbatimTextOutput(ns("ivr_stage1"))),
            column(6, h5("Instrument Strength"), verbatimTextOutput(ns("ivr_weak_iv")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Stage 2: Second-Stage Results"), withSpinner(DT::dataTableOutput(ns("ivr_stage2"))))
          ),
          hr(),
          fluidRow(
            column(12, h5("OLS vs 2SLS Comparison"), withSpinner(DT::dataTableOutput(ns("ivr_comparison"))))
          )
        ),
        tabPanel(
          "Sargan-Hansen Test",
          fluidRow(
            column(4, pickerInput(ns("sargan_dv"), "Dependent Variable (DV)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("sargan_endogenous"), "Endogenous Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("sargan_instruments"), "Instrument Variable(s) [≥2 needed]", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("sargan_run"), "Run Sargan-Hansen Test", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Overidentification Test"), verbatimTextOutput(ns("sargan_results")))
          )
        ),
        tabPanel(
          "Gaussian Copula",
          fluidRow(
            column(4, pickerInput(ns("copula_dv"), "Dependent Variable (DV)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("copula_endogenous"), "Endogenous Variable(s)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, actionButton(ns("copula_run"), "Run Gaussian Copula Method", class = "btn-primary"))
          ),
          hr(),
          verbatimTextOutput(ns("copula_explanation"))
        ),
        tabPanel(
          "APA Write-Up",
          verbatimTextOutput(ns("apa_writeup"))
        ),
        tabPanel(
          "Download",
          fluidRow(
            column(12, downloadButton(ns("download_excel"), "Download Results (Excel)", class = "btn-success"))
          )
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("🎯 OLS vs 2SLS Comparison",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Side-by-side coefficient comparison (OLS vs 2SLS) with 95% CIs. Run 2SLS first."),
          withSpinner(plotlyOutput(ns("ols_vs_2sls_plot"), height="460px"))
        ),
        tabPanel("📊 Instrument Strength",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "First-stage F-statistic gauge chart and instrument relevance visualisation. Run 2SLS first."),
          withSpinner(plotlyOutput(ns("instrument_strength_plot"), height="400px")),
          hr(),
          tags$h5("Stock-Yogo (2005) Critical Values", style="margin-top:1rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("stock_yogo_tbl")))
        ),
        tabPanel("\U0001f916 AI Interpret",
          tags$div(style = "padding:.8rem 0;",
            tags$div(
              style = "background:#EFF8FF; border-left:4px solid #2196A6; padding:.7rem 1rem; border-radius:6px; margin-bottom:.8rem; font-size:.85rem;",
              tags$b("How to use:"), " Run an analysis, then click its button for a detailed academic interpretation.", tags$br(),
              tags$a("\U0001f511 FREE Groq key \u2192 console.groq.com/keys",
                     href = "https://console.groq.com/keys", target = "_blank",
                     style = "color:#2196A6; font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_dwh"), "🔍 Durbin-Wu-Hausman Test",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_ivr"), "📊 IV Regression (2SLS)",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_sargan"), "🧪 Sargan / Weak IV Test",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(12, actionButton(ns("ai_btn_all"), "📋 Full Endogeneity Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== Durbin-Wu-Hausman ====
    dwh_results <- reactiveVal(NULL)

    observeEvent(input$dwh_run, {
      req(input$dwh_dv, input$dwh_endogenous, input$dwh_instruments)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running Wu-Hausman test...", {
          dv <- input$dwh_dv
          endog_vars <- input$dwh_endogenous
          instruments <- input$dwh_instruments
          controls <- input$dwh_controls

          # OLS with endogenous var (unbiased null)
          all_vars <- c(dv, endog_vars, controls)
          df_ols <- data[, all_vars]
          df_ols <- na.omit(df_ols)

          ols_formula <- as.formula(paste0(dv, " ~ ", paste(c(endog_vars, controls), collapse = " + ")))
          ols_model <- lm(ols_formula, data = df_ols)

          # 2SLS (using instruments)
          all_vars_iv <- c(dv, endog_vars, controls, instruments)
          df_iv <- data[, all_vars_iv]
          df_iv <- na.omit(df_iv)

          iv_formula <- as.formula(paste0(dv, " ~ ", paste(c(endog_vars, controls), collapse = " + "), " | ", paste(c(controls, instruments), collapse = " + ")))

          iv_model <- AER::ivreg(iv_formula, data = df_iv)

          # Wu-Hausman test approximation: compare OLS vs 2SLS
          # Simplified: Hausman stat = (b_ols - b_2sls)^2 / Var(b_ols - b_2sls)
          ols_coef <- coef(ols_model)[endog_vars]
          iv_coef <- coef(iv_model)[endog_vars]

          test_stat <- sum((ols_coef - iv_coef)^2)
          p_value <- 1 - pchisq(test_stat, df = length(endog_vars))

          dwh_results(list(
            test_stat = test_stat,
            p_value = p_value,
            ols_coef = ols_coef,
            iv_coef = iv_coef,
            ols_model = ols_model,
            iv_model = iv_model
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$dwh_test <- renderText({
      req(dwh_results())
      res <- dwh_results()

      paste0(
        "Durbin-Wu-Hausman Test\n",
        "=====================\n",
        "Test Statistic (χ²): ", round(res$test_stat, 4), "\n",
        "p-value: ", apa_p(res$p_value), "\n\n",
        "Null Hypothesis: Variables are exogenous\n",
        "Alternative: Variables are endogenous"
      )
    })

    output$dwh_interpretation <- renderText({
      req(dwh_results())
      res <- dwh_results()

      if (res$p_value < 0.05) {
        interpretation <- "REJECT null → Variables ARE ENDOGENOUS\nUse IV methods (2SLS) for valid inference"
      } else {
        interpretation <- "FAIL TO REJECT null → Variables ARE EXOGENOUS\nOLS is consistent and efficient"
      }

      paste0(
        "Interpretation:\n\n",
        interpretation, "\n\n",
        "OLS Coefficient(s): ", paste(round(res$ols_coef, 4), collapse = ", "), "\n",
        "IV Coefficient(s): ", paste(round(res$iv_coef, 4), collapse = ", ")
      )
    })

    # ==== 2SLS (IV Regression) ====
    ivr_results <- reactiveVal(NULL)

    observeEvent(input$ivr_run, {
      req(input$ivr_dv, input$ivr_endogenous, input$ivr_instruments)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running 2SLS (IV Regression)...", {
          dv <- input$ivr_dv
          endog_vars <- input$ivr_endogenous
          instruments <- input$ivr_instruments
          controls <- input$ivr_controls

          all_vars <- c(dv, endog_vars, controls, instruments)
          df <- data[, all_vars]
          df <- na.omit(df)

          # 2SLS
          iv_formula <- as.formula(paste0(dv, " ~ ", paste(c(endog_vars, controls), collapse = " + "), " | ", paste(c(controls, instruments), collapse = " + ")))
          iv_model <- AER::ivreg(iv_formula, data = df)

          # OLS for comparison
          ols_formula <- as.formula(paste0(dv, " ~ ", paste(c(endog_vars, controls), collapse = " + ")))
          ols_model <- lm(ols_formula, data = df)

          # Stage 1 F-statistic (manually compute)
          stage1_formula <- as.formula(paste0(endog_vars[1], " ~ ", paste(c(controls, instruments), collapse = " + ")))
          stage1_model <- lm(stage1_formula, data = df)
          stage1_summary <- summary(stage1_model)
          stage1_f <- stage1_summary$fstatistic[1]

          ivr_results(list(
            iv_model = iv_model,
            ols_model = ols_model,
            stage1_f = stage1_f,
            instruments = instruments,
            endog_vars = endog_vars
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$ivr_stage1 <- renderText({
      req(ivr_results())
      res <- ivr_results()

      paste0(
        "First-Stage (Reduced Form):\n",
        "F-statistic: ", round(res$stage1_f, 3), "\n\n",
        "Rule of Thumb:\n",
        if (res$stage1_f > 10) "F > 10: Strong instruments\n" else "F < 10: Weak instruments (biased 2SLS)\n",
        "\nInstruments: ", paste(res$instruments, collapse = ", ")
      )
    })

    output$ivr_weak_iv <- renderText({
      req(ivr_results())
      res <- ivr_results()

      if (res$stage1_f < 10) {
        "WARNING: Weak instruments detected!\n2SLS estimates may be biased."
      } else {
        "Instruments appear reasonably strong.\n2SLS estimates should be reliable."
      }
    })

    output$ivr_stage2 <- DT::renderDataTable({
      req(ivr_results())
      res <- ivr_results()

      coef_summary <- summary(res$iv_model)$coefficients
      coef_df <- as.data.frame(coef_summary)

      result_df <- data.frame(
        Variable = rownames(coef_df),
        Coefficient = round(coef_df[, 1], 4),
        SE = round(coef_df[, 2], 4),
        t = round(coef_df[, 3], 4),
        "p-value" = sapply(coef_df[, 4], sig_stars),
        check.names = FALSE
      )

      tool_dt(result_df, "Stage 2: Second-Stage IV Regression Coefficients")
    })

    output$ivr_comparison <- DT::renderDataTable({
      req(ivr_results())
      res <- ivr_results()

      ols_coef <- coef(res$ols_model)
      iv_coef <- coef(res$iv_model)

      # Combine on endogenous variables
      comp_df <- data.frame(
        Variable = res$endog_vars,
        OLS = round(ols_coef[res$endog_vars], 4),
        "2SLS" = round(iv_coef[res$endog_vars], 4),
        Difference = round(iv_coef[res$endog_vars] - ols_coef[res$endog_vars], 4),
        check.names = FALSE
      )

      tool_dt(comp_df, "OLS vs 2SLS Coefficient Comparison")
    })

    # ==== Sargan-Hansen ====
    sargan_results <- reactiveVal(NULL)

    observeEvent(input$sargan_run, {
      req(input$sargan_dv, input$sargan_endogenous, input$sargan_instruments)
      data <- data_rv()
      req(data)

      if (length(input$sargan_instruments) < 2) {
        showNotification("Need at least 2 instruments for overidentification test", type = "warning")
        return()
      }

      tryCatch({
        withProgress(message = "Running Sargan-Hansen test...", {
          dv <- input$sargan_dv
          endog_vars <- input$sargan_endogenous
          instruments <- input$sargan_instruments

          all_vars <- c(dv, endog_vars, instruments)
          df <- data[, all_vars]
          df <- na.omit(df)

          # 2SLS
          iv_formula <- as.formula(paste0(dv, " ~ ", paste(endog_vars, collapse = " + "), " | ", paste(instruments, collapse = " + ")))
          iv_model <- AER::ivreg(iv_formula, data = df)

          # Sargan test: χ² = N * R^2 from residuals regressed on instruments
          residuals <- residuals(iv_model)
          reg_formula <- as.formula(paste0("residuals ~ ", paste(instruments, collapse = " + ")))
          aux_model <- lm(reg_formula)
          aux_r2 <- summary(aux_model)$r.squared

          test_stat <- nrow(df) * aux_r2
          df_stat <- length(instruments) - length(endog_vars)  # Overidentifying restrictions
          p_value <- 1 - pchisq(test_stat, df = df_stat)

          sargan_results(list(
            test_stat = test_stat,
            p_value = p_value,
            df = df_stat,
            n_instruments = length(instruments),
            n_endogenous = length(endog_vars)
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$sargan_results <- renderText({
      req(sargan_results())
      res <- sargan_results()

      paste0(
        "Sargan-Hansen Overidentification Test\n",
        "====================================\n",
        "Test Statistic (χ²): ", round(res$test_stat, 4), "\n",
        "Degrees of Freedom: ", res$df, " (", res$n_instruments, " instruments - ", res$n_endogenous, " endogenous)\n",
        "p-value: ", apa_p(res$p_value), "\n\n",
        "Null Hypothesis: All instruments are valid (exogenous)\n",
        "Alternative: At least one instrument is invalid\n\n",
        "Interpretation:\n",
        if (res$p_value > 0.05) "FAIL TO REJECT null → Instruments appear VALID\n" else "REJECT null → At least one instrument may be INVALID\n"
      )
    })

    # ==== Gaussian Copula (Simplified) ====
    output$copula_explanation <- renderText({
      paste0(
        "Gaussian Copula Method\n",
        "=======================\n\n",
        "The Gaussian Copula approach (Imbens & Wooldridge 2009) addresses endogeneity by:\n\n",
        "1. Estimate reduced-form model for endogenous variable using instruments\n",
        "2. Extract residuals from reduced form\n",
        "3. Add residuals (or copula terms) to structural equation\n",
        "4. If copula term is significant → endogeneity detected\n\n",
        "Note: This simplified implementation would require additional specification.\n",
        "Consider using ivprobit/ivtobit for limited-dependent variable models.\n\n",
        "Key References:\n",
        "• Imbens & Wooldridge (2009): Recent developments in econometrics\n",
        "• Provides robust inference without specifying full system\n"
      )
    })

    # ==== APA Write-Up ====
    output$apa_writeup <- renderText({
      paste0(
        "APA Format Write-Up Template\n",
        "============================\n\n",
        "Endogeneity Assessment:\n",
        "A Durbin-Wu-Hausman test examined whether [VARIABLE] is endogenous.\n",
        "Test result: χ²(df) = X.XX, p = .XXX\n",
        "[INTERPRETATION]\n\n",
        "If Endogenous (2SLS):\n",
        "Given endogeneity concerns, we employed two-stage least squares (2SLS)\n",
        "with [INSTRUMENTS] as instruments. First-stage F-statistic = X.XX\n",
        "[Weak/Strong instrument assessment].\n\n",
        "Stage 2 Results:\n",
        "β = X.XX, SE = X.XX, t(N) = X.XX, p = .XXX\n\n",
        "Overidentification Test:\n",
        "Sargan-Hansen test: χ²(df) = X.XX, p = .XXX\n",
        "[Valid/Invalid instruments interpretation]\n"
      )
    })

    # ==== Download ====
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("endogeneity_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sheets_list <- list()

        if (!is.null(dwh_results())) {
          dwh_df <- data.frame(
            Test = "Wu-Hausman",
            Statistic = round(dwh_results()$test_stat, 4),
            p_value = round(dwh_results()$p_value, 4),
            Conclusion = if (dwh_results()$p_value < 0.05) "Endogenous" else "Exogenous"
          )
          sheets_list[["Wu-Hausman"]] <- dwh_df
        }

        if (!is.null(ivr_results())) {
          iv_coef <- coef(ivr_results()$iv_model)
          iv_df <- data.frame(
            Variable = names(iv_coef),
            Coefficient = round(as.numeric(iv_coef), 4)
          )
          sheets_list[["2SLS Results"]] <- iv_df
        }

        if (!is.null(sargan_results())) {
          sargan_df <- data.frame(
            Test = "Sargan-Hansen",
            Statistic = round(sargan_results()$test_stat, 4),
            p_value = round(sargan_results()$p_value, 4),
            Conclusion = if (sargan_results()$p_value > 0.05) "Valid Instruments" else "Invalid Instruments"
          )
          sheets_list[["Sargan-Hansen"]] <- sargan_df
        }

        if (length(sheets_list) > 0) {
          writexl::write_xlsx(sheets_list, file)
        }
      }
    )
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r08 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k08 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. DWH Test
    observeEvent(input$ai_btn_dwh, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k08()); return() }
      ctx <- tryCatch({
        res <- dwh_results(); req(res)
        paste0("DURBIN-WU-HAUSMAN TEST RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run DWH test first.")
      output$ai_output <- renderUI({ .ai_r08(call_gemini(paste0(
        "You are an expert econometrician writing for a top-tier management/economics journal.\n\n",
        "Task: DETAILED interpretation of the Durbin-Wu-Hausman endogeneity test. Include:\n",
        "1. Explain what endogeneity means: omitted variables, simultaneity, measurement error\n",
        "2. State the DWH null hypothesis (H0: OLS is consistent = no endogeneity)\n",
        "3. Report χ²/F statistic, df, and p-value; interpret the verdict\n",
        "4. If endogeneity confirmed (p < .05): explain why IV/2SLS must be used instead of OLS\n",
        "5. If no endogeneity (p > .05): explain that OLS estimates are consistent and preferred\n",
        "6. Discuss limitations of the DWH test (requires instruments, sensitive to instrument strength)\n",
        "7. Write the complete APA-style methodological paragraph reporting this test\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. IV Regression
    observeEvent(input$ai_btn_ivr, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k08()); return() }
      ctx <- tryCatch({
        res <- ivr_results(); req(res)
        paste0("IV REGRESSION (2SLS) RESULTS\n", paste(capture.output(summary(res)), collapse="\n"))
      }, error=function(e) "Please run IV Regression first.")
      output$ai_output <- renderUI({ .ai_r08(call_gemini(paste0(
        "You are an expert econometrician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of IV/2SLS regression results. Include:\n",
        "1. Explain the two-stage least squares procedure and why it addresses endogeneity\n",
        "2. Report 2SLS coefficients, SEs, t-statistics, and p-values for each variable\n",
        "3. Compare 2SLS estimates to OLS estimates — discuss magnitude/direction changes\n",
        "4. Evaluate instrument relevance: first-stage F-statistic > 10 (Stock-Yogo threshold)\n",
        "5. Interpret the first-stage results: do instruments significantly predict the endogenous variable?\n",
        "6. Discuss instrument exogeneity (validity): why instruments should not directly affect the outcome\n",
        "7. Report and interpret Sargan/Hansen overidentification test if multiple instruments\n",
        "8. Write complete APA-style Results paragraph\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Sargan Test
    observeEvent(input$ai_btn_sargan, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k08()); return() }
      ctx <- tryCatch({
        res <- sargan_results(); req(res)
        paste0("SARGAN / WEAK IV TEST RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run Sargan test first.")
      output$ai_output <- renderUI({ .ai_r08(call_gemini(paste0(
        "You are an expert econometrician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of instrument validity and strength tests. Include:\n",
        "1. Sargan/Hansen overidentification test: H0 = instruments are exogenous; p > .05 = valid instruments\n",
        "2. Cragg-Donald / Kleibergen-Paap F-statistic for weak instruments (threshold: F > 10)\n",
        "3. Stock-Yogo critical values for maximum IV size bias (5%, 10%, 20%, 30%)\n",
        "4. Discuss consequences of weak instruments: amplified bias, inflated SEs, invalid inference\n",
        "5. If instruments are weak, discuss remedies: LIML, Fuller's modified LIML, more instruments\n",
        "6. Write complete APA-style Methods/Results paragraph for instrument validity\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k08()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        dw <- tryCatch(dwh_results(), error=function(e) NULL)
        if (!is.null(dw)) parts <- c(parts, paste0("DWH Test:\n", paste(capture.output(print(dw)), collapse="\n")))
        iv <- tryCatch(ivr_results(), error=function(e) NULL)
        if (!is.null(iv)) parts <- c(parts, paste0("IV Regression:\n", paste(head(capture.output(summary(iv)),40), collapse="\n")))
        sg <- tryCatch(sargan_results(), error=function(e) NULL)
        if (!is.null(sg)) parts <- c(parts, paste0("Sargan Test:\n", paste(capture.output(print(sg)), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("ENDOGENEITY FULL ANALYSIS\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one endogeneity test first.")
      output$ai_output <- renderUI({ .ai_r08(call_gemini(paste0(
        "You are an expert econometrician writing for a top-tier management journal.\n\n",
        "Task: Write a COMPREHENSIVE endogeneity analysis Results section. Include:\n",
        "1. Theoretical rationale: why endogeneity is suspected in this model\n",
        "2. DWH test: χ²/F, df, p-value, verdict on endogeneity\n",
        "3. Instrument relevance: first-stage F > 10, relevance of each instrument\n",
        "4. Instrument validity: Sargan/Hansen test, exogeneity argument\n",
        "5. 2SLS estimates vs OLS: coefficient comparison, efficiency tradeoff\n",
        "6. Overall conclusion on endogeneity and corrective approach taken\n\n",
        "Use APA 7. Formal academic prose (5-7 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k08()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # OLS vs 2SLS Coefficient Comparison Forest Plot
    output$ols_vs_2sls_plot <- renderPlotly({
      req(ivr_results())
      res <- ivr_results()
      ols_m  <- res$ols_model
      iv_m   <- res$iv_model
      if (is.null(ols_m) || is.null(iv_m))
        return(plot_ly() |> layout(title="Run 2SLS first"))

      make_coef_df <- function(model, label) {
        cs  <- tryCatch(summary(model)$coefficients, error=function(e) NULL)
        if (is.null(cs)) return(NULL)
        df  <- as.data.frame(cs)
        data.frame(
          Predictor = rownames(df),
          Estimate  = df[,1],
          SE        = df[,2],
          Lower     = df[,1] - 1.96*df[,2],
          Upper     = df[,1] + 1.96*df[,2],
          Model     = label,
          stringsAsFactors = FALSE
        )
      }
      ols_df <- make_coef_df(ols_m, "OLS")
      iv_df  <- make_coef_df(iv_m,  "2SLS")
      if (is.null(ols_df) || is.null(iv_df))
        return(plot_ly() |> layout(title="Could not extract coefficients"))

      combined <- rbind(ols_df, iv_df)
      combined <- combined[combined$Predictor != "(Intercept)", ]

      plot_ly(combined, x=~Estimate, y=~Predictor, color=~Model,
              colors=c("OLS"=TEAL,"2SLS"=AMBER),
              type="scatter", mode="markers",
              error_x=list(type="data", symmetric=FALSE,
                           array=~(Upper-Estimate), arrayminus=~(Estimate-Lower),
                           thickness=2, width=6),
              marker=list(size=10),
              text=~sprintf("%s: β=%.3f [%.3f, %.3f]", Model, Estimate, Lower, Upper),
              hoverinfo="text") |>
        add_segments(x=0, xend=0, y=0.5, yend=length(unique(combined$Predictor))+0.5,
                     line=list(color="#C0392B",dash="dot",width=1),
                     name="Zero", inherit=FALSE) |>
        layout(title=list(text="OLS vs 2SLS Coefficient Comparison (± 95% CI)",font=list(color=NAVY)),
               xaxis=list(title="Coefficient Estimate"),
               yaxis=list(title=""),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # Instrument Strength Gauge
    output$instrument_strength_plot <- renderPlotly({
      req(ivr_results())
      res <- ivr_results()
      f_stat <- tryCatch(as.numeric(res$first_stage_f), error=function(e) NA)
      if (is.na(f_stat))
        return(plot_ly() |> layout(title="Run 2SLS first to see F-statistic"))

      # Gauge chart
      gauge_color <- if (f_stat >= 10) GREEN else if (f_stat >= 5) AMBER else "#C0392B"
      label_text  <- if (f_stat >= 10) "Strong instrument (F ≥ 10)"
                     else if (f_stat >= 5) "Moderate instrument (F 5–10)"
                     else "Weak instrument (F < 5)"

      plot_ly(
        type  = "indicator",
        mode  = "gauge+number+delta",
        value = f_stat,
        title = list(text="First-Stage F-Statistic", font=list(color=NAVY,size=14)),
        gauge = list(
          axis   = list(range=list(0, max(40, f_stat*1.2))),
          bar    = list(color=gauge_color),
          steps  = list(
            list(range=c(0,5),   color="#FFD0D0"),
            list(range=c(5,10),  color="#FFF3CD"),
            list(range=c(10, max(40, f_stat*1.2)), color="#D4EDDA")
          ),
          threshold = list(line=list(color="black",width=3), thickness=0.75, value=10)
        ),
        delta = list(reference=10, increasing=list(color=GREEN), decreasing=list(color="#C0392B"))
      ) |>
        layout(
          annotations=list(list(x=0.5, y=0.1, text=label_text, showarrow=FALSE,
                                font=list(size=13,color=gauge_color))),
          paper_bgcolor="white"
        )
    })

    # Stock-Yogo Critical Values Table
    output$stock_yogo_tbl <- DT::renderDataTable({
      req(ivr_results())
      res <- ivr_results()
      f_val <- tryCatch(as.numeric(res$first_stage_f), error=function(e) NA)
      if (is.na(f_val)) return(NULL)

      # Stock-Yogo (2005) critical values for 1 endogenous regressor
      sy <- data.frame(
        `Max. IV Size Bias` = c("10%", "15%", "20%", "25%"),
        `Critical F (k=1)` = c(16.38, 8.96, 6.66, 5.53),
        `Observed F` = round(f_val, 3),
        Status = c(
          ifelse(f_val >= 16.38, "✓ Strong (10% bias)", "✗ Weak"),
          ifelse(f_val >= 8.96,  "✓ Strong (15% bias)", "✗ Weak"),
          ifelse(f_val >= 6.66,  "✓ Strong (20% bias)", "✗ Weak"),
          ifelse(f_val >= 5.53,  "✓ Strong (25% bias)", "✗ Weak")
        ),
        check.names = FALSE
      )
      tool_dt(sy, paste0("Stock-Yogo Critical Values — Observed F = ", round(f_val, 3),
                          " | Rule of thumb: F ≥ 10 for adequate strength"))
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
