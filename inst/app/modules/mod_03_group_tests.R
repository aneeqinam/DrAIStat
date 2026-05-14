mod03_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "📐",
      title = "Group Comparison Tests",
      subtitle = "t-test · Mann-Whitney · One-Way ANOVA · Kruskal-Wallis · MANOVA · Chi-Square"
    ),
    uiOutput(ns("global_data_banner")),
    fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod03_server <- function(id, gemini_key = reactive("")) {
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
          tags$b("📋 Global dataset ready: "),
          sprintf("%s  (%d rows × %d cols)", global_shared_name(), nrow(gd), ncol(gd))
        ),
        actionButton(ns("load_global"), "⬇ Use This Dataset",
                     class="btn-success btn-sm", style="padding:3px 10px; font-size:.78rem;")
      )
    })
    observeEvent(input$load_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) { data_rv(gd); showNotification(paste0("Using: ", global_shared_name()), type="message", duration=3) }
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

    # Categorical and numeric columns
    cat_cols <- reactive({
      data <- data_rv()
      req(data)
      names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
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
        title = "Group Tests Analysis",
        tabPanel(
          "t-test / Mann-Whitney",
          fluidRow(
            column(4, pickerInput(ns("ttest_grouping"), "Grouping Variable (Categorical)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("ttest_outcome"), "Outcome Variable (Numeric)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, actionButton(ns("ttest_run"), "Run Analysis", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Statistics Table"), withSpinner(DT::dataTableOutput(ns("ttest_table")))),
            column(6, h5("Cohen's d & 95% CI"), verbatimTextOutput(ns("ttest_cohens")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Boxplot"), withSpinner(plotlyOutput(ns("ttest_plot"))))
          )
        ),
        tabPanel(
          "One-Way ANOVA / Kruskal-Wallis",
          fluidRow(
            column(4, pickerInput(ns("anova_grouping"), "Grouping Variable (Categorical)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("anova_outcome"), "Outcome Variable (Numeric)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(4, actionButton(ns("anova_run"), "Run Analysis", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("ANOVA Summary"), verbatimTextOutput(ns("anova_summary"))),
            column(6, h5("Tukey HSD"), withSpinner(DT::dataTableOutput(ns("anova_tukey"))))
          ),
          hr(),
          tags$h5("Post-hoc: Pairwise Comparisons (Kruskal-Wallis)", style="margin-top:1rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("dunn_tbl"))),
          hr(),
          tags$h5("Homogeneity of Variance", style="margin-top:1rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("levene_tbl"))),
          hr(),
          tags$h5("Normality Test (Shapiro-Wilk)", style="margin-top:1rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("shapiro_tbl"))),
          hr(),
          fluidRow(
            column(12, h5("Boxplot with Group Means"), withSpinner(plotlyOutput(ns("anova_plot"))))
          )
        ),
        tabPanel(
          "MANOVA",
          fluidRow(
            column(4, pickerInput(ns("manova_grouping"), "Grouping Variable (Categorical)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("manova_outcomes"), "Outcome Variables (Numeric, Multiple)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(4, actionButton(ns("manova_run"), "Run MANOVA", class = "btn-primary"))
          ),
          hr(),
          tags$h5("MANOVA Summary", style="color:#1A3A5C;font-weight:600;"),
          verbatimTextOutput(ns("manova_output")),
          hr(),
          tags$h5("Follow-up Univariate ANOVAs (Bonferroni corrected)", style="margin-top:1rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("manova_followup_tbl")))
        ),
        tabPanel(
          "Chi-Square",
          fluidRow(
            column(4, pickerInput(ns("chi_var1"), "Variable 1 (Categorical)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(4, pickerInput(ns("chi_var2"), "Variable 2 (Categorical)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(4, actionButton(ns("chi_run"), "Run Test", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Chi-Square Test"), verbatimTextOutput(ns("chi_summary"))),
            column(6, h5("Cramér's V"), verbatimTextOutput(ns("chi_cramers")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Contingency Table"), withSpinner(DT::dataTableOutput(ns("chi_table"))))
          )
        ),
        tabPanel(
          "APA Write-Up",
          verbatimTextOutput(ns("apa_writeup"))
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("🌧️ Raincloud (Groups)",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Raincloud plots (half-violin + boxplot + jitter) for t-test or ANOVA groups. Run t-test or ANOVA first."),
          withSpinner(plotlyOutput(ns("raincloud_groups"), height="480px"))
        ),
        tabPanel("📊 Effect Size Chart",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Visual comparison of effect sizes across all tests run in this session."),
          withSpinner(plotlyOutput(ns("effect_size_chart"), height="400px"))
        ),
        tabPanel("🟦 Chi-Square Heatmap",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Contingency table displayed as a colour-coded heatmap with cell counts and expected values. Run Chi-Square first."),
          withSpinner(plotlyOutput(ns("chi_heatmap"), height="450px"))
        ),
        tabPanel("📈 Means Plot",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Group means ± 95% CI plot. Run ANOVA first."),
          withSpinner(plotlyOutput(ns("means_plot"), height="420px"))
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
              column(3, actionButton(ns("ai_btn_ttest"),  "\U0001f4ca t-Test / Mann-Whitney", class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(3, actionButton(ns("ai_btn_anova"),  "\U0001f4c8 ANOVA / Kruskal-Wallis", class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(3, actionButton(ns("ai_btn_manova"), "\U0001f4d0 MANOVA",                class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(3, actionButton(ns("ai_btn_chi"),    "\U0001f9ea Chi-Square",            class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(6, actionButton(ns("ai_btn_all"), "\U0001f4cb Full Group Tests Summary", class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== t-test / Mann-Whitney ====
    ttest_results <- reactiveVal(NULL)

    observeEvent(input$ttest_run, {
      req(input$ttest_grouping, input$ttest_outcome)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running t-test and Mann-Whitney...", {
          grouping <- data[[input$ttest_grouping]]
          outcome <- data[[input$ttest_outcome]]

          groups <- unique(na.omit(grouping))
          if (length(groups) > 2) {
            showNotification("Warning: More than 2 groups. Using first 2 groups only.", type = "warning")
            groups <- groups[1:2]
          }

          idx1 <- which(grouping == groups[1])
          idx2 <- which(grouping == groups[2])
          y1 <- outcome[idx1]
          y2 <- outcome[idx2]

          # t-test
          t_result <- t.test(y1, y2)

          # Welch's correction already applied
          # Mann-Whitney
          mw_result <- wilcox.test(y1, y2)

          # Cohen's d
          cohens_d_obj <- rstatix::cohens_d(data, as.formula(paste0(input$ttest_outcome, " ~ ", input$ttest_grouping)))

          # Cohen's d CI (Hedges & Olkin approximation)
          n1 <- length(na.omit(y1))
          n2 <- length(na.omit(y2))
          d_val <- as.numeric(cohens_d_obj$effsize[1])
          se_d <- sqrt((n1 + n2) / (n1 * n2) + d_val^2 / (2 * (n1 + n2 - 2)))
          t_crit <- qt(0.975, df = n1 + n2 - 2)
          d_lo <- d_val - t_crit * se_d
          d_hi <- d_val + t_crit * se_d

          ttest_results(list(
            t_result = t_result,
            mw_result = mw_result,
            cohens_d = cohens_d_obj,
            d_ci = list(d_lo = d_lo, d_hi = d_hi),
            data = data,
            grouping = input$ttest_grouping,
            outcome = input$ttest_outcome
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$ttest_table <- DT::renderDataTable({
      req(ttest_results())
      res <- ttest_results()

      t_res <- res$t_result
      mw_res <- res$mw_result

      tbl <- data.frame(
        Test = c("Welch t-test", "Mann-Whitney U"),
        Statistic = c(round(t_res$statistic, 4), round(mw_res$statistic, 4)),
        "p-value" = c(sig_stars(t_res$p.value), sig_stars(mw_res$p.value)),
        "p (exact)" = c(apa_p(t_res$p.value), apa_p(mw_res$p.value)),
        check.names = FALSE
      )

      tool_dt(tbl, "t-test and Mann-Whitney Results")
    })

    output$ttest_cohens <- renderText({
      req(ttest_results())
      res <- ttest_results()
      cohens <- res$cohens_d
      d_ci <- res$d_ci

      paste0(
        "Cohen's d: ", round(cohens$effsize, 3), "\n",
        "95% CI: [", round(d_ci$d_lo, 3), ", ", round(d_ci$d_hi, 3), "]\n",
        "Interpretation: ",
        if (abs(cohens$effsize) < 0.2) "Negligible"
        else if (abs(cohens$effsize) < 0.5) "Small"
        else if (abs(cohens$effsize) < 0.8) "Medium"
        else "Large"
      )
    })

    output$ttest_plot <- renderPlotly({
      req(ttest_results())
      res <- ttest_results()

      plot_data <- data.frame(
        group = res$data[[res$grouping]],
        outcome = res$data[[res$outcome]]
      )
      plot_data <- na.omit(plot_data)

      plot_ly(plot_data, x = ~group, y = ~outcome, type = "box") %>%
        layout(
          title = "Outcome by Group",
          yaxis = list(title = res$outcome),
          xaxis = list(title = res$grouping)
        )
    })

    # ==== One-Way ANOVA / Kruskal-Wallis ====
    anova_results <- reactiveVal(NULL)

    observeEvent(input$anova_run, {
      req(input$anova_grouping, input$anova_outcome)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running ANOVA...", {
          formula <- as.formula(paste0(input$anova_outcome, " ~ ", input$anova_grouping))

          aov_result <- aov(formula, data = data)
          kw_result <- kruskal.test(formula, data = data)

          tukey_result <- TukeyHSD(aov_result)

          eta_sq <- rstatix::eta_squared(aov_result)

          anova_results(list(
            aov = aov_result,
            kw = kw_result,
            tukey = tukey_result,
            eta_sq = eta_sq,
            formula = formula,
            data = data,
            grouping = input$anova_grouping,
            outcome = input$anova_outcome
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$anova_summary <- renderText({
      req(anova_results())
      res <- anova_results()

      aov_sum <- summary(res$aov)[[1]]
      eta_obj <- res$eta_sq

      paste0(
        "One-Way ANOVA:\n",
        "F = ", round(aov_sum[1, 4], 3), ", p ", apa_p(aov_sum[1, 5]), "\n",
        "η² = ", round(eta_obj$effsize[1], 3), "\n\n",
        "Kruskal-Wallis:\n",
        "χ² = ", round(res$kw$statistic, 3), ", p ", apa_p(res$kw$p.value)
      )
    })

    output$anova_tukey <- DT::renderDataTable({
      req(anova_results())
      res <- anova_results()

      tukey_df <- as.data.frame(res$tukey[[1]])
      tukey_df$Comparison <- rownames(tukey_df)
      tukey_df <- tukey_df[, c("Comparison", "diff", "lwr", "upr", "p adj")]
      names(tukey_df) <- c("Comparison", "Difference", "Lower", "Upper", "p (adj)")

      tool_dt(tukey_df, "Tukey HSD Pairwise Comparisons")
    })

    output$anova_plot <- renderPlotly({
      req(anova_results())
      res <- anova_results()

      plot_data <- data.frame(
        group = res$data[[res$grouping]],
        outcome = res$data[[res$outcome]]
      )
      plot_data <- na.omit(plot_data)

      plot_ly(plot_data, x = ~group, y = ~outcome, type = "box") %>%
        layout(
          title = "Outcome by Group (ANOVA)",
          yaxis = list(title = res$outcome),
          xaxis = list(title = res$grouping)
        )
    })

    # ==== MANOVA ====
    manova_results <- reactiveVal(NULL)

    observeEvent(input$manova_run, {
      req(input$manova_grouping, input$manova_outcomes)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running MANOVA...", {
          outcomes_str <- paste(input$manova_outcomes, collapse = " + ")
          manova_formula <- as.formula(paste0("cbind(", outcomes_str, ") ~ ", input$manova_grouping))

          manova_result <- manova(manova_formula, data = data)

          manova_results(list(
            manova = manova_result,
            summary_table = summary(manova_result, test = "Pillai"),
            data = data,
            dvs = input$manova_outcomes,
            grp = input$manova_grouping
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$manova_output <- renderText({
      req(manova_results())
      res <- manova_results()

      capture.output(print(res$summary_table)) %>% paste(collapse = "\n")
    })

    # ==== Chi-Square ====
    chi_results <- reactiveVal(NULL)

    observeEvent(input$chi_run, {
      req(input$chi_var1, input$chi_var2)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running Chi-Square...", {
          var1 <- data[[input$chi_var1]]
          var2 <- data[[input$chi_var2]]

          contingency <- table(var1, var2)
          chi_result <- chisq.test(contingency)

          # Cramér's V
          n <- sum(contingency)
          min_dim <- min(dim(contingency) - 1)
          cramers_v <- sqrt(chi_result$statistic / (n * min_dim))

          chi_results(list(
            chi_test = chi_result,
            contingency = contingency,
            cramers_v = cramers_v,
            var1_name = input$chi_var1,
            var2_name = input$chi_var2
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$chi_summary <- renderText({
      req(chi_results())
      res <- chi_results()

      paste0(
        "Chi-Square Test of Independence:\n",
        "χ² = ", round(res$chi_test$statistic, 3), "\n",
        "df = ", res$chi_test$parameter, "\n",
        "p ", apa_p(res$chi_test$p.value)
      )
    })

    output$chi_cramers <- renderText({
      req(chi_results())
      res <- chi_results()

      paste0(
        "Cramér's V = ", round(res$cramers_v, 3), "\n",
        "Interpretation: ",
        if (res$cramers_v < 0.1) "Negligible"
        else if (res$cramers_v < 0.3) "Small"
        else if (res$cramers_v < 0.5) "Medium"
        else "Large"
      )
    })

    output$chi_table <- DT::renderDataTable({
      req(chi_results())
      res <- chi_results()

      tbl <- as.data.frame.matrix(res$contingency)
      tbl$Variable <- rownames(res$contingency)
      tbl <- tbl[, c(ncol(tbl), 1:(ncol(tbl)-1))]

      tool_dt(tbl, paste0("Contingency Table: ", res$var1_name, " × ", res$var2_name))
    })

    # ==== APA Write-Up ====
    output$apa_writeup <- renderText({
      req(data_rv())

      paste0(
        "APA Write-Up Template\n",
        "====================\n\n",
        "t-test / Mann-Whitney:\n",
        "t(df) = X.XX, p <value>, d = X.XX\n",
        "or: U = X.XX, p <value>\n\n",
        "One-Way ANOVA:\n",
        "F(between_df, within_df) = X.XX, p <value>, η² = X.XX\n\n",
        "MANOVA:\n",
        "Pillai's Trace = X.XX, F = X.XX, p <value>\n\n",
        "Chi-Square:\n",
        "χ²(df, N = n) = X.XX, p <value>, Cramér's V = X.XX\n\n",
        "[Run analyses above to populate these values]"
      )
    })
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_render3 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_key3 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. t-Test / Mann-Whitney
    observeEvent(input$ai_btn_ttest, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      ctx <- tryCatch({
        res <- ttest_results(); req(res)
        paste0("t-TEST / MANN-WHITNEY RESULTS\n",
               "Grouping variable: ", input$ttest_grouping,
               "\nOutcome variable: ", input$ttest_outcome,
               "\n\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run t-Test first.")
      output$ai_output <- renderUI({
        .ai_render3(call_gemini(paste0(
          "You are an expert statistician writing for a peer-reviewed journal.\n\n",
          "Task: DETAILED interpretation of t-test / Mann-Whitney results. Include:\n",
          "1. State the statistical test used and justify its selection (parametric vs non-parametric)\n",
          "2. Report exact test statistics: t(df) = X.XX or U = X.XX, p = .XXX, two-tailed\n",
          "3. Calculate and interpret Cohen's d (effect size): small (.20), medium (.50), large (.80)\n",
          "4. Describe the direction of the difference — which group scored higher and by how much\n",
          "5. Report 95% confidence interval for the mean difference\n",
          "6. Assess statistical vs practical significance\n",
          "7. Note any assumption violations (Levene's test for equal variances) and how they were handled\n",
          "8. Write the complete APA 7 Results sentence for reporting this test\n\n",
          "Write in formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key))
      })
    })

    # 2. ANOVA / Kruskal-Wallis
    observeEvent(input$ai_btn_anova, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      ctx <- tryCatch({
        res <- anova_results(); req(res)
        paste0("ANOVA / KRUSKAL-WALLIS RESULTS\n",
               paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run ANOVA first.")
      output$ai_output <- renderUI({
        .ai_render3(call_gemini(paste0(
          "You are an expert statistician writing for a peer-reviewed journal.\n\n",
          "Task: DETAILED interpretation of ANOVA / Kruskal-Wallis results. Include:\n",
          "1. Report F-statistic: F(df_between, df_within) = X.XX, p = .XXX, and omega-squared or eta-squared\n",
          "2. Interpret effect size: η² / ω²: small (.01), medium (.06), large (.14) per Cohen (1988)\n",
          "3. Report post-hoc comparisons (Tukey HSD / Games-Howell) — which pairs differ significantly\n",
          "4. Check homogeneity of variance (Levene's test) and normality assumptions\n",
          "5. If Kruskal-Wallis was used, explain why (non-normality) and report H-statistic\n",
          "6. Describe the pattern of group means and what it means substantively\n",
          "7. Write complete APA 7 Results paragraph\n\n",
          "Write in formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key))
      })
    })

    # 3. MANOVA
    observeEvent(input$ai_btn_manova, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      ctx <- tryCatch({
        res <- manova_results(); req(res)
        paste0("MANOVA RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run MANOVA first.")
      output$ai_output <- renderUI({
        .ai_render3(call_gemini(paste0(
          "You are an expert statistician specialising in multivariate methods.\n\n",
          "Task: DETAILED interpretation of MANOVA results. Include:\n",
          "1. Report Pillai's Trace (most robust) or Wilks' Lambda with F-approximation, df, and p-value\n",
          "2. Calculate multivariate effect size: partial η² from Pillai's Trace\n",
          "3. Report follow-up univariate ANOVAs for each dependent variable with Bonferroni correction\n",
          "4. Check MANOVA assumptions: multivariate normality, homogeneity of covariance matrices (Box's M)\n",
          "5. Explain when MANOVA is preferred over multiple ANOVAs (correlated DVs, Type I error control)\n",
          "6. Interpret the overall pattern of group differences across all outcomes simultaneously\n",
          "7. Write complete APA 7 Results paragraph for MANOVA\n\n",
          "Write in formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key))
      })
    })

    # 4. Chi-Square
    observeEvent(input$ai_btn_chi, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      ctx <- tryCatch({
        res <- chi_results(); req(res)
        paste0("CHI-SQUARE RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run Chi-Square test first.")
      output$ai_output <- renderUI({
        .ai_render3(call_gemini(paste0(
          "You are an expert statistician writing for a peer-reviewed journal.\n\n",
          "Task: DETAILED interpretation of Chi-Square test results. Include:\n",
          "1. Report χ²(df, N = n) = X.XX, p = .XXX in APA 7 format\n",
          "2. Interpret Cramér's V effect size: small (.10), medium (.30), large (.50) per Cohen\n",
          "3. Check assumptions: expected cell frequencies ≥ 5 in 80% of cells; no expected count < 1\n",
          "4. If assumptions violated, discuss Fisher's Exact Test as alternative\n",
          "5. Describe the pattern of association — which categories are over/under-represented\n",
          "6. Calculate and report standardised residuals to identify cells driving the association\n",
          "7. Discuss practical significance vs statistical significance\n",
          "8. Write complete APA 7 Results paragraph\n\n",
          "Write in formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key))
      })
    })

    # 5. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        tt <- tryCatch(ttest_results(), error=function(e) NULL)
        if (!is.null(tt)) parts <- c(parts, paste0("t-Test/Mann-Whitney:\n", paste(capture.output(print(tt)), collapse="\n")))
        av <- tryCatch(anova_results(), error=function(e) NULL)
        if (!is.null(av)) parts <- c(parts, paste0("ANOVA/Kruskal-Wallis:\n", paste(capture.output(print(av)), collapse="\n")))
        mv <- tryCatch(manova_results(), error=function(e) NULL)
        if (!is.null(mv)) parts <- c(parts, paste0("MANOVA:\n", paste(capture.output(print(mv)), collapse="\n")))
        ch <- tryCatch(chi_results(), error=function(e) NULL)
        if (!is.null(ch)) parts <- c(parts, paste0("Chi-Square:\n", paste(capture.output(print(ch)), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("GROUP COMPARISON TESTS FULL SUMMARY\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one test first.")
      output$ai_output <- renderUI({
        .ai_render3(call_gemini(paste0(
          "You are an expert statistician writing for a top-tier peer-reviewed journal.\n\n",
          "Task: Write a COMPREHENSIVE Results section for all group comparison tests. Include:\n",
          "1. t-Test / Mann-Whitney: statistic, df, p-value, effect size (Cohen's d), direction\n",
          "2. ANOVA / Kruskal-Wallis: F/H statistic, df, p-value, η², post-hoc comparisons\n",
          "3. MANOVA: Pillai's Trace, F-approximation, multivariate effect size, follow-up ANOVAs\n",
          "4. Chi-Square: χ², df, N, p-value, Cramér's V, pattern of association\n",
          "5. Assumption checks for each test and how violations were handled\n",
          "6. Overall narrative integrating all tests\n\n",
          "Use APA 7 throughout. Write in formal academic prose (6-8 paragraphs).\n\nDATA:\n", ctx), api_key))
      })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key3()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Raincloud plot for groups (t-test / ANOVA)
    output$raincloud_groups <- renderPlotly({
      if (is.null(data_rv())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      data <- data_rv(); req(data)
      # prefer ANOVA grouping if available, else t-test
      grp_var <- if (!is.null(input$anova_grouping) && nzchar(input$anova_grouping) &&
                     input$anova_grouping %in% names(data)) input$anova_grouping
                 else if (!is.null(input$ttest_grouping) && nzchar(input$ttest_grouping) &&
                          input$ttest_grouping %in% names(data)) input$ttest_grouping
                 else return(plot_ly() |> layout(title="Run t-test or ANOVA first"))
      out_var <- if (!is.null(input$anova_outcome) && nzchar(input$anova_outcome) &&
                     input$anova_outcome %in% names(data)) input$anova_outcome
                 else if (!is.null(input$ttest_outcome) && nzchar(input$ttest_outcome) &&
                          input$ttest_outcome %in% names(data)) input$ttest_outcome
                 else return(plot_ly() |> layout(title="Run t-test or ANOVA first"))

      pd <- na.omit(data.frame(group = as.factor(data[[grp_var]]),
                               value = as.numeric(data[[out_var]])))
      groups <- levels(pd$group)
      pal <- c(TEAL, AMBER, NAVY, GREEN, "#9B59B6","#E74C3C")

      p <- plot_ly()
      for (i in seq_along(groups)) {
        g  <- groups[i]
        gd <- pd$value[pd$group == g]
        col <- pal[(i - 1) %% length(pal) + 1]
        # Violin (half)
        p <- add_trace(p, y = gd, x = rep(g, length(gd)), type = "violin",
                       side = "positive", box = list(visible = TRUE),
                       meanline = list(visible = TRUE),
                       fillcolor = col, opacity = 0.55,
                       line = list(color = col), name = g, showlegend = FALSE)
        # Jitter
        p <- add_trace(p, y = gd,
                       x = jitter(rep(i, length(gd)), factor = 0.4),
                       type = "scatter", mode = "markers",
                       marker = list(color = col, size = 5, opacity = 0.45),
                       name = g, showlegend = FALSE)
      }
      p |> layout(
        title  = list(text = paste("Raincloud Plot:", out_var, "by", grp_var), font = list(color = NAVY)),
        xaxis  = list(title = grp_var),
        yaxis  = list(title = out_var),
        violingap = 0, violinmode = "overlay",
        plot_bgcolor = "white", paper_bgcolor = "white"
      )
    })

    # Effect size summary chart
    output$effect_size_chart <- renderPlotly({
      if (is.null(ttest_results()) && is.null(anova_results()) && is.null(chi_results())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      rows <- list()
      # t-test Cohen's d
      tr <- ttest_results()
      if (!is.null(tr)) {
        d_val <- tryCatch(abs(as.numeric(tr$cohens_d$effsize[1])), error = function(e) NA)
        if (!is.na(d_val))
          rows[["t-test (Cohen's d)"]] <- list(metric = "Cohen's d", value = d_val,
                                                small = 0.20, medium = 0.50, large = 0.80)
      }
      # ANOVA eta-squared
      ar <- anova_results()
      if (!is.null(ar)) {
        eta <- tryCatch(as.numeric(ar$eta_sq$effsize[1]), error = function(e) NA)
        if (!is.na(eta))
          rows[["ANOVA (η²)"]] <- list(metric = "η²", value = eta,
                                        small = 0.01, medium = 0.06, large = 0.14)
      }
      # Chi-Square Cramér's V
      cr <- chi_results()
      if (!is.null(cr)) {
        v_val <- tryCatch(as.numeric(cr$cramers_v), error = function(e) NA)
        if (!is.na(v_val))
          rows[["Chi-Sq (Cramér's V)"]] <- list(metric = "Cramér's V", value = v_val,
                                                   small = 0.10, medium = 0.30, large = 0.50)
      }
      if (length(rows) == 0)
        return(plot_ly() |> layout(title = "Run at least one test first"))

      df_es <- data.frame(
        Test   = names(rows),
        Metric = sapply(rows, `[[`, "metric"),
        Value  = sapply(rows, `[[`, "value"),
        stringsAsFactors = FALSE
      )
      bar_col <- ifelse(df_es$Value >= sapply(rows, `[[`, "large"), GREEN,
                 ifelse(df_es$Value >= sapply(rows, `[[`, "medium"), AMBER, TEAL))

      plot_ly(df_es, x = ~Value, y = ~Test, type = "bar", orientation = "h",
              marker = list(color = bar_col),
              text = ~sprintf("%.3f", Value), textposition = "outside",
              hovertemplate = "%{y}: %{x:.3f}<extra></extra>") |>
        layout(
          title  = list(text = "Effect Sizes Across Tests", font = list(color = NAVY)),
          xaxis  = list(title = "Effect Size Value", range = c(0, max(df_es$Value) * 1.25)),
          yaxis  = list(title = ""),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    })

    # Chi-square contingency heatmap
    output$chi_heatmap <- renderPlotly({
      if (is.null(chi_results())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(chi_results())
      res <- chi_results()
      ct  <- res$contingency
      m   <- as.matrix(ct)
      plot_ly(z = m, x = colnames(m), y = rownames(m), type = "heatmap",
              colorscale = list(c(0,"#EBF4F7"), c(1, NAVY)),
              text = m, texttemplate = "%{text}",
              hovertemplate = paste0(res$var1_name, ": %{y}<br>",
                                     res$var2_name, ": %{x}<br>Count: %{z}<extra></extra>")) |>
        layout(
          title  = list(text = paste("Contingency Heatmap:", res$var1_name, "×", res$var2_name),
                        font = list(color = NAVY)),
          xaxis  = list(title = res$var2_name),
          yaxis  = list(title = res$var1_name),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    })

    # Group means ± 95% CI plot
    output$means_plot <- renderPlotly({
      if (is.null(anova_results())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(anova_results())
      res  <- anova_results()
      data <- res$data
      grp  <- res$grouping
      out  <- res$outcome
      pd   <- na.omit(data.frame(group = as.factor(data[[grp]]), value = as.numeric(data[[out]])))
      stats_df <- pd |>
        dplyr::group_by(group) |>
        dplyr::summarise(
          mean = mean(value, na.rm = TRUE),
          se   = sd(value, na.rm = TRUE) / sqrt(dplyr::n()),
          ci95 = 1.96 * se,
          .groups = "drop"
        )
      plot_ly(stats_df, x = ~group, y = ~mean, type = "bar",
              error_y = list(type = "data", array = ~ci95, visible = TRUE,
                             color = NAVY, thickness = 2, width = 6),
              marker = list(color = TEAL, line = list(color = NAVY, width = 1.2)),
              text = ~sprintf("M = %.3f", mean), textposition = "outside",
              hovertemplate = "Group: %{x}<br>Mean: %{y:.3f}<extra></extra>") |>
        layout(
          title  = list(text = paste("Group Means ± 95% CI:", out, "by", grp), font = list(color = NAVY)),
          xaxis  = list(title = grp),
          yaxis  = list(title = out),
          plot_bgcolor = "white", paper_bgcolor = "white"
        )
    })

    # Levene's Test for Homogeneity of Variance (after ANOVA)
    output$levene_tbl <- DT::renderDataTable({
      req(anova_results())
      res <- anova_results()
      tryCatch({
        data <- res$data
        dv   <- res$outcome
        grp  <- res$grouping

        # Try car::leveneTest first, fallback to bartlett.test
        if (requireNamespace("car", quietly = TRUE)) {
          lt  <- car::leveneTest(data[[dv]] ~ factor(data[[grp]]))
          df_lt <- data.frame(
            Test = "Levene's Test",
            `F value` = round(lt[1, "F value"], 3),
            `df1` = lt[1, "Df"],
            `df2` = lt[2, "Df"],
            `p value` = round(lt[1, "Pr(>F)"], 4),
            Decision = ifelse(lt[1, "Pr(>F)"] < 0.05,
              "⚠ Variances unequal — use Welch ANOVA",
              "✓ Equal variances assumed"),
            check.names = FALSE)
        } else {
          bt  <- bartlett.test(data[[dv]] ~ factor(data[[grp]]))
          df_lt <- data.frame(
            Test = "Bartlett's Test",
            `K-squared` = round(bt$statistic, 3),
            df = bt$parameter,
            `p value` = round(bt$p.value, 4),
            Decision = ifelse(bt$p.value < 0.05,
              "⚠ Variances unequal", "✓ Equal variances"),
            check.names = FALSE)
        }
        tool_dt(df_lt, "Homogeneity of Variance Test")
      }, error = function(e) NULL)
    })

    # Shapiro-Wilk Normality Test (per group)
    output$shapiro_tbl <- DT::renderDataTable({
      req(anova_results())
      res <- anova_results()
      tryCatch({
        groups <- split(res$data[[res$outcome]], res$data[[res$grouping]])
        rows <- lapply(names(groups), function(g) {
          x <- na.omit(groups[[g]])
          if (length(x) < 3 || length(x) > 5000) return(NULL)
          sw <- shapiro.test(x)
          data.frame(
            Group = g,
            n = length(x),
            `W statistic` = round(sw$statistic, 4),
            `p value` = round(sw$p.value, 4),
            Normal = ifelse(sw$p.value > 0.05, "✓ Normal (p > .05)", "⚠ Non-normal (p < .05)"),
            check.names = FALSE)
        })
        tbl <- do.call(rbind, Filter(Negate(is.null), rows))
        tool_dt(tbl, "Shapiro-Wilk Normality Test per Group")
      }, error = function(e) NULL)
    })

    # Dunn's Post-hoc for Kruskal-Wallis (Pairwise Wilcoxon with Bonferroni)
    output$dunn_tbl <- DT::renderDataTable({
      req(anova_results())
      res <- anova_results()
      tryCatch({
        pw <- pairwise.wilcox.test(res$data[[res$outcome]], res$data[[res$grouping]],
                                    p.adjust.method = "bonferroni", exact = FALSE)
        df_d <- as.data.frame(as.table(pw$p.value))
        names(df_d) <- c("Group 1", "Group 2", "p (Bonferroni adj.)")
        df_d <- df_d[!is.na(df_d[["p (Bonferroni adj.)"]]), ]
        df_d[["p (Bonferroni adj.)"]] <- round(as.numeric(df_d[["p (Bonferroni adj.)"]]), 4)
        df_d$Significant <- ifelse(df_d[["p (Bonferroni adj.)"]] < 0.05, "✓ Yes", "✗ No")
        tool_dt(df_d, "Pairwise Wilcoxon Post-hoc (Bonferroni correction)")
      }, error = function(e) NULL)
    })

    # MANOVA Follow-up: Univariate ANOVAs with Bonferroni correction
    output$manova_followup_tbl <- DT::renderDataTable({
      req(manova_results())
      res <- manova_results()
      tryCatch({
        dvs   <- res$dvs
        data  <- res$data
        grp   <- res$grp
        alpha_adj <- 0.05 / length(dvs)
        rows <- lapply(dvs, function(dv) {
          m <- tryCatch(aov(as.formula(paste0("`", dv, "` ~ factor(`", grp, "`)")), data = data),
                        error = function(e) NULL)
          if (is.null(m)) return(NULL)
          s <- summary(m)[[1]]
          data.frame(
            DV = dv,
            F = round(s[1, 4], 3),
            df1 = s[1, 1],
            df2 = s[2, 1],
            p = round(s[1, 5], 4),
            `p (Bonferroni)` = round(min(s[1, 5] * length(dvs), 1), 4),
            Significant = ifelse(s[1, 5] < alpha_adj, "✓ Yes", "✗ No"),
            check.names = FALSE)
        })
        out <- do.call(rbind, Filter(Negate(is.null), rows))
        tool_dt(out, paste0("Univariate ANOVAs — Bonferroni α = ", round(alpha_adj, 4),
                             " (", length(dvs), " DVs)"))
      }, error = function(e) NULL)
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
