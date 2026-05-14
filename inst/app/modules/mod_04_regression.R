# ── Module 04: Regression Analysis ───────────────────────────────────────────
# Simple · Multiple · Hierarchical OLS · Curvilinear (Polynomial) · Diagnostics
# Dr. Aneeq Inam

mod04_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon     = "📈",
      title    = "Regression Analysis",
      subtitle = paste0("Simple · Multiple · Hierarchical OLS · ",
                        "Curvilinear (Quadratic / Cubic / Response Surface) · ",
                        "Diagnostics · APA Write-Up")
    ),
    uiOutput(ns("global_data_banner")),
    fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV / Excel",
                          accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod04_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Data ─────────────────────────────────────────────────────────────────
    data_rv <- reactiveVal(NULL)

    # Global data banner
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
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
    })

    output$file_info <- renderUI({
      d <- data_rv(); if (is.null(d)) return(NULL)
      tags$div(
        style = "background:#f5f5f5;padding:10px;border-radius:4px;",
        tags$strong(paste0("Loaded: ", nrow(d), " rows × ", ncol(d), " columns"))
      )
    })

    num_cols <- reactive({
      d <- data_rv(); req(d)
      numeric_cols(d)
    })

    # ── Tab layout ────────────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      req(data_rv())
      tabBox(
        width = 12, title = "Regression Analysis",

        # ── Tab 1: Simple / Multiple ─────────────────────────────────────────
        tabPanel("Simple / Multiple Regression",
          fluidRow(
            column(4, pickerInput(ns("simple_dv"), "Dependent Variable (DV)",
                                  choices = num_cols(),
                                  options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("simple_ivs"), "Independent Variables (IVs)",
                                  choices = num_cols(), multiple = TRUE,
                                  options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(4, numericInput(ns("vif_threshold"), "VIF warning threshold:", value = 10, min = 2, max = 30, step = 1))
          ),
          actionButton(ns("simple_run"), "Run Regression", class = "btn-primary"),
          hr(),
          h5("Regression Coefficients (B, SE, β, t, p)"),
          withSpinner(DT::dataTableOutput(ns("simple_table"))),
          hr(),
          fluidRow(
            column(6, h5("Model Fit"),   verbatimTextOutput(ns("simple_fit"))),
            column(6, h5("VIF (Multicollinearity)"), verbatimTextOutput(ns("simple_vif")))
          ),
          hr(),
          h5("Residuals vs Fitted"),
          withSpinner(plotlyOutput(ns("simple_plot")))
        ),

        # ── Tab 2: Hierarchical ──────────────────────────────────────────────
        tabPanel("Hierarchical Regression",
          fluidRow(
            column(4, pickerInput(ns("hier_dv"), "Dependent Variable (DV)",
                                  choices = num_cols(),
                                  options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("hier_block1"), "Block 1 — Control Variables",
                                  choices = num_cols(), multiple = TRUE,
                                  options = list(`actions-box` = TRUE)))
          ),
          pickerInput(ns("hier_block2"), "Block 2 — Theory Variables",
                      choices = num_cols(), multiple = TRUE,
                      options = list(`actions-box` = TRUE)),
          pickerInput(ns("hier_block3"), "Block 3 — Interaction / Moderator",
                      choices = num_cols(), multiple = TRUE,
                      options = list(`actions-box` = TRUE)),
          actionButton(ns("hier_run"), "Run Hierarchical Regression", class = "btn-primary"),
          hr(),
          h5("Block Comparison (R², ΔR², ΔF, p)"),
          withSpinner(DT::dataTableOutput(ns("hier_table")))
        ),

        # ── Tab 3: Curvilinear ───────────────────────────────────────────────
        tabPanel("Curvilinear Regression",
          tags$div(
            style = paste0(
              "background:#EBF4FA;padding:1rem;border-radius:8px;",
              "margin-bottom:1rem;border-left:4px solid #2196A6;"
            ),
            tags$b("📐 When to use curvilinear regression:"),
            tags$p(
              style = "margin:4px 0 0;",
              "Use when theory predicts an inverted-U (or U-shaped) relationship between ",
              "predictor and outcome. Common in HRM/OB research: e.g., optimal challenge, ",
              "over-qualification, workload–performance curves, diminishing returns. ",
              "A significant quadratic term (X²) confirms a curvilinear pattern."
            )
          ),

          # Variable selectors
          fluidRow(
            column(4,
              pickerInput(ns("curv_dv"), "Dependent Variable (DV)",
                          choices = num_cols(),
                          options = list(`actions-box` = TRUE))),
            column(4,
              pickerInput(ns("curv_iv"), "Primary Predictor (X)",
                          choices = num_cols(),
                          options = list(`actions-box` = TRUE))),
            column(4,
              pickerInput(ns("curv_covs"), "Covariates (optional)",
                          choices = num_cols(), multiple = TRUE,
                          options = list(`actions-box` = TRUE)))
          ),

          fluidRow(
            column(3,
              selectInput(ns("curv_type"), "Polynomial Degree",
                          choices = c(
                            "Quadratic (X + X²)" = "quadratic",
                            "Cubic (X + X² + X³)" = "cubic"
                          ), selected = "quadratic")),
            column(3,
              checkboxInput(ns("curv_center"), "Mean-center X before polynomial terms",
                            value = TRUE)),
            column(3,
              checkboxInput(ns("curv_response"), "Also run Response Surface (2 IVs)",
                            value = FALSE)),
            column(3,
              conditionalPanel(
                condition = paste0("input['", ns("curv_response"), "'] == true"),
                pickerInput(ns("curv_iv2"), "Second Predictor (Z) for RSA",
                            choices = num_cols(),
                            options = list(`actions-box` = TRUE))
              )
            )
          ),

          actionButton(ns("curv_run"), "Run Curvilinear Analysis", class = "btn-primary"),
          hr(),

          # Sub-tabs inside Curvilinear
          tabsetPanel(
            tabPanel("Model Comparison",
              br(),
              tags$p(style = "color:#555;",
                "Compares linear model vs. models with polynomial terms. ",
                "A significant ΔF for the quadratic model confirms curvilinearity."),
              withSpinner(DT::dataTableOutput(ns("curv_compare_table")))
            ),
            tabPanel("Coefficients",
              br(),
              withSpinner(DT::dataTableOutput(ns("curv_coef_table"))),
              hr(),
              verbatimTextOutput(ns("curv_inflection"))
            ),
            tabPanel("Curve Plot",
              br(),
              fluidRow(
                column(4,
                  sliderInput(ns("curv_points"), "Smoothness of fitted curve",
                              min = 50, max = 500, value = 200, step = 50)),
                column(4,
                  checkboxInput(ns("curv_ci"), "Show 95% confidence band", value = TRUE)),
                column(4,
                  checkboxInput(ns("curv_raw"), "Show raw data points", value = TRUE))
              ),
              withSpinner(plotlyOutput(ns("curv_plot"), height = "500px"))
            ),
            tabPanel("Response Surface (RSA)",
              br(),
              tags$p(style = "color:#555;",
                "Response Surface Analysis tests polynomial regression with two predictors ",
                "(X and Z). Requires 'Also run Response Surface' checkbox above. ",
                "Common for testing congruence/incongruence hypotheses (Edwards & Parry, 1993)."),
              withSpinner(DT::dataTableOutput(ns("rsa_table"))),
              hr(),
              verbatimTextOutput(ns("rsa_surface_tests")),
              hr(),
              withSpinner(plotlyOutput(ns("rsa_plot"), height = "500px"))
            ),
            tabPanel("APA Write-Up",
              br(),
              verbatimTextOutput(ns("curv_apa"))
            )
          )
        ),

        # ── Tab 4: Diagnostics ───────────────────────────────────────────────
        tabPanel("Diagnostics",
          tags$p("Runs diagnostic plots for the Simple/Multiple Regression model. ",
                 "Run that model first."),
          actionButton(ns("diag_run"), "Generate Diagnostic Plots", class = "btn-primary"),
          hr(),
          h5("4-Panel Diagnostics"),
          withSpinner(plotOutput(ns("diag_plots"), height = "600px"))
        ),

        # ── Tab 5: APA Write-Up (Linear) ─────────────────────────────────────
        tabPanel("APA Write-Up (Linear)",
          verbatimTextOutput(ns("apa_writeup"))
        ),

        # ── Tab 6: Download ──────────────────────────────────────────────────
        tabPanel("Download",
          fluidRow(
            column(6,
              downloadButton(ns("download_excel"), "Download Linear Regression (Excel)",
                             class = "btn-success")),
            column(6,
              downloadButton(ns("download_curv"), "Download Curvilinear Results (Excel)",
                             class = "btn-info"))
          ),
          hr(),
          verbatimTextOutput(ns("download_info"))
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("🎯 Coefficient Plot",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Interactive forest plot of standardised coefficients with 95% CIs. Run Simple/Multiple Regression first."),
          withSpinner(plotlyOutput(ns("coef_forest_plot"), height="450px"))
        ),
        tabPanel("🔬 Residual Dashboard",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Interactive 4-panel residual diagnostic dashboard. Run Simple/Multiple Regression first."),
          withSpinner(plotlyOutput(ns("residual_dashboard"), height="550px")),
          tags$h5("Heteroscedasticity Test", style="margin-top:1.2rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(DT::dataTableOutput(ns("bp_test_tbl"))),
          tags$h5("Influential Cases (Cook's D & Leverage)", style="margin-top:1.2rem;color:#1A3A5C;font-weight:600;"),
          withSpinner(plotlyOutput(ns("influence_plot"), height="320px")),
          br(),
          withSpinner(DT::dataTableOutput(ns("influence_tbl")))
        ),
        tabPanel("📊 R² Change Chart",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Lollipop chart showing cumulative R² and ΔR² across hierarchical blocks. Run Hierarchical Regression first."),
          withSpinner(plotlyOutput(ns("r2_change_plot"), height="400px"))
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
              column(4, actionButton(ns("ai_btn_lm"), "📊 Simple/Multiple Regression",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_hier"), "📈 Hierarchical Regression",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_curv"), "📐 Curvilinear / Polynomial",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(6, actionButton(ns("ai_btn_diag"), "🔍 Model Diagnostics",
                                     class="btn-warning btn-block", style="margin-bottom:.5rem;")),
              column(6, actionButton(ns("ai_btn_all"), "📋 Full Regression Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ═══════════════════════════════════════════════════════════════════════
    # SIMPLE / MULTIPLE REGRESSION
    # ═══════════════════════════════════════════════════════════════════════
    simple_model <- reactiveVal(NULL)

    observeEvent(input$simple_run, {
      req(input$simple_dv, input$simple_ivs)
      d <- data_rv(); req(d)
      tryCatch({
        withProgress(message = "Running regression...", {
          f <- as.formula(paste0("`", input$simple_dv, "` ~ ",
                                 paste(paste0("`", input$simple_ivs, "`"), collapse = " + ")))
          simple_model(lm(f, data = d))
        })
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })

    output$simple_table <- DT::renderDataTable({
      req(simple_model())
      model <- simple_model()
      cs    <- summary(model)$coefficients
      cs_df <- as.data.frame(cs)
      d     <- data_rv()
      betas <- rep(NA, nrow(cs_df))
      dv_sd <- sd(d[[all.vars(formula(model))[1]]], na.rm = TRUE)
      for (i in seq_len(nrow(cs_df))) {
        nm <- rownames(cs_df)[i]
        if (nm %in% names(d))
          betas[i] <- cs_df[i, 1] * (sd(d[[nm]], na.rm = TRUE) / dv_sd)
      }
      tool_dt(data.frame(
        Predictor = rownames(cs_df),
        B         = round(cs_df[, 1], 4),
        SE        = round(cs_df[, 2], 4),
        Beta      = round(betas, 4),
        t         = round(cs_df[, 3], 4),
        `p-value` = sapply(cs_df[, 4], sig_stars),
        check.names = FALSE
      ), "Regression Coefficients")
    })

    output$simple_fit <- renderText({
      req(simple_model())
      s <- summary(simple_model())
      paste0(
        "R²         = ", round(s$r.squared, 4), "\n",
        "Adj. R²    = ", round(s$adj.r.squared, 4), "\n",
        "F(", s$fstatistic[2], ", ", s$fstatistic[3],
        ") = ", round(s$fstatistic[1], 3), "\n",
        "p ", apa_p(pf(s$fstatistic[1], s$fstatistic[2],
                       s$fstatistic[3], lower.tail = FALSE))
      )
    })

    output$simple_vif <- renderText({
      req(simple_model())
      model <- simple_model()
      threshold <- input$vif_threshold %||% 10
      if (length(coef(model)) > 2) {
        v <- car::vif(model)
        paste0("VIF Values:\n",
               paste(names(v), ":", round(v, 3), collapse = "\n"),
               "\n\nVIF > ", threshold, " = warning threshold\nVIF > ", threshold * 0.5, " = caution")
      } else {
        "VIF not applicable for simple regression (1 IV only)."
      }
    })

    output$simple_plot <- renderPlotly({
      req(simple_model())
      model <- simple_model()
      pd    <- data.frame(Fitted = fitted(model), Residuals = residuals(model))
      plot_ly(pd, x = ~Fitted, y = ~Residuals, type = "scatter", mode = "markers",
              marker = list(color = TEAL, opacity = 0.7), name = "Residuals") %>%
        add_lines(x = range(pd$Fitted), y = c(0, 0),
                  line = list(color = "red", dash = "dash"), name = "Zero line") %>%
        layout(title = "Residuals vs. Fitted Values",
               xaxis = list(title = "Fitted"), yaxis = list(title = "Residuals"))
    })

    # ═══════════════════════════════════════════════════════════════════════
    # HIERARCHICAL REGRESSION
    # ═══════════════════════════════════════════════════════════════════════
    hier_results <- reactiveVal(NULL)

    observeEvent(input$hier_run, {
      req(input$hier_dv)
      d <- data_rv(); req(d)
      tryCatch({
        withProgress(message = "Running hierarchical regression...", {
          dv  <- input$hier_dv
          b1  <- input$hier_block1
          b2  <- input$hier_block2
          b3  <- input$hier_block3
          mods <- list(); rsq <- numeric(3)

          mk_lm <- function(vars)
            lm(as.formula(paste0("`", dv, "` ~ ",
                                 paste(paste0("`", vars, "`"), collapse = " + "))),
               data = d)

          if (length(b1) > 0) { mods[[1]] <- mk_lm(b1); rsq[1] <- summary(mods[[1]])$r.squared }
          if (length(b1) > 0 && length(b2) > 0) { mods[[2]] <- mk_lm(c(b1, b2)); rsq[2] <- summary(mods[[2]])$r.squared }
          if (length(b1) > 0 && length(b2) > 0 && length(b3) > 0) { mods[[3]] <- mk_lm(c(b1, b2, b3)); rsq[3] <- summary(mods[[3]])$r.squared }

          hier_results(list(models = mods, r_squared = rsq,
                            labels  = c("Block 1 (Controls)", "Block 1+2 (+Theory)", "Block 1+2+3 (+Interaction)")))
        })
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })

    output$hier_table <- DT::renderDataTable({
      req(hier_results())
      res <- hier_results()
      n   <- length(res$models)
      cmp <- data.frame(
        Block    = res$labels[1:n],
        `R²`     = round(res$r_squared[1:n], 4),
        `Adj R²` = sapply(res$models[1:n], function(m) round(summary(m)$adj.r.squared, 4)),
        `ΔR²`    = c(NA, round(diff(res$r_squared[1:n]), 4)),
        `ΔF`     = NA,
        `p (ΔF)` = NA,
        check.names = FALSE
      )
      for (i in seq_len(n)) {
        s <- summary(res$models[[i]])
        cmp$`R²`[i] <- round(s$r.squared, 4)
        if (i > 1) {
          av <- anova(res$models[[i-1]], res$models[[i]])
          cmp$`ΔF`[i]     <- round(av$F[2], 3)
          cmp$`p (ΔF)`[i] <- sig_stars(av$`Pr(>F)`[2])
        }
      }
      tool_dt(cmp, "Hierarchical Regression — Block Comparison")
    })

    # ═══════════════════════════════════════════════════════════════════════
    # CURVILINEAR REGRESSION
    # ═══════════════════════════════════════════════════════════════════════
    curv_res <- reactiveVal(NULL)

    observeEvent(input$curv_run, {
      req(input$curv_dv, input$curv_iv)
      d    <- data_rv(); req(d)
      dv   <- input$curv_dv
      iv   <- input$curv_iv
      covs <- input$curv_covs
      type <- input$curv_type

      tryCatch({
        withProgress(message = "Running curvilinear analysis...", {

          # --- Mean-center X if requested
          x_raw <- d[[iv]]
          x_use <- if (isTRUE(input$curv_center)) scale(x_raw, center = TRUE, scale = FALSE)[,1]
                   else x_raw

          x_lbl <- if (isTRUE(input$curv_center)) paste0(iv, "_c") else iv

          # Build working data frame
          wdf           <- d
          wdf[[x_lbl]] <- x_use
          wdf[[paste0(x_lbl, "2")]] <- x_use^2
          if (type == "cubic") wdf[[paste0(x_lbl, "3")]] <- x_use^3

          cov_str   <- if (length(covs) > 0) paste0(" + ", paste(paste0("`", covs, "`"), collapse = " + ")) else ""

          # Model 0: Covariates only (baseline)
          f0 <- if (length(covs) > 0)
            as.formula(paste0("`", dv, "` ~ ", paste(paste0("`", covs, "`"), collapse = " + ")))
          else
            as.formula(paste0("`", dv, "` ~ 1"))
          m0 <- lm(f0, data = wdf)

          # Model 1: Linear
          f1 <- as.formula(paste0("`", dv, "` ~ `", x_lbl, "`", cov_str))
          m1 <- lm(f1, data = wdf)

          # Model 2: Quadratic
          f2 <- as.formula(paste0("`", dv, "` ~ `", x_lbl, "` + `",
                                  x_lbl, "2`", cov_str))
          m2 <- lm(f2, data = wdf)

          # Model 3: Cubic (only if requested)
          m3 <- NULL
          if (type == "cubic") {
            f3 <- as.formula(paste0("`", dv, "` ~ `", x_lbl, "` + `",
                                    x_lbl, "2` + `", x_lbl, "3`", cov_str))
            m3 <- lm(f3, data = wdf)
          }

          # --- Response Surface Analysis (if requested and second IV given)
          rsa_model <- NULL
          if (isTRUE(input$curv_response) && !is.null(input$curv_iv2) &&
              input$curv_iv2 != "") {
            iv2 <- input$curv_iv2
            z_raw <- d[[iv2]]
            z_use <- if (isTRUE(input$curv_center)) scale(z_raw, center = TRUE, scale = FALSE)[,1] else z_raw
            z_lbl <- if (isTRUE(input$curv_center)) paste0(iv2, "_c") else iv2
            wdf[[z_lbl]]              <- z_use
            wdf[[paste0(z_lbl, "2")]] <- z_use^2
            wdf[[paste0(x_lbl, "_", z_lbl)]] <- x_use * z_use

            f_rsa <- as.formula(paste0(
              "`", dv, "` ~ `", x_lbl, "` + `", z_lbl, "` + `",
              x_lbl, "2` + `", z_lbl, "2` + `", x_lbl, "_", z_lbl, "`",
              cov_str
            ))
            rsa_model <- tryCatch(lm(f_rsa, data = wdf), error = function(e) NULL)
          }

          curv_res(list(
            m0 = m0, m1 = m1, m2 = m2, m3 = m3,
            wdf = wdf, x_lbl = x_lbl, x_use = x_use, x_raw = x_raw,
            dv = dv, iv = iv, type = type,
            centered = isTRUE(input$curv_center),
            rsa_model = rsa_model,
            iv2 = if (!is.null(input$curv_iv2)) input$curv_iv2 else NULL
          ))
          showNotification("Curvilinear analysis complete.", type = "message")
        })
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })

    # ── Curvilinear: Model Comparison Table ──────────────────────────────────
    output$curv_compare_table <- DT::renderDataTable({
      req(curv_res())
      res <- curv_res()

      model_list   <- list(res$m1, res$m2)
      model_labels <- c("Model 1: Linear (X)", "Model 2: Quadratic (X + X²)")
      if (res$type == "cubic" && !is.null(res$m3)) {
        model_list   <- c(model_list, list(res$m3))
        model_labels <- c(model_labels, "Model 3: Cubic (X + X² + X³)")
      }

      rows <- lapply(seq_along(model_list), function(i) {
        m   <- model_list[[i]]
        s   <- summary(m)
        r2  <- round(s$r.squared, 4)
        ar2 <- round(s$adj.r.squared, 4)
        aic <- round(AIC(m), 2)
        bic <- round(BIC(m), 2)
        df_res <- s$df[2]
        n  <- nrow(model.frame(m))

        if (i == 1) {
          # compare vs null (or baseline)
          av <- anova(res$m0, m)
          f_chg <- round(av$F[2], 3)
          p_chg <- sig_stars(av$`Pr(>F)`[2])
          dr2   <- round(s$r.squared - summary(res$m0)$r.squared, 4)
        } else {
          prev  <- model_list[[i - 1]]
          av    <- anova(prev, m)
          f_chg <- round(av$F[2], 3)
          p_chg <- sig_stars(av$`Pr(>F)`[2])
          dr2   <- round(s$r.squared - summary(prev)$r.squared, 4)
        }

        data.frame(
          Model       = model_labels[i],
          `R²`        = r2,
          `Adj. R²`   = ar2,
          `ΔR²`       = dr2,
          `ΔF`        = f_chg,
          `p (ΔF)`    = p_chg,
          AIC         = aic,
          BIC         = bic,
          check.names = FALSE
        )
      })

      cmp_df <- do.call(rbind, rows)
      tool_dt(cmp_df, "Curvilinear Regression — Model Comparison")
    })

    # ── Curvilinear: Coefficient Table ───────────────────────────────────────
    output$curv_coef_table <- DT::renderDataTable({
      req(curv_res())
      res <- curv_res()

      # Display highest-order model
      final_m <- if (!is.null(res$m3)) res$m3 else res$m2
      cs      <- summary(final_m)$coefficients
      cs_df   <- as.data.frame(cs)
      d       <- res$wdf
      dv_sd   <- sd(d[[res$dv]], na.rm = TRUE)
      betas   <- rep(NA, nrow(cs_df))
      for (i in seq_len(nrow(cs_df))) {
        nm <- rownames(cs_df)[i]
        if (nm %in% names(d))
          betas[i] <- cs_df[i, 1] * (sd(d[[nm]], na.rm = TRUE) / dv_sd)
      }

      # Friendly row names
      rn <- rownames(cs_df)
      # Robust regex helper for special characters
      re_esc <- function(x) gsub("([.+*?^${}()|\\[\\]\\\\])", "\\\\\\1", x)
      rn <- gsub(paste0("^`?", re_esc(res$x_lbl), "2`?$"), paste0(res$iv, "² (Quadratic)"), rn)
      rn <- gsub(paste0("^`?", re_esc(res$x_lbl), "3`?$"), paste0(res$iv, "³ (Cubic)"),     rn)
      rn <- gsub(paste0("^`?", re_esc(res$x_lbl),  "`?$"), paste0(res$iv, " (Linear)"),     rn)
      rn <- gsub("`", "", rn)

      tool_dt(data.frame(
        Term      = rn,
        B         = round(cs_df[, 1], 4),
        SE        = round(cs_df[, 2], 4),
        Beta      = round(betas, 4),
        t         = round(cs_df[, 3], 4),
        `p-value` = sapply(cs_df[, 4], sig_stars),
        check.names = FALSE
      ), paste0("Curvilinear Coefficients (",
                if (!is.null(res$m3)) "Cubic" else "Quadratic", " Model)"))
    })

    # ── Curvilinear: Inflection / Turning Point ───────────────────────────────
    output$curv_inflection <- renderText({
      req(curv_res())
      res <- curv_res()

      coefs2 <- coef(res$m2)
      b1_name <- paste0("`", res$x_lbl, "`")
      b2_name <- paste0("`", res$x_lbl, "2`")
      b1 <- coefs2[b1_name]; if (is.na(b1)) b1 <- coefs2[res$x_lbl]
      b2 <- coefs2[b2_name]; if (is.na(b2)) b2 <- coefs2[paste0(res$x_lbl, "2")]

      if (is.na(b1) || is.na(b2) || b2 == 0) return("Could not compute turning point.")

      vertex_c <- -b1 / (2 * b2)
      vertex_raw <- if (res$centered) vertex_c + mean(res$x_raw, na.rm = TRUE) else vertex_c
      shape <- if (b2 < 0) "Inverted-U (concave down)" else "U-shape (concave up)"

      cubic_info <- ""
      if (!is.null(res$m3)) {
        c3 <- coef(res$m3)
        b3_name <- paste0("`", res$x_lbl, "3`")
        b3 <- c3[b3_name]; if (is.na(b3)) b3 <- c3[paste0(res$x_lbl, "3")]
        if (!is.na(b3)) {
          # Inflection of cubic: solve 2*b2 + 6*b3*x = 0 → x = -b2/(3*b3)
          infl_c   <- -b2 / (3 * b3)
          infl_raw <- if (res$centered) infl_c + mean(res$x_raw, na.rm = TRUE) else infl_c
          cubic_info <- paste0(
            "\n\nCubic Inflection Point\n",
            "  At centered X = ", round(infl_c, 3),
            if (res$centered) paste0(" (raw scale ≈ ", round(infl_raw, 3), ")"),
            "\n  Interpretation: rate of change reverses direction at this point."
          )
        }
      }

      paste0(
        "── Quadratic Turning Point (Vertex) ──────────────────────\n",
        "  b1 (linear)    = ", round(b1, 4), "\n",
        "  b2 (quadratic) = ", round(b2, 4), "\n",
        "  Vertex = -b1 / (2·b2) = ", round(vertex_c, 3),
        if (res$centered) paste0(" (raw scale ≈ ", round(vertex_raw, 3), ")"),
        "\n\n  Shape: ", shape,
        "\n\n  Interpretation:\n",
        "  The relationship between ", res$iv, " and ", res$dv,
        "\n  is maximized/minimized when ", res$iv, " ≈ ",
        round(vertex_raw, 2), ".\n",
        "  Below this value the slope is ",
        if (b2 < 0) "positive (DV increases);" else "negative (DV decreases);",
        "\n  above it the slope is ",
        if (b2 < 0) "negative (DV decreases)." else "positive (DV increases).",
        cubic_info
      )
    })

    # ── Curvilinear: Curve Plot ───────────────────────────────────────────────
    output$curv_plot <- renderPlotly({
      req(curv_res())
      res  <- curv_res()
      wdf  <- res$wdf
      dv   <- res$dv
      x_l  <- res$x_lbl
      x_raw <- res$x_raw

      final_m <- if (!is.null(res$m3)) res$m3 else res$m2

      # Prediction grid on X range
      n_pts <- isolate(input$curv_points)
      x_seq_c <- seq(min(res$x_use, na.rm = TRUE), max(res$x_use, na.rm = TRUE),
                     length.out = n_pts)
      pred_df <- setNames(data.frame(x_seq_c, x_seq_c^2),
                          c(x_l, paste0(x_l, "2")))
      if (!is.null(res$m3))
        pred_df[[paste0(x_l, "3")]] <- x_seq_c^3

      # Add covariates at their mean
      for (cv in names(model.frame(final_m))) {
        if (!cv %in% c(dv, x_l, paste0(x_l, "2"), paste0(x_l, "3")) &&
            cv %in% names(wdf))
          pred_df[[cv]] <- mean(wdf[[cv]], na.rm = TRUE)
      }

      pred_obj <- tryCatch(
        predict(final_m, newdata = pred_df, interval = "confidence"),
        error = function(e) NULL
      )

      x_display <- if (res$centered) x_seq_c + mean(x_raw, na.rm = TRUE) else x_seq_c

      p <- plot_ly()

      # Raw points
      if (isolate(input$curv_raw)) {
        p <- add_trace(p,
                       x = x_raw, y = wdf[[dv]],
                       type = "scatter", mode = "markers",
                       marker = list(color = TEAL, opacity = 0.5, size = 6),
                       name  = "Observed data")
      }

      # CI ribbon
      if (!is.null(pred_obj) && isolate(input$curv_ci)) {
        p <- add_ribbons(p,
                         x = x_display,
                         ymin = pred_obj[, "lwr"], ymax = pred_obj[, "upr"],
                         fillcolor = "rgba(33,150,166,0.15)",
                         line = list(color = "transparent"),
                         name = "95% CI")
      }

      # Fitted curve
      y_fit <- if (!is.null(pred_obj)) pred_obj[, "fit"] else predict(final_m, newdata = pred_df)
      p <- add_lines(p, x = x_display, y = y_fit,
                     line = list(color = NAVY, width = 2.5),
                     name = paste0("Fitted curve (",
                                   if (!is.null(res$m3)) "Cubic" else "Quadratic", ")"))

      # Vertex line for quadratic
      if (is.null(res$m3)) {
        cc2 <- coef(res$m2)
        b1  <- cc2[x_l]; b2 <- cc2[paste0(x_l, "2")]
        if (!is.na(b1) && !is.na(b2) && b2 != 0) {
          vx_c   <- -b1 / (2 * b2)
          vx_raw <- if (res$centered) vx_c + mean(x_raw, na.rm = TRUE) else vx_c
          vy     <- predict(final_m,
                            newdata = setNames(
                              data.frame(vx_c, vx_c^2), c(x_l, paste0(x_l, "2"))
                            ))
          p <- add_trace(p, x = c(vx_raw, vx_raw), y = c(min(wdf[[dv]], na.rm = TRUE), vy),
                         type = "scatter", mode = "lines",
                         line = list(color = AMBER, dash = "dot", width = 1.5),
                         name = paste0("Vertex (", round(vx_raw, 2), ")"))
        }
      }

      layout(p,
             title  = paste0("Curvilinear Relationship: ", res$iv, " → ", dv),
             xaxis  = list(title = res$iv),
             yaxis  = list(title = dv),
             legend = list(orientation = "h"))
    })

    # ── Response Surface Analysis ─────────────────────────────────────────────
    output$rsa_table <- DT::renderDataTable({
      req(curv_res())
      res <- curv_res()
      req(res$rsa_model)
      m   <- res$rsa_model
      cs  <- summary(m)$coefficients
      cs_df <- as.data.frame(cs)
      rn    <- gsub("`", "", rownames(cs_df))

      tool_dt(data.frame(
        Term      = rn,
        B         = round(cs_df[, 1], 4),
        SE        = round(cs_df[, 2], 4),
        t         = round(cs_df[, 3], 4),
        `p-value` = sapply(cs_df[, 4], sig_stars),
        check.names = FALSE
      ), "Response Surface Analysis — Polynomial Regression")
    })

    output$rsa_surface_tests <- renderText({
      req(curv_res())
      res <- curv_res()
      req(res$rsa_model)
      m   <- res$rsa_model
      s   <- summary(m)
      cc  <- coef(m)

      x_l <- res$x_lbl
      iv2 <- res$iv2
      z_l <- if (!is.null(iv2) && isTRUE(input$curv_center))
        paste0(iv2, "_c") else iv2

      b1_name <- paste0("`", x_l, "`"); b2_name <- paste0("`", z_l, "`")
      b3_name <- paste0("`", x_l, "2`"); b4_name <- paste0("`", z_l, "2`")
      b5_name <- paste0("`", x_l, "_", z_l, "`")

      get_c <- function(nm) {
        v <- cc[nm]; if (is.na(v)) v <- cc[gsub("`", "", nm)]; if (is.na(v)) 0 else v
      }
      b1 <- get_c(b1_name); b2 <- get_c(b2_name)
      b3 <- get_c(b3_name); b4 <- get_c(b4_name); b5 <- get_c(b5_name)

      # Surface tests (Edwards & Parry, 1993)
      a1 <- b1 + b2; a2 <- b3 + b4 + b5
      a3 <- b1 - b2; a4 <- b3 + b4 - b5

      paste0(
        "── Response Surface Tests (Edwards & Parry, 1993) ─────────\n\n",
        "Surface coefficients:\n",
        "  a1 = b1 + b2 = ", round(a1, 4), "  (slope on congruence line)\n",
        "  a2 = b3 + b4 + b5 = ", round(a2, 4), "  (curvature on congruence line)\n",
        "  a3 = b1 - b2 = ", round(a3, 4), "  (slope on incongruence line)\n",
        "  a4 = b3 + b4 - b5 = ", round(a4, 4), "  (curvature on incongruence line)\n\n",
        "Model Fit:\n",
        "  R² = ", round(s$r.squared, 4),
        "  |  Adj. R² = ", round(s$adj.r.squared, 4), "\n\n",
        "Interpretation guide:\n",
        "  If a1 ≠ 0 and a2 ≠ 0 → curvilinear congruence effect\n",
        "  If a3 ≠ 0 and a4 ≠ 0 → asymmetric incongruence effect\n",
        "  If a2 < 0 → inverted-U on congruence line (optimal match)\n",
        "  If a2 > 0 → U-shape on congruence line (extremes better than match)"
      )
    })

    output$rsa_plot <- renderPlotly({
      req(curv_res())
      res <- curv_res()
      req(res$rsa_model)
      m   <- res$rsa_model
      wdf <- res$wdf
      dv  <- res$dv
      x_l <- res$x_lbl
      iv2 <- res$iv2
      z_l <- if (!is.null(iv2) && isTRUE(input$curv_center)) paste0(iv2, "_c") else iv2

      nx <- 30
      xr <- seq(min(wdf[[x_l]], na.rm = TRUE), max(wdf[[x_l]], na.rm = TRUE), length.out = nx)
      zr <- seq(min(wdf[[z_l]], na.rm = TRUE), max(wdf[[z_l]], na.rm = TRUE), length.out = nx)
      grd <- expand.grid(x = xr, z = zr)
      grd[[x_l]]                  <- grd$x
      grd[[z_l]]                  <- grd$z
      grd[[paste0(x_l, "2")]]     <- grd$x^2
      grd[[paste0(z_l, "2")]]     <- grd$z^2
      grd[[paste0(x_l,"_",z_l)]]  <- grd$x * grd$z

      # Covariates at mean
      for (cv in names(model.frame(m))) {
        if (!cv %in% c(dv, x_l, z_l,
                        paste0(x_l,"2"), paste0(z_l,"2"),
                        paste0(x_l,"_",z_l)) && cv %in% names(wdf))
          grd[[cv]] <- mean(wdf[[cv]], na.rm = TRUE)
      }

      z_mat <- tryCatch({
        pv <- predict(m, newdata = grd)
        matrix(pv, nrow = nx, ncol = nx)
      }, error = function(e) NULL)

      if (is.null(z_mat)) return(plot_ly() %>% layout(title = "RSA plot unavailable"))

      x_display <- if (res$centered) xr + mean(res$x_raw, na.rm = TRUE) else xr
      z_display <- if (res$centered && !is.null(iv2)) {
        zr + mean(res$wdf[[z_l]], na.rm = TRUE)
      } else zr

      plot_ly(x = x_display, y = z_display, z = z_mat,
              type = "surface",
              colorscale = list(c(0, LIGHT), c(0.5, TEAL), c(1, NAVY)),
              showscale = TRUE) %>%
        layout(
          title = paste0("Response Surface: ", res$iv, " × ", iv2, " → ", dv),
          scene = list(
            xaxis = list(title = res$iv),
            yaxis = list(title = iv2),
            zaxis = list(title = dv)
          )
        )
    })

    # ── Curvilinear: APA Write-Up ─────────────────────────────────────────────
    output$curv_apa <- renderText({
      req(curv_res())
      res <- curv_res()

      m1 <- res$m1; m2 <- res$m2; m3 <- res$m3
      s1 <- summary(m1); s2 <- summary(m2)
      av <- anova(m1, m2)
      f_chg  <- round(av$F[2], 2)
      p_raw  <- av$`Pr(>F)`[2]
      p_chg  <- apa_p(p_raw)
      dr2    <- round(s2$r.squared - s1$r.squared, 3)
      sig_txt <- if (!is.na(p_raw) && p_raw < .05) "significant" else "non-significant"

      # Quadratic coefficient
      cc2     <- coef(res$m2)
      x_l     <- res$x_lbl
      b2_val  <- round(cc2[paste0("`", x_l, "2`")], 3)
      if (is.na(b2_val)) b2_val <- round(cc2[paste0(x_l, "2")], 3)
      b1_val  <- round(cc2[paste0("`", x_l, "`")], 3)
      if (is.na(b1_val)) b1_val <- round(cc2[x_l], 3)

      # Vertex
      vertex_txt <- ""
      if (!is.na(b1_val) && !is.na(b2_val) && b2_val != 0) {
        vtx <- round(-b1_val / (2 * b2_val), 2)
        vtx_raw <- if (res$centered) round(vtx + mean(res$x_raw, na.rm = TRUE), 2) else vtx
        vertex_txt <- paste0(
          "The turning point (vertex) of the quadratic curve is at ",
          res$iv, " = ", vtx_raw,
          if (res$centered) paste0(" (mean-centered value = ", vtx, ")"),
          ". "
        )
        shape_txt <- if (!is.na(b2_val) && b2_val < 0)
          "This inverted-U pattern indicates a positive relationship up to this point, followed by diminishing returns."
        else
          "This U-shaped pattern indicates a negative relationship that reverses after this point."
        vertex_txt <- paste0(vertex_txt, shape_txt)
      }

      cubic_section <- ""
      if (!is.null(m3)) {
        s3   <- summary(m3)
        av3  <- anova(m2, m3)
        f3   <- round(av3$F[2], 2)
        p3   <- apa_p(av3$`Pr(>F)`[2])
        dr3  <- round(s3$r.squared - s2$r.squared, 3)
        cubic_section <- paste0(
          "\n\nCubic Extension:\n",
          "Adding the cubic term (", res$iv, "³) resulted in ",
          if (!is.na(av3$`Pr(>F)`[2]) && av3$`Pr(>F)`[2] < .05) "a significant" else "a non-significant",
          " improvement over the quadratic model, ΔR² = ", dr3,
          ", ΔF(1, ", av3$Df[2], ") = ", f3, ", p ", p3, "."
        )
      }

      center_note <- if (res$centered)
        paste0("\nNote. The predictor ", res$iv,
               " was mean-centered prior to computing polynomial terms to ",
               "reduce multicollinearity (Aiken & West, 1991).\n")
      else ""

      paste0(
        "APA-Style Write-Up — Curvilinear Regression\n",
        "═════════════════════════════════════════════\n\n",

        "Step 1 — Test for Curvilinearity:\n",
        "To test whether the relationship between ", res$iv, " and ", res$dv,
        " is curvilinear, a hierarchical polynomial regression was conducted. ",
        if (res$centered) paste0("The predictor was mean-centered before squaring (Aiken & West, 1991). "),
        "In the first step, the linear term was entered (R² = ", round(s1$r.squared, 3), "). ",
        "In the second step, the quadratic term (", res$iv, "²) was added. ",
        "The quadratic term accounted for a ", sig_txt, " increment in variance, ",
        "ΔR² = ", dr2, ", ΔF(1, ", av$Res.Df[2], ") = ", f_chg, ", p ", p_chg, ".\n\n",

        "Step 2 — Coefficients:\n",
        "In the final quadratic model (R² = ", round(s2$r.squared, 3),
        ", Adj. R² = ", round(s2$adj.r.squared, 3), "), ",
        "the linear term was B = ", b1_val, " and the quadratic term was B = ", b2_val, ". ",
        if (!is.na(b2_val) && b2_val < 0)
          "The negative quadratic coefficient indicates an inverted-U (concave-down) relationship. "
        else
          "The positive quadratic coefficient indicates a U-shaped (concave-up) relationship. ",
        "\n\n",

        "Step 3 — Turning Point:\n",
        vertex_txt, "\n",

        cubic_section, "\n\n",

        "References:\n",
        "Aiken, L. S., & West, S. G. (1991). Multiple regression: Testing and\n",
        "  interpreting interactions. Sage.\n",
        "Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied multiple\n",
        "  regression/correlation analysis for the behavioral sciences (3rd ed.). Erlbaum.\n",
        if (!is.null(res$rsa_model))
          "Edwards, J. R., & Parry, M. E. (1993). On the use of polynomial regression\n  equations as an alternative to difference scores. AMJ, 36(6), 1577-1613.\n"
        else "",

        center_note
      )
    })

    # ═══════════════════════════════════════════════════════════════════════
    # DIAGNOSTICS (uses simple_model)
    # ═══════════════════════════════════════════════════════════════════════
    diag_model <- reactiveVal(NULL)

    observeEvent(input$diag_run, {
      req(simple_model())
      diag_model(simple_model())
    })

    output$diag_plots <- renderPlot({
      req(diag_model())
      model <- diag_model()
      par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
      plot(model, which = 1)
      plot(model, which = 2)
      hist(residuals(model), main = "Histogram of Residuals",
           xlab = "Residuals", col = "#C8E6F0", border = "white", breaks = 20)
      plot(model, which = 3)
      par(mfrow = c(1, 1))
    })

    # ═══════════════════════════════════════════════════════════════════════
    # APA WRITE-UP (Linear)
    # ═══════════════════════════════════════════════════════════════════════
    output$apa_writeup <- renderText({
      req(simple_model())
      model <- simple_model(); s <- summary(model)
      paste0(
        "APA-Style Write-Up — Linear Regression\n",
        "═══════════════════════════════════════\n\n",
        "A multiple linear regression was conducted to examine the relationship\n",
        "between the predictor(s) and the outcome variable.\n\n",
        "Model Summary:\n",
        "  R² = ", round(s$r.squared, 3),
        ", Adjusted R² = ", round(s$adj.r.squared, 3), "\n",
        "  F(", s$fstatistic[2], ", ", s$fstatistic[3],
        ") = ", round(s$fstatistic[1], 2),
        ", p < .001\n\n",
        "Regression Equation:\n",
        "  Ŷ = ", round(coef(model)[1], 3),
        paste0(" + (", round(coef(model)[-1], 3), " × ",
               names(coef(model))[-1], ")", collapse = ""), "\n\n",
        "Assumptions Checklist:\n",
        "  ☐ Linearity        — examined via residual plots\n",
        "  ☐ Homoscedasticity — checked with Scale-Location plot\n",
        "  ☐ Normality        — verified with Q-Q plot\n",
        "  ☐ Independence     — assumed from study design\n",
        paste0("  ☐ Multicollinearity — VIF values checked (threshold: VIF < ", input$vif_threshold %||% 10, ")")
      )
    })

    # ═══════════════════════════════════════════════════════════════════════
    # DOWNLOAD
    # ═══════════════════════════════════════════════════════════════════════
    output$download_excel <- downloadHandler(
      filename = function() paste0("regression_linear_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(simple_model())
        m <- simple_model(); s <- summary(m)
        cs <- as.data.frame(s$coefficients)
        cs$Predictor <- rownames(cs)
        cs <- cs[, c(ncol(cs), 1:(ncol(cs) - 1))]
        fit <- data.frame(
          Metric = c("R²", "Adjusted R²", "F-statistic", "p-value"),
          Value  = c(s$r.squared, s$adj.r.squared, s$fstatistic[1],
                     pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE))
        )
        writexl::write_xlsx(list(Coefficients = cs, `Model Fit` = fit), file)
      }
    )

    output$download_curv <- downloadHandler(
      filename = function() paste0("regression_curvilinear_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(curv_res())
        res <- curv_res()

        # Comparison table
        mk_cmp <- function(ml, lbls) {
          rows <- lapply(seq_along(ml), function(i) {
            m <- ml[[i]]; s <- summary(m)
            prev <- if (i == 1) res$m0 else ml[[i-1]]
            av   <- anova(prev, m)
            data.frame(
              Model    = lbls[i],
              R2       = round(s$r.squared, 4),
              AdjR2    = round(s$adj.r.squared, 4),
              DeltaR2  = round(s$r.squared - summary(prev)$r.squared, 4),
              DeltaF   = round(av$F[2], 3),
              p_DeltaF = round(av$`Pr(>F)`[2], 4),
              AIC      = round(AIC(m), 2),
              BIC      = round(BIC(m), 2),
              check.names = FALSE
            )
          })
          do.call(rbind, rows)
        }

        model_list <- list(res$m1, res$m2)
        lbls       <- c("Linear", "Quadratic")
        if (!is.null(res$m3)) { model_list <- c(model_list, list(res$m3)); lbls <- c(lbls, "Cubic") }
        cmp_df <- mk_cmp(model_list, lbls)

        final_m <- if (!is.null(res$m3)) res$m3 else res$m2
        cs      <- as.data.frame(summary(final_m)$coefficients)
        cs$Term <- rownames(cs)
        cs      <- cs[, c(ncol(cs), 1:(ncol(cs) - 1))]

        sheets <- list(`Model Comparison` = cmp_df, Coefficients = cs)
        if (!is.null(res$rsa_model)) {
          rsa_cs <- as.data.frame(summary(res$rsa_model)$coefficients)
          rsa_cs$Term <- rownames(rsa_cs)
          rsa_cs <- rsa_cs[, c(ncol(rsa_cs), 1:(ncol(rsa_cs) - 1))]
          sheets[["RSA Coefficients"]] <- rsa_cs
        }
        writexl::write_xlsx(sheets, file)
      }
    )

    output$download_info <- renderText({
      paste0(
        "Linear Regression Export: coefficients (B, SE, β, t, p) + model fit (R², F, p).\n",
        "Curvilinear Export: model comparison table (R², ΔR², ΔF, AIC, BIC) + ",
        "polynomial coefficients + RSA sheet (if run)."
      )
    })
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r04 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k04 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Simple/Multiple Regression
    observeEvent(input$ai_btn_lm, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      ctx <- tryCatch({
        m <- simple_model(); req(m)
        paste0("REGRESSION RESULTS\n", paste(capture.output(summary(m)), collapse="\n"))
      }, error=function(e) "Please run the regression first.")
      output$ai_output <- renderUI({ .ai_r04(call_gemini(paste0(
        "You are an expert statistician writing for a peer-reviewed management journal.\n\n",
        "Task: DETAILED interpretation of regression results. Include:\n",
        "1. Report overall model fit: F(df1,df2) = X.XX, p = .XXX, R² = .XX, Adjusted R² = .XX\n",
        "2. Interpret R²: variance explained by predictors (Cohen: small=.02, medium=.13, large=.26)\n",
        "3. For each predictor: β = .XX (standardised), b = .XX (unstandardised), SE, t, p, 95% CI\n",
        "4. Identify which predictors are significant (p < .05) and their relative importance (β size)\n",
        paste0("5. Check VIF for multicollinearity (user threshold: VIF > ", input$vif_threshold %||% 10, ")\n"),
        "6. Assess model assumptions: linearity, normality of residuals, homoscedasticity\n",
        "7. Discuss practical significance vs statistical significance\n",
        "8. Write complete APA 7 Results paragraph\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Hierarchical Regression
    observeEvent(input$ai_btn_hier, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      ctx <- tryCatch({
        res <- hier_results(); req(res)
        paste0("HIERARCHICAL REGRESSION RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run hierarchical regression first.")
      output$ai_output <- renderUI({ .ai_r04(call_gemini(paste0(
        "You are an expert statistician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of hierarchical regression. Include:\n",
        "1. Describe the rationale for hierarchical entry of variables (theoretical precedence)\n",
        "2. Report R² and Adjusted R² for each step/block\n",
        "3. Report ΔR² (R² change) for each step: F-change statistic, df, p-value\n",
        "4. Interpret ΔR² effect size: small=.02, medium=.13, large=.26 (Cohen, 1988)\n",
        "5. Report significant predictors in each step with β, SE, t, p, 95% CI\n",
        "6. Discuss how coefficients change across steps (mediation/suppression indicators)\n",
        "7. Write complete APA 7 Results section paragraph\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Curvilinear
    observeEvent(input$ai_btn_curv, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      ctx <- tryCatch({
        res <- curv_res(); req(res)
        paste0("CURVILINEAR REGRESSION RESULTS\n", paste(capture.output(summary(res)), collapse="\n"))
      }, error=function(e) "Please run curvilinear regression first.")
      output$ai_output <- renderUI({ .ai_r04(call_gemini(paste0(
        "You are an expert statistician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of curvilinear/polynomial regression. Include:\n",
        "1. Explain what the polynomial term tests — whether the relationship is U-shaped or inverted-U\n",
        "2. Report linear term β and polynomial term β with significance\n",
        "3. Calculate the turning point (inflection point) using -b1/2b2 formula\n",
        "4. Compare linear vs polynomial model using ΔR² to justify the curvilinear specification\n",
        "5. Assess Response Surface Analysis implications if applicable\n",
        "6. Discuss theoretical meaning of the non-linear relationship\n",
        "7. Write APA 7 Results paragraph\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Diagnostics
    observeEvent(input$ai_btn_diag, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      ctx <- tryCatch({
        m <- diag_model()
        if (is.null(m)) m <- simple_model()
        req(m)
        vif_txt <- tryCatch(paste(capture.output(print(car::vif(m))), collapse="\n"), error=function(e) "VIF not available")
        dw_txt  <- tryCatch(paste(capture.output(print(lmtest::dwtest(m))), collapse="\n"), error=function(e) "DW not available")
        paste0("REGRESSION DIAGNOSTICS\nVIF:\n", vif_txt, "\n\nDurbin-Watson:\n", dw_txt,
               "\n\nResiduals summary:\n", paste(capture.output(summary(residuals(m))), collapse="\n"))
      }, error=function(e) "Please run regression first.")
      output$ai_output <- renderUI({ .ai_r04(call_gemini(paste0(
        "You are an expert statistician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED assessment of regression model diagnostics. Include:\n",
        paste0("1. Multicollinearity: VIF < ", input$vif_threshold %||% 10, " (acceptable), VIF threshold: ", input$vif_threshold %||% 10, " (user defined)\n"),
        "2. Autocorrelation: Durbin-Watson statistic (values near 2 = no autocorrelation)\n",
        "3. Normality of residuals: skewness and kurtosis of residual distribution\n",
        "4. Homoscedasticity: describe residual plot patterns, Breusch-Pagan test if available\n",
        "5. Influential cases: Cook's D threshold (> 4/n problematic), leverage, DFFITS\n",
        "6. Linearity: partial regression plots assessment\n",
        "7. Overall verdict on assumption compliance and any corrective actions needed\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 5. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        lm <- tryCatch(simple_model(), error=function(e) NULL)
        if (!is.null(lm)) parts <- c(parts, paste0("Linear Model:\n", paste(capture.output(summary(lm)), collapse="\n")))
        hr <- tryCatch(hier_results(), error=function(e) NULL)
        if (!is.null(hr)) parts <- c(parts, paste0("Hierarchical:\n", paste(head(capture.output(print(hr)),40), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("FULL REGRESSION ANALYSIS\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one regression first.")
      output$ai_output <- renderUI({ .ai_r04(call_gemini(paste0(
        "You are an expert quantitative researcher writing for a top-tier management journal.\n\n",
        "Task: Write a COMPREHENSIVE Results section for all regression analyses. Include:\n",
        "1. Preliminary analysis: descriptives, correlations, multicollinearity (VIF)\n",
        "2. Main regression: model fit (R², F, p), significant predictors (β, SE, t, p, CI)\n",
        "3. Hierarchical regression: ΔR² across blocks, F-change tests\n",
        "4. Assumption checks: normality, homoscedasticity, independence, linearity\n",
        "5. Effect size interpretation and practical significance\n",
        "6. Overall conclusion about the regression model\n\n",
        "Use APA 7 throughout. Formal academic prose (6-8 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k04()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Coefficient Forest Plot
    output$coef_forest_plot <- renderPlotly({
      req(simple_model())
      model  <- simple_model()
      d      <- data_rv()
      cs     <- summary(model)$coefficients
      cs_df  <- as.data.frame(cs)
      dv_sd  <- sd(d[[all.vars(formula(model))[1]]], na.rm = TRUE)
      betas  <- ci_lo <- ci_hi <- rep(NA, nrow(cs_df))
      for (i in seq_len(nrow(cs_df))) {
        nm <- rownames(cs_df)[i]
        if (nm %in% names(d)) {
          betas[i] <- cs_df[i,1] * (sd(d[[nm]], na.rm=TRUE) / dv_sd)
          se_b     <- cs_df[i,2] * (sd(d[[nm]], na.rm=TRUE) / dv_sd)
          ci_lo[i] <- betas[i] - 1.96 * se_b
          ci_hi[i] <- betas[i] + 1.96 * se_b
        }
      }
      keep    <- !is.na(betas)
      df_coef <- data.frame(Predictor=rownames(cs_df)[keep], Beta=betas[keep],
                             Lower=ci_lo[keep], Upper=ci_hi[keep],
                             Sig=ifelse(cs_df[keep,4]<.05,"p<.05","p≥.05"))
      df_coef <- df_coef[order(df_coef$Beta), ]
      plot_ly(df_coef) |>
        add_segments(x=~Lower, xend=~Upper, y=~Predictor, yend=~Predictor,
                     line=list(color=TEAL, width=3), name="95% CI") |>
        add_markers(x=~Beta, y=~Predictor,
                    marker=list(color=~ifelse(Sig=="p<.05",NAVY,"#AAAAAA"), size=10),
                    text=~sprintf("β=%.3f [%.3f,%.3f]",Beta,Lower,Upper),
                    hoverinfo="text", name="β") |>
        add_segments(x=0, xend=0, y=0.5, yend=nrow(df_coef)+0.5,
                     line=list(color="#C0392B",dash="dot",width=1),
                     name="Zero", inherit=FALSE) |>
        layout(title=list(text="Standardised Coefficient Forest Plot (β ± 95% CI)",
                          font=list(color=NAVY)),
               xaxis=list(title="Standardised β"), yaxis=list(title=""),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # Interactive 4-panel Residual Dashboard
    output$residual_dashboard <- renderPlotly({
      req(simple_model())
      model       <- simple_model()
      fitted_vals <- fitted(model)
      resid_vals  <- residuals(model)
      std_resid   <- rstandard(model)
      p1 <- plot_ly(x=fitted_vals, y=resid_vals, type="scatter", mode="markers",
                    marker=list(color=TEAL,opacity=0.6,size=5), name="Res") |>
        add_lines(x=range(fitted_vals),y=c(0,0),line=list(color="#C0392B",dash="dash"),name="Zero") |>
        layout(xaxis=list(title="Fitted"),yaxis=list(title="Residuals"))
      qq  <- qqnorm(std_resid, plot.it=FALSE)
      p2  <- plot_ly(x=qq$x, y=qq$y, type="scatter", mode="markers",
                     marker=list(color=AMBER,opacity=0.7,size=5), name="Q-Q") |>
        add_lines(x=range(qq$x),y=range(qq$x),line=list(color="#C0392B",dash="dash"),name="Diag") |>
        layout(xaxis=list(title="Theoretical"),yaxis=list(title="Sample"))
      p3  <- plot_ly(x=fitted_vals, y=sqrt(abs(std_resid)), type="scatter", mode="markers",
                     marker=list(color=GREEN,opacity=0.6,size=5), name="Scale-Loc") |>
        layout(xaxis=list(title="Fitted"),yaxis=list(title="√|Std Res|"))
      p4  <- plot_ly(x=resid_vals, type="histogram",
                     marker=list(color=NAVY,line=list(color="white",width=0.5)),
                     opacity=0.8, name="Hist") |>
        layout(xaxis=list(title="Residuals"),yaxis=list(title="Count"))
      subplot(p1,p2,p3,p4, nrows=2, shareX=FALSE, shareY=FALSE, margin=0.08) |>
        layout(title=list(text="Residual Diagnostics Dashboard",font=list(color=NAVY)),
               showlegend=FALSE, plot_bgcolor="white", paper_bgcolor="white")
    })

    # R² Change Lollipop Chart
    output$r2_change_plot <- renderPlotly({
      req(hier_results())
      res <- hier_results(); n <- length(res$models)
      if (n == 0) return(plot_ly() |> layout(title="Run Hierarchical Regression first"))
      df_r2 <- data.frame(
        Block   = res$labels[1:n],
        R2      = round(res$r_squared[1:n], 4),
        DeltaR2 = c(round(res$r_squared[1],4),
                    if(n>1) round(diff(res$r_squared[1:n]),4) else numeric(0))
      )
      plot_ly(df_r2) |>
        add_segments(x=0, xend=~R2, y=~Block, yend=~Block,
                     line=list(color=TEAL,width=3), name="R²") |>
        add_markers(x=~R2, y=~Block, marker=list(color=TEAL,size=14),
                    text=~sprintf("R²=%.4f",R2), hoverinfo="text", name="R²") |>
        add_annotations(x=~R2, y=~Block,
                        text=~ifelse(seq_len(n)>1,sprintf("+Δ%.4f",DeltaR2),""),
                        xanchor="left", xshift=8, showarrow=FALSE,
                        font=list(color=AMBER,size=11)) |>
        layout(title=list(text="Hierarchical R² by Block",font=list(color=NAVY)),
               xaxis=list(title="R²",range=c(0,max(df_r2$R2)*1.3)),
               yaxis=list(title=""), plot_bgcolor="white", paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════
    # BREUSCH-PAGAN HETEROSCEDASTICITY TEST
    # ══════════════════════════════════════════════════════════════════════
    output$bp_test_tbl <- DT::renderDataTable({
      req(simple_model())
      m <- simple_model()
      tryCatch({
        n    <- length(residuals(m))
        # Manual Breusch-Pagan (no extra package needed)
        r2_bp <- summary(lm(residuals(m)^2 ~ fitted(m)))$r.squared
        bp_stat <- n * r2_bp
        bp_p    <- pchisq(bp_stat, df = 1, lower.tail = FALSE)
        # Also try lmtest if available
        if (requireNamespace("lmtest", quietly = TRUE)) {
          bp2 <- tryCatch(lmtest::bptest(m), error = function(e) NULL)
          if (!is.null(bp2)) { bp_stat <- bp2$statistic; bp_p <- bp2$p.value }
        }
        df_bp <- data.frame(
          Test = "Breusch-Pagan Test",
          `BP statistic` = round(as.numeric(bp_stat), 4),
          df = 1L,
          `p value` = round(as.numeric(bp_p), 4),
          Interpretation = ifelse(as.numeric(bp_p) < 0.05,
            "⚠ Heteroscedasticity detected — consider robust SEs (HC3)",
            "✓ Residuals appear homoscedastic"),
          check.names = FALSE
        )
        tool_dt(df_bp, "Breusch-Pagan Heteroscedasticity Test")
      }, error = function(e) {
        data.frame(Note = paste("BP test error:", e$message), check.names = FALSE)
      })
    })

    # ══════════════════════════════════════════════════════════════════════
    # COOK'S D & LEVERAGE — INFLUENTIAL CASES
    # ══════════════════════════════════════════════════════════════════════
    output$influence_plot <- renderPlotly({
      req(simple_model())
      m <- simple_model()
      tryCatch({
        n   <- length(fitted(m))
        p   <- length(coef(m))
        cd  <- cooks.distance(m)
        lev <- hatvalues(m)
        thr_cd  <- 4 / n
        thr_lev <- 2 * p / n
        col_vec <- ifelse(cd > thr_cd, AMBER,
                   ifelse(lev > thr_lev, TEAL, "#AACFE4"))
        plot_ly(x = seq_len(n), y = as.numeric(cd), type = "bar",
                marker = list(color = col_vec),
                hovertemplate = paste0(
                  "Obs #%{x}<br>Cook's D: %{y:.5f}<br>Leverage: ",
                  round(lev, 4), "<extra></extra>")) |>
          add_segments(x = 0.5, xend = n + 0.5,
                       y = thr_cd, yend = thr_cd,
                       line = list(color = "red", dash = "dash", width = 1.5),
                       name = paste0("4/n = ", round(thr_cd, 4)),
                       showlegend = TRUE, hoverinfo = "skip") |>
          layout(
            title = list(text = paste0("Cook's Distance — Influential Cases",
                                       "<br><sup>Amber = Cook's D > 4/n threshold</sup>"),
                         font = list(color = NAVY, size = 13)),
            xaxis = list(title = "Observation Index"),
            yaxis = list(title = "Cook's D"),
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.15),
            plot_bgcolor = "white", paper_bgcolor = "white"
          )
      }, error = function(e) plotly_empty() |> layout(title = paste("Error:", e$message)))
    })

    output$influence_tbl <- DT::renderDataTable({
      req(simple_model())
      m <- simple_model()
      tryCatch({
        n   <- length(fitted(m))
        p   <- length(coef(m))
        cd  <- cooks.distance(m)
        lev <- hatvalues(m)
        sr  <- tryCatch(rstandard(m), error = function(e) rep(NA, n))
        df_inf <- data.frame(
          Obs = seq_len(n),
          `Cook's D`        = round(cd, 5),
          Leverage          = round(lev, 5),
          `Std. Residual`   = round(sr, 3),
          `Flag Cook's D`   = ifelse(cd  > 4/n,     "⚠ Influential",    ""),
          `Flag Leverage`   = ifelse(lev > 2*p/n,   "⚠ High leverage",  ""),
          check.names = FALSE
        )
        flagged <- df_inf[df_inf[["Flag Cook's D"]] != "" | df_inf[["Flag Leverage"]] != "", ]
        if (nrow(flagged) == 0) {
          flagged <- head(df_inf[order(-df_inf[["Cook's D"]]), ], 10)
          attr(flagged, "caption_extra") <- " (top 10 by Cook's D — no influential cases detected)"
        }
        tool_dt(flagged, paste0("Influential Cases — n=", nrow(flagged),
                                 " flagged (Cook's D > ", round(4/n, 4),
                                 " or Leverage > ", round(2*p/n, 4), ")"))
      }, error = function(e) NULL)
    })

  })
}
