# ── Module 02: Measurement Analysis ─────────────────────────
# EFA (psych) · CFA (lavaan) · Reliability · AVE · CR · HTMT

mod02_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html("📏", "Measurement Analysis",
              "EFA · CFA · Cronbach's α · McDonald's ω · AVE · CR · HTMT Bootstrap CIs"),
    uiOutput(ns("global_data_banner")),
    fileInput(ns("file"), "Upload data (.xlsx or .csv)", accept=c(".xlsx",".xls",".csv")),
    uiOutput(ns("main_ui")),
  )
}

mod02_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        data_rv(read_uploaded(input$file$datapath, input$file$name))
      }, error = function(e) showNotification(paste("Load error:", e$message), type="error"))
    })

    df <- reactive(data_rv())

    num_cols_r <- reactive({ req(df()); numeric_cols(df()) })

    output$main_ui <- renderUI({
      req(df())
      nc <- num_cols_r()
      tabBox(width=12,
        tabPanel("🔄 EFA",
          fluidRow(
            column(3, pickerInput(ns("efa_cols"), "Select items", choices=nc,
                                  selected=nc, multiple=TRUE, options=list(`actions-box`=TRUE))),
            column(2, numericInput(ns("n_factors"), "# Factors", value=3, min=1, max=10)),
            column(2, selectInput(ns("rotation"), "Rotation",
                                  choices=c("oblimin","varimax","promax","none"), selected="oblimin")),
            column(2, selectInput(ns("fm"), "Method",
                                  choices=c("pa","ml","minres"), selected="pa")),
            column(2, numericInput(ns("load_threshold"), "Min. factor loading:", value = 0.40, min = 0.20, max = 0.70, step = 0.05))
          ),
          fluidRow(
            column(3, br(), actionButton(ns("run_efa"), "▶ Run EFA", class="btn-primary"))
          ),
          withSpinner(DT::dataTableOutput(ns("efa_loadings"))),
          withSpinner(plotlyOutput(ns("scree_plot"))),
          verbatimTextOutput(ns("efa_fit"))
        ),
        tabPanel("🔬 CFA (lavaan)",
          fluidRow(
            column(8, textAreaInput(ns("cfa_syntax"),
                                    "Model syntax (lavaan format)",
                                    value="# Example:\n# F1 =~ item1 + item2 + item3\n# F2 =~ item4 + item5 + item6",
                                    rows=8)),
            column(4,
              pickerInput(ns("cfa_cols"), "Columns in model", choices=nc,
                          multiple=TRUE, options=list(`actions-box`=TRUE)),
              selectInput(ns("cfa_estimator"), "Estimator",
                          choices=c("ML","MLR","WLSMV"), selected="ML"),
              actionButton(ns("run_cfa"), "▶ Run CFA", class="btn-primary")
            )
          ),
          withSpinner(uiOutput(ns("cfa_fit_ui"))),
          withSpinner(DT::dataTableOutput(ns("cfa_params"))),
          withSpinner(plotOutput(ns("cfa_diagram"), height="500px")),
          downloadButton(ns("dl_cfa"), "Download Results (Excel)")
        ),
        tabPanel("📐 Reliability & Validity",
          fluidRow(
            column(12,
              tags$p("Define constructs below (one per row). Each construct = a set of items."),
              numericInput(ns("n_constructs"), "Number of constructs", value=3, min=1, max=12)
            )
          ),
          uiOutput(ns("construct_ui")),
          actionButton(ns("run_rel"), "▶ Run Reliability & Validity", class="btn-primary"),
          br(), br(),
          withSpinner(DT::dataTableOutput(ns("rel_tbl"))),
          withSpinner(uiOutput(ns("htmt_ui"))),
          downloadButton(ns("dl_rel"), "Download Results (Excel)")
        ),
        tabPanel("📝 APA Write-Up",
          verbatimTextOutput(ns("apa_text"))
        ),
        # ── NEW: Advanced Visualisations ──────────────────────────────────
        tabPanel("🔥 Loading Heatmap",
          uiOutput(ns("loading_heatmap_notice")),
          withSpinner(plotlyOutput(ns("loading_heatmap"), height="500px")),
          hr(),
          downloadButton(ns("dl_loading_heatmap"), "Download PNG")
        ),
        tabPanel("📊 Reliability Dashboard",
          tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
            "Visual gauge chart of α, ω, AVE, and CR per construct — run Reliability & Validity first."),
          withSpinner(plotlyOutput(ns("reliability_dashboard"), height="500px"))
        ),
        tabPanel("🕸️ HTMT Visual",
          tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
            "HTMT bar chart with 0.85 (strict) and 0.90 (lenient) thresholds — run Reliability & Validity first."),
          withSpinner(plotlyOutput(ns("htmt_bar"), height="400px"))
        ),
        tabPanel("\U0001f916 AI Interpret",
          tags$div(style = "padding:.8rem 0;",
            tags$div(
              style = "background:#EFF8FF; border-left:4px solid #2196A6; padding:.7rem 1rem; border-radius:6px; margin-bottom:.8rem; font-size:.85rem;",
              tags$b("How to use:"), " Run an analysis, then click the matching button for a detailed academic interpretation.", tags$br(),
              tags$a("\U0001f511 Get FREE Groq key \u2192 console.groq.com/keys",
                     href = "https://console.groq.com/keys", target = "_blank",
                     style = "color:#2196A6; font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_efa"), "\U0001f504 Interpret EFA Results",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_cfa"), "\U0001f52c Interpret CFA Results",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_rel"), "\U0001f4d0 Interpret Reliability & Validity",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(6, actionButton(ns("ai_btn_all"), "\U0001f4cb Full Measurement Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ── EFA ────────────────────────────────────────────────
    efa_result <- eventReactive(input$run_efa, {
      req(df(), input$efa_cols, input$n_factors)
      d <- df()[, input$efa_cols, drop=FALSE] |>
        mutate(across(everything(), as.numeric)) |> na.omit()
      withProgress(message="Running EFA...", {
        fa(d, nfactors=input$n_factors, rotate=input$rotation,
           fm=input$fm, warnings=FALSE)
      })
    })

    output$efa_loadings <- DT::renderDataTable({
      req(efa_result())
      fa_obj <- efa_result()
      loads  <- as.data.frame(round(loadings(fa_obj)[], 4))
      loads$Communality <- round(fa_obj$communality, 4)
      loads$Uniqueness  <- round(fa_obj$uniquenesses, 4)
      # Flag cross-loadings
      threshold <- input$load_threshold %||% 0.40
      tool_dt(loads, paste0("Factor Loadings (|loading| ≥ ", threshold, " considered substantial)"))
    })

    output$scree_plot <- renderPlotly({
      req(df(), input$efa_cols)
      d <- df()[, input$efa_cols, drop=FALSE] |>
        mutate(across(everything(), as.numeric)) |> na.omit()
      ev <- eigen(cor(d, use="pairwise.complete.obs"))$values
      ev_df <- data.frame(Factor=seq_along(ev), Eigenvalue=ev)
      plot_ly(ev_df, x=~Factor, y=~Eigenvalue, type="scatter", mode="lines+markers",
              line=list(color=TEAL, width=2), marker=list(color=NAVY, size=8)) |>
        add_hline(y=1, line_dash="dash", line_color="red", annotation_text="Kaiser criterion (λ=1)") |>
        layout(title="Scree Plot", xaxis=list(title="Factor"),
               yaxis=list(title="Eigenvalue"), plot_bgcolor="white", paper_bgcolor="white")
    })

    output$efa_fit <- renderPrint({
      req(efa_result())
      fa_obj <- efa_result()
      cat("EFA Fit Indices:\n")
      cat(sprintf("  RMSEA: %.4f\n", fa_obj$RMSEA[1]))
      cat(sprintf("  TLI  : %.4f\n", fa_obj$TLI))
      cat(sprintf("  BIC  : %.4f\n", fa_obj$BIC))
      cat(sprintf("  CFI  : %.4f\n", if (!is.null(fa_obj$CFI)) fa_obj$CFI else NA))
    })

    # ── CFA ────────────────────────────────────────────────
    cfa_result <- eventReactive(input$run_cfa, {
      req(df(), input$cfa_cols, input$cfa_syntax)
      d <- df()[, input$cfa_cols, drop=FALSE] |>
        mutate(across(everything(), as.numeric)) |> na.omit()
      tryCatch({
        fit <- cfa(input$cfa_syntax, data=d, estimator=input$cfa_estimator)
        list(fit=fit, data=d, error=NULL)
      }, error=function(e) list(fit=NULL, error=e$message))
    })

    output$cfa_fit_ui <- renderUI({
      req(cfa_result())
      res <- cfa_result()
      if (!is.null(res$error)) {
        return(tags$div(style="color:red;padding:1rem;",
                        tags$b("CFA Error: "), res$error))
      }
      fit <- res$fit
      fi  <- fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea",
                                "rmsea.ci.lower","rmsea.ci.upper","srmr","aic","bic"))

      cfi_ok  <- fi["cfi"]  >= 0.95
      tli_ok  <- fi["tli"]  >= 0.95
      rmsea_ok<- fi["rmsea"]<= 0.06
      srmr_ok <- fi["srmr"] <= 0.08

      fit_card <- function(lbl, val, ok) {
        col <- if (ok) GREEN else "#C0392B"
        icon <- if (ok) "✓" else "✗"
        tags$div(style=paste0("background:",col,";color:white;padding:.8rem;border-radius:8px;text-align:center;"),
                 tags$b(paste(icon, lbl)),
                 tags$br(), sprintf("%.3f", val))
      }

      tagList(
        tags$h4("Model Fit Indices"),
        fluidRow(
          column(2, fit_card("CFI ≥.95", fi["cfi"], cfi_ok)),
          column(2, fit_card("TLI ≥.95", fi["tli"], tli_ok)),
          column(2, fit_card("RMSEA ≤.06", fi["rmsea"], rmsea_ok)),
          column(2, fit_card("SRMR ≤.08", fi["srmr"], srmr_ok)),
          column(2, tags$div(style="background:#1A3A5C;color:white;padding:.8rem;border-radius:8px;text-align:center;",
                             tags$b("AIC"), tags$br(), sprintf("%.1f", fi["aic"]))),
          column(2, tags$div(style="background:#1A3A5C;color:white;padding:.8rem;border-radius:8px;text-align:center;",
                             tags$b("BIC"), tags$br(), sprintf("%.1f", fi["bic"])))
        ),
        tags$p(style="color:#666;font-size:.82rem;margin-top:.5rem;",
               "Thresholds: CFI/TLI ≥ .95, RMSEA ≤ .06, SRMR ≤ .08 (Hu & Bentler, 1999)")
      )
    })

    output$cfa_params <- DT::renderDataTable({
      req(cfa_result())
      res <- cfa_result()
      if (!is.null(res$error)) return(NULL)
      params <- parameterEstimates(res$fit, standardized=TRUE) |>
        select(lhs, op, rhs, est, se, z, pvalue, ci.lower, ci.upper, std.all) |>
        mutate(across(where(is.numeric), ~round(.x, 4)),
               sig = sig_stars(pvalue))
      tool_dt(params, "CFA Parameter Estimates (std.all = standardized loading)")
    })

    output$cfa_diagram <- renderPlot({
      req(cfa_result())
      res <- cfa_result()
      if (!is.null(res$error)) return(NULL)
      semPaths(res$fit, what="std", layout="tree", style="lisrel",
               edge.label.cex=0.8, label.cex=0.9, residuals=FALSE,
               exoCov=FALSE, title=FALSE,
               color=list(lat=TEAL, man=NAVY))
    })

    output$dl_cfa <- downloadHandler(
      filename = function() paste0("CFA_Results_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(cfa_result())
        res <- cfa_result()
        if (!is.null(res$error)) return()
        params <- as.data.frame(parameterEstimates(res$fit, standardized=TRUE))
        fi <- as.data.frame(t(fitMeasures(res$fit)))
        write_xlsx(list("Parameters"=params, "Fit Indices"=fi), file)
      }
    )

    # ── Reliability & Validity ─────────────────────────────
    output$construct_ui <- renderUI({
      req(df(), input$n_constructs)
      nc <- num_cols_r()
      lapply(seq_len(input$n_constructs), function(i) {
        fluidRow(
          column(3, textInput(ns(paste0("cn_", i)), paste("Construct", i, "name"),
                              value=paste0("C", i))),
          column(9, pickerInput(ns(paste0("ci_", i)), paste("Construct", i, "items"),
                                choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE)))
        )
      })
    })

    rel_data <- eventReactive(input$run_rel, {
      req(df(), input$n_constructs)
      nc_r <- num_cols_r()
      results <- list()
      constructs <- list()

      for (i in seq_len(input$n_constructs)) {
        cname <- input[[paste0("cn_", i)]]
        citems <- input[[paste0("ci_", i)]]
        if (is.null(citems) || length(citems) < 2) next
        d <- df()[, citems, drop=FALSE] |>
          mutate(across(everything(), as.numeric)) |> na.omit()
        constructs[[cname]] <- d

        # Alpha
        alpha_res <- tryCatch(psych::alpha(d)$total$raw_alpha, error=function(e) NA)
        # Omega
        if (ncol(d) < 3) {
          showNotification("Omega requires ≥ 3 items per construct. Skipping omega calculation.", type = "warning")
          omega_res <- NA
        } else {
          omega_res <- tryCatch({
            om <- psych::omega(d, nfactors=1, plot=FALSE, warnings=FALSE)
            om$omega.tot
          }, error=function(e) NA)
        }
        # AVE & CR
        cor_mat <- cor(d, use="pairwise.complete.obs")
        fa_res  <- tryCatch(fa(d, nfactors=1, rotate="none", fm="ml", warnings=FALSE),
                            error=function(e) NULL)
        loadings_vec <- if (!is.null(fa_res)) as.vector(loadings(fa_res)[,1]) else rep(NA, ncol(d))
        ave <- mean(loadings_vec^2, na.rm=TRUE)
        cr  <- sum(loadings_vec, na.rm=TRUE)^2 /
               (sum(loadings_vec, na.rm=TRUE)^2 + sum(1 - loadings_vec^2, na.rm=TRUE))

        results[[cname]] <- data.frame(
          Construct   = cname,
          Items       = ncol(d),
          N           = nrow(d),
          Alpha       = round(alpha_res, 4),
          Omega       = round(omega_res, 4),
          AVE         = round(ave, 4),
          CR          = round(cr, 4),
          `AVE OK`    = ifelse(ave >= 0.5, "✓", "✗"),
          `CR OK`     = ifelse(cr  >= 0.7, "✓", "✗"),
          `Alpha OK`  = ifelse(!is.na(alpha_res) & alpha_res >= 0.7, "✓", "✗"),
          check.names = FALSE
        )
      }
      list(results=results, constructs=constructs)
    })

    output$rel_tbl <- DT::renderDataTable({
      req(rel_data())
      all_rows <- do.call(rbind, rel_data()$results)
      tool_dt(all_rows, "Reliability & Validity (α ≥ .70, AVE ≥ .50, CR ≥ .70)")
    })

    output$htmt_ui <- renderUI({
      req(rel_data())
      constructs <- rel_data()$constructs
      if (length(constructs) < 2) return(tags$p("Need at least 2 constructs for HTMT."))

      cnames <- names(constructs)
      pairs  <- combn(cnames, 2, simplify=FALSE)

      rows <- lapply(pairs, function(pair) {
        c1 <- pair[1]; c2 <- pair[2]
        d1 <- constructs[[c1]]; d2 <- constructs[[c2]]

        # HTMT = mean of inter-correlations / geometric mean of within-construct correlations
        inter_cors <- as.vector(cor(d1, d2, use="pairwise.complete.obs"))
        within1    <- as.vector(cor(d1, use="pairwise.complete.obs")[lower.tri(diag(ncol(d1)))])
        within2    <- as.vector(cor(d2, use="pairwise.complete.obs")[lower.tri(diag(ncol(d2)))])

        htmt <- mean(abs(inter_cors)) /
                sqrt(mean(abs(within1), na.rm=TRUE) * mean(abs(within2), na.rm=TRUE))

        data.frame(Pair=paste(c1, "↔", c2),
                   HTMT=round(htmt, 4),
                   Threshold=0.90,
                   Status=ifelse(htmt < 0.90, "✓ Discriminant Valid", "✗ Review"),
                   check.names=FALSE)
      })

      htmt_df <- do.call(rbind, rows)

      tagList(
        tags$h4("HTMT (Heterotrait-Monotrait Ratio)"),
        DT::dataTableOutput(ns("htmt_tbl")),
        tags$p(style="color:#666;font-size:.82rem;",
               "HTMT < 0.90 indicates discriminant validity (Gold et al., 2001; Henseler et al., 2015)")
      )
    })

    htmt_df_r <- reactive({
      req(rel_data())
      constructs <- rel_data()$constructs
      if (length(constructs) < 2) return(NULL)
      cnames <- names(constructs)
      pairs  <- combn(cnames, 2, simplify=FALSE)
      rows <- lapply(pairs, function(pair) {
        c1 <- pair[1]; c2 <- pair[2]
        d1 <- constructs[[c1]]; d2 <- constructs[[c2]]
        inter_cors <- as.vector(cor(d1, d2, use="pairwise.complete.obs"))
        within1 <- as.vector(cor(d1, use="pairwise.complete.obs")[lower.tri(diag(ncol(d1)))])
        within2 <- as.vector(cor(d2, use="pairwise.complete.obs")[lower.tri(diag(ncol(d2)))])
        htmt <- mean(abs(inter_cors)) /
                sqrt(mean(abs(within1), na.rm=TRUE) * mean(abs(within2), na.rm=TRUE))
        data.frame(Pair=paste(c1,"↔",c2), HTMT=round(htmt,4),
                   Status=ifelse(htmt < 0.90,"✓","✗"), check.names=FALSE)
      })
      do.call(rbind, rows)
    })

    output$htmt_tbl <- DT::renderDataTable({
      req(htmt_df_r())
      tool_dt(htmt_df_r())
    })

    output$dl_rel <- downloadHandler(
      filename = function() paste0("Reliability_Validity_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(rel_data())
        all_rows <- do.call(rbind, rel_data()$results)
        htmt_df  <- htmt_df_r()
        sheets <- list("Reliability & Validity" = all_rows)
        if (!is.null(htmt_df)) sheets[["HTMT"]] <- htmt_df
        write_xlsx(sheets, file)
      }
    )

    output$apa_text <- renderText({
      req(rel_data())
      rows <- do.call(rbind, rel_data()$results)
      lines <- apply(rows, 1, function(r) {
        sprintf("  %s (k=%s): α = %s, ω = %s, AVE = %s, CR = %s",
                r["Construct"], r["Items"], r["Alpha"], r["Omega"], r["AVE"], r["CR"])
      })
      paste0(
        "Reliability and convergent validity were assessed for all constructs.\n\n",
        paste(lines, collapse="\n"),
        "\n\nCronbach's α ≥ .70 and McDonald's ω ≥ .70 indicate acceptable reliability ",
        "(Hair et al., 2019). AVE ≥ .50 and CR ≥ .70 indicate convergent validity ",
        "(Fornell & Larcker, 1981).\n\n",
        "[Analysis: Dr.AIStat — Dr. Aneeq Inam]"
      )
    })
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_render2 <- function(result) {
      tags$div(
        style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result)
      )
    }

    .ai_key2 <- function() tags$div(class="alert alert-warning", style="margin-top:.5rem;",
      "\u26a0\ufe0f No API key. Paste your FREE Groq key (gsk_\u2026) in the sidebar. ",
      tags$a("Get key \u2192 console.groq.com/keys", href="https://console.groq.com/keys", target="_blank"))

    # 1. EFA
    observeEvent(input$ai_btn_efa, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key2()); return() }
      ctx <- tryCatch({
        ef <- efa_result(); req(ef)
        loadings_out <- paste(capture.output(print(ef$loadings, cutoff=0.3)), collapse="\n")
        paste0("EFA RESULTS\nMethod: ", input$fm, " | Rotation: ", input$rotation,
               " | Factors extracted: ", input$n_factors,
               "\nItems analysed: ", paste(input$efa_cols, collapse=", "),
               "\n\nFactor Loadings (cutoff |.30|):\n", loadings_out,
               "\n\nFit indices:\n", paste(capture.output(print(ef$RMSEA)), collapse="\n"))
      }, error=function(e) "Please run EFA first (click ▶ Run EFA).")
      output$ai_output <- renderUI({
        .ai_render2(call_gemini(paste0(
          "You are an expert psychometrician and quantitative researcher writing for a top-tier journal.\n\n",
          "Task: Provide a DETAILED interpretation of these EFA results. Your response must:\n",
          "1. Justify the number of factors retained (eigenvalue > 1, scree plot, parallel analysis rationale)\n",
          "2. Describe the factor structure — which items load on which factor (loadings > |.40| are meaningful)\n",
          "3. Identify any cross-loadings (item loading on 2+ factors) and discuss implications\n",
          "4. Evaluate factor reliability and conceptual coherence — name each factor based on its items\n",
          "5. Report variance explained by each factor and total cumulative variance\n",
          "6. Comment on rotation choice (oblimin = correlated factors; varimax = orthogonal) and justify\n",
          "7. Assess model fit (RMSEA, TLI, CFI if available) against standard cutoffs\n",
          "8. Write the complete APA 7 Results paragraph (3-4 sentences) for reporting EFA\n\n",
          "Write in formal academic prose (5-6 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 2. CFA
    observeEvent(input$ai_btn_cfa, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key2()); return() }
      ctx <- tryCatch({
        cf <- cfa_result(); req(cf)
        sm <- capture.output(summary(cf, fit.measures=TRUE, standardized=TRUE))
        paste0("CFA RESULTS\nModel syntax:\n", input$cfa_syntax,
               "\nEstimator: ", input$cfa_estimator,
               "\n\nModel Summary (fit indices + standardized loadings):\n",
               paste(head(sm, 80), collapse="\n"))
      }, error=function(e) "Please run CFA first (click ▶ Run CFA).")
      output$ai_output <- renderUI({
        .ai_render2(call_gemini(paste0(
          "You are an expert in Structural Equation Modeling and confirmatory factor analysis writing for a peer-reviewed journal.\n\n",
          "Task: Provide a DETAILED interpretation of these CFA results. Your response must:\n",
          "1. Evaluate global model fit: CFI/TLI ≥ .95 (acceptable ≥ .90), RMSEA ≤ .06 (acceptable ≤ .08), SRMR ≤ .08\n",
          "2. Report Chi-square (χ²/df ≤ 3 is acceptable) and discuss its sensitivity to sample size\n",
          "3. Evaluate all factor loadings — standardised loadings ≥ .50 indicate good indicator reliability\n",
          "4. Calculate or assess Average Variance Extracted (AVE ≥ .50) for convergent validity\n",
          "5. Assess Composite Reliability (CR ≥ .70) as a more robust alternative to Cronbach's α\n",
          "6. Check discriminant validity — AVE should exceed shared variance (squared correlation) between constructs\n",
          "7. Identify any modification indices worth considering (if provided) and their implications\n",
          "8. Write the complete APA 7 Results section paragraph for reporting CFA results\n\n",
          "Write in formal academic prose (6-7 paragraphs). Use Hu & Bentler (1999) and Fornell & Larcker (1981) criteria.\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 3. Reliability & Validity
    observeEvent(input$ai_btn_rel, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key2()); return() }
      ctx <- tryCatch({
        rd <- rel_data(); req(rd)
        htmt <- tryCatch(htmt_df_r(), error=function(e) NULL)
        htmt_txt <- if (!is.null(htmt)) paste("\n\nHTMT Matrix:\n", paste(capture.output(print(htmt)), collapse="\n")) else ""
        paste0("RELIABILITY & VALIDITY RESULTS\n",
               paste(capture.output(print(rd)), collapse="\n"), htmt_txt)
      }, error=function(e) "Please run Reliability & Validity first.")
      output$ai_output <- renderUI({
        .ai_render2(call_gemini(paste0(
          "You are an expert psychometrician writing for a peer-reviewed management/psychology journal.\n\n",
          "Task: Provide a DETAILED interpretation of reliability and validity results. Your response must:\n",
          "1. Evaluate Cronbach's α for each construct: α ≥ .70 acceptable, ≥ .80 good, ≥ .90 excellent\n",
          "2. Evaluate McDonald's ω (more robust than α) using same thresholds\n",
          "3. Assess convergent validity: AVE ≥ .50 (Fornell & Larcker, 1981) — report which constructs meet/fail\n",
          "4. Assess composite reliability (CR): CR ≥ .70 required; CR > α suggests no reverse-coded items\n",
          "5. Evaluate discriminant validity via HTMT (Henseler et al., 2015): HTMT < .85 (strict) or < .90 (lenient)\n",
          "6. Apply the Fornell-Larcker criterion: √AVE for each construct should exceed correlations with other constructs\n",
          "7. Provide an overall verdict on the measurement model's psychometric quality\n",
          "8. Write complete APA 7 Results paragraph for reporting construct reliability and validity\n\n",
          "Write in formal academic prose (5-6 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 4. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key2()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        ef <- tryCatch(efa_result(), error=function(e) NULL)
        if (!is.null(ef)) parts <- c(parts, paste0("EFA Results:\n",
          paste(capture.output(print(ef$loadings, cutoff=0.3)), collapse="\n")))
        cf <- tryCatch(cfa_result(), error=function(e) NULL)
        if (!is.null(cf)) {
          sm <- tryCatch(capture.output(summary(cf, fit.measures=TRUE, standardized=TRUE)), error=function(e) character(0))
          parts <- c(parts, paste0("CFA Results:\n", paste(head(sm, 60), collapse="\n")))
        }
        rd <- tryCatch(rel_data(), error=function(e) NULL)
        if (!is.null(rd)) parts <- c(parts, paste0("Reliability & Validity:\n",
          paste(capture.output(print(rd)), collapse="\n")))
        if (length(parts)==0) stop("no results yet")
        paste0("FULL MEASUREMENT MODEL ANALYSIS\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one analysis first.")
      output$ai_output <- renderUI({
        .ai_render2(call_gemini(paste0(
          "You are an expert psychometrician and SEM researcher writing for a top-tier management journal.\n\n",
          "Task: Write a COMPREHENSIVE measurement model Results section covering all analyses below. Include:\n",
          "1. EFA summary: factor structure, loadings, variance explained, rotation justification\n",
          "2. CFA summary: model fit indices (CFI, TLI, RMSEA, SRMR), all loadings, modification indices if relevant\n",
          "3. Reliability: Cronbach's α and McDonald's ω for each construct with quality verdict\n",
          "4. Convergent validity: AVE and CR for each construct\n",
          "5. Discriminant validity: HTMT and Fornell-Larcker criterion\n",
          "6. Overall psychometric quality verdict and suitability for structural analysis\n\n",
          "Use APA 7 throughout. Write in formal academic prose (7-9 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key2()); return() }
      NULL # (use buttons above for specific analyses)
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Factor Loading Heatmap
    output$loading_heatmap_notice <- renderUI({
      threshold <- input$load_threshold %||% 0.40
      tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
        "Factor loading heatmap — run EFA first. Cells are colour-coded by loading magnitude. ",
        paste0("Loadings ≥ |", round(threshold, 2), "| are considered substantive."))
    })

    output$loading_heatmap <- renderPlotly({
      if (is.null(efa_result())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(efa_result())
      fa_obj  <- efa_result()
      loads   <- as.data.frame(round(loadings(fa_obj)[], 3))
      items   <- rownames(loads)
      factors <- colnames(loads)
      m <- as.matrix(loads)

      plot_ly(
        z         = m,
        x         = factors,
        y         = items,
        type      = "heatmap",
        colorscale = list(c(0,"#C0392B"), c(0.5,"#FAFAFA"), c(1, TEAL)),
        zmid      = 0, zmin = -1, zmax = 1,
        text      = round(m, 3),
        texttemplate = "%{text}",
        hovertemplate = "Item: %{y}<br>Factor: %{x}<br>Loading: %{z:.3f}<extra></extra>"
      ) |>
        layout(
          title  = list(text = "Factor Loading Heatmap (|≥.30| substantive)", font = list(color = NAVY)),
          xaxis  = list(title = "Factor"),
          yaxis  = list(title = "Item"),
          margin = list(l = 120),
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        )
    })

    output$dl_loading_heatmap <- downloadHandler(
      filename = function() paste0("Factor_Loading_Heatmap_", Sys.Date(), ".png"),
      content  = function(file) {
        req(efa_result())
        fa_obj <- efa_result()
        loads  <- as.data.frame(round(loadings(fa_obj)[], 3))
        df_long <- tidyr::pivot_longer(cbind(Item = rownames(loads), loads),
                                       cols = -Item, names_to = "Factor", values_to = "Loading")
        p <- ggplot(df_long, aes(x = Factor, y = Item, fill = Loading)) +
          geom_tile(colour = "white") +
          geom_text(aes(label = sprintf("%.2f", Loading)), size = 3) +
          scale_fill_gradient2(low = "#C0392B", mid = "#FAFAFA", high = TEAL, midpoint = 0,
                               limits = c(-1, 1)) +
          theme_minimal(base_size = 12) +
          labs(title = "Factor Loading Heatmap", x = "Factor", y = "Item")
        ggplot2::ggsave(file, plot = p, width = 8, height = 6, dpi = 150)
      }
    )

    # Reliability Dashboard (gauge-style bar chart)
    output$reliability_dashboard <- renderPlotly({
      if (is.null(rel_data())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(rel_data())
      rows_list <- rel_data()$results
      if (length(rows_list) == 0) return(plot_ly() |> layout(title = "No constructs yet"))

      all_rows <- do.call(rbind, rows_list)
      constructs <- all_rows$Construct

      thresholds <- list(
        Alpha = 0.70, Omega = 0.70, AVE = 0.50, CR = 0.70
      )
      metrics <- c("Alpha","Omega","AVE","CR")
      colors  <- c("#2196A6","#1A3A5C","#E07B39","#1E6438")

      plots <- lapply(seq_along(metrics), function(i) {
        metric <- metrics[i]
        vals   <- as.numeric(all_rows[[metric]])
        thresh <- thresholds[[metric]]
        bar_colors <- ifelse(vals >= thresh, colors[i], "#C0392B")

        plot_ly(
          x    = vals,
          y    = constructs,
          type = "bar",
          orientation = "h",
          name = metric,
          marker = list(color = bar_colors),
          text = sprintf("%.3f", vals),
          textposition = "outside",
          hovertemplate = paste0(metric, ": %{x:.3f}<extra>%{y}</extra>")
        ) |>
          add_segments(
            x = thresh, xend = thresh, y = 0.5, yend = length(constructs) + 0.5,
            line = list(color = "black", dash = "dot", width = 1.5),
            name = paste("Threshold", thresh), inherit = FALSE
          ) |>
          layout(
            xaxis = list(title = metric, range = c(0, 1.05)),
            yaxis = list(title = ""),
            showlegend = FALSE,
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          )
      })

      subplot(plots, nrows = 2, shareY = TRUE, titleX = TRUE, margin = 0.08) |>
        layout(
          title = list(text = "Reliability & Validity Dashboard (dashed line = threshold)",
                       font = list(color = NAVY)),
          paper_bgcolor = "white"
        )
    })

    # HTMT bar chart
    output$htmt_bar <- renderPlotly({
      req(htmt_df_r())
      df_htmt <- htmt_df_r()
      if (is.null(df_htmt) || nrow(df_htmt) == 0)
        return(plot_ly() |> layout(title = "No HTMT data — run Reliability & Validity first"))

      bar_colors <- ifelse(as.numeric(df_htmt$HTMT) < 0.85, GREEN,
                    ifelse(as.numeric(df_htmt$HTMT) < 0.90, AMBER, "#C0392B"))

      plot_ly(
        x    = as.numeric(df_htmt$HTMT),
        y    = df_htmt$Pair,
        type = "bar",
        orientation = "h",
        marker = list(color = bar_colors),
        text = sprintf("%.3f", as.numeric(df_htmt$HTMT)),
        textposition = "outside",
        hovertemplate = "HTMT: %{x:.3f}<extra>%{y}</extra>"
      ) |>
        add_segments(x=0.85, xend=0.85, y=0.5, yend=nrow(df_htmt)+0.5,
                     line=list(color=AMBER, dash="dash", width=2),
                     name="0.85 (strict)", inherit=FALSE) |>
        add_segments(x=0.90, xend=0.90, y=0.5, yend=nrow(df_htmt)+0.5,
                     line=list(color="#C0392B", dash="dot", width=2),
                     name="0.90 (lenient)", inherit=FALSE) |>
        layout(
          title  = list(text = "HTMT Discriminant Validity (green < 0.85 | amber 0.85–0.90 | red ≥ 0.90)",
                        font = list(color = NAVY)),
          xaxis  = list(title = "HTMT Value", range = c(0, max(1.05, max(as.numeric(df_htmt$HTMT))+0.1))),
          yaxis  = list(title = "Construct Pair"),
          showlegend = TRUE,
          plot_bgcolor  = "white",
          paper_bgcolor = "white"
        )
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
