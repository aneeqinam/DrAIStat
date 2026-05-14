# ── Module 01: Descriptive Statistics ───────────────────────
# psych::describe() · correlations · CMB (Harman) · distributions

mod01_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html("📊", "Descriptive Statistics",
              "Mean · SD · Skewness · Kurtosis · Correlations · CMB Test (Harman's Single Factor)"),
    uiOutput(ns("global_data_banner")),
    fileInput(ns("file"), "Upload data (.xlsx or .csv)", accept=c(".xlsx",".xls",".csv")),
    uiOutput(ns("main_ui")),
  )
}

mod01_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Global data banner ────────────────────────────────────
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

    data_rv <- reactiveVal(NULL)

    observeEvent(input$file, {
      req(input$file)
      tryCatch({
        data_rv(read_uploaded(input$file$datapath, input$file$name))
      }, error = function(e) {
        showNotification(paste("Load error:", e$message), type="error")
      })
    })

    observeEvent(input$load_global, {
      gd <- global_shared_data()
      if (!is.null(gd)) {
        data_rv(gd)
        showNotification(paste0("Using: ", global_shared_name()), type="message", duration=3)
      }
    })

    df <- reactive(data_rv())

    num_cols_r <- reactive({
      req(df()); numeric_cols(df())
    })

    output$main_ui <- renderUI({
      req(df(), num_cols_r())
      nc <- num_cols_r()
      tagList(
        fluidRow(
          box(title="Data Preview", width=12, status="primary", solidHeader=TRUE,
              DT::dataTableOutput(ns("preview")))
        ),
        fluidRow(
          column(4,
            pickerInput(ns("sel_cols"), "Select columns for analysis",
                        choices=nc, selected=nc, multiple=TRUE,
                        options=list(`actions-box`=TRUE))
          ),
          column(4, numericInput(ns("ci_level"), "CI Level (%)", value=95, min=90, max=99))
        ),
        tabBox(width=12,
          tabPanel("📊 Descriptive Stats",
            withSpinner(DT::dataTableOutput(ns("desc_tbl"))),
            hr(),
            downloadButton(ns("dl_desc"), "Download Excel")
          ),
          tabPanel("📈 Distributions",
            uiOutput(ns("hist_cap_notice")),
            withSpinner(plotlyOutput(ns("dist_plot"), height="500px"))
          ),
          tabPanel("🔗 Correlation Matrix",
            withSpinner(plotlyOutput(ns("corr_plot"), height="500px")),
            hr(),
            withSpinner(DT::dataTableOutput(ns("corr_tbl")))
          ),
          tabPanel("🧪 CMB Test (Harman)",
            withSpinner(uiOutput(ns("cmb_ui")))
          ),
          tabPanel("📝 APA Write-Up",
            verbatimTextOutput(ns("apa_text"))
          ),
          # ── Advanced Visualizations ─────────────────────────────────────────
          tabPanel("☁️ Raincloud Plot",
            tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
                   "Half-violin + boxplot + raw data points — reveals full distribution shape simultaneously."),
            withSpinner(plotlyOutput(ns("raincloud_plot_adv"), height="520px")),
            downloadButton(ns("dl_raincloud_adv"), "Download PNG")
          ),
          tabPanel("🎻 Violin + Jitter",
            tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
                   "Violin plots with overlaid jittered raw data points per variable."),
            withSpinner(plotlyOutput(ns("violin_plot_adv"), height="520px"))
          ),
          tabPanel("📏 Q-Q Plots",
            tags$p(style="color:#555;font-size:.85rem;padding:.5rem 0;",
                   "Quantile-quantile plots with 95% confidence bands to assess normality."),
            withSpinner(plotOutput(ns("qq_plot_adv"), height="520px"))
          ),
          tabPanel("Advanced Viz",
            fluidRow(
              column(12, h4("Raincloud Plot"), plotlyOutput(ns("adv_raincloud"), height = "320px")),
              column(12, br(), h4("Q-Q Normality Plot"), plotlyOutput(ns("adv_qqplot"), height = "300px")),
              column(12, br(), h4("Violin + Boxplot"), plotlyOutput(ns("adv_violin"), height = "300px")),
              column(12, br(), h4("Interactive Summary Statistics"), DT::DTOutput(ns("adv_desc_table")))
            )
          ),
        tabPanel("\U0001f916 AI Interpret",
          tags$div(style = "padding:.8rem 0;",
            tags$div(
              style = "background:#EFF8FF; border-left:4px solid #2196A6; padding:.7rem 1rem; border-radius:6px; margin-bottom:.8rem; font-size:.85rem;",
              tags$b("How to use:"), " Run an analysis, then click the matching button below for a detailed academic interpretation.", tags$br(),
              tags$span(style="color:#555;", "Requires FREE Groq key in sidebar — "),
              tags$a("\U0001f511 Get key at console.groq.com/keys \u2192",
                     href = "https://console.groq.com/keys", target = "_blank",
                     style = "color:#2196A6; font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_desc"), "\U0001f4ca Interpret Descriptive Stats",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_corr"), "\U0001f517 Interpret Correlations",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_cmb"),  "\U0001f9ea Interpret CMB / Harman Test",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_dist"), "\U0001f4c8 Interpret Distributions",
                                     class="btn-warning btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_all"),  "\U0001f4cb Full Module Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
        ),
        creator_footer()
      )
    })

    sel_df <- reactive({
      req(df(), input$sel_cols)
      df()[, input$sel_cols, drop=FALSE] |>
        mutate(across(everything(), as.numeric)) |>
        na.omit()
    })

    # Descriptive table
    output$desc_tbl <- DT::renderDataTable({
      req(sel_df())
      d <- psych::describe(sel_df()) |>
        as.data.frame() |>
        mutate(across(where(is.numeric), ~round(.x, 4)))
      tool_dt(d, "Descriptive Statistics (psych::describe)")
    })

    # Distribution plots
    output$hist_cap_notice <- renderUI({
      req(sel_df())
      n_vars <- ncol(sel_df())
      if (n_vars > 9) {
        tags$div(
          class = "alert alert-info",
          style = "font-size:.85rem;padding:.5rem .8rem;margin-bottom:.5rem;border-radius:6px;",
          paste0("ℹ️ Your dataset has ", n_vars,
                 " variables. The histogram grid shows the first 9. ",
                 "Reorder columns in your data to view others.")
        )
      }
    })

    output$dist_plot <- renderPlotly({
      req(sel_df())
      d <- sel_df()
      cols <- names(d)[1:min(9, ncol(d))]
      plots <- lapply(cols, function(col) {
        plot_ly(x=d[[col]], type="histogram", name=col,
                marker=list(color=TEAL, line=list(color="white", width=0.5)),
                opacity=0.8) |>
          layout(xaxis=list(title=col), yaxis=list(title="Count"))
      })
      n_col <- min(3, length(cols))
      n_row <- ceiling(length(cols)/n_col)
      subplot(plots, nrows=n_row, shareY=FALSE, titleX=TRUE) |>
        layout(showlegend=FALSE,
               title=list(text="Variable Distributions", font=list(color=NAVY)),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # Correlation
    corr_mat <- reactive({
      req(sel_df())
      cor(sel_df(), use="pairwise.complete.obs", method="pearson")
    })

    output$corr_plot <- renderPlotly({
      req(corr_mat())
      m <- corr_mat()
      plot_ly(z=m, x=colnames(m), y=rownames(m), type="heatmap",
              colorscale=list(c(0,"#C0392B"), c(0.5,"white"), c(1,TEAL)),
              zmid=0, zmin=-1, zmax=1,
              text=round(m,3), texttemplate="%{text}",
              hovertemplate="r(%{x},%{y}) = %{z:.3f}<extra></extra>") |>
        layout(title=list(text="Pearson Correlation Matrix", font=list(color=NAVY)),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$corr_tbl <- DT::renderDataTable({
      req(corr_mat())
      m <- as.data.frame(round(corr_mat(), 4))
      tool_dt(m, "Correlation Coefficients")
    })

    # CMB Harman's test
    output$cmb_ui <- renderUI({
      req(sel_df())
      d <- sel_df()
      if (ncol(d) < 2) return(tags$p("Need at least 2 columns."))

      tryCatch({
        pca <- prcomp(scale(d))
        var_exp <- summary(pca)$importance[2,]  # proportion of variance
        first_factor_var <- var_exp[1] * 100

        verdict <- if (first_factor_var < 50) {
          list(color=GREEN, icon="✓", text="CMB unlikely",
               detail="First unrotated factor explains < 50% of variance. Common method bias is not a major concern.")
        } else {
          list(color="#C0392B", icon="⚠", text="CMB possible",
               detail="First unrotated factor explains ≥ 50% of variance. Consider procedural or statistical remedies.")
        }

        var_df <- data.frame(
          Factor = paste0("PC", seq_along(var_exp)),
          `Variance Explained (%)` = round(var_exp * 100, 2),
          `Cumulative (%)` = round(cumsum(var_exp) * 100, 2),
          check.names=FALSE
        )

        tagList(
          tags$div(style=paste0("background:", verdict$color, ";color:white;padding:1rem;border-radius:8px;margin-bottom:1rem;"),
            tags$h4(paste(verdict$icon, verdict$text)),
            tags$p(paste0("First unrotated factor: ", round(first_factor_var,2), "%")),
            tags$p(verdict$detail)
          ),
          tags$h4("Eigenvalue / Variance Table"),
          DT::dataTableOutput(ns("cmb_tbl")),
          tags$hr(),
          tags$h4("APA Sentence"),
          tags$div(style="background:#EBF4F7;padding:1rem;border-radius:6px;font-family:monospace;",
            paste0("Harman's (1976) single-factor test was conducted to examine common method bias. ",
                   "The first unrotated factor accounted for ", round(first_factor_var, 2),
                   "% of the variance (threshold: 50%), ",
                   if (first_factor_var < 50) "suggesting that common method bias is unlikely to be a serious concern in this study."
                   else "suggesting potential common method bias. Researchers should interpret results cautiously.")
          )
        )
      }, error=function(e) tags$p(paste("CMB error:", e$message)))
    })

    output$cmb_tbl <- DT::renderDataTable({
      req(sel_df())
      d <- sel_df()
      pca <- prcomp(scale(d))
      var_exp <- summary(pca)$importance[2,]
      var_df <- data.frame(
        Factor = paste0("PC", seq_along(var_exp)),
        `Variance Explained (%)` = round(var_exp * 100, 2),
        `Cumulative (%)` = round(cumsum(var_exp) * 100, 2),
        check.names=FALSE
      )
      tool_dt(var_df)
    })

    # APA
    output$apa_text <- renderText({
      req(sel_df())
      d <- sel_df(); desc <- psych::describe(d)
      lines <- apply(desc, 1, function(row) {
        sprintf("  %s: M = %.3f, SD = %.3f, skewness = %.3f, kurtosis = %.3f",
                rownames(desc)[which(apply(desc, 1, function(r) all(r == row)))[1]],
                row["mean"], row["sd"], row["skew"], row["kurtosis"])
      })
      paste0(
        "Descriptive statistics were computed for all study variables (N = ", nrow(d), ").\n\n",
        paste(capture.output(psych::describe(d)), collapse="\n"), "\n\n",
        "[Analysis: Dr.AIStat — Dr. Aneeq Inam. ORCID: 0000-0001-7682-2244]"
      )
    })

    # Download
    output$dl_desc <- downloadHandler(
      filename = function() paste0("Descriptive_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(sel_df())
        desc_df <- as.data.frame(round(psych::describe(sel_df()), 4))
        corr_df <- as.data.frame(round(corr_mat(), 4))
        write_xlsx(list("Descriptive" = cbind(Variable=rownames(desc_df), desc_df),
                        "Correlations" = cbind(Variable=rownames(corr_df), corr_df)),
                   file)
      }
    )

    # === ADVANCED VISUALIZATIONS: Module 01 ===
    output$adv_raincloud <- renderPlotly({
      if (is.null(sel_df()) || nrow(sel_df()) == 0) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      sel <- if(!is.null(input$sel_cols) && input$sel_cols[1] %in% num_cols) input$sel_cols[1] else num_cols[1]
      p <- ggplot(df, aes(x = factor(1), y = .data[[sel]])) +
        ggdist::stat_halfeye(adjust = 0.5, width = 0.5, .width = 0, justification = -0.2, fill = "#4285F4", alpha = 0.8) +
        geom_boxplot(width = 0.15, outlier.shape = NA, color = "#333333") +
        geom_jitter(width = 0.05, alpha = 0.25, color = "#EA4335", size = 1.2) +
        coord_flip() + theme_minimal(base_size = 13) +
        labs(title = paste("Raincloud Plot \u2014", sel), x = "", y = sel) +
        theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      plotly::ggplotly(p) %>% plotly::layout(hoverlabel = list(bgcolor = "white"))
    })

    output$adv_qqplot <- renderPlotly({
      if (is.null(sel_df()) || nrow(sel_df()) == 0) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      sel <- if(!is.null(input$sel_cols) && input$sel_cols[1] %in% num_cols) input$sel_cols[1] else num_cols[1]
      qq_data <- qqnorm(df[[sel]], plot.it = FALSE)
      p <- ggplot(data.frame(theoretical = qq_data$x, sample = qq_data$y), aes(x = theoretical, y = sample)) +
        geom_point(alpha = 0.5, color = "#4285F4") +
        geom_smooth(method = "lm", se = TRUE, color = "#EA4335", fill = "#FBBC04", alpha = 0.2) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Q-Q Plot \u2014", sel), x = "Theoretical Quantiles", y = "Sample Quantiles")
      plotly::ggplotly(p)
    })

    output$adv_violin <- renderPlotly({
      if (is.null(sel_df()) || nrow(sel_df()) == 0) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      sel <- if(!is.null(input$sel_cols) && input$sel_cols[1] %in% num_cols) input$sel_cols[1] else num_cols[1]
      p <- ggplot(df, aes(x = factor(1), y = .data[[sel]], fill = factor(1))) +
        geom_violin(trim = FALSE, alpha = 0.7, fill = "#34A853") +
        geom_boxplot(width = 0.1, fill = "white", outlier.shape = 21, outlier.fill = "#EA4335") +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "#FBBC04") +
        theme_minimal(base_size = 13) + theme(legend.position = "none") +
        coord_flip() +
        labs(title = paste("Violin + Boxplot \u2014", sel), x = "", y = sel)
      plotly::ggplotly(p)
    })

    output$adv_desc_table <- DT::renderDT({
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      stats_df <- do.call(rbind, lapply(num_cols, function(col) {
        x <- df[[col]]
        data.frame(
          Variable = col, N = sum(!is.na(x)), Mean = round(mean(x, na.rm=TRUE), 3),
          SD = round(sd(x, na.rm=TRUE), 3), Min = round(min(x, na.rm=TRUE), 3),
          Max = round(max(x, na.rm=TRUE), 3), Skewness = round(moments::skewness(x, na.rm=TRUE), 3),
          Kurtosis = round(moments::kurtosis(x, na.rm=TRUE), 3), stringsAsFactors = FALSE
        )
      }))
      DT::datatable(stats_df, extensions = "Buttons",
        options = list(dom = "Bfrtip", buttons = c("csv", "excel"), pageLength = 15),
        rownames = FALSE, class = "stripe hover")
    })

    # Server functions for advanced viz tabs (raincloud_plot_adv, violin_plot_adv, qq_plot_adv)
    output$raincloud_plot_adv <- renderPlotly({
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      plots <- lapply(num_cols[1:min(4, length(num_cols))], function(sel) {
        ggplot(df, aes(x = factor(1), y = .data[[sel]])) +
          ggdist::stat_halfeye(adjust = 0.5, width = 0.5, .width = 0, justification = -0.2, fill = "#4285F4", alpha = 0.8) +
          geom_boxplot(width = 0.15, outlier.shape = NA, color = "#333333") +
          geom_jitter(width = 0.05, alpha = 0.25, color = "#EA4335", size = 1) +
          coord_flip() + theme_minimal(base_size = 11) +
          labs(title = sel, x = "", y = sel) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      })
      plotly_list <- lapply(plots, plotly::ggplotly)
      do.call(plotly::subplot, c(plotly_list, list(nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE)))
    })

    output$violin_plot_adv <- renderPlotly({
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      plots <- lapply(num_cols[1:min(6, length(num_cols))], function(sel) {
        ggplot(df, aes(x = factor(1), y = .data[[sel]])) +
          geom_violin(trim = FALSE, alpha = 0.7, fill = "#34A853") +
          geom_jitter(width = 0.08, alpha = 0.3, color = "#EA4335", size = 0.9) +
          theme_minimal(base_size = 11) + theme(legend.position = "none") +
          labs(title = sel, x = "", y = sel)
      })
      plotly_list <- lapply(plots, plotly::ggplotly)
      do.call(plotly::subplot, c(plotly_list, list(nrows = 2, shareX = FALSE, shareY = FALSE, titleX = TRUE)))
    })

    output$qq_plot_adv <- renderPlot({
      req(sel_df())
      df <- sel_df()
      num_cols <- names(df)[sapply(df, is.numeric)]
      req(length(num_cols) > 0)
      n <- min(6, length(num_cols))
      par(mfrow = c(ceiling(n/3), 3), mar = c(4, 4, 3, 1))
      for (col in num_cols[1:n]) {
        qqnorm(df[[col]], main = paste("Q-Q:", col), pch = 16, col = "#4285F4", cex = 0.7)
        qqline(df[[col]], col = "#EA4335", lwd = 2)
      }
    })

    # Download PNG for raincloud
    output$dl_raincloud_adv <- downloadHandler(
      filename = function() paste0("Raincloud_", Sys.Date(), ".png"),
      content  = function(file) {
        req(sel_df())
        df <- sel_df()
        num_cols <- names(df)[sapply(df, is.numeric)]
        sel <- num_cols[1]
        p <- ggplot(df, aes(x = factor(1), y = .data[[sel]])) +
          ggdist::stat_halfeye(adjust = 0.5, width = 0.5, .width = 0, justification = -0.2, fill = "#4285F4", alpha = 0.8) +
          geom_boxplot(width = 0.15, outlier.shape = NA, color = "#333333") +
          geom_jitter(width = 0.05, alpha = 0.25, color = "#EA4335", size = 1.2) +
          coord_flip() + theme_minimal(base_size = 13) +
          labs(title = paste("Raincloud Plot —", sel), x = "", y = sel) +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
        ggplot2::ggsave(file, plot = p, width = 8, height = 5, dpi = 150)
      }
    )

    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_render <- function(result) {
      tags$div(
        style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result)
      )
    }

    .ai_key_warn <- function() {
      tags$div(class="alert alert-warning", style="margin-top:.5rem;",
        "\u26a0\ufe0f No API key. Paste your FREE Groq key (gsk_\u2026) in the sidebar. ",
        tags$a("Get key \u2192 console.groq.com/keys", href="https://console.groq.com/keys", target="_blank"))
    }

    # 1. Descriptive Stats
    observeEvent(input$ai_btn_desc, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      ctx <- tryCatch({
        d <- sel_df(); desc <- psych::describe(d)
        paste0("DESCRIPTIVE STATISTICS RESULTS\nDataset: ", nrow(d), " rows × ", ncol(d),
               " variables\nVariables analysed: ", paste(names(d), collapse=", "),
               "\n\nFull Descriptive Statistics Table (mean, sd, median, min, max, skew, kurtosis, SE):\n",
               paste(capture.output(print(round(desc, 4))), collapse="\n"))
      }, error=function(e) "Please upload data and select columns first.")
      output$ai_output <- renderUI({
        .ai_render(call_gemini(paste0(
          "You are an expert statistician and quantitative researcher writing for a top-tier peer-reviewed journal.\n\n",
          "Task: Provide a DETAILED interpretation of the following descriptive statistics results. Your response must:\n",
          "1. Report exact values (M, SD, skewness, kurtosis) for each variable with proper APA 7th edition formatting\n",
          "2. Assess normality using skewness (<|2|) and kurtosis (<|7|) benchmarks — flag any violations\n",
          "3. Compare variability across variables and identify the most/least variable measures\n",
          "4. Discuss what the mean levels suggest about the sample characteristics\n",
          "5. Note any variables with unusually high/low values and what they imply\n",
          "6. Provide a sample-level summary paragraph suitable for the Method/Results section\n",
          "7. Suggest any data transformations needed before further analysis\n\n",
          "Write in formal academic prose (5-7 paragraphs). Use exact numbers throughout.\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 2. Correlations
    observeEvent(input$ai_btn_corr, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      ctx <- tryCatch({
        d <- sel_df()
        cm <- round(cor(d, use="pairwise.complete.obs"), 4)
        paste0("CORRELATION MATRIX RESULTS\nVariables: ", paste(names(d), collapse=", "),
               "\nSample size: ", nrow(d),
               "\n\nPearson Correlation Matrix:\n",
               paste(capture.output(print(cm)), collapse="\n"),
               "\n\nNote: |r| > 0.7 indicates potential multicollinearity concern.")
      }, error=function(e) "Please upload data and run analysis first.")
      output$ai_output <- renderUI({
        .ai_render(call_gemini(paste0(
          "You are an expert statistician writing for a peer-reviewed journal.\n\n",
          "Task: Provide a DETAILED interpretation of the following correlation matrix. Your response must:\n",
          "1. Identify all statistically significant correlations (assuming p < .05 two-tailed)\n",
          "2. Classify correlation strength: weak (|r| < .30), moderate (.30-.50), strong (> .50) per Cohen (1988)\n",
          "3. Flag any multicollinearity concerns (|r| > .70) and explain their implications\n",
          "4. Identify the strongest positive and negative relationships and what they theoretically mean\n",
          "5. Discuss the direction of relationships in terms of the research context\n",
          "6. Comment on any unexpected or noteworthy patterns\n",
          "7. Advise whether correlations support or undermine subsequent regression/SEM analysis\n",
          "8. Write a complete Results-section paragraph using APA 7 format (r = .XX, p < .05)\n\n",
          "Write in formal academic prose (5-6 paragraphs). Reference specific variables by name.\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 3. CMB / Harman Test
    observeEvent(input$ai_btn_cmb, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      ctx <- tryCatch({
        d <- sel_df()
        pc <- prcomp(d, scale.=TRUE)
        var_exp <- summary(pc)$importance[2,]
        paste0("CMB / HARMAN SINGLE-FACTOR TEST RESULTS\n",
               "Variables: ", paste(names(d), collapse=", "),
               "\nN = ", nrow(d),
               "\n\nVariance explained by each principal component:\n",
               paste(names(var_exp), round(var_exp*100,2), "%", collapse="\n"),
               "\n\nFirst factor variance explained: ", round(var_exp[1]*100,2), "%",
               "\n(Harman threshold: <50% suggests CMB is not a severe concern)")
      }, error=function(e) "Please upload data first.")
      output$ai_output <- renderUI({
        .ai_render(call_gemini(paste0(
          "You are an expert methodologist specialising in survey research and common method bias.\n\n",
          "Task: Provide a DETAILED interpretation of the Harman Single-Factor CMB test. Your response must:\n",
          "1. State the percentage of variance explained by the first unrotated factor\n",
          "2. Apply the Harman criterion (< 50% = CMB not a major concern) and give a clear verdict\n",
          "3. Explain what Common Method Bias (CMB) is and why it matters in self-report survey research\n",
          "4. Discuss limitations of the Harman test and recommend additional CMB checks (marker variable, CLF)\n",
          "5. Provide the exact APA-formatted sentence for the Methods section reporting this result\n",
          "6. If CMB is a concern, suggest remedies (procedural and statistical)\n\n",
          "Write in formal academic prose (4-5 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 4. Distributions
    observeEvent(input$ai_btn_dist, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      ctx <- tryCatch({
        d <- sel_df(); desc <- psych::describe(d)
        skew_kurt <- data.frame(
          Variable = rownames(desc),
          Skewness = round(desc$skew, 4),
          Kurtosis = round(desc$kurtosis, 4),
          Shapiro_Note = ifelse(abs(desc$skew) > 2 | abs(desc$kurtosis) > 7, "Potential non-normality", "Acceptable")
        )
        paste0("DISTRIBUTION ANALYSIS RESULTS\nN = ", nrow(d),
               "\n\nSkewness & Kurtosis Summary:\n",
               paste(capture.output(print(skew_kurt)), collapse="\n"))
      }, error=function(e) "Please upload data first.")
      output$ai_output <- renderUI({
        .ai_render(call_gemini(paste0(
          "You are an expert statistician evaluating data distributions for peer-reviewed research.\n\n",
          "Task: Provide a DETAILED interpretation of distribution characteristics. Your response must:\n",
          "1. Evaluate normality for each variable using skewness and kurtosis values with thresholds\n",
          "2. Identify positively vs negatively skewed variables and their implications\n",
          "3. Discuss leptokurtic vs platykurtic distributions and what they mean for the data\n",
          "4. Recommend whether parametric or non-parametric tests are appropriate\n",
          "5. Suggest data transformations (log, square root, Box-Cox) for non-normal variables\n",
          "6. Explain implications for regression, SEM, or other planned downstream analyses\n\n",
          "Write in formal academic prose (4-5 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # 5. Full Module Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      ctx <- tryCatch({
        d <- sel_df(); desc <- psych::describe(d)
        cm <- round(cor(d, use="pairwise.complete.obs"), 3)
        pc <- prcomp(d, scale.=TRUE)
        var1 <- round(summary(pc)$importance[2,1]*100, 2)
        paste0("FULL DESCRIPTIVE ANALYSIS SUMMARY\nDataset: ", nrow(d), " rows × ", ncol(d), " variables\n",
               "Variables: ", paste(names(d), collapse=", "),
               "\n\nDescriptive Stats:\n", paste(capture.output(print(round(desc, 3))), collapse="\n"),
               "\n\nCorrelation Matrix:\n", paste(capture.output(print(cm)), collapse="\n"),
               "\n\nHarman First Factor Variance: ", var1, "%")
      }, error=function(e) "Please upload data and run analysis first.")
      output$ai_output <- renderUI({
        .ai_render(call_gemini(paste0(
          "You are an expert statistician and academic researcher writing for a top-tier peer-reviewed journal.\n\n",
          "Task: Write a COMPREHENSIVE Results section covering all descriptive analyses below. Include:\n",
          "1. Sample characteristics paragraph (means, SDs, ranges for all variables)\n",
          "2. Normality assessment paragraph (skewness, kurtosis, implications)\n",
          "3. Correlation analysis paragraph (significant relationships, strengths, directions, APA format)\n",
          "4. Common Method Bias assessment paragraph (Harman test result and interpretation)\n",
          "5. Data quality and suitability paragraph (readiness for further analysis)\n",
          "6. Brief methodological notes for the Discussion section\n\n",
          "Use APA 7th edition formatting throughout. Write in formal academic prose (6-8 paragraphs).\n\n",
          "DATA:\n", ctx), api_key))
      })
    })

    # Keep backward-compatible generic button (ai_btn → routes to full summary)
    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_key_warn()); return() }
      NULL # (use buttons above for specific analyses)
    }, ignoreInit = TRUE)

    # ══════════════════════════════════════════════════════════════════════

  })
}
