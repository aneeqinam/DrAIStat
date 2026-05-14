mod07_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "📏",
      title = "Item Response Theory (IRT)",
      subtitle = "Rasch (1PL) · 2PL IRT · Item Fit · Person-Item Map"
    ),
      uiOutput(ns("global_data_banner")),
  fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod07_server <- function(id, gemini_key = reactive("")) {
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
        title = "Item Response Theory",
        tabPanel(
          "Rasch / 1PL",
          fluidRow(
            column(12, pickerInput(ns("rasch_items"), "Select Binary Items (0/1)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("rasch_run"), "Run Rasch Model (1PL)", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Item Difficulty Parameters"), withSpinner(DT::dataTableOutput(ns("rasch_item_params"))))
          ),
          hr(),
          fluidRow(
            column(12, h5("Person Parameters (Abilities)"), withSpinner(DT::dataTableOutput(ns("rasch_person_params"))))
          )
        ),
        tabPanel(
          "2PL IRT",
          fluidRow(
            column(12, pickerInput(ns("twopl_items"), "Select Binary Items (0/1)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("twopl_run"), "Run 2PL Model", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Item Parameters (Discrimination & Difficulty)"), withSpinner(DT::dataTableOutput(ns("twopl_item_params"))))
          ),
          hr(),
          fluidRow(
            column(12, h5("Item Characteristic Curves"), withSpinner(plotOutput(ns("twopl_icc"), height = "400px")))
          )
        ),
        tabPanel(
          "Item Fit",
          fluidRow(
            column(12, pickerInput(ns("itemfit_items"), "Select Binary Items (0/1)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("itemfit_run"), "Compute Item Fit Statistics", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Infit/Outfit MSQ Values"), withSpinner(DT::dataTableOutput(ns("itemfit_table"))))
          ),
          hr(),
          verbatimTextOutput(ns("itemfit_interpretation"))
        ),
        tabPanel(
          "Person-Item Map",
          fluidRow(
            column(12, pickerInput(ns("pimap_items"), "Select Binary Items (0/1)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("pimap_run"), "Generate Person-Item Map", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Person-Item Map"), withSpinner(plotOutput(ns("pimap_plot"), height = "500px")))
          )
        ),
        tabPanel(
          "Download",
          fluidRow(
            column(12, downloadButton(ns("download_excel"), "Download IRT Results (Excel)", class = "btn-success"))
          )
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("📈 Interactive ICC",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Interactive Item Characteristic Curves (ICC) for the 2PL model. Run 2PL IRT first."),
          withSpinner(plotlyOutput(ns("icc_interactive"), height="500px"))
        ),
        tabPanel("💡 Item Information",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Item information curves showing where each item provides the most measurement precision. Run 2PL first."),
          withSpinner(plotlyOutput(ns("item_info_curves"), height="480px"))
        ),
        tabPanel("🔵 Difficulty-Discrimination",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Bubble chart of item difficulty (b) vs discrimination (a) — larger bubbles = higher maximum information."),
          withSpinner(plotlyOutput(ns("diff_disc_plot"), height="460px"))
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
              column(4, actionButton(ns("ai_btn_rasch"), "📐 Rasch Model",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_twopl"), "📊 2PL IRT Model",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_fit"), "🔍 Item Fit Analysis",
                                     class="btn-warning btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(12, actionButton(ns("ai_btn_all"), "📋 Full IRT Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== Rasch (1PL) ====
    rasch_model <- reactiveVal(NULL)

    observeEvent(input$rasch_run, {
      req(input$rasch_items)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Fitting Rasch model...", {
          df <- data[, input$rasch_items]
          df <- na.omit(df)

          # Validate binary matrix
          unique_vals <- unique(as.vector(as.matrix(df)))
          unique_vals <- unique_vals[!is.na(unique_vals)]
          if (!all(unique_vals %in% c(0, 1))) {
            showNotification("Rasch model requires binary (0/1) data. Found non-binary values. Please recode items.", type = "error")
            return(NULL)
          }
          if (ncol(df) < 2) {
            showNotification("IRT requires at least 2 items.", type = "warning")
            return(NULL)
          }

          # Fit Rasch model using eRm
          model <- eRm::RM(df)

          # Extract parameters
          item_params <- coef(model)

          rasch_model(list(
            model = model,
            item_params = item_params,
            items = input$rasch_items
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$rasch_item_params <- DT::renderDataTable({
      req(rasch_model())
      res <- rasch_model()

      param_df <- data.frame(
        Item = names(res$item_params),
        Difficulty = round(as.numeric(res$item_params), 3)
      )
      param_df <- param_df[order(param_df$Difficulty), ]

      tool_dt(param_df, "Rasch Item Difficulty Parameters (β)")
    })

    output$rasch_person_params <- DT::renderDataTable({
      req(rasch_model())
      res <- rasch_model()

      person_ability <- eRm::person.parameter(res$model)
      person_df <- data.frame(
        Person = 1:length(person_ability$theta),
        Ability = round(person_ability$theta, 3)
      )

      tool_dt(head(person_df, 50), "Person Ability Parameters (First 50)")
    })

    # ==== 2PL IRT ====
    twopl_model <- reactiveVal(NULL)

    observeEvent(input$twopl_run, {
      req(input$twopl_items)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Fitting 2PL model...", {
          df <- data[, input$twopl_items]
          df <- na.omit(df)

          # Fit 2PL using ltm
          model <- ltm::ltm(df ~ z1)

          twopl_model(list(
            model = model,
            items = input$twopl_items
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$twopl_item_params <- DT::renderDataTable({
      req(twopl_model())
      res <- twopl_model()

      coef_summary <- summary(res$model)$coefficients
      coef_df <- as.data.frame(coef_summary)

      param_df <- data.frame(
        Item = rownames(coef_df),
        Discrimination = round(coef_df[, 1], 3),
        Difficulty = round(coef_df[, 2], 3)
      )

      tool_dt(param_df, "2PL Item Parameters (Discrimination & Difficulty)")
    })

    output$twopl_icc <- renderPlot({
      req(twopl_model())
      res <- twopl_model()

      plot(res$model, type = "ICC")
      title("Item Characteristic Curves (2PL)")
    })

    # ==== Item Fit ====
    itemfit_results <- reactiveVal(NULL)

    observeEvent(input$itemfit_run, {
      req(input$itemfit_items)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Computing item fit...", {
          df <- data[, input$itemfit_items]
          df <- na.omit(df)

          # Fit Rasch model
          model <- eRm::RM(df)

          # Item fit using eRm
          ifit <- eRm::itemfit(model)

          itemfit_results(ifit)
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$itemfit_table <- DT::renderDataTable({
      req(itemfit_results())
      ifit <- itemfit_results()

      # Extract infit/outfit from itemfit object
      if (!is.null(ifit)) {
        ifit_df <- as.data.frame(ifit)
        ifit_df$Item <- rownames(ifit_df)
        ifit_df <- ifit_df[, c(ncol(ifit_df), 1:(ncol(ifit_df)-1))]

        tool_dt(ifit_df, "Item Fit Statistics (Infit/Outfit MSQ)")
      }
    })

    output$itemfit_interpretation <- renderText({
      req(itemfit_results())

      paste0(
        "Item Fit Interpretation:\n\n",
        "Infit MSQ & Outfit MSQ range from 0 to ~3\n",
        "Acceptable range: 0.5 - 1.5 (or 0.7 - 1.3 strict)\n\n",
        "MSQ < 0.5: Overfitting (too predictable)\n",
        "MSQ 0.5-1.5: Good fit\n",
        "MSQ > 1.5: Poor fit (mismatch with model)\n\n",
        "ZSTD (standardized): should be close to 0\n",
        "ZSTD > |2|: Significant misfit at p < .05"
      )
    })

    # ==== Person-Item Map ====
    pimap_model <- reactiveVal(NULL)

    observeEvent(input$pimap_run, {
      req(input$pimap_items)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Generating Person-Item Map...", {
          df <- data[, input$pimap_items]
          df <- na.omit(df)

          model <- eRm::RM(df)

          pimap_model(model)
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$pimap_plot <- renderPlot({
      req(pimap_model())
      model <- pimap_model()

      eRm::plotPImap(model)
    })

    # ==== Download ====
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("irt_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sheets_list <- list()

        if (!is.null(rasch_model())) {
          res <- rasch_model()
          param_df <- data.frame(
            Item = names(res$item_params),
            Difficulty = round(as.numeric(res$item_params), 3)
          )
          sheets_list[["Rasch Items"]] <- param_df
        }

        if (!is.null(twopl_model())) {
          res <- twopl_model()
          coef_summary <- summary(res$model)$coefficients
          coef_df <- as.data.frame(coef_summary)
          param_df <- data.frame(
            Item = rownames(coef_df),
            Discrimination = round(coef_df[, 1], 3),
            Difficulty = round(coef_df[, 2], 3)
          )
          sheets_list[["2PL Items"]] <- param_df
        }

        if (length(sheets_list) > 0) {
          writexl::write_xlsx(sheets_list, file)
        }
      }
    )
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r07 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k07 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Rasch Model
    observeEvent(input$ai_btn_rasch, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k07()); return() }
      ctx <- tryCatch({
        m <- rasch_model(); req(m)
        paste0("RASCH MODEL RESULTS\n", paste(head(capture.output(summary(m)), 50), collapse="\n"))
      }, error=function(e) "Please run Rasch Model first.")
      output$ai_output <- renderUI({ .ai_r07(call_gemini(paste0(
        "You are an expert in Item Response Theory and psychometrics writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Rasch Model results. Include:\n",
        "1. Explain Rasch model assumptions: unidimensionality, local independence, sufficient statistics\n",
        "2. Report item difficulty parameters (β) — rank items from easiest to hardest\n",
        "3. Assess item fit: infit/outfit MNSQ (acceptable range: 0.6–1.4) and standardised fit (|zstd| < 2)\n",
        "4. Evaluate person fit: separation reliability (> .70 good) and person reliability\n",
        "5. Report item discrimination — in Rasch all items assumed equal; test this assumption\n",
        "6. Assess Wright map: does item difficulty distribution match person ability distribution?\n",
        "7. Identify misfitting items and recommend actions (revise, drop, or accept with caution)\n",
        "8. Write APA-style Results paragraph for IRT/Rasch analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. 2PL
    observeEvent(input$ai_btn_twopl, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k07()); return() }
      ctx <- tryCatch({
        m <- twopl_model(); req(m)
        paste0("2PL IRT RESULTS\n", paste(head(capture.output(summary(m)), 50), collapse="\n"))
      }, error=function(e) "Please run 2PL model first.")
      output$ai_output <- renderUI({ .ai_r07(call_gemini(paste0(
        "You are an expert in Item Response Theory writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of 2PL IRT Model. Include:\n",
        "1. Explain 2PL: two parameters — difficulty (b) and discrimination (a) per item\n",
        "2. Report discrimination parameters: a > 1.0 = good discrimination; a < 0.5 = poor\n",
        "3. Report difficulty parameters: b values across the latent trait continuum\n",
        "4. Compare 2PL to Rasch (1PL) — is allowing varying discrimination justified (LRT)?\n",
        "5. Assess item characteristic curves: do they show expected sigmoid shape?\n",
        "6. Report test information function: where is the test most informative on the trait continuum?\n",
        "7. Identify items with extremely high/low discrimination and discuss implications\n",
        "8. Write APA-style Results paragraph\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Item Fit
    observeEvent(input$ai_btn_fit, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k07()); return() }
      ctx <- tryCatch({
        res <- itemfit_results(); req(res)
        paste0("ITEM FIT ANALYSIS RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run Item Fit Analysis first.")
      output$ai_output <- renderUI({ .ai_r07(call_gemini(paste0(
        "You are an expert psychometrician writing for a peer-reviewed measurement journal.\n\n",
        "Task: DETAILED interpretation of IRT Item Fit statistics. Include:\n",
        "1. Explain what infit and outfit statistics measure (residual-based fit)\n",
        "2. Classify each item: infit MNSQ 0.6-1.4 acceptable; outfit more sensitive to outliers\n",
        "3. Identify overfitting items (MNSQ < 0.6): predictable but don't add information\n",
        "4. Identify underfitting items (MNSQ > 1.4): noisy, unexpected responses\n",
        "5. Report chi-square item fit tests with Bonferroni-corrected p-values\n",
        "6. Make specific recommendations: keep, revise, or remove each misfitting item\n",
        "7. Discuss implications for scale validity and construct measurement\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Full IRT Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k07()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        rm <- tryCatch(rasch_model(), error=function(e) NULL)
        if (!is.null(rm)) parts <- c(parts, paste0("Rasch:\n", paste(head(capture.output(summary(rm)),30), collapse="\n")))
        tp <- tryCatch(twopl_model(), error=function(e) NULL)
        if (!is.null(tp)) parts <- c(parts, paste0("2PL:\n", paste(head(capture.output(summary(tp)),30), collapse="\n")))
        ifit <- tryCatch(itemfit_results(), error=function(e) NULL)
        if (!is.null(ifit)) parts <- c(parts, paste0("Item Fit:\n", paste(capture.output(print(ifit)), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("IRT FULL ANALYSIS\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one IRT model first.")
      output$ai_output <- renderUI({ .ai_r07(call_gemini(paste0(
        "You are an expert in Item Response Theory writing for a top-tier psychometrics journal.\n\n",
        "Task: Write a COMPREHENSIVE IRT Results section. Include:\n",
        "1. Model comparison: Rasch (1PL) vs 2PL — likelihood ratio test, model fit\n",
        "2. Item parameters: discrimination and difficulty for all items\n",
        "3. Item fit: infit/outfit for all items, misfitting items\n",
        "4. Person fit and reliability: separation index, test reliability\n",
        "5. Test information function: measurement precision along the trait continuum\n",
        "6. Wright map interpretation: item-person targeting\n\n",
        "Use APA 7. Formal academic prose (6-8 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k07()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Interactive ICC curves (2PL)
    output$icc_interactive <- renderPlotly({
      if (is.null(twopl_model())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(twopl_model())
      res <- twopl_model()
      params <- res$item_params
      if (is.null(params)) return(plot_ly() |> layout(title="Run 2PL IRT first"))

      theta_seq <- seq(-4, 4, length.out = 200)
      pal <- colorRampPalette(c(TEAL, NAVY, AMBER, GREEN, "#9B59B6"))(nrow(params))

      p <- plot_ly()
      for (i in seq_len(nrow(params))) {
        a <- as.numeric(params[i, "Discrimination"])
        b <- as.numeric(params[i, "Difficulty"])
        icc_vals <- 1 / (1 + exp(-a * (theta_seq - b)))
        p <- add_lines(p, x = theta_seq, y = icc_vals,
                       name = rownames(params)[i],
                       line = list(color = pal[i], width = 2),
                       hovertemplate = paste0(rownames(params)[i],
                         "<br>θ=%{x:.2f}<br>P(θ)=%{y:.3f}<extra></extra>"))
      }
      p <- add_segments(p, x=0, xend=0, y=0, yend=1,
                        line=list(color="#AAAAAA",dash="dot",width=1),
                        name="θ=0", inherit=FALSE)
      p |> layout(title=list(text="Item Characteristic Curves (2PL)",font=list(color=NAVY)),
                  xaxis=list(title="Ability (θ)"),
                  yaxis=list(title="P(correct | θ)", range=c(0,1)),
                  plot_bgcolor="white", paper_bgcolor="white")
    })

    # Item Information Curves
    output$item_info_curves <- renderPlotly({
      if (is.null(twopl_model())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(twopl_model())
      res    <- twopl_model()
      params <- res$item_params
      if (is.null(params)) return(plot_ly() |> layout(title="Run 2PL IRT first"))

      theta_seq <- seq(-4, 4, length.out=200)
      pal <- colorRampPalette(c(TEAL,NAVY,AMBER,GREEN,"#9B59B6"))(nrow(params))
      p   <- plot_ly()

      for (i in seq_len(nrow(params))) {
        a    <- as.numeric(params[i,"Discrimination"])
        b    <- as.numeric(params[i,"Difficulty"])
        prob <- 1 / (1 + exp(-a*(theta_seq-b)))
        info <- a^2 * prob * (1-prob)
        p <- add_lines(p, x=theta_seq, y=info,
                       name=rownames(params)[i],
                       line=list(color=pal[i],width=2))
      }
      p |> layout(title=list(text="Item Information Curves (2PL)",font=list(color=NAVY)),
                  xaxis=list(title="Ability (θ)"),
                  yaxis=list(title="Information I(θ)"),
                  plot_bgcolor="white", paper_bgcolor="white")
    })

    # Difficulty-Discrimination Bubble Chart
    output$diff_disc_plot <- renderPlotly({
      if (is.null(twopl_model())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(twopl_model())
      res    <- twopl_model()
      params <- res$item_params
      if (is.null(params)) return(plot_ly() |> layout(title="Run 2PL IRT first"))

      a_vals <- as.numeric(params[,"Discrimination"])
      b_vals <- as.numeric(params[,"Difficulty"])
      # max info = a^2/4
      max_info <- a_vals^2 / 4

      plot_ly(x=b_vals, y=a_vals, type="scatter", mode="markers",
              marker=list(size=pmax(10, max_info*40),
                          color=TEAL, opacity=0.7,
                          line=list(color=NAVY,width=1)),
              text=rownames(params),
              hovertemplate="Item: %{text}<br>Difficulty (b): %{x:.3f}<br>Discrimination (a): %{y:.3f}<extra></extra>") |>
        add_annotations(x=b_vals, y=a_vals, text=rownames(params),
                        xanchor="center", yanchor="bottom", yshift=8,
                        showarrow=FALSE, font=list(size=9,color=NAVY)) |>
        layout(title=list(text="Item Difficulty vs Discrimination (bubble size = max info)",
                          font=list(color=NAVY)),
               xaxis=list(title="Difficulty (b)"),
               yaxis=list(title="Discrimination (a)"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
