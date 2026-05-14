mod09_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "⚡",
      title = "Advanced Methods",
      subtitle = "Bayesian Regression (rstanarm) · Network Analysis · Correlation Network"
    ),
      uiOutput(ns("global_data_banner")),
  fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod09_server <- function(id, gemini_key = reactive("")) {
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
        title = "Advanced Methods",
        tabPanel(
          "Bayesian Regression",
          fluidRow(
            column(4, pickerInput(ns("bayes_dv"), "Dependent Variable (DV)", choices = num_cols(), options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("bayes_ivs"), "Independent Variables (IVs)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(12, actionButton(ns("bayes_run"), "Run Bayesian Regression (rstanarm)", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Posterior Credible Intervals (95%)"), withSpinner(DT::dataTableOutput(ns("bayes_table")))),
            column(6, h5("Summary Statistics"), verbatimTextOutput(ns("bayes_summary")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Posterior Samples Trace Plot"), withSpinner(plotOutput(ns("bayes_trace"), height = "400px")))
          )
        ),
        tabPanel(
          "Correlation Network",
          fluidRow(
            column(8, pickerInput(ns("net_cols"), "Select Numeric Variables", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, sliderInput(ns("net_threshold"), "Min |Correlation| Threshold", min = 0, max = 1, value = 0.3, step = 0.05))
          ),
          fluidRow(
            column(12, actionButton(ns("net_run"), "Build Network", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Correlation Network Graph"), withSpinner(plotlyOutput(ns("net_plot"), height = "600px")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Network Statistics"), verbatimTextOutput(ns("net_stats")))
          )
        ),
        tabPanel(
          "Guidance",
          verbatimTextOutput(ns("guidance_text"))
        ),
        tabPanel(
          "Download",
          fluidRow(
            column(12, downloadButton(ns("download_excel"), "Download Results (Excel)", class = "btn-success"))
          )
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("🕸️ visNetwork Graph",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Interactive visNetwork correlation graph — drag nodes, hover for details. Run Correlation Network first."),
          withSpinner(visNetwork::visNetworkOutput(ns("vis_network"), height="520px"))
        ),
        tabPanel("📊 Posterior Densities",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Posterior density plots for each coefficient with 95% credible intervals. Run Bayesian Regression first."),
          withSpinner(plotlyOutput(ns("posterior_densities"), height="500px"))
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
              column(4, actionButton(ns("ai_btn_bayes"), "🔵 Bayesian Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_network"), "🕸️ Network Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_all"), "📋 Full Advanced Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== Bayesian Regression ====
    bayes_model <- reactiveVal(NULL)

    observeEvent(input$bayes_run, {
      req(input$bayes_dv, input$bayes_ivs)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Fitting Bayesian model (this may take a minute)...", {
          dv <- input$bayes_dv
          ivs <- input$bayes_ivs

          df <- data[, c(dv, ivs)]
          df <- na.omit(df)

          # Bayesian regression using rstanarm
          # Using default priors: normal(0, 2.5) for coefficients
          formula <- as.formula(paste0(dv, " ~ ", paste(ivs, collapse = " + ")))

          model <- rstanarm::stan_glm(
            formula,
            data = df,
            family = gaussian(),
            prior = rstanarm::normal(0, 2.5),
            prior_intercept = rstanarm::normal(0, 10),
            chains = 2,
            iter = 1000,
            refresh = 0,
            seed = 42
          )

          bayes_model(model)
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$bayes_table <- DT::renderDataTable({
      req(bayes_model())
      model <- bayes_model()

      posterior_samples <- as.data.frame(model)
      posterior_summary <- data.frame(
        Variable = colnames(posterior_samples),
        Mean = round(apply(posterior_samples, 2, mean), 4),
        SD = round(apply(posterior_samples, 2, sd), 4),
        "Q2.5%" = round(apply(posterior_samples, 2, quantile, probs = 0.025), 4),
        "Q97.5%" = round(apply(posterior_samples, 2, quantile, probs = 0.975), 4),
        check.names = FALSE
      )

      tool_dt(posterior_summary, "Bayesian Regression: Posterior Credible Intervals (95%)")
    })

    output$bayes_summary <- renderText({
      req(bayes_model())
      model <- bayes_model()

      capture.output({
        print(model, digits = 3)
      }) %>% paste(collapse = "\n")
    })

    output$bayes_trace <- renderPlot({
      req(bayes_model())
      model <- bayes_model()

      plot(model, ask = FALSE)
    })

    # ==== Correlation Network ====
    network_data <- reactiveVal(NULL)

    observeEvent(input$net_run, {
      req(input$net_cols)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Building correlation network...", {
          df <- data[, input$net_cols]
          df <- na.omit(df)

          # Compute correlation matrix
          cor_matrix <- cor(df)

          # Apply threshold
          threshold <- input$net_threshold
          cor_matrix_threshold <- cor_matrix
          cor_matrix_threshold[abs(cor_matrix_threshold) < threshold] <- 0
          diag(cor_matrix_threshold) <- 0

          # Convert to igraph
          # Create adjacency matrix from correlation
          adj_matrix <- ifelse(abs(cor_matrix_threshold) > 0, 1, 0)
          g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected", weighted = TRUE)

          # Store edge weights (correlations)
          igraph::E(g)$weight <- cor_matrix_threshold[which(adj_matrix != 0, arr.ind = TRUE)]

          network_data(list(
            graph = g,
            cor_matrix = cor_matrix,
            threshold = threshold,
            vars = input$net_cols
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$net_plot <- renderPlotly({
      req(network_data())
      net <- network_data()
      g <- net$graph

      if (igraph::ecount(g) == 0) {
        plot_ly() %>%
          add_text(x = 0.5, y = 0.5, text = "No correlations above threshold. Increase threshold or check data.", mode = "text")
      } else {
        # Layout
        layout_mat <- igraph::layout_with_fr(g)

        # Degree centrality for node size
        degree_centrality <- igraph::degree(g)
        node_size <- 20 + degree_centrality * 5

        # Create node and edge traces
        edge_x <- c()
        edge_y <- c()

        for (edge in igraph::E(g)) {
          u <- igraph::head_of(edge, g)
          v <- igraph::tail_of(edge, g)
          x0 <- layout_mat[u, 1]
          y0 <- layout_mat[u, 2]
          x1 <- layout_mat[v, 1]
          y1 <- layout_mat[v, 2]

          edge_x <- c(edge_x, x0, x1, NA)
          edge_y <- c(edge_y, y0, y1, NA)
        }

        edge_trace <- plot_ly(
          x = edge_x, y = edge_y,
          mode = "lines",
          line = list(width = 0.5, color = "#888"),
          hoverinfo = "none",
          showlegend = FALSE
        )

        node_x <- layout_mat[, 1]
        node_y <- layout_mat[, 2]
        node_text <- names(igraph::V(g))

        node_trace <- plot_ly(
          x = node_x, y = node_y,
          mode = "markers+text",
          text = node_text,
          textposition = "top center",
          hoverinfo = "text",
          marker = list(
            size = node_size,
            color = TEAL,
            line = list(width = 2, color = NAVY)
          ),
          showlegend = FALSE
        )

        # Combine traces
        subplot(edge_trace, node_trace, nrows = 1, margin = 0) %>%
          layout(
            title = paste0("Correlation Network (|r| > ", net$threshold, ")"),
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            hovermode = "closest"
          )
      }
    })

    output$net_stats <- renderText({
      req(network_data())
      net <- network_data()
      g <- net$graph

      num_nodes <- igraph::vcount(g)
      num_edges <- igraph::ecount(g)
      density <- igraph::edge_density(g)
      degree_centrality <- igraph::degree(g)

      paste0(
        "Network Statistics\n",
        "==================\n",
        "Nodes: ", num_nodes, "\n",
        "Edges: ", num_edges, "\n",
        "Density: ", round(density, 3), "\n\n",
        "Node Degree Centrality:\n",
        paste(
          paste0(names(degree_centrality), ": ", degree_centrality),
          collapse = "\n"
        ),
        "\n\nInterpretation:\n",
        "Density (0-1): Higher = more interconnected\n",
        "Degree: Number of connections per variable\n"
      )
    })

    # ==== Guidance ====
    output$guidance_text <- renderText({
      paste0(
        "Advanced Methods Guidance\n",
        "=========================\n\n",
        "1. BAYESIAN REGRESSION (rstanarm)\n",
        "When to use:\n",
        "• Small sample sizes (n < 100)\n",
        "• Complex models with many parameters\n",
        "• Prior information available\n",
        "• Want uncertainty estimates (credible intervals vs CI)\n\n",
        "Advantages:\n",
        "• Incorporates prior knowledge\n",
        "• Full posterior distributions (not just point estimates)\n",
        "• Better handling of uncertainty\n",
        "• Direct probability statements on parameters\n\n",
        "Interpretation:\n",
        "95% credible interval: 'There is a 95% probability the true parameter\n",
        "falls within this range' (Bayesian interpretation)\n",
        "vs. Frequentist CI: 'If we repeated the study many times, 95% of CIs\n",
        "would contain the true parameter'\n\n",
        "---\n\n",
        "2. CORRELATION NETWORK\n",
        "When to use:\n",
        "• Visualize multivariate relationships\n",
        "• Identify clusters of related variables\n",
        "• Detect outlier relationships\n",
        "• Explore structural patterns in data\n\n",
        "Interpretation:\n",
        "• Node size = degree centrality (# connections)\n",
        "• Edge color/thickness = strength of correlation\n",
        "• Clusters = groups of highly correlated variables\n\n",
        "Key References:\n",
        "• Gelman et al. (2013): Bayesian Data Analysis\n",
        "• Csardi & Nepusz (2006): igraph - network analysis\n"
      )
    })

    # ==== Download ====
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("advanced_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sheets_list <- list()

        if (!is.null(bayes_model())) {
          model <- bayes_model()
          posterior_samples <- as.data.frame(model)
          posterior_summary <- data.frame(
            Variable = colnames(posterior_samples),
            Mean = round(apply(posterior_samples, 2, mean), 4),
            SD = round(apply(posterior_samples, 2, sd), 4),
            "Q2.5%" = round(apply(posterior_samples, 2, quantile, probs = 0.025), 4),
            "Q97.5%" = round(apply(posterior_samples, 2, quantile, probs = 0.975), 4),
            check.names = FALSE
          )
          sheets_list[["Bayesian Results"]] <- posterior_summary
        }

        if (!is.null(network_data())) {
          net <- network_data()
          cor_df <- as.data.frame(net$cor_matrix)
          cor_df$Variable <- rownames(net$cor_matrix)
          cor_df <- cor_df[, c(ncol(cor_df), 1:(ncol(cor_df)-1))]
          sheets_list[["Correlation Matrix"]] <- cor_df
        }

        if (length(sheets_list) > 0) {
          writexl::write_xlsx(sheets_list, file)
        }
      }
    )
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r09 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k09 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Bayesian
    observeEvent(input$ai_btn_bayes, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k09()); return() }
      ctx <- tryCatch({
        m <- bayes_model(); req(m)
        paste0("BAYESIAN ANALYSIS RESULTS\n", paste(head(capture.output(print(m)), 50), collapse="\n"))
      }, error=function(e) "Please run Bayesian analysis first.")
      output$ai_output <- renderUI({ .ai_r09(call_gemini(paste0(
        "You are an expert Bayesian statistician writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Bayesian analysis results. Include:\n",
        "1. Report posterior distributions for each parameter: mean, SD, 95% credible interval (HDI)\n",
        "2. Interpret credible intervals — explain the Bayesian vs frequentist CI distinction\n",
        "3. Report Bayes Factors if available: BF10 > 3 moderate evidence, > 10 strong, > 100 decisive\n",
        "4. Assess prior sensitivity — did prior choice substantially affect posteriors?\n",
        "5. Report MCMC diagnostics: R-hat < 1.01 convergence, effective sample size > 1000\n",
        "6. Interpret posterior predictive checks — does the model reproduce observed data?\n",
        "7. Compare to frequentist results: do conclusions agree? Where do they diverge?\n",
        "8. Write APA-style Results paragraph for Bayesian analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Network Analysis
    observeEvent(input$ai_btn_network, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k09()); return() }
      ctx <- tryCatch({
        nd <- network_data(); req(nd)
        paste0("NETWORK ANALYSIS RESULTS\n", paste(head(capture.output(print(nd)), 50), collapse="\n"))
      }, error=function(e) "Please run Network Analysis first.")
      output$ai_output <- renderUI({ .ai_r09(call_gemini(paste0(
        "You are an expert in network analysis and complex systems writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of network analysis results. Include:\n",
        "1. Report basic network properties: nodes, edges, density, average degree\n",
        "2. Identify central nodes by degree centrality, betweenness centrality, and eigenvector centrality\n",
        "3. Assess network clustering: modularity, clustering coefficient, community detection\n",
        "4. Report shortest path length and network diameter\n",
        "5. Identify bridge nodes and their structural importance for information flow\n",
        "6. Assess network resilience: what happens if central nodes are removed?\n",
        "7. Discuss theoretical implications of the network structure for the research context\n",
        "8. Write APA-style Results paragraph for network analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k09()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        bm <- tryCatch(bayes_model(), error=function(e) NULL)
        if (!is.null(bm)) parts <- c(parts, paste0("Bayesian:\n", paste(head(capture.output(print(bm)),30), collapse="\n")))
        nd <- tryCatch(network_data(), error=function(e) NULL)
        if (!is.null(nd)) parts <- c(parts, paste0("Network:\n", paste(head(capture.output(print(nd)),30), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("ADVANCED METHODS SUMMARY\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one advanced analysis first.")
      output$ai_output <- renderUI({ .ai_r09(call_gemini(paste0(
        "You are an expert in advanced statistical methods writing for a top-tier journal.\n\n",
        "Task: Write a COMPREHENSIVE Results section for all advanced analyses. Include:\n",
        "1. Bayesian analysis: posterior estimates, credible intervals, Bayes Factors, convergence\n",
        "2. Network analysis: centrality, clustering, community structure, key actors\n",
        "3. Integration: how do findings from different methods complement each other?\n",
        "4. Methodological contribution of using these advanced approaches\n\n",
        "Use APA 7. Formal academic prose (5-7 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k09()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Interactive visNetwork correlation graph
    output$vis_network <- visNetwork::renderVisNetwork({
      nd <- network_data()
      if (is.null(nd)) {
        return(visNetwork::visNetwork(
          data.frame(id=1, label="Run 'Build Network' first"),
          data.frame(from=integer(0), to=integer(0))
        ))
      }
      g      <- nd$graph
      thresh <- nd$threshold %||% 0.3
      data   <- data_rv()
      vars   <- nd$vars

      # Build nodes data frame
      deg    <- igraph::degree(g)
      nodes_df <- data.frame(
        id    = seq_len(igraph::vcount(g)),
        label = igraph::V(g)$name,
        value = deg + 1,
        title = paste0("<b>", igraph::V(g)$name, "</b><br>Degree: ", deg),
        color = TEAL,
        font  = list(color = "white"),
        stringsAsFactors = FALSE
      )

      # Build edges data frame
      el <- igraph::as_edgelist(g)
      ew <- igraph::E(g)$weight
      edges_df <- data.frame(
        from   = match(el[,1], igraph::V(g)$name),
        to     = match(el[,2], igraph::V(g)$name),
        value  = abs(ew),
        title  = paste0("r = ", round(ew, 3)),
        color  = ifelse(ew > 0, TEAL, "#C0392B"),
        stringsAsFactors = FALSE
      )

      visNetwork::visNetwork(nodes_df, edges_df,
                             main = paste0("Correlation Network (|r| ≥ ", thresh, ")")) |>
        visNetwork::visOptions(highlightNearest=TRUE, nodesIdSelection=TRUE) |>
        visNetwork::visEdges(smooth=TRUE) |>
        visNetwork::visLayout(randomSeed=42) |>
        visNetwork::visPhysics(stabilization=TRUE)
    })

    # Posterior density plots
    output$posterior_densities <- renderPlotly({
      req(bayes_model())
      model <- bayes_model()
      post  <- tryCatch(as.data.frame(model), error=function(e) NULL)
      if (is.null(post)) return(plot_ly() |> layout(title="Run Bayesian Regression first"))

      # Keep only coefficient columns (exclude sigma, log-posterior etc.)
      coef_cols <- names(post)[!grepl("^\\(|sigma|log|lp__", names(post))]
      coef_cols <- coef_cols[seq_len(min(8, length(coef_cols)))]
      if (length(coef_cols) == 0) return(plot_ly() |> layout(title="No posterior samples available"))

      pal <- colorRampPalette(c(TEAL, NAVY, AMBER, GREEN, "#9B59B6"))(length(coef_cols))
      plots <- lapply(seq_along(coef_cols), function(i) {
        col    <- coef_cols[i]
        samps  <- post[[col]]
        ci_lo  <- quantile(samps, 0.025)
        ci_hi  <- quantile(samps, 0.975)
        med    <- median(samps)
        dens   <- density(samps)
        plot_ly(x=dens$x, y=dens$y, type="scatter", mode="lines",
                fill="tozeroy", fillcolor=paste0(pal[i],"44"),
                line=list(color=pal[i],width=2), name=col,
                showlegend=FALSE) |>
          add_segments(x=med,xend=med,y=0,yend=max(dens$y)*1.1,
                       line=list(color=NAVY,dash="dash",width=1.5),
                       name=sprintf("Med=%.3f",med),inherit=FALSE) |>
          layout(xaxis=list(title=col,titlefont=list(size=10)),
                 yaxis=list(title="",showticklabels=FALSE),
                 annotations=list(list(text=sprintf("95%%CI:[%.2f,%.2f]",ci_lo,ci_hi),
                                        x=0.5,y=1.1,xref="paper",yref="paper",
                                        showarrow=FALSE,font=list(size=9,color=NAVY))))
      })
      nr <- ceiling(length(plots)/2)
      do.call(subplot, c(plots, list(nrows=nr, shareX=FALSE, shareY=FALSE, margin=0.06))) |>
        layout(title=list(text="Posterior Density Distributions (median + 95% CI)",
                          font=list(color=NAVY)),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
