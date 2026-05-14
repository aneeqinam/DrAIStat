mod06_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "🔵",
      title = "Clustering Analysis",
      subtitle = "K-Means (Elbow) · Hierarchical (Dendrogram) · Model-Based (GMM/LCA)"
    ),
      uiOutput(ns("global_data_banner")),
  fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod06_server <- function(id, gemini_key = reactive("")) {
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
        title = "Clustering Analysis",
        tabPanel(
          "K-Means",
          fluidRow(
            column(8, pickerInput(ns("kmeans_cols"), "Select Numeric Columns", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, sliderInput(ns("kmeans_k_max"), "Max K to Test", min = 2, max = 10, value = 5))
          ),
          fluidRow(
            column(12, actionButton(ns("kmeans_elbow_run"), "Show Elbow Plot", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Elbow Method"), withSpinner(plotOutput(ns("kmeans_elbow"))))
          ),
          hr(),
          fluidRow(
            column(4, sliderInput(ns("kmeans_final_k"), "Select Final K", min = 2, max = 10, value = 3)),
            column(8, actionButton(ns("kmeans_final_run"), "Run K-Means Clustering", class = "btn-success"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Cluster Sizes"), verbatimTextOutput(ns("kmeans_sizes"))),
            column(6, h5("Silhouette Statistics"), verbatimTextOutput(ns("kmeans_silhouette")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Cluster Assignments"), withSpinner(DT::dataTableOutput(ns("kmeans_assignments"))))
          )
        ),
        tabPanel(
          "Hierarchical",
          fluidRow(
            column(8, pickerInput(ns("hclust_cols"), "Select Numeric Columns", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, selectInput(ns("hclust_method"), "Linkage Method", choices = c("complete", "average", "single", "ward.D2")))
          ),
          fluidRow(
            column(12, actionButton(ns("hclust_run"), "Generate Dendrogram", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Dendrogram"), withSpinner(plotOutput(ns("hclust_dendrogram"), height = "500px")))
          )
        ),
        tabPanel(
          "GMM / Mclust",
          fluidRow(
            column(8, pickerInput(ns("mclust_cols"), "Select Numeric Columns", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(4, actionButton(ns("mclust_run"), "Run Mclust", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("BIC Plot"), withSpinner(plotOutput(ns("mclust_bic"), height = "400px"))),
            column(6, h5("Model Summary"), verbatimTextOutput(ns("mclust_summary")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Cluster Assignments"), withSpinner(DT::dataTableOutput(ns("mclust_assignments"))))
          )
        ),
        tabPanel(
          "Download",
          fluidRow(
            column(12, downloadButton(ns("download_excel"), "Download Clustering Results (Excel)", class = "btn-success"))
          )
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("🔵 PCA Cluster Biplot",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "PCA biplot with cluster membership colours. Run K-Means first."),
          withSpinner(plotlyOutput(ns("pca_biplot"), height="500px"))
        ),
        tabPanel("📊 Silhouette Plot",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Silhouette width per observation — values near 1 indicate tight, well-separated clusters. Run K-Means first."),
          withSpinner(plotlyOutput(ns("silhouette_plot"), height="480px"))
        ),
        tabPanel("🕸️ Cluster Profile",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Standardised cluster centroid radar chart — shows the profile of each cluster. Run K-Means first."),
          withSpinner(plotlyOutput(ns("cluster_profile"), height="460px"))
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
              column(4, actionButton(ns("ai_btn_km"), "🔵 K-Means Clustering",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_hc"), "🌳 Hierarchical Clustering",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_mc"), "📊 Model-Based Clustering",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(12, actionButton(ns("ai_btn_all"), "📋 Full Clustering Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== K-Means: Elbow ====
    kmeans_elbow_result <- reactiveVal(NULL)

    observeEvent(input$kmeans_elbow_run, {
      req(input$kmeans_cols)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Computing elbow plot...", {
          df <- data[, input$kmeans_cols]
          df <- na.omit(df)
          df <- scale(df)

          elbow_result <- factoextra::fviz_nbclust(df, kmeans, method = "wss", k.max = input$kmeans_k_max)

          kmeans_elbow_result(elbow_result)
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$kmeans_elbow <- renderPlot({
      req(kmeans_elbow_result())
      print(kmeans_elbow_result())
    })

    # ==== K-Means: Final Model ====
    kmeans_final <- reactiveVal(NULL)

    observeEvent(input$kmeans_final_run, {
      req(input$kmeans_cols)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running K-Means...", {
          df <- data[, input$kmeans_cols]
          df <- na.omit(df)
          df_scaled <- scale(df)

          model <- kmeans(df_scaled, centers = input$kmeans_final_k, nstart = 25)

          # Validate silhouette requirements: 2 ≤ k < n
          k_val <- length(unique(model$cluster))
          n_cases <- nrow(df_scaled)
          if (k_val < 2 || k_val >= n_cases) {
            showNotification("Silhouette requires 2 ≤ k < n cases. Adjust k.", type = "warning")
            sil <- NULL
          } else {
            sil <- cluster::silhouette(model$cluster, dist(df_scaled))
          }

          kmeans_final(list(
            model = model,
            silhouette = sil,
            data = data[rownames(df_scaled), ],
            cluster_assignment = model$cluster
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$kmeans_sizes <- renderText({
      req(kmeans_final())
      res <- kmeans_final()
      model <- res$model

      paste0(
        "Cluster Sizes:\n\n",
        paste(paste0("Cluster ", 1:length(table(model$cluster)), ": ", as.numeric(table(model$cluster)), " observations"), collapse = "\n"),
        "\n\nWithin SS by cluster:\n",
        paste(round(model$withinss, 1), collapse = ", ")
      )
    })

    output$kmeans_silhouette <- renderText({
      req(kmeans_final())
      res <- kmeans_final()
      sil <- res$silhouette

      avg_sil <- mean(sil[, 3])

      paste0(
        "Average Silhouette Width = ", round(avg_sil, 3), "\n\n",
        "Interpretation:\n",
        if (avg_sil < 0.25) "Weak structure (poorly separated clusters)\n"
        else if (avg_sil < 0.5) "Reasonable structure\n"
        else if (avg_sil < 0.75) "Strong structure\n"
        else "Very strong structure (well-separated clusters)\n",
        "\nBy cluster:\n",
        paste(
          paste0(
            "Cluster ", unique(sil[, 1]), ": ",
            round(sapply(unique(sil[, 1]), function(c) mean(sil[sil[, 1] == c, 3])), 3)
          ),
          collapse = "\n"
        )
      )
    })

    output$kmeans_assignments <- DT::renderDataTable({
      req(kmeans_final())
      res <- kmeans_final()

      assign_df <- data.frame(
        ID = 1:length(res$cluster_assignment),
        Cluster = res$cluster_assignment
      )

      tool_dt(assign_df, "K-Means Cluster Assignments")
    })

    # ==== Hierarchical Clustering ====
    hclust_result <- reactiveVal(NULL)

    observeEvent(input$hclust_run, {
      req(input$hclust_cols)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Computing hierarchical clustering...", {
          df <- data[, input$hclust_cols]
          df <- na.omit(df)
          df_scaled <- scale(df)

          dist_matrix <- dist(df_scaled)
          hclust_obj <- hclust(dist_matrix, method = input$hclust_method)

          hclust_result(hclust_obj)
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$hclust_dendrogram <- renderPlot({
      req(hclust_result())
      obj <- hclust_result()
      plot(obj, main = paste0("Dendrogram (", input$hclust_method, " linkage)"), xlab = "Observation", ylab = "Distance")
    })

    # ==== Mclust (GMM) ====
    mclust_result <- reactiveVal(NULL)

    observeEvent(input$mclust_run, {
      req(input$mclust_cols)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Running Mclust...", {
          df <- data[, input$mclust_cols]
          df <- na.omit(df)

          model <- mclust::Mclust(df)

          mclust_result(list(
            model = model,
            data = data[rownames(df), ],
            cluster_assignment = model$classification
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$mclust_bic <- renderPlot({
      req(mclust_result())
      res <- mclust_result()
      model <- res$model

      plot(model, what = "BIC", main = "BIC Values by Model and Number of Clusters")
    })

    output$mclust_summary <- renderText({
      req(mclust_result())
      res <- mclust_result()
      model <- res$model

      paste0(
        "Model: ", model$modelName, "\n",
        "Number of Clusters: ", model$G, "\n",
        "BIC: ", round(model$bic, 2), "\n",
        "Log-likelihood: ", round(model$loglik, 2), "\n\n",
        "Cluster Sizes:\n",
        paste(paste0("Cluster ", 1:model$G, ": ", model$n, " (", round(100*model$n/sum(model$n), 1), "%)"), collapse = "\n")
      )
    })

    output$mclust_assignments <- DT::renderDataTable({
      req(mclust_result())
      res <- mclust_result()

      assign_df <- data.frame(
        ID = 1:length(res$cluster_assignment),
        Cluster = res$cluster_assignment
      )

      tool_dt(assign_df, "Mclust Cluster Assignments")
    })

    # ==== Download ====
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("clustering_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sheets_list <- list()

        if (!is.null(kmeans_final())) {
          sheets_list[["K-Means"]] <- data.frame(
            ID = 1:length(kmeans_final()$cluster_assignment),
            Cluster = kmeans_final()$cluster_assignment
          )
        }

        if (!is.null(mclust_result())) {
          sheets_list[["Mclust"]] <- data.frame(
            ID = 1:length(mclust_result()$cluster_assignment),
            Cluster = mclust_result()$cluster_assignment
          )
        }

        if (length(sheets_list) > 0) {
          writexl::write_xlsx(sheets_list, file)
        }
      }
    )
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r06 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k06 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. K-Means
    observeEvent(input$ai_btn_km, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k06()); return() }
      ctx <- tryCatch({
        km <- kmeans_final(); req(km)
        paste0("K-MEANS CLUSTERING RESULTS\nK = ", length(km$centers[,1]),
               "\nTotal within-cluster SS: ", round(km$tot.withinss, 2),
               "\nBetween-cluster SS: ", round(km$betweenss, 2),
               "\nRatio (between/total): ", round(km$betweenss/km$totss*100, 1), "%",
               "\n\nCluster sizes: ", paste(km$size, collapse=", "),
               "\n\nCluster centers:\n", paste(capture.output(print(round(km$centers, 3))), collapse="\n"))
      }, error=function(e) "Please run K-Means first.")
      output$ai_output <- renderUI({ .ai_r06(call_gemini(paste0(
        "You are an expert in cluster analysis writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of K-Means clustering results. Include:\n",
        "1. Justify the number of clusters selected (elbow method, silhouette, gap statistic)\n",
        "2. Report within-cluster SS and between-cluster SS ratio — higher = better separation\n",
        "3. Characterise each cluster based on its centroid profile — name/label each cluster\n",
        "4. Report cluster sizes and discuss imbalance implications\n",
        "5. Evaluate silhouette width (> .50 reasonable, > .70 strong) for cluster validity\n",
        "6. Discuss practical meaning of each cluster for the research context\n",
        "7. Note K-Means assumptions: spherical clusters, equal variance, Euclidean distance\n",
        "8. Write APA-style Results paragraph for cluster analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Hierarchical Clustering
    observeEvent(input$ai_btn_hc, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k06()); return() }
      ctx <- tryCatch({
        hc <- hclust_result(); req(hc)
        paste0("HIERARCHICAL CLUSTERING RESULTS\n",
               "Method: ", hc$method, "\nDistance: ", hc$dist.method,
               "\nMerge heights (last 10):\n",
               paste(round(tail(hc$height, 10), 3), collapse=", "))
      }, error=function(e) "Please run Hierarchical Clustering first.")
      output$ai_output <- renderUI({ .ai_r06(call_gemini(paste0(
        "You are an expert in cluster analysis writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Hierarchical Clustering results. Include:\n",
        "1. Justify the linkage method used (Ward, complete, average) and distance metric\n",
        "2. Interpret the dendrogram — identify natural clusters from major height jumps\n",
        "3. Report the cophenetic correlation coefficient as a measure of dendrogram fit\n",
        "4. Compare to K-Means if available — do both solutions agree?\n",
        "5. Discuss agglomerative vs divisive hierarchical approaches\n",
        "6. Describe what the cluster hierarchy reveals about data structure\n",
        "7. Write APA-style Results paragraph\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Model-Based
    observeEvent(input$ai_btn_mc, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k06()); return() }
      ctx <- tryCatch({
        mc <- mclust_result(); req(mc)
        paste0("MODEL-BASED CLUSTERING (GMM) RESULTS\n",
               "Best model: ", mc$modelName, " | G = ", mc$G,
               "\nBIC: ", round(mc$bic, 2),
               "\n\nClassification table:\n", paste(capture.output(print(table(mc$classification))), collapse="\n"))
      }, error=function(e) "Please run Model-Based Clustering first.")
      output$ai_output <- renderUI({ .ai_r06(call_gemini(paste0(
        "You are an expert in Gaussian Mixture Models writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of model-based clustering (GMM/mclust) results. Include:\n",
        "1. Explain what GMM assumes (multivariate Gaussian components, probabilistic assignment)\n",
        "2. Report BIC and how it was used to select the optimal number of components and covariance structure\n",
        "3. Interpret the mclust model name (e.g., VVV = unconstrained, EEE = equal covariance)\n",
        "4. Report component means, sizes, and mixing proportions\n",
        "5. Discuss uncertainty in cluster assignments (soft vs hard assignment)\n",
        "6. Compare GMM to K-Means in terms of flexibility and fit\n",
        "7. Write APA-style Results paragraph\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k06()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        km <- tryCatch(kmeans_final(), error=function(e) NULL)
        if (!is.null(km)) parts <- c(parts, paste0("K-Means (k=", length(km$size), "):\nBetween/Total=",
          round(km$betweenss/km$totss*100,1), "%\nCluster sizes: ", paste(km$size, collapse=", ")))
        mc <- tryCatch(mclust_result(), error=function(e) NULL)
        if (!is.null(mc)) parts <- c(parts, paste0("GMM: ", mc$modelName, " G=", mc$G, " BIC=", round(mc$bic,2)))
        if (length(parts)==0) stop("no results")
        paste0("CLUSTERING FULL SUMMARY\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one clustering method first.")
      output$ai_output <- renderUI({ .ai_r06(call_gemini(paste0(
        "You are an expert in multivariate cluster analysis writing for a top-tier journal.\n\n",
        "Task: Write a COMPREHENSIVE clustering Results section. Include:\n",
        "1. Data preparation: standardisation, feature selection, distance metric justification\n",
        "2. K-Means results: optimal k, cluster profiles, silhouette scores\n",
        "3. Hierarchical clustering: dendrogram interpretation, linkage method, recommended k\n",
        "4. GMM results: BIC-optimal model, component structure, uncertainty\n",
        "5. Cross-method agreement: do different methods converge on similar solutions?\n",
        "6. Cluster interpretation: name and profile each cluster theoretically\n\n",
        "Use APA 7. Formal academic prose (6-8 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k06()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # PCA Cluster Biplot
    output$pca_biplot <- renderPlotly({
      if (is.null(kmeans_final())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(kmeans_final())
      res    <- kmeans_final()
      df_sc  <- res$scaled_data
      if (is.null(df_sc)) return(plot_ly() |> layout(title="No scaled data available"))
      pca    <- prcomp(df_sc, scale. = FALSE)
      pc_df  <- as.data.frame(pca$x[, 1:min(2, ncol(pca$x))])
      colnames(pc_df) <- c("PC1","PC2")[seq_len(ncol(pc_df))]
      pc_df$Cluster   <- factor(res$model$cluster)
      var1 <- round(summary(pca)$importance[2,1]*100,1)
      var2 <- if (ncol(pca$x)>=2) round(summary(pca)$importance[2,2]*100,1) else 0
      pal  <- c(TEAL, NAVY, AMBER, GREEN, "#9B59B6","#E74C3C","#F39C12","#1ABC9C")
      plot_ly(pc_df, x=~PC1, y=~PC2, color=~Cluster,
              type="scatter", mode="markers",
              colors=pal[seq_len(nlevels(pc_df$Cluster))],
              marker=list(size=7, opacity=0.75),
              text=~paste("Cluster:", Cluster),
              hoverinfo="text") |>
        layout(title=list(text="K-Means Clusters — PCA Biplot",font=list(color=NAVY)),
               xaxis=list(title=sprintf("PC1 (%.1f%% var)",var1)),
               yaxis=list(title=sprintf("PC2 (%.1f%% var)",var2)),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # Silhouette Plot
    output$silhouette_plot <- renderPlotly({
      if (is.null(kmeans_final())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(kmeans_final())
      res   <- kmeans_final()
      df_sc <- res$scaled_data
      if (is.null(df_sc)) return(plot_ly() |> layout(title="No data"))
      # Use cached silhouette if available, otherwise recalculate with guards
      sil <- res$silhouette
      if (is.null(sil)) {
        k     <- res$model$cluster
        dist_m <- dist(df_sc)
        k_val <- length(unique(k))
        n_cases <- nrow(df_sc)
        if (k_val < 2 || k_val >= n_cases) {
          return(plot_ly() |> layout(title="Cannot compute silhouette: invalid k"))
        }
        sil <- cluster::silhouette(k, dist_m)
      }
      sil_df <- as.data.frame(sil[, c("cluster","sil_width")])
      sil_df$obs     <- seq_len(nrow(sil_df))
      sil_df$Cluster <- factor(sil_df$cluster)
      sil_df         <- sil_df[order(sil_df$cluster, -sil_df$sil_width), ]
      sil_df$obs_ord <- seq_len(nrow(sil_df))
      pal <- c(TEAL,NAVY,AMBER,GREEN,"#9B59B6","#E74C3C")
      plot_ly(sil_df, x=~sil_width, y=~obs_ord,
              color=~Cluster, colors=pal[seq_len(nlevels(sil_df$Cluster))],
              type="bar", orientation="h",
              hovertemplate="Silhouette: %{x:.3f}<extra>Obs %{y}</extra>") |>
        add_segments(x=mean(sil_df$sil_width), xend=mean(sil_df$sil_width),
                     y=0, yend=nrow(sil_df)+1,
                     line=list(color="#C0392B",dash="dash",width=2),
                     name=sprintf("Mean=%.3f",mean(sil_df$sil_width)),inherit=FALSE) |>
        layout(title=list(text="Silhouette Width by Observation",font=list(color=NAVY)),
               xaxis=list(title="Silhouette Width"),
               yaxis=list(title="Observation",showticklabels=FALSE),
               bargap=0, plot_bgcolor="white", paper_bgcolor="white")
    })

    # Cluster Profile Radar
    output$cluster_profile <- renderPlotly({
      if (is.null(kmeans_final())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      req(kmeans_final())
      res    <- kmeans_final()
      df_sc  <- res$scaled_data
      model  <- res$model
      if (is.null(df_sc)) return(plot_ly() |> layout(title="No data"))
      centers <- as.data.frame(model$centers)
      vars    <- colnames(centers)
      pal     <- c(TEAL,NAVY,AMBER,GREEN,"#9B59B6","#E74C3C")
      p <- plot_ly(type="scatterpolar", fill="toself")
      for (i in seq_len(nrow(centers))) {
        vals <- as.numeric(centers[i, ])
        vals <- c(vals, vals[1])
        p <- add_trace(p, r=vals, theta=c(vars, vars[1]),
                       name=paste("Cluster", i),
                       line=list(color=pal[(i-1)%%length(pal)+1]),
                       fillcolor=paste0(pal[(i-1)%%length(pal)+1],"33"))
      }
      p |> layout(polar=list(radialaxis=list(visible=TRUE)),
                  title=list(text="Cluster Centroid Profile (Standardised)",font=list(color=NAVY)),
                  paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
