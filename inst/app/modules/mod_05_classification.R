mod05_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html(
      icon = "🤖",
      title = "ML Classification",
      subtitle = "Decision Tree · Random Forest · SVM · AUC · Confusion Matrix · Feature Importance"
    ),
    uiOutput(ns("global_data_banner")),
    fluidRow(
      column(3, fileInput(ns("file"), "Upload CSV/Excel", accept = c(".csv", ".xlsx"))),
      column(9, uiOutput(ns("file_info")))
    ),
    uiOutput(ns("main_ui")),
  )
}

mod05_server <- function(id, gemini_key = reactive("")) {
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

    num_cols <- reactive({
      data <- data_rv()
      req(data)
      numeric_cols(data)
    })

    cat_cols <- reactive({
      data <- data_rv()
      req(data)
      names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
    })

    output$main_ui <- renderUI({
      req(data_rv())
      tabBox(
        width = 12,
        title = "Classification Models",
        tabPanel(
          "Decision Tree",
          fluidRow(
            column(4, pickerInput(ns("dt_target"), "Target Variable (Binary/Multi-class)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("dt_features"), "Features (Numeric)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(4, sliderInput(ns("dt_split"), "Train/Test Split (%)", min = 50, max = 90, value = 70)),
            column(8, actionButton(ns("dt_run"), "Train Decision Tree", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Confusion Matrix"), verbatimTextOutput(ns("dt_cm"))),
            column(6, h5("Performance Metrics"), verbatimTextOutput(ns("dt_metrics")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Feature Importance"), withSpinner(plotlyOutput(ns("dt_importance"))))
          )
        ),
        tabPanel(
          "Random Forest",
          fluidRow(
            column(4, pickerInput(ns("rf_target"), "Target Variable (Binary/Multi-class)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("rf_features"), "Features (Numeric)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(4, sliderInput(ns("rf_split"), "Train/Test Split (%)", min = 50, max = 90, value = 70)),
            column(8, actionButton(ns("rf_run"), "Train Random Forest", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Confusion Matrix"), verbatimTextOutput(ns("rf_cm"))),
            column(6, h5("Performance Metrics"), verbatimTextOutput(ns("rf_metrics")))
          ),
          hr(),
          fluidRow(
            column(12, h5("Feature Importance"), withSpinner(plotlyOutput(ns("rf_importance"))))
          )
        ),
        tabPanel(
          "SVM",
          fluidRow(
            column(4, pickerInput(ns("svm_target"), "Target Variable (Binary/Multi-class)", choices = cat_cols(), options = list(`actions-box` = TRUE))),
            column(8, pickerInput(ns("svm_features"), "Features (Numeric)", choices = num_cols(), multiple = TRUE, options = list(`actions-box` = TRUE)))
          ),
          fluidRow(
            column(4, sliderInput(ns("svm_split"), "Train/Test Split (%)", min = 50, max = 90, value = 70)),
            column(8, actionButton(ns("svm_run"), "Train SVM", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(6, h5("Confusion Matrix"), verbatimTextOutput(ns("svm_cm"))),
            column(6, h5("Performance Metrics"), verbatimTextOutput(ns("svm_metrics")))
          )
        ),
        tabPanel(
          "Compare Models",
          fluidRow(
            column(12, actionButton(ns("compare_run"), "Compare All Models", class = "btn-primary"))
          ),
          hr(),
          fluidRow(
            column(12, h5("Model Comparison"), withSpinner(DT::dataTableOutput(ns("compare_table"))))
          ),
          hr(),
          fluidRow(
            column(12, h5("AUC Comparison"), withSpinner(plotlyOutput(ns("compare_plot"))))
          )
        ),
        tabPanel(
          "Download",
          fluidRow(
            column(12, downloadButton(ns("download_excel"), "Download Results (Excel)", class = "btn-success"))
          )
        ),
        # ── NEW: Advanced Visualisations ────────────────────────────────
        tabPanel("📈 ROC Curves",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Interactive AUC-ROC curves for all trained models. Train models first (binary classification only)."),
          withSpinner(plotlyOutput(ns("roc_curves"), height="480px")),
          hr(),
          tags$h5("AUC with 95% DeLong Confidence Intervals"),
          withSpinner(DT::dataTableOutput(ns("auc_ci_tbl")))
        ),
        tabPanel("🟦 Confusion Heatmaps",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Colour-coded confusion matrix heatmaps for all trained models."),
          withSpinner(plotlyOutput(ns("conf_heatmaps"), height="480px"))
        ),
        tabPanel("🕸️ Model Comparison Radar",
          tags$p(style="color:#555;font-size:.85rem;padding:.4rem 0;",
            "Spider/radar chart comparing Accuracy, Sensitivity, Specificity, and AUC across all trained models."),
          withSpinner(plotlyOutput(ns("model_radar"), height="460px"))
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
              column(4, actionButton(ns("ai_btn_dt"), "🌳 Decision Tree",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_rf"), "🌲 Random Forest",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_svm"), "🤖 SVM Classifier",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(6, actionButton(ns("ai_btn_cmp"), "📊 Model Comparison",
                                     class="btn-warning btn-block", style="margin-bottom:.5rem;")),
              column(6, actionButton(ns("ai_btn_all"), "📋 Full ML Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        )
      )
    })

    # ==== Helper: Calculate metrics ====
    calc_metrics <- function(cm) {
      if (length(dim(cm)) == 2 && nrow(cm) == 2) {
        # Binary classification
        TP <- cm[2, 2]
        TN <- cm[1, 1]
        FP <- cm[1, 2]
        FN <- cm[2, 1]

        accuracy <- (TP + TN) / sum(cm)
        sensitivity <- TP / (TP + FN)
        specificity <- TN / (TN + FP)
        precision <- TP / (TP + FP)

        return(list(
          accuracy = accuracy,
          sensitivity = sensitivity,
          specificity = specificity,
          precision = precision
        ))
      } else {
        # Multi-class
        accuracy <- sum(diag(cm)) / sum(cm)
        return(list(accuracy = accuracy))
      }
    }

    # ==== Decision Tree ====
    dt_model <- reactiveVal(NULL)
    dt_data_split <- reactiveVal(NULL)

    observeEvent(input$dt_run, {
      req(input$dt_target, input$dt_features)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Training Decision Tree...", {
          # Prepare data
          df <- data[, c(input$dt_target, input$dt_features)]
          df <- na.omit(df)
          df[[input$dt_target]] <- factor(df[[input$dt_target]])

          # Train/test split
          set.seed(42)
          split_idx <- sample(1:nrow(df), size = floor(nrow(df) * input$dt_split / 100))
          train_data <- df[split_idx, ]
          test_data <- df[-split_idx, ]

          # Train model
          formula <- as.formula(paste0(input$dt_target, " ~ ."))
          model <- rpart::rpart(formula, data = train_data)

          # Predictions
          pred <- predict(model, test_data, type = "class")

          dt_model(list(
            model = model,
            pred = pred,
            actual = test_data[[input$dt_target]],
            features = input$dt_features
          ))
          dt_data_split(list(train = train_data, test = test_data))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$dt_cm <- renderText({
      req(dt_model())
      res <- dt_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)

      paste0(
        "Confusion Matrix:\n\n",
        paste(capture.output(print(cm)), collapse = "\n")
      )
    })

    output$dt_metrics <- renderText({
      req(dt_model())
      res <- dt_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)
      metrics <- calc_metrics(cm)

      paste0(
        "Accuracy = ", round(metrics$accuracy, 3), "\n",
        if (!is.null(metrics$sensitivity)) paste0("Sensitivity = ", round(metrics$sensitivity, 3), "\n") else "",
        if (!is.null(metrics$specificity)) paste0("Specificity = ", round(metrics$specificity, 3), "\n") else "",
        if (!is.null(metrics$precision)) paste0("Precision = ", round(metrics$precision, 3)) else ""
      )
    })

    output$dt_importance <- renderPlotly({
      req(dt_model())
      res <- dt_model()
      model <- res$model

      importance_df <- data.frame(
        Feature = rownames(model$variable.importance),
        Importance = as.numeric(model$variable.importance)
      )
      importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

      plot_ly(importance_df, x = ~Importance, y = ~Feature, type = "bar", orientation = "h", marker = list(color = TEAL)) %>%
        layout(
          title = "Decision Tree Feature Importance",
          xaxis = list(title = "Importance"),
          yaxis = list(title = "Feature")
        )
    })

    # ==== Random Forest ====
    rf_model <- reactiveVal(NULL)

    observeEvent(input$rf_run, {
      req(input$rf_target, input$rf_features)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Training Random Forest...", {
          # Prepare data
          df <- data[, c(input$rf_target, input$rf_features)]
          df <- na.omit(df)
          df[[input$rf_target]] <- factor(df[[input$rf_target]])

          # Train/test split
          set.seed(42)
          split_idx <- sample(1:nrow(df), size = floor(nrow(df) * input$rf_split / 100))
          train_data <- df[split_idx, ]
          test_data <- df[-split_idx, ]

          # Train model
          formula <- as.formula(paste0(input$rf_target, " ~ ."))
          model <- randomForest::randomForest(formula, data = train_data, ntree = 100)

          # Predictions
          pred <- predict(model, test_data)

          rf_model(list(
            model = model,
            pred = pred,
            actual = test_data[[input$rf_target]],
            features = input$rf_features
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$rf_cm <- renderText({
      req(rf_model())
      res <- rf_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)

      paste0(
        "Confusion Matrix:\n\n",
        paste(capture.output(print(cm)), collapse = "\n")
      )
    })

    output$rf_metrics <- renderText({
      req(rf_model())
      res <- rf_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)
      metrics <- calc_metrics(cm)

      paste0(
        "Accuracy = ", round(metrics$accuracy, 3), "\n",
        if (!is.null(metrics$sensitivity)) paste0("Sensitivity = ", round(metrics$sensitivity, 3), "\n") else "",
        if (!is.null(metrics$specificity)) paste0("Specificity = ", round(metrics$specificity, 3), "\n") else "",
        if (!is.null(metrics$precision)) paste0("Precision = ", round(metrics$precision, 3)) else ""
      )
    })

    output$rf_importance <- renderPlotly({
      req(rf_model())
      res <- rf_model()
      model <- res$model

      importance_df <- data.frame(
        Feature = rownames(model$importance),
        Importance = as.numeric(model$importance[, 1])
      )
      importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

      plot_ly(importance_df, x = ~Importance, y = ~Feature, type = "bar", orientation = "h", marker = list(color = GREEN)) %>%
        layout(
          title = "Random Forest Feature Importance",
          xaxis = list(title = "Importance (Mean Decrease in Gini)"),
          yaxis = list(title = "Feature")
        )
    })

    # ==== SVM ====
    svm_model <- reactiveVal(NULL)

    observeEvent(input$svm_run, {
      req(input$svm_target, input$svm_features)
      data <- data_rv()
      req(data)

      tryCatch({
        withProgress(message = "Training SVM...", {
          # Prepare data
          df <- data[, c(input$svm_target, input$svm_features)]
          df <- na.omit(df)
          df[[input$svm_target]] <- factor(df[[input$svm_target]])

          # Train/test split
          set.seed(42)
          split_idx <- sample(1:nrow(df), size = floor(nrow(df) * input$svm_split / 100))
          train_data <- df[split_idx, ]
          test_data <- df[-split_idx, ]

          # Train model
          formula <- as.formula(paste0(input$svm_target, " ~ ."))
          model <- e1071::svm(formula, data = train_data, kernel = "radial", probability = TRUE)

          # Predictions
          pred <- predict(model, test_data)

          svm_model(list(
            model = model,
            pred = pred,
            actual = test_data[[input$svm_target]],
            features = input$svm_features
          ))
        })
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })

    output$svm_cm <- renderText({
      req(svm_model())
      res <- svm_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)

      paste0(
        "Confusion Matrix:\n\n",
        paste(capture.output(print(cm)), collapse = "\n")
      )
    })

    output$svm_metrics <- renderText({
      req(svm_model())
      res <- svm_model()
      cm <- table(Predicted = res$pred, Actual = res$actual)
      metrics <- calc_metrics(cm)

      paste0(
        "Accuracy = ", round(metrics$accuracy, 3), "\n",
        if (!is.null(metrics$sensitivity)) paste0("Sensitivity = ", round(metrics$sensitivity, 3), "\n") else "",
        if (!is.null(metrics$specificity)) paste0("Specificity = ", round(metrics$specificity, 3), "\n") else "",
        if (!is.null(metrics$precision)) paste0("Precision = ", round(metrics$precision, 3)) else ""
      )
    })

    # ==== Model Comparison ====
    comparison_results <- reactiveVal(NULL)

    observeEvent(input$compare_run, {
      req(dt_model(), rf_model(), svm_model())

      dt_res <- dt_model()
      rf_res <- rf_model()
      svm_res <- svm_model()

      dt_cm <- table(Predicted = dt_res$pred, Actual = dt_res$actual)
      rf_cm <- table(Predicted = rf_res$pred, Actual = rf_res$actual)
      svm_cm <- table(Predicted = svm_res$pred, Actual = svm_res$actual)

      dt_metrics <- calc_metrics(dt_cm)
      rf_metrics <- calc_metrics(rf_cm)
      svm_metrics <- calc_metrics(svm_cm)

      comparison_results(data.frame(
        Model = c("Decision Tree", "Random Forest", "SVM"),
        Accuracy = c(dt_metrics$accuracy, rf_metrics$accuracy, svm_metrics$accuracy),
        Sensitivity = c(
          if (!is.null(dt_metrics$sensitivity)) dt_metrics$sensitivity else NA,
          if (!is.null(rf_metrics$sensitivity)) rf_metrics$sensitivity else NA,
          if (!is.null(svm_metrics$sensitivity)) svm_metrics$sensitivity else NA
        ),
        Specificity = c(
          if (!is.null(dt_metrics$specificity)) dt_metrics$specificity else NA,
          if (!is.null(rf_metrics$specificity)) rf_metrics$specificity else NA,
          if (!is.null(svm_metrics$specificity)) svm_metrics$specificity else NA
        )
      ))
    })

    output$compare_table <- DT::renderDataTable({
      req(comparison_results())
      tool_dt(comparison_results(), "Model Performance Comparison")
    })

    output$compare_plot <- renderPlotly({
      req(comparison_results())
      comp_df <- comparison_results()

      plot_ly(comp_df, x = ~Model, y = ~Accuracy, type = "bar", marker = list(color = NAVY), name = "Accuracy") %>%
        layout(
          title = "Model Accuracy Comparison",
          yaxis = list(title = "Accuracy", range = c(0, 1)),
          xaxis = list(title = "Model")
        )
    })

    # ==== Download ====
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("classification_results_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        req(comparison_results())
        writexl::write_xlsx(list(
          "Model Comparison" = comparison_results()
        ), file)
      }
    )
    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r05 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k05 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Decision Tree
    observeEvent(input$ai_btn_dt, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      ctx <- tryCatch({
        m <- dt_model(); req(m)
        ds <- dt_data_split(); req(ds)
        pred <- predict(m, ds$test, type="class")
        cm   <- table(Predicted=pred, Actual=ds$test[[input$target_var]])
        paste0("DECISION TREE RESULTS\nTarget: ", input$target_var,
               "\n\nConfusion Matrix:\n", paste(capture.output(print(cm)), collapse="\n"),
               "\n\nModel summary:\n", paste(head(capture.output(print(m)), 30), collapse="\n"))
      }, error=function(e) "Please run Decision Tree first.")
      output$ai_output <- renderUI({ .ai_r05(call_gemini(paste0(
        "You are an expert in machine learning and classification analysis writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Decision Tree classifier results. Include:\n",
        "1. Report overall accuracy, precision, recall, F1-score from the confusion matrix\n",
        "2. Calculate and interpret AUC-ROC (> .90 excellent, .80-.90 good, .70-.80 acceptable)\n",
        "3. Identify the most important splitting features and their decision thresholds\n",
        "4. Explain the tree depth, number of leaves, and complexity control parameters\n",
        "5. Discuss overfitting risk and cross-validation results\n",
        "6. Compare performance across classes — identify misclassification patterns\n",
        "7. Discuss interpretability advantage of decision trees vs black-box models\n",
        "8. Write APA-style Methods + Results paragraphs for ML classification\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Random Forest
    observeEvent(input$ai_btn_rf, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      ctx <- tryCatch({
        m <- rf_model(); req(m)
        imp <- importance(m)
        paste0("RANDOM FOREST RESULTS\nTarget: ", input$target_var,
               "\nTrees: ", m$ntree, " | mtry: ", m$mtry,
               "\n\nOOB Error: ", round(m$err.rate[nrow(m$err.rate), "OOB"]*100, 2), "%",
               "\n\nVariable Importance (MeanDecreaseGini):\n",
               paste(capture.output(print(round(imp, 4))), collapse="\n"))
      }, error=function(e) "Please run Random Forest first.")
      output$ai_output <- renderUI({ .ai_r05(call_gemini(paste0(
        "You are an expert in ensemble machine learning methods writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Random Forest results. Include:\n",
        "1. Report Out-of-Bag (OOB) error rate and what it means for generalisability\n",
        "2. Report overall accuracy, precision, recall, F1 from confusion matrix\n",
        "3. Interpret variable importance (MeanDecreaseGini / MeanDecreaseAccuracy) — rank top predictors\n",
        "4. Explain hyperparameters: ntrees, mtry, node size — justify choices\n",
        "5. Discuss advantages over single decision trees: variance reduction, ensemble averaging\n",
        "6. Compare OOB performance to test set performance — check for overfitting\n",
        "7. Discuss partial dependence plots interpretation if available\n",
        "8. Write academic Methods + Results paragraph for RF classification\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. SVM
    observeEvent(input$ai_btn_svm, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      ctx <- tryCatch({
        m <- svm_model(); req(m)
        paste0("SVM RESULTS\nTarget: ", input$target_var,
               "\nKernel: ", m$kernel, " | Cost: ", m$cost,
               "\n\nModel summary:\n", paste(capture.output(print(m)), collapse="\n"))
      }, error=function(e) "Please run SVM first.")
      output$ai_output <- renderUI({ .ai_r05(call_gemini(paste0(
        "You are an expert in support vector machines writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of SVM classifier results. Include:\n",
        "1. Report kernel type (linear/RBF/polynomial) and justify its selection\n",
        "2. Explain cost parameter C — balance between margin maximisation and misclassification\n",
        "3. Report accuracy, precision, recall, F1, AUC-ROC\n",
        "4. Discuss support vectors: how many, what they represent geometrically\n",
        "5. Explain class separability from the margin width perspective\n",
        "6. Discuss SVM's strengths (high-dimensional data, robust to outliers) and limitations\n",
        "7. Compare to other classifiers in terms of performance and interpretability\n",
        "8. Write APA-style Results paragraph for SVM classification\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Model Comparison
    observeEvent(input$ai_btn_cmp, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      ctx <- tryCatch({
        res <- comparison_results(); req(res)
        paste0("MODEL COMPARISON RESULTS\n", paste(capture.output(print(res)), collapse="\n"))
      }, error=function(e) "Please run Model Comparison first.")
      output$ai_output <- renderUI({ .ai_r05(call_gemini(paste0(
        "You are an expert in machine learning model evaluation writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED comparison of ML classifier models. Include:\n",
        "1. Compare accuracy, precision, recall, F1, AUC-ROC across all models in a table format\n",
        "2. Identify the best-performing model and explain why it outperforms others\n",
        "3. Discuss bias-variance tradeoff for each model type\n",
        "4. Explain which metric matters most for this problem (imbalanced classes → F1/AUC; balanced → accuracy)\n",
        "5. Report statistical significance of performance differences if available\n",
        "6. Recommend the final model with justification\n",
        "7. Discuss generalisability and deployment considerations\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 5. Full Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      ctx <- tryCatch({
        parts <- character(0)
        dt <- tryCatch(dt_model(), error=function(e) NULL)
        if (!is.null(dt)) parts <- c(parts, paste0("Decision Tree OOB:\n", paste(head(capture.output(print(dt)),15), collapse="\n")))
        rf <- tryCatch(rf_model(), error=function(e) NULL)
        if (!is.null(rf)) parts <- c(parts, paste0("Random Forest OOB Error: ", round(rf$err.rate[nrow(rf$err.rate),"OOB"]*100,2), "%"))
        cmp <- tryCatch(comparison_results(), error=function(e) NULL)
        if (!is.null(cmp)) parts <- c(parts, paste0("Model Comparison:\n", paste(capture.output(print(cmp)), collapse="\n")))
        if (length(parts)==0) stop("no results")
        paste0("ML CLASSIFICATION FULL SUMMARY\n", paste(parts, collapse="\n\n"))
      }, error=function(e) "Please run at least one classifier first.")
      output$ai_output <- renderUI({ .ai_r05(call_gemini(paste0(
        "You are an expert in machine learning writing for a top-tier management/IS journal.\n\n",
        "Task: Write a COMPREHENSIVE Results section for all ML classification analyses. Include:\n",
        "1. Dataset description: features, target variable, train/test split, class balance\n",
        "2. Decision Tree: accuracy, feature importance, tree structure\n",
        "3. Random Forest: OOB error, variable importance, ensemble performance\n",
        "4. SVM: kernel, accuracy, support vector interpretation\n",
        "5. Model comparison: head-to-head performance table, best model recommendation\n",
        "6. Generalisability and overfitting assessment\n\n",
        "Use APA 7 for reporting. Formal academic prose (6-8 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k05()); return() }
      NULL
    }, ignoreInit = TRUE)

    # ══ NEW ADVANCED VISUALIZATIONS ══════════════════════════════════════

    # Helper: safe ROC from model + test data (binary only)
    get_roc <- function(model, test_data, target_col) {
      tryCatch({
        probs <- predict(model, newdata = test_data, type = "prob")
        if (is.null(probs) || ncol(probs) < 2) return(NULL)
        lvls  <- levels(test_data[[target_col]])
        pos   <- lvls[2]
        pROC::roc(test_data[[target_col]], probs[, pos], quiet = TRUE)
      }, error = function(e) NULL)
    }

    # Helper: compute AUC with DeLong CI
    get_auc_ci <- function(roc_obj) {
      tryCatch({
        ci_obj <- pROC::ci.auc(roc_obj, method = "delong", conf.level = 0.95)
        c(lower = as.numeric(ci_obj)[1], auc = as.numeric(ci_obj)[2], upper = as.numeric(ci_obj)[3])
      }, error = function(e) c(lower = NA, auc = NA, upper = NA))
    }

    # ROC Curves
    output$roc_curves <- renderPlotly({
      if (is.null(dt_model()) && is.null(rf_model()) && is.null(svm_model())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      p <- plot_ly()
      added <- 0
      # Decision Tree
      dt <- dt_model()
      if (!is.null(dt)) {
        split_data <- dt_data_split()
        if (!is.null(split_data)) {
          roc_obj <- get_roc(dt$model, split_data$test, dt$target)
          if (!is.null(roc_obj)) {
            ci <- get_auc_ci(roc_obj)
            label <- paste0("Decision Tree (AUC=", round(ci["auc"], 3), " [", round(ci["lower"], 3), ", ", round(ci["upper"], 3), "])")
            p <- add_lines(p, x = 1 - roc_obj$specificities, y = roc_obj$sensitivities,
                           name = label,
                           line = list(color = TEAL, width = 2)); added <- added + 1
          }
        }
      }
      # Random Forest
      rf <- rf_model()
      if (!is.null(rf)) {
        roc_obj <- get_roc(rf$model, rf$test_data, rf$target)
        if (!is.null(roc_obj)) {
          ci <- get_auc_ci(roc_obj)
          label <- paste0("Random Forest (AUC=", round(ci["auc"], 3), " [", round(ci["lower"], 3), ", ", round(ci["upper"], 3), "])")
          p <- add_lines(p, x = 1 - roc_obj$specificities, y = roc_obj$sensitivities,
                         name = label,
                         line = list(color = NAVY, width = 2)); added <- added + 1
        }
      }
      # SVM
      sv <- svm_model()
      if (!is.null(sv)) {
        roc_obj <- get_roc(sv$model, sv$test_data, sv$target)
        if (!is.null(roc_obj)) {
          ci <- get_auc_ci(roc_obj)
          label <- paste0("SVM (AUC=", round(ci["auc"], 3), " [", round(ci["lower"], 3), ", ", round(ci["upper"], 3), "])")
          p <- add_lines(p, x = 1 - roc_obj$specificities, y = roc_obj$sensitivities,
                         name = label,
                         line = list(color = AMBER, width = 2)); added <- added + 1
        }
      }
      if (added == 0) return(plot_ly() |> layout(title="Train at least one binary classifier first"))
      # Diagonal
      p <- add_lines(p, x=c(0,1), y=c(0,1),
                     line=list(color="#AAAAAA",dash="dash",width=1),
                     name="Random", inherit=FALSE)
      p |> layout(title=list(text="ROC Curves",font=list(color=NAVY)),
                  xaxis=list(title="1 - Specificity (FPR)", range=c(0,1)),
                  yaxis=list(title="Sensitivity (TPR)", range=c(0,1)),
                  plot_bgcolor="white", paper_bgcolor="white")
    })

    # Confusion Matrix Heatmaps
    output$conf_heatmaps <- renderPlotly({
      if (is.null(dt_model()) && is.null(rf_model()) && is.null(svm_model())) {
        return(plotly_empty() |>
          layout(title = list(
            text = "Run the analysis above first, then return to this tab.",
            font = list(color = "#999999", size = 13)),
            paper_bgcolor = "white", plot_bgcolor = "white"))
      }
      cms   <- list()
      names_cms <- c()
      dt <- dt_model()
      if (!is.null(dt) && !is.null(dt$cm))   { cms[[length(cms)+1]] <- dt$cm;  names_cms <- c(names_cms,"Decision Tree") }
      rf <- rf_model()
      if (!is.null(rf) && !is.null(rf$cm))   { cms[[length(cms)+1]] <- rf$cm;  names_cms <- c(names_cms,"Random Forest") }
      sv <- svm_model()
      if (!is.null(sv) && !is.null(sv$cm))   { cms[[length(cms)+1]] <- sv$cm;  names_cms <- c(names_cms,"SVM") }
      if (length(cms)==0) return(plot_ly() |> layout(title="Train models first"))

      plots <- lapply(seq_along(cms), function(i) {
        m <- as.matrix(cms[[i]])
        plot_ly(z=m, x=colnames(m), y=rownames(m), type="heatmap",
                colorscale=list(c(0,"#EBF4F7"),c(1,NAVY)),
                text=m, texttemplate="%{text}",
                hovertemplate="Predicted: %{x}<br>Actual: %{y}<br>Count: %{z}<extra></extra>",
                showscale=FALSE) |>
          layout(xaxis=list(title="Predicted"), yaxis=list(title="Actual"),
                 annotations=list(list(text=names_cms[i],x=0.5,y=1.1,
                                       xref="paper",yref="paper",showarrow=FALSE,
                                       font=list(size=12,color=NAVY))))
      })
      do.call(subplot, c(plots, list(nrows=1, shareX=FALSE, shareY=FALSE, margin=0.06))) |>
        layout(title=list(text="Confusion Matrix Heatmaps",font=list(color=NAVY)),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    # AUC with DeLong CI table
    output$auc_ci_tbl <- DT::renderDataTable({
      rows <- list()
      dt <- dt_model()
      if (!is.null(dt)) {
        split_data <- dt_data_split()
        if (!is.null(split_data)) {
          roc_obj <- get_roc(dt$model, split_data$test, dt$target)
          if (!is.null(roc_obj)) {
            ci <- get_auc_ci(roc_obj)
            rows[[length(rows)+1]] <- data.frame(
              Model = "Decision Tree",
              AUC = round(ci["auc"], 4),
              `95% CI Lower` = round(ci["lower"], 4),
              `95% CI Upper` = round(ci["upper"], 4),
              check.names = FALSE
            )
          }
        }
      }
      rf <- rf_model()
      if (!is.null(rf)) {
        roc_obj <- get_roc(rf$model, rf$test_data, rf$target)
        if (!is.null(roc_obj)) {
          ci <- get_auc_ci(roc_obj)
          rows[[length(rows)+1]] <- data.frame(
            Model = "Random Forest",
            AUC = round(ci["auc"], 4),
            `95% CI Lower` = round(ci["lower"], 4),
            `95% CI Upper` = round(ci["upper"], 4),
            check.names = FALSE
          )
        }
      }
      sv <- svm_model()
      if (!is.null(sv)) {
        roc_obj <- get_roc(sv$model, sv$test_data, sv$target)
        if (!is.null(roc_obj)) {
          ci <- get_auc_ci(roc_obj)
          rows[[length(rows)+1]] <- data.frame(
            Model = "SVM",
            AUC = round(ci["auc"], 4),
            `95% CI Lower` = round(ci["lower"], 4),
            `95% CI Upper` = round(ci["upper"], 4),
            check.names = FALSE
          )
        }
      }
      if (length(rows) == 0) return(NULL)
      tool_dt(do.call(rbind, rows),
              "AUC with 95% DeLong Confidence Intervals")
    })

    # Model Comparison Radar Chart
    output$model_radar <- renderPlotly({
      cr <- comparison_results()
      if (is.null(cr)) return(plot_ly() |> layout(title="Run 'Compare All Models' first"))
      tbl <- cr$comparison_table
      if (is.null(tbl) || nrow(tbl)==0) return(plot_ly() |> layout(title="No comparison data"))

      metrics <- c("Accuracy","Sensitivity","Specificity","AUC")
      pal     <- c(TEAL, NAVY, AMBER)
      p       <- plot_ly(type="scatterpolar", fill="toself")
      for (i in seq_len(nrow(tbl))) {
        vals <- tryCatch(as.numeric(tbl[i, metrics]), error=function(e) rep(0,4))
        vals <- c(vals, vals[1])   # close polygon
        p <- add_trace(p, r=vals, theta=c(metrics, metrics[1]),
                       name=tbl$Model[i],
                       line=list(color=pal[(i-1)%%length(pal)+1]),
                       fillcolor=paste0(pal[(i-1)%%length(pal)+1],"44"))
      }
      p |> layout(polar=list(radialaxis=list(visible=TRUE,range=c(0,1))),
                  title=list(text="Model Comparison Radar (Accuracy / Sensitivity / Specificity / AUC)",
                             font=list(color=NAVY)),
                  paper_bgcolor="white")
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
