# ── Module 11: CoMe — Conditional Mediation Analysis ────────
# Models A · B · C · D · E · Bootstrap CI · CoMe Index (ω)
# Based on Cheah et al. (2021) and Hayes (2018)

mod11_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html("📊", "CoMe — Conditional Mediation Analysis",
              "Models A · B · C · D · E | Bootstrap CI | CoMe Index (ω) | Based on Cheah et al. (2021)"),
    uiOutput(ns("global_data_banner")),
    fileInput(ns("file"), "Upload data (.xlsx or .csv)", accept=c(".xlsx",".xls",".csv")),
    uiOutput(ns("main_ui")),
  )
}

mod11_server <- function(id, gemini_key = reactive("")) {
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

    MODELS <- list(
      A = list(name="Model A — First-Stage Moderation",
               desc="W moderates X → M path",
               eq1="M = b0 + p1·X + p4·W + p5·(X·W)",
               eq2="Y = b0 + p2·M + p3·X",
               omega="ω = p2 · p5",
               ie="IE(w) = (p1 + p5·w) · p2",
               needs_z=FALSE),
      B = list(name="Model B — Second-Stage Moderation",
               desc="W moderates M → Y path",
               eq1="M = b0 + p1·X",
               eq2="Y = b0 + p2·M + p3·X + p4·W + p5·(M·W)",
               omega="ω = p1 · p5",
               ie="IE(w) = p1 · (p2 + p5·w)",
               needs_z=FALSE),
      C = list(name="Model C — Both Stages Moderated",
               desc="W moderates both X → M and M → Y",
               eq1="M = b0 + p1·X + p4·W + p5·(X·W)",
               eq2="Y = b0 + p2·M + p3·X + p6·W + p7·(M·W)",
               omega="ω = p2·p5 + p1·p7",
               ie="IE(w) = (p1 + p5·w) · (p2 + p7·w)",
               needs_z=FALSE),
      D = list(name="Model D — Two Moderators (W & Z)",
               desc="W moderates X → M; Z moderates M → Y",
               eq1="M = b0 + p1·X + p4·W + p5·(X·W)",
               eq2="Y = b0 + p2·M + p3·X + p6·Z + p7·(M·Z)",
               omega="ω_W=p2·p5, ω_Z=p1·p7",
               ie="IE(w,z) = (p1+p5·w)·(p2+p7·z)",
               needs_z=TRUE),
      E = list(name="Model E — X Moderates Its Own Effect",
               desc="X moderates M → Y path",
               eq1="M = b0 + p1·X",
               eq2="Y = b0 + p2·M + p3·X + p4·(M·X)",
               omega="ω = p1 · p4",
               ie="IE(x) = p1 · (p2 + p4·x)",
               needs_z=FALSE)
    )

    output$main_ui <- renderUI({
      req(df())
      nc <- num_cols_r()
      tagList(
        fluidRow(
          box(title="1. Select CoMe Model", width=12, status="primary", solidHeader=TRUE,
            radioButtons(ns("model_choice"), NULL,
                         choices=setNames(names(MODELS),
                                          sapply(names(MODELS), function(k) MODELS[[k]]$name)),
                         inline=FALSE),
            uiOutput(ns("model_info"))
          )
        ),
        fluidRow(
          box(title="2. Assign Variables", width=12, status="primary", solidHeader=TRUE,
            fluidRow(
              column(3,
                tags$span(style=paste0("background:",NAVY,";color:white;border-radius:4px;padding:.1rem .5rem;"), "X — Independent"),
                textInput(ns("X_label"), "X label", value="Predictor (X)"),
                pickerInput(ns("X_items"), "X items", choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE))
              ),
              column(3,
                tags$span(style=paste0("background:",TEAL,";color:white;border-radius:4px;padding:.1rem .5rem;"), "M — Mediator"),
                textInput(ns("M_label"), "M label", value="Mediator (M)"),
                pickerInput(ns("M_items"), "M items", choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE))
              ),
              column(3,
                tags$span(style=paste0("background:",GREEN,";color:white;border-radius:4px;padding:.1rem .5rem;"), "Y — Outcome"),
                textInput(ns("Y_label"), "Y label", value="Outcome (Y)"),
                pickerInput(ns("Y_items"), "Y items", choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE))
              ),
              column(3,
                tags$span(style=paste0("background:",AMBER,";color:white;border-radius:4px;padding:.1rem .5rem;"), "W — Moderator"),
                textInput(ns("W_label"), "W label", value="Moderator (W)"),
                pickerInput(ns("W_items"), "W items", choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE))
              )
            ),
            uiOutput(ns("z_ui"))
          )
        ),
        fluidRow(
          box(title="3. Settings & Run", width=12, status="primary", solidHeader=TRUE,
            fluidRow(
              column(3, textInput(ns("study_name"), "Study name", value="My Study")),
              column(3, numericInput(ns("n_boot"), "Bootstrap samples", value=5000, min=500, max=10000, step=500)),
              column(2, numericInput(ns("ci_level"), "CI Level (%)", value=95, min=90, max=99)),
              column(2, numericInput(ns("seed"), "Random seed", value=42, min=1)),
              column(2, br(), actionButton(ns("run_come"), "▶ Run CoMe", class="btn-primary btn-block"))
            )
          )
        ),
        uiOutput(ns("results_ui")),
        box(width=12, status="info", solidHeader=FALSE,
          title=tags$span("\U0001f916 AI Interpretation", style="font-weight:bold;color:#1A3A5C;"),
          tags$div(style="padding:.5rem 0;",
            tags$div(style="background:#EFF8FF;border-left:4px solid #2196A6;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;font-size:.85rem;",
              tags$b("How to use:")," Run an analysis, then click its button for a detailed academic interpretation.", tags$br(),
              tags$a("\U0001f511 FREE Groq key \u2192 console.groq.com/keys",
                     href="https://console.groq.com/keys",target="_blank",style="color:#2196A6;font-weight:bold;")
            ),
            fluidRow(
              column(4, actionButton(ns("ai_btn_come"), "\U0001f517 CoMe Path Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_jn"),   "\U0001f4c9 Johnson-Neyman Floodlight",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_all"),  "\U0001f4cb Full CoMe Summary",
                                     class="btn-success btn-block", style="margin-bottom:.5rem;"))
            ),
            tags$hr(),
            uiOutput(ns("ai_output"))
          )
        ),
        creator_footer()
      )
    })

    output$model_info <- renderUI({
      req(input$model_choice)
      m <- MODELS[[input$model_choice]]
      tags$div(style="background:#EBF4F7;padding:.8rem;border-radius:6px;margin-top:.5rem;",
        tags$b(m$desc), tags$br(),
        tags$code(paste("Eq 1:", m$eq1)), tags$br(),
        tags$code(paste("Eq 2:", m$eq2)), tags$br(),
        tags$code(paste("CoMe Index:", m$omega)), tags$br(),
        tags$code(paste("IE formula:", m$ie))
      )
    })

    output$z_ui <- renderUI({
      req(input$model_choice)
      if (!MODELS[[input$model_choice]]$needs_z) return(NULL)
      nc <- num_cols_r()
      fluidRow(column(3,
        tags$span(style="background:#6B3A8C;color:white;border-radius:4px;padding:.1rem .5rem;", "Z — Second Moderator"),
        textInput(ns("Z_label"), "Z label", value="Moderator Z"),
        pickerInput(ns("Z_items"), "Z items", choices=nc, multiple=TRUE, options=list(`actions-box`=TRUE))
      ))
    })

    # ── CoMe Engine ────────────────────────────────────────
    fit_model_r <- function(model_key, X, M, Y, W_c, Z_c=NULL) {
      n <- length(X)
      lm_coef <- function(y, X_mat) {
        fit <- lm.fit(X_mat, y)
        ss_res <- sum(fit$residuals^2)
        ss_tot <- sum((y - mean(y))^2)
        r2 <- 1 - ss_res/ss_tot
        list(coef=fit$coefficients, r2=r2)
      }

      if (model_key == "A") {
        D1 <- cbind(1, X, W_c, X*W_c)
        D2 <- cbind(1, M, X)
        r1 <- lm_coef(M, D1); r2 <- lm_coef(Y, D2)
        list(p1=r1$coef[2],p2=r2$coef[2],p3=r2$coef[3],p4=r1$coef[3],p5=r1$coef[4],
             r2_eq1=r1$r2, r2_eq2=r2$r2)
      } else if (model_key == "B") {
        D1 <- cbind(1, X)
        D2 <- cbind(1, M, X, W_c, M*W_c)
        r1 <- lm_coef(M, D1); r2 <- lm_coef(Y, D2)
        list(p1=r1$coef[2],p2=r2$coef[2],p3=r2$coef[3],p4=r2$coef[4],p5=r2$coef[5],
             r2_eq1=r1$r2, r2_eq2=r2$r2)
      } else if (model_key == "C") {
        D1 <- cbind(1, X, W_c, X*W_c)
        D2 <- cbind(1, M, X, W_c, M*W_c)
        r1 <- lm_coef(M, D1); r2 <- lm_coef(Y, D2)
        list(p1=r1$coef[2],p2=r2$coef[2],p3=r2$coef[3],p4=r1$coef[3],p5=r1$coef[4],
             p6=r2$coef[4],p7=r2$coef[5], r2_eq1=r1$r2, r2_eq2=r2$r2)
      } else if (model_key == "D") {
        D1 <- cbind(1, X, W_c, X*W_c)
        D2 <- cbind(1, M, X, Z_c, M*Z_c)
        r1 <- lm_coef(M, D1); r2 <- lm_coef(Y, D2)
        list(p1=r1$coef[2],p2=r2$coef[2],p3=r2$coef[3],p4=r1$coef[3],p5=r1$coef[4],
             p6=r2$coef[4],p7=r2$coef[5], r2_eq1=r1$r2, r2_eq2=r2$r2)
      } else if (model_key == "E") {
        D1 <- cbind(1, X)
        D2 <- cbind(1, M, X, X*M)
        r1 <- lm_coef(M, D1); r2 <- lm_coef(Y, D2)
        list(p1=r1$coef[2],p2=r2$coef[2],p3=r2$coef[3],p4=r2$coef[4],
             r2_eq1=r1$r2, r2_eq2=r2$r2)
      }
    }

    cond_ie_r <- function(model_key, p, W_sd) {
      if (model_key=="A") list(Low=(p$p1+p$p5*(-W_sd))*p$p2, Mean=(p$p1)*p$p2, High=(p$p1+p$p5*W_sd)*p$p2)
      else if (model_key=="B") list(Low=p$p1*(p$p2+p$p5*(-W_sd)), Mean=p$p1*p$p2, High=p$p1*(p$p2+p$p5*W_sd))
      else if (model_key=="C") list(
        Low =(p$p1+p$p5*(-W_sd))*(p$p2+p$p7*(-W_sd)),
        Mean= p$p1*p$p2,
        High=(p$p1+p$p5*(+W_sd))*(p$p2+p$p7*(+W_sd)))
      else if (model_key=="D") list(Low=(p$p1+p$p5*(-W_sd))*p$p2, Mean=p$p1*p$p2, High=(p$p1+p$p5*W_sd)*p$p2)
      else if (model_key=="E") list(Low=p$p1*(p$p2+p$p4*(-W_sd)), Mean=p$p1*p$p2, High=p$p1*(p$p2+p$p4*W_sd))
    }

    come_omega_r <- function(model_key, p) {
      if (model_key=="A") p$p2*p$p5
      else if (model_key=="B") p$p1*p$p5
      else if (model_key=="C") p$p2*p$p5 + p$p1*p$p7
      else if (model_key=="D") p$p2*p$p5  # omega_W
      else if (model_key=="E") p$p1*p$p4
    }

    # Compute IE at any mean-centred moderator value (for J-N sweep)
    compute_ie_at_w_r <- function(model_key, p, w_c) {
      if      (model_key == "A") (p$p1 + p$p5 * w_c) * p$p2
      else if (model_key == "B") p$p1 * (p$p2 + p$p5 * w_c)
      else if (model_key == "C") (p$p1 + p$p5 * w_c) * (p$p2 + p$p7 * w_c)
      else if (model_key == "D") (p$p1 + p$p5 * w_c) * p$p2   # Z fixed at mean
      else if (model_key == "E") p$p1 * (p$p2 + p$p4 * w_c)   # sweep X (focal)
    }

    come_result <- eventReactive(input$run_come, {
      req(df(), input$X_items, input$M_items, input$Y_items, input$W_items)

      d <- df()
      X_arr <- rowMeans(d[, input$X_items, drop=FALSE], na.rm=TRUE)
      M_arr <- rowMeans(d[, input$M_items, drop=FALSE], na.rm=TRUE)
      Y_arr <- rowMeans(d[, input$Y_items, drop=FALSE], na.rm=TRUE)
      W_arr <- rowMeans(d[, input$W_items, drop=FALSE], na.rm=TRUE)
      Z_arr <- if (!is.null(input$Z_items) && length(input$Z_items)>0)
                 rowMeans(d[, input$Z_items, drop=FALSE], na.rm=TRUE) else NULL

      ok <- complete.cases(X_arr, M_arr, Y_arr, W_arr)
      if (!is.null(Z_arr) && length(Z_arr) > 0) ok <- ok & !is.na(Z_arr)
      X_arr <- X_arr[ok]; M_arr <- M_arr[ok]; Y_arr <- Y_arr[ok]; W_arr <- W_arr[ok]
      if (!is.null(Z_arr)) Z_arr <- Z_arr[ok]
      N <- sum(ok)

      W_mean <- mean(W_arr); W_sd <- sd(W_arr)
      W_c    <- W_arr - W_mean
      Z_c    <- if (!is.null(Z_arr) && length(Z_arr) > 0) Z_arr - mean(Z_arr, na.rm=TRUE) else NULL

      model_key <- input$model_choice

      # Original paths
      orig_p  <- fit_model_r(model_key, X_arr, M_arr, Y_arr, W_c, Z_c)
      orig_ie <- cond_ie_r(model_key, orig_p, W_sd)
      orig_om <- come_omega_r(model_key, orig_p)

      # Bootstrap
      set.seed(input$seed)
      n_boot <- input$n_boot
      lo_pct <- (100 - input$ci_level) / 2
      hi_pct <- 100 - lo_pct

      boot_ie    <- list(Low=numeric(n_boot), Mean=numeric(n_boot), High=numeric(n_boot))
      boot_omega <- numeric(n_boot)
      boot_paths <- vector("list", n_boot)

      withProgress(message="Bootstrap running...", value=0, {
        for (i in seq_len(n_boot)) {
          idx <- sample(N, N, replace=TRUE)
          bp  <- fit_model_r(model_key, X_arr[idx], M_arr[idx], Y_arr[idx], W_c[idx],
                             if (!is.null(Z_c)) Z_c[idx] else NULL)
          bie <- cond_ie_r(model_key, bp, W_sd)
          for (lvl in c("Low","Mean","High")) boot_ie[[lvl]][i] <- bie[[lvl]]
          boot_omega[i] <- come_omega_r(model_key, bp)
          boot_paths[[i]] <- bp
          if (i %% 500 == 0) incProgress(500/n_boot)
        }
      })

      ci_ie <- lapply(boot_ie, function(arr) {
        list(lower=quantile(arr, lo_pct/100), upper=quantile(arr, hi_pct/100),
             se=sd(arr), mean=mean(arr))
      })
      ci_om <- list(lower=quantile(boot_omega, lo_pct/100),
                    upper=quantile(boot_omega, hi_pct/100),
                    se=sd(boot_omega), mean=mean(boot_omega))

      list(model_key=model_key, N=N, W_sd=W_sd,
           orig_p=orig_p, orig_ie=orig_ie, orig_om=orig_om,
           ci_ie=ci_ie, ci_om=ci_om, ci_level=input$ci_level,
           n_boot=n_boot, boot_paths=boot_paths,
           W_arr_orig=W_arr, W_mean=W_mean,
           X_label=input$X_label, M_label=input$M_label,
           Y_label=input$Y_label, W_label=input$W_label,
           study=input$study_name)
    })

    output$results_ui <- renderUI({
      req(come_result())
      res <- come_result()
      m   <- MODELS[[res$model_key]]
      om_sig <- res$ci_om$lower > 0 | res$ci_om$upper < 0

      tagList(
        fluidRow(
          box(title=paste0("✅ Results — ", res$study, " (Model ", res$model_key, ")"),
              width=12, status="success", solidHeader=TRUE,
            fluidRow(
              column(3, tags$div(style="background:white;border-radius:8px;padding:1rem;text-align:center;box-shadow:0 2px 6px rgba(0,0,0,.08);",
                tags$p(style="color:#666;margin:0;font-size:.78rem;","IE — Low W (−1 SD)"),
                tags$h4(style="color:#1A3A5C;margin:.2rem 0;", round(res$orig_ie$Low,4)),
                tags$p(style="margin:0;font-size:.78rem;",
                       sprintf("%d%% CI [%.4f, %.4f]", res$ci_level, res$ci_ie$Low$lower, res$ci_ie$Low$upper)),
                if(res$ci_ie$Low$lower>0|res$ci_ie$Low$upper<0)
                  tags$span(class="sig-yes","✓ Significant") else tags$span(class="sig-no","✗ n.s.")
              )),
              column(3, tags$div(style="background:white;border-radius:8px;padding:1rem;text-align:center;box-shadow:0 2px 6px rgba(0,0,0,.08);",
                tags$p(style="color:#666;margin:0;font-size:.78rem;","IE — Mean W (0)"),
                tags$h4(style="color:#1A3A5C;margin:.2rem 0;", round(res$orig_ie$Mean,4)),
                tags$p(style="margin:0;font-size:.78rem;",
                       sprintf("%d%% CI [%.4f, %.4f]", res$ci_level, res$ci_ie$Mean$lower, res$ci_ie$Mean$upper)),
                if(res$ci_ie$Mean$lower>0|res$ci_ie$Mean$upper<0)
                  tags$span(class="sig-yes","✓ Significant") else tags$span(class="sig-no","✗ n.s.")
              )),
              column(3, tags$div(style="background:white;border-radius:8px;padding:1rem;text-align:center;box-shadow:0 2px 6px rgba(0,0,0,.08);",
                tags$p(style="color:#666;margin:0;font-size:.78rem;","IE — High W (+1 SD)"),
                tags$h4(style="color:#1A3A5C;margin:.2rem 0;", round(res$orig_ie$High,4)),
                tags$p(style="margin:0;font-size:.78rem;",
                       sprintf("%d%% CI [%.4f, %.4f]", res$ci_level, res$ci_ie$High$lower, res$ci_ie$High$upper)),
                if(res$ci_ie$High$lower>0|res$ci_ie$High$upper<0)
                  tags$span(class="sig-yes","✓ Significant") else tags$span(class="sig-no","✗ n.s.")
              )),
              column(3, tags$div(style=paste0("background:",if(om_sig) GREEN else "#C0392B",";color:white;border-radius:8px;padding:1rem;text-align:center;"),
                tags$p(style="margin:0;font-size:.78rem;", paste("CoMe Index ω:", m$omega)),
                tags$h4(style="color:white;margin:.2rem 0;", round(res$orig_om,4)),
                tags$p(style="margin:0;font-size:.78rem;",
                       sprintf("%d%% CI [%.4f, %.4f]", res$ci_level, res$ci_om$lower, res$ci_om$upper)),
                tags$b(if(om_sig) "✓ CoMe Confirmed" else "✗ Not Confirmed")
              ))
            )
          )
        ),
        tabBox(width=12,
          tabPanel("📈 Path Coefficients", DT::dataTableOutput(ns("path_tbl"))),
          tabPanel("🔍 Conditional Effects", DT::dataTableOutput(ns("ie_tbl"))),
          tabPanel("📊 Graph", plotlyOutput(ns("come_plot"), height="450px")),
          tabPanel("📉 Johnson-Neyman",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Johnson-Neyman Floodlight Analysis"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "The shaded green band marks the range of the moderator (W) where the indirect effect is ",
                tags$b("statistically significant"), " (bootstrap CI excludes zero). ",
                "Vertical dashed lines mark the exact transition points. ",
                "Based on Hayes & Matthes (2009).")),
            plotlyOutput(ns("jn_plot"), height="460px"),
            br(),
            h5("Transition Points"),
            DT::dataTableOutput(ns("jn_tbl"))
          ),
          tabPanel("📝 APA Write-Up", verbatimTextOutput(ns("apa_text"))),
          tabPanel("📥 Download", downloadButton(ns("dl_come"), "Download Results (Excel)")),
          tabPanel("🔗 Path Diagram",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Interactive Path Diagram"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Visualises the CoMe model structure with estimated path coefficients on each arrow. ",
                "Nodes: ", tags$b(style="color:#2196A6;","Blue = X"), ", ",
                tags$b(style="color:#1A3A5C;","Navy = M"), ", ",
                tags$b(style="color:#1E6438;","Green = Y"), ", ",
                tags$b(style="color:#E07B39;","Amber = W (moderator)"), ".")),
            DiagrammeR::grVizOutput(ns("path_diagram"), height = "420px")
          ),
          tabPanel("🌲 Bootstrap CI Forest",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Bootstrap CI Forest Plot — Conditional Indirect Effects"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Indirect effects (a×b) at three moderator values (−1 SD, Mean, +1 SD) with ",
                "bootstrapped confidence intervals. ", tags$b("Green"), " = CI excludes zero (significant); ",
                tags$b("Red"), " = CI crosses zero (non-significant). Dashed red line marks zero.")),
            withSpinner(plotlyOutput(ns("boot_forest"), height = "400px"))
          ),
          tabPanel("📈 Simple Slopes",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Simple Slopes Interaction Plot"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Shows how the total X → Y relationship (direct + indirect) changes at low (−1 SD), ",
                "mean, and high (+1 SD) levels of the moderator W. Diverging slopes indicate moderation; ",
                "steeper slope at high W suggests stronger X→Y when W is high.")),
            withSpinner(plotlyOutput(ns("simple_slopes"), height = "420px"))
          )
        )
      )
    })

    output$path_tbl <- DT::renderDataTable({
      req(come_result())
      res <- come_result()
      p   <- res$orig_p

      path_def <- list(
        A=list(c("X → M","p1"),c("M → Y","p2"),c("X → Y","p3"),c("W → M","p4"),c("X·W → M","p5")),
        B=list(c("X → M","p1"),c("M → Y","p2"),c("X → Y","p3"),c("W → Y","p4"),c("M·W → Y","p5")),
        C=list(c("X → M","p1"),c("M → Y","p2"),c("X → Y","p3"),c("W → M","p4"),c("X·W → M","p5"),
               c("W → Y","p6"),c("M·W → Y","p7")),
        D=list(c("X → M","p1"),c("M → Y","p2"),c("X → Y","p3"),c("W → M","p4"),c("X·W → M","p5"),
               c("Z → Y","p6"),c("M·Z → Y","p7")),
        E=list(c("X → M","p1"),c("M → Y","p2"),c("X → Y","p3"),c("M·X → Y","p4"))
      )

      rows <- lapply(path_def[[res$model_key]], function(pd) {
        data.frame(Path=pd[1], Symbol=pd[2], Estimate=round(p[[pd[2]]],4), check.names=FALSE)
      })
      tbl <- do.call(rbind, rows)
      tbl$`R² Eq1` <- round(p$r2_eq1, 4)
      tbl$`R² Eq2` <- round(p$r2_eq2, 4)
      tool_dt(tbl, paste0("Path Coefficients — Model ", res$model_key, " (N=", res$N, ")"))
    })

    output$ie_tbl <- DT::renderDataTable({
      req(come_result())
      res <- come_result()
      rows <- lapply(c("Low","Mean","High"), function(lvl) {
        b   <- res$ci_ie[[lvl]]
        ie  <- res$orig_ie[[lvl]]
        sig <- b$lower > 0 | b$upper < 0
        label <- switch(lvl,
                        Low  = sprintf("Low W (−1SD = %.4f)", -res$W_sd),
                        Mean = "Mean W (0)",
                        High = sprintf("High W (+1SD = %.4f)", res$W_sd))
        data.frame(`Level of W`=label, `IE (original)`=round(ie,6),
                   `Lower CI`=round(b$lower,6), `Upper CI`=round(b$upper,6),
                   SE=round(b$se,6), Significant=if(sig)"✓ Yes" else "✗ No",
                   check.names=FALSE)
      })
      tbl <- do.call(rbind, rows)
      tool_dt(tbl, paste0(res$ci_level, "% Bootstrap CI — ", res$n_boot, " resamples"))
    })

    output$come_plot <- renderPlotly({
      req(come_result())
      res <- come_result()
      lvls <- c("Low","Mean","High")
      labels <- c(sprintf("Low W\n(−1SD=%.3f)",-res$W_sd), "Mean W\n(=0)", sprintf("High W\n(+1SD=%.3f)",res$W_sd))
      ie_vals <- sapply(lvls, function(l) res$orig_ie[[l]])
      lo_vals <- sapply(lvls, function(l) res$ci_ie[[l]]$lower)
      hi_vals <- sapply(lvls, function(l) res$ci_ie[[l]]$upper)
      colors  <- ifelse(lo_vals > 0 | hi_vals < 0, TEAL, "#BDC3C7")

      plot_ly(x=labels, y=ie_vals, type="bar", marker=list(color=colors),
              error_y=list(type="data", array=hi_vals-ie_vals, arrayminus=ie_vals-lo_vals,
                           color=NAVY, thickness=2, width=8),
              hovertemplate=paste0("IE = %{y:.4f}<br>CI [%.4f, %.4f]<extra></extra>")) |>
        add_segments(x=0.5, xend=3.5, y=0, yend=0, line=list(color="red", dash="dash", width=2)) |>
        layout(title=list(text=paste0("CoMe Model ", res$model_key, " — ", res$study),
                          font=list(color=NAVY)),
               yaxis=list(title="Indirect Effect (IE)"),
               xaxis=list(title="Level of Moderator (W)"),
               plot_bgcolor="white", paper_bgcolor="white", showlegend=FALSE)
    })

    output$apa_text <- renderText({
      req(come_result())
      res <- come_result(); m <- MODELS[[res$model_key]]
      om_sig <- res$ci_om$lower > 0 | res$ci_om$upper < 0
      paste0(
        "A conditional process analysis was conducted using CoMe Model ", res$model_key,
        " (", m$name, ") following Cheah et al. (2021) and Hayes (2018).\n",
        "The indirect effect of ", res$X_label, " on ", res$Y_label,
        " through ", res$M_label, " was hypothesised to be moderated by ", res$W_label,
        " (N = ", res$N, "; ", res$n_boot, " bootstrap resamples; ",
        res$ci_level, "% CI).\n\n",
        "The CoMe index (ω = ", round(res$orig_om, 3), ") was ",
        if(om_sig) "statistically significant" else "not statistically significant",
        " (Boot ", res$ci_level, "% CI [", round(res$ci_om$lower,3), ", ", round(res$ci_om$upper,3), "]), ",
        if(om_sig) "confirming" else "not confirming", " conditional mediation.\n\n",
        "Conditional indirect effects at three levels of W:\n",
        sprintf("  Low  (−1SD = %.3f): IE = %.3f, %d%% CI [%.3f, %.3f]\n",
                -res$W_sd, res$orig_ie$Low, res$ci_level, res$ci_ie$Low$lower, res$ci_ie$Low$upper),
        sprintf("  Mean (0):           IE = %.3f, %d%% CI [%.3f, %.3f]\n",
                res$orig_ie$Mean, res$ci_level, res$ci_ie$Mean$lower, res$ci_ie$Mean$upper),
        sprintf("  High (+1SD = %.3f): IE = %.3f, %d%% CI [%.3f, %.3f]\n\n",
                res$W_sd, res$orig_ie$High, res$ci_level, res$ci_ie$High$lower, res$ci_ie$High$upper),
        "REFERENCES\n",
        "Cheah, J. H., Nitzl, C., Roldan, J. L., Cepeda-Carrion, G., & Gudergan, S. (2021). ",
        "A primer on the conditional mediation analysis in PLS-SEM. ",
        "The DATA BASE for Advances in Information Systems, 52(SI), 43-100.\n",
        "Hayes, A. F. (2018). Introduction to mediation, moderation, and conditional process analysis (2nd ed.). Guilford Press.\n\n",
        "[Dr.AIStat (R Edition) — Dr. Aneeq Inam. ORCID: 0000-0001-7682-2244]"
      )
    })

    output$dl_come <- downloadHandler(
      filename = function() paste0("CoMe_Model", come_result()$model_key, "_", Sys.Date(), ".xlsx"),
      content  = function(file) {
        req(come_result())
        res <- come_result()
        ie_rows <- lapply(c("Low","Mean","High"), function(lvl) {
          b <- res$ci_ie[[lvl]]
          data.frame(`Level`=lvl, IE=round(res$orig_ie[[lvl]],6),
                     Lower=round(b$lower,6), Upper=round(b$upper,6), SE=round(b$se,6),
                     Significant=if(b$lower>0|b$upper<0)"Yes" else "No", check.names=FALSE)
        })
        omega_df <- data.frame(
          `CoMe Index (ω)`=round(res$orig_om,6),
          Lower=round(res$ci_om$lower,6), Upper=round(res$ci_om$upper,6),
          SE=round(res$ci_om$se,6), Significant=if(res$ci_om$lower>0|res$ci_om$upper<0)"Yes" else "No",
          check.names=FALSE)
        write_xlsx(list("Conditional IEs"=do.call(rbind,ie_rows), "CoMe Index"=omega_df), file)
      }
    )

    # ── Johnson-Neyman sweep ───────────────────────────────────
    jn_data <- reactive({
      req(come_result())
      res <- come_result()
      if (is.null(res$boot_paths) || length(res$boot_paths) == 0) return(NULL)

      W_orig   <- res$W_arr_orig
      W_mean   <- res$W_mean
      W_vals   <- seq(min(W_orig), max(W_orig), length.out = 100)
      lo_pct   <- (100 - res$ci_level) / 2 / 100
      hi_pct   <- 1 - lo_pct
      mk       <- res$model_key

      rows <- lapply(W_vals, function(w) {
        w_c     <- w - W_mean
        ie_orig <- compute_ie_at_w_r(mk, res$orig_p, w_c)
        boot_ies <- vapply(res$boot_paths,
                           function(bp) compute_ie_at_w_r(mk, bp, w_c),
                           numeric(1))
        lo  <- quantile(boot_ies, lo_pct, na.rm = TRUE)
        hi  <- quantile(boot_ies, hi_pct, na.rm = TRUE)
        sig <- lo > 0 | hi < 0
        data.frame(W = w, W_centered = w_c, IE = ie_orig,
                   Lower = lo, Upper = hi, Significant = sig,
                   stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    })

    output$jn_plot <- renderPlotly({
      req(jn_data(), come_result())
      jn  <- jn_data()
      res <- come_result()
      if (is.null(jn)) return(NULL)

      # Determine significant vs non-significant rows
      jn$sig_label <- ifelse(jn$Significant, "Significant", "Non-significant")

      # Build upper CI trace (invisible line)
      # Use two ribbon traces: one per significance group is complex; use shapes instead
      # Approach: full ribbon in light grey, then overlay green only in sig regions
      fig <- plot_ly(data = jn, x = ~W)

      # ── Upper / Lower CI ribbons ────────────────────────────
      # Non-significant ribbon (grey)
      jn_ns <- jn[!jn$Significant, ]
      jn_s  <- jn[ jn$Significant, ]

      if (nrow(jn_ns) > 0) {
        fig <- fig |>
          add_trace(data = jn_ns, x = ~W, y = ~Upper, type = "scatter",
                    mode = "lines", line = list(color = "transparent"),
                    showlegend = FALSE, hoverinfo = "skip", name = "upper_ns") |>
          add_trace(data = jn_ns, x = ~W, y = ~Lower, type = "scatter",
                    mode = "lines", fill = "tonexty",
                    fillcolor = "rgba(189,195,199,0.35)",
                    line = list(color = "transparent"),
                    showlegend = FALSE, hoverinfo = "skip", name = "lower_ns")
      }

      if (nrow(jn_s) > 0) {
        fig <- fig |>
          add_trace(data = jn_s, x = ~W, y = ~Upper, type = "scatter",
                    mode = "lines", line = list(color = "transparent"),
                    showlegend = FALSE, hoverinfo = "skip", name = "upper_s") |>
          add_trace(data = jn_s, x = ~W, y = ~Lower, type = "scatter",
                    mode = "lines", fill = "tonexty",
                    fillcolor = "rgba(30,100,56,0.22)",
                    line = list(color = "transparent"),
                    showlegend = FALSE, hoverinfo = "skip", name = "lower_s")
      }

      # ── CI boundary lines ───────────────────────────────────
      fig <- fig |>
        add_trace(data = jn, x = ~W, y = ~Upper, type = "scatter", mode = "lines",
                  line = list(color = TEAL, width = 1, dash = "dot"),
                  name = paste0(res$ci_level, "% CI Upper"),
                  hovertemplate = paste0(res$W_label, " = %{x:.3f}<br>Upper = %{y:.4f}<extra></extra>")) |>
        add_trace(data = jn, x = ~W, y = ~Lower, type = "scatter", mode = "lines",
                  line = list(color = TEAL, width = 1, dash = "dot"),
                  name = paste0(res$ci_level, "% CI Lower"),
                  hovertemplate = paste0(res$W_label, " = %{x:.3f}<br>Lower = %{y:.4f}<extra></extra>"))

      # ── IE line ─────────────────────────────────────────────
      fig <- fig |>
        add_trace(data = jn, x = ~W, y = ~IE, type = "scatter", mode = "lines",
                  line = list(color = NAVY, width = 2.5),
                  name = "Indirect Effect (IE)",
                  hovertemplate = paste0(res$W_label, " = %{x:.3f}<br>IE = %{y:.4f}<extra></extra>"))

      # ── Zero line ───────────────────────────────────────────
      fig <- fig |>
        add_segments(x = min(jn$W), xend = max(jn$W), y = 0, yend = 0,
                     line = list(color = "red", dash = "dash", width = 1.5),
                     showlegend = FALSE, hoverinfo = "skip")

      # ── Transition point annotations ────────────────────────
      trans_idx <- which(diff(jn$Significant) != 0)
      shapes_list <- list()
      annots_list <- list()
      for (ti in trans_idx) {
        w_t <- (jn$W[ti] + jn$W[ti + 1]) / 2
        shapes_list <- c(shapes_list, list(
          list(type = "line", x0 = w_t, x1 = w_t,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(color = AMBER, width = 1.8, dash = "dashdot"))
        ))
        annots_list <- c(annots_list, list(
          list(x = w_t, y = 1, yref = "paper", xanchor = "center",
               text = paste0("W=", round(w_t, 3)),
               showarrow = FALSE, font = list(size = 10, color = AMBER),
               bgcolor = "white", bordercolor = AMBER, borderwidth = 1)
        ))
      }

      # ── Mean of W marker ───────────────────────────────────
      shapes_list <- c(shapes_list, list(
        list(type = "line", x0 = res$W_mean, x1 = res$W_mean,
             y0 = 0, y1 = 1, yref = "paper",
             line = list(color = "#888888", width = 1, dash = "dot"))
      ))

      w_label <- if (mk == "E") res$X_label else res$W_label

      fig |> layout(
        title  = list(text = paste0("Johnson-Neyman Floodlight — CoMe Model ", mk,
                                    "<br><sup>Green band = significant IE region; ",
                                    res$ci_level, "% bootstrap CI</sup>"),
                      font = list(color = NAVY, size = 14)),
        xaxis  = list(title = paste0(w_label, " (observed range)"),
                      zeroline = FALSE),
        yaxis  = list(title = "Indirect Effect (IE)", zeroline = FALSE),
        shapes = shapes_list,
        annotations = annots_list,
        legend = list(orientation = "h", y = -0.18),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        hovermode     = "x unified"
      )
    })

    output$jn_tbl <- DT::renderDataTable({
      req(jn_data(), come_result())
      jn  <- jn_data()
      res <- come_result()
      if (is.null(jn)) return(NULL)

      # Identify transition points
      trans_idx <- which(diff(jn$Significant) != 0)
      if (length(trans_idx) == 0) {
        # No transitions — summarise the region
        all_sig <- all(jn$Significant)
        out <- data.frame(
          `Transition Point` = "No transition found",
          `W value` = NA_real_,
          `Region to the LEFT`  = if (all_sig) "Significant" else "Non-significant",
          `Region to the RIGHT` = if (all_sig) "Significant" else "Non-significant",
          `IE at transition`    = NA_real_,
          check.names = FALSE
        )
      } else {
        out_rows <- lapply(trans_idx, function(ti) {
          w_t  <- (jn$W[ti] + jn$W[ti + 1]) / 2
          ie_t <- (jn$IE[ti] + jn$IE[ti + 1]) / 2
          data.frame(
            `Transition Point`    = paste0("W = ", round(w_t, 4)),
            `W value`             = round(w_t, 6),
            `Region to the LEFT`  = if (jn$Significant[ti])   "✓ Significant" else "✗ n.s.",
            `Region to the RIGHT` = if (jn$Significant[ti+1]) "✓ Significant" else "✗ n.s.",
            `IE at transition`    = round(ie_t, 6),
            check.names = FALSE
          )
        })
        out <- do.call(rbind, out_rows)
      }
      tool_dt(out, paste0("Johnson-Neyman Transition Points (", res$ci_level, "% CI, ",
                          res$n_boot, " bootstrap resamples)"))
    })

    # ── Path Diagram (DiagrammeR) ─────────────────────────────────────────
    output$path_diagram <- DiagrammeR::renderGrViz({
      req(come_result())
      res <- come_result()
      p   <- res$orig_p
      mk  <- res$model_key
      xl  <- res$X_label %||% "X"
      ml  <- res$M_label %||% "M"
      yl  <- res$Y_label %||% "Y"
      wl  <- res$W_label %||% "W"
      fmt <- function(x) sprintf("%.3f", x)

      a_lbl  <- fmt(p$p1)
      b_lbl  <- fmt(p$p2)
      cp_lbl <- fmt(p$p3)

      if (mk == "A") {
        gv <- sprintf('
digraph CoMe_A {
  graph [rankdir=LR, bgcolor=white, fontname="Helvetica", splines=curved]
  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12]
  X   [label="%s\\n(Predictor)",   fillcolor="#2196A6", fontcolor=white]
  M   [label="%s\\n(Mediator)",    fillcolor="#1A3A5C", fontcolor=white]
  Y   [label="%s\\n(Outcome)",     fillcolor="#1E6438", fontcolor=white]
  W   [label="%s\\n(Moderator)",   fillcolor="#E07B39", fontcolor=white, shape=diamond]
  XW  [label="X\\U00D7W\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  X  -> M  [label="a = %s",  fontsize=9, penwidth=2.0, color="#2196A6"]
  M  -> Y  [label="b = %s",  fontsize=9, penwidth=2.0, color="#1A3A5C"]
  X  -> Y  [label="c\\U02B9= %s", fontsize=9, style=dashed, color="#999999"]
  W  -> M  [label="p4=%s",   fontsize=9, color="#E07B39"]
  XW -> M  [label="p5=%s",   fontsize=9, color="#E07B39"]
}', xl, ml, yl, wl, a_lbl, b_lbl, cp_lbl, fmt(p$p4), fmt(p$p5))

      } else if (mk == "B") {
        gv <- sprintf('
digraph CoMe_B {
  graph [rankdir=LR, bgcolor=white, fontname="Helvetica", splines=curved]
  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12]
  X   [label="%s\\n(Predictor)",   fillcolor="#2196A6", fontcolor=white]
  M   [label="%s\\n(Mediator)",    fillcolor="#1A3A5C", fontcolor=white]
  Y   [label="%s\\n(Outcome)",     fillcolor="#1E6438", fontcolor=white]
  W   [label="%s\\n(Moderator)",   fillcolor="#E07B39", fontcolor=white, shape=diamond]
  MW  [label="M\\U00D7W\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  X  -> M  [label="a = %s",  fontsize=9, penwidth=2.0, color="#2196A6"]
  M  -> Y  [label="b = %s",  fontsize=9, penwidth=2.0, color="#1A3A5C"]
  X  -> Y  [label="c\\U02B9= %s", fontsize=9, style=dashed, color="#999999"]
  W  -> Y  [label="p4=%s",   fontsize=9, color="#E07B39"]
  MW -> Y  [label="p5=%s",   fontsize=9, color="#E07B39"]
}', xl, ml, yl, wl, a_lbl, b_lbl, cp_lbl, fmt(p$p4), fmt(p$p5))

      } else if (mk == "C") {
        gv <- sprintf('
digraph CoMe_C {
  graph [rankdir=LR, bgcolor=white, fontname="Helvetica", splines=curved]
  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12]
  X   [label="%s\\n(Predictor)",   fillcolor="#2196A6", fontcolor=white]
  M   [label="%s\\n(Mediator)",    fillcolor="#1A3A5C", fontcolor=white]
  Y   [label="%s\\n(Outcome)",     fillcolor="#1E6438", fontcolor=white]
  W   [label="%s\\n(Moderator)",   fillcolor="#E07B39", fontcolor=white, shape=diamond]
  XW  [label="X\\U00D7W\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  MW  [label="M\\U00D7W\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  X  -> M  [label="a = %s",  fontsize=9, penwidth=2.0, color="#2196A6"]
  M  -> Y  [label="b = %s",  fontsize=9, penwidth=2.0, color="#1A3A5C"]
  X  -> Y  [label="c\\U02B9= %s", fontsize=9, style=dashed, color="#999999"]
  W  -> M  [label="p4=%s",   fontsize=9, color="#E07B39"]
  XW -> M  [label="p5=%s",   fontsize=9, color="#E07B39"]
  W  -> Y  [label="p6=%s",   fontsize=9, color="#E07B39"]
  MW -> Y  [label="p7=%s",   fontsize=9, color="#E07B39"]
}', xl, ml, yl, wl, a_lbl, b_lbl, cp_lbl,
    fmt(p$p4), fmt(p$p5), fmt(p$p6), fmt(p$p7))

      } else if (mk == "D") {
        zl <- "Z"
        gv <- sprintf('
digraph CoMe_D {
  graph [rankdir=LR, bgcolor=white, fontname="Helvetica", splines=curved]
  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12]
  X   [label="%s\\n(Predictor)",   fillcolor="#2196A6", fontcolor=white]
  M   [label="%s\\n(Mediator)",    fillcolor="#1A3A5C", fontcolor=white]
  Y   [label="%s\\n(Outcome)",     fillcolor="#1E6438", fontcolor=white]
  W   [label="%s\\n(Moderator 1)", fillcolor="#E07B39", fontcolor=white, shape=diamond]
  Z   [label="%s\\n(Moderator 2)", fillcolor="#9B59B6", fontcolor=white, shape=diamond]
  XW  [label="X\\U00D7W\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  MZ  [label="M\\U00D7Z\\n(Int.)", fillcolor="#9B59B6", fontcolor=white, fontsize=9]
  X  -> M  [label="a = %s",  fontsize=9, penwidth=2.0, color="#2196A6"]
  M  -> Y  [label="b = %s",  fontsize=9, penwidth=2.0, color="#1A3A5C"]
  X  -> Y  [label="c\\U02B9= %s", fontsize=9, style=dashed, color="#999999"]
  W  -> M  [label="p4=%s",   fontsize=9, color="#E07B39"]
  XW -> M  [label="p5=%s",   fontsize=9, color="#E07B39"]
  Z  -> Y  [label="p6=%s",   fontsize=9, color="#9B59B6"]
  MZ -> Y  [label="p7=%s",   fontsize=9, color="#9B59B6"]
}', xl, ml, yl, wl, zl, a_lbl, b_lbl, cp_lbl,
    fmt(p$p4), fmt(p$p5), fmt(p$p6), fmt(p$p7))

      } else {  # Model E
        gv <- sprintf('
digraph CoMe_E {
  graph [rankdir=LR, bgcolor=white, fontname="Helvetica", splines=curved]
  node  [shape=box, style="rounded,filled", fontname="Helvetica", fontsize=12]
  X   [label="%s\\n(Predictor)",   fillcolor="#2196A6", fontcolor=white]
  M   [label="%s\\n(Mediator)",    fillcolor="#1A3A5C", fontcolor=white]
  Y   [label="%s\\n(Outcome)",     fillcolor="#1E6438", fontcolor=white]
  XM  [label="M\\U00D7X\\n(Int.)", fillcolor="#E07B39", fontcolor=white, fontsize=9]
  X  -> M  [label="a = %s",  fontsize=9, penwidth=2.0, color="#2196A6"]
  M  -> Y  [label="b = %s",  fontsize=9, penwidth=2.0, color="#1A3A5C"]
  X  -> Y  [label="c\\U02B9= %s", fontsize=9, style=dashed, color="#999999"]
  XM -> Y  [label="p4=%s",   fontsize=9, color="#E07B39"]
}', xl, ml, yl, a_lbl, b_lbl, cp_lbl, fmt(p$p4))
      }

      DiagrammeR::grViz(gv)
    })

    # ── Bootstrap CI Forest Plot ──────────────────────────────────────────
    output$boot_forest <- renderPlotly({
      req(come_result())
      res  <- come_result()
      lvls <- c("Low W (−1 SD)", "Mean W (0)", "High W (+1 SD)")
      keys <- c("Low", "Mean", "High")
      est  <- sapply(keys, function(k) res$orig_ie[[k]])
      lo   <- sapply(keys, function(k) res$ci_ie[[k]]$lower)
      hi   <- sapply(keys, function(k) res$ci_ie[[k]]$upper)
      sig  <- lo > 0 | hi < 0
      cols <- ifelse(sig, GREEN, "#C0392B")

      plot_ly(
        y         = factor(lvls, levels = rev(lvls)),
        x         = est,
        type      = "scatter",
        mode      = "markers",
        marker    = list(size = 14, color = cols, symbol = "diamond"),
        error_x   = list(
          type       = "data",
          symmetric  = FALSE,
          array      = hi - est,
          arrayminus = est - lo,
          color      = cols,
          thickness  = 2.5,
          width      = 10
        ),
        text      = paste0(
          lvls, "<br>",
          "IE = ", round(est, 4), "<br>",
          res$ci_level, "% CI [", round(lo, 4), ", ", round(hi, 4), "]<br>",
          ifelse(sig, "✓ Significant", "✗ Non-significant")
        ),
        hoverinfo = "text",
        showlegend = FALSE
      ) |>
        add_segments(
          x = 0, xend = 0, y = 0.4, yend = length(lvls) + 0.6,
          line = list(color = "red", dash = "dash", width = 1.5),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        layout(
          title = list(
            text = paste0(
              "Bootstrap CI Forest — Conditional Indirect Effects<br>",
              "<sup>", res$ci_level, "% CI | ", res$n_boot,
              " bootstrap resamples | Model ", res$model_key, "</sup>"
            ),
            font = list(color = NAVY, size = 14)
          ),
          xaxis = list(title = "Indirect Effect (a×b)", zeroline = FALSE),
          yaxis = list(title = ""),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          margin = list(l = 140)
        )
    })

    # ── Simple Slopes Interaction Plot ────────────────────────────────────
    output$simple_slopes <- renderPlotly({
      req(come_result())
      res    <- come_result()
      p      <- res$orig_p
      mk     <- res$model_key
      xl_lbl <- res$X_label %||% "X"
      yl_lbl <- res$Y_label %||% "Y"
      wl_lbl <- res$W_label %||% "W"

      x_seq <- seq(-2, 2, length.out = 50)   # ±2 SD of X

      w_specs <- list(
        list(val = -res$W_sd, label = paste0(wl_lbl, " Low (−1 SD)"),   col = TEAL,  dash = "solid"),
        list(val =  0,        label = paste0(wl_lbl, " Mean"),           col = "#888888", dash = "dot"),
        list(val =  res$W_sd, label = paste0(wl_lbl, " High (+1 SD)"),  col = AMBER, dash = "dash")
      )

      fig <- plot_ly()

      for (ws in w_specs) {
        # w_c = mean-centred W value for this level
        w_c <- ws$val - res$W_mean

        # Total effect of X on Y (direct + indirect) at this W level
        slope <- switch(mk,
          "A" = p$p3 + (p$p1 + p$p5 * w_c) * p$p2,
          "B" = p$p3 + p$p1 * (p$p2 + p$p5 * w_c),
          "C" = p$p3 + (p$p1 + p$p5 * w_c) * (p$p2 + p$p7 * w_c),
          "D" = p$p3 + (p$p1 + p$p5 * w_c) * p$p2,
          "E" = p$p3 + p$p1 * (p$p2 + p$p4 * w_c),
          p$p3
        )
        y_vals <- slope * x_seq

        fig <- fig |> add_lines(
          x    = x_seq,
          y    = y_vals,
          name = ws$label,
          line = list(color = ws$col, width = 2.5, dash = ws$dash),
          hovertemplate = paste0(
            ws$label, "<br>",
            xl_lbl, " = %{x:.2f}<br>",
            "Total ", yl_lbl, " = %{y:.4f}",
            "<extra></extra>"
          )
        )
      }

      fig |> layout(
        title = list(
          text = paste0(
            "Simple Slopes — Total X→Y at Different W Levels<br>",
            "<sup>Direct + Indirect effect of ", xl_lbl, " on ", yl_lbl,
            " (CoMe Model ", mk, ")</sup>"
          ),
          font = list(color = NAVY, size = 14)
        ),
        xaxis = list(
          title     = paste0(xl_lbl, " (standardised units)"),
          zeroline  = TRUE,
          zerolinecolor = "#dddddd",
          zerolinewidth = 1
        ),
        yaxis = list(
          title     = paste0("Predicted ", yl_lbl),
          zeroline  = TRUE,
          zerolinecolor = "#dddddd",
          zerolinewidth = 1
        ),
        legend        = list(orientation = "h", y = -0.18),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        hovermode     = "x unified"
      )
    })

    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r11 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k11 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. CoMe
    observeEvent(input$ai_btn_come, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k11()); return() }
      ctx <- tryCatch({
        cr <- come_result(); req(cr)
        paste0("CoMe ANALYSIS RESULTS\n", paste(head(capture.output(print(cr)), 60), collapse="\n"))
      }, error=function(e) "Please run CoMe Analysis first.")
      output$ai_output <- renderUI({ .ai_r11(call_gemini(paste0(
        "You are an expert in complementary mediation analysis writing for a top-tier management journal.\n\n",
        "Task: DETAILED interpretation of Complementary Mediation (CoMe) results. Include:\n",
        "1. Describe the CoMe framework: X → M → Y (mediation) plus X × W → Y (moderation)\n",
        "2. Report the direct effect (c'): β, SE, t, p, 95% CI — is it significant after mediation?\n",
        "3. Report indirect effect (a × b): point estimate, bootstrapped SE, 95% CI [LL, UL]\n",
        "4. Apply Baron & Kenny (1986) and Hayes (2013) criteria for mediation type:\n",
        "   - Full mediation: c non-significant; c' significant; a, b significant\n",
        "   - Partial mediation: both c and c' significant\n",
        "   - No mediation: indirect effect CI includes zero\n",
        "5. Report proportion mediated (PM = indirect/total effect) and interpret\n",
        "6. If moderated mediation: report index of moderated mediation and its 95% CI\n",
        "7. Discuss theoretical implications: what mechanism does M represent?\n",
        "8. Write complete APA-style Results paragraph using Hayes (2018) PROCESS notation\n\n",
        "Formal academic prose (6-7 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Johnson-Neyman
    observeEvent(input$ai_btn_jn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k11()); return() }
      ctx <- tryCatch({
        jnd <- jn_data(); req(jnd)
        paste0("JOHNSON-NEYMAN FLOODLIGHT ANALYSIS\n",
               "Moderator range: ", round(min(jnd$w, na.rm=TRUE), 3), " to ", round(max(jnd$w, na.rm=TRUE), 3),
               "\n\nSignificance regions:\n",
               paste(head(capture.output(print(jnd)), 20), collapse="\n"))
      }, error=function(e) "Please run analysis first.")
      output$ai_output <- renderUI({ .ai_r11(call_gemini(paste0(
        "You are an expert in moderation analysis writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of Johnson-Neyman floodlight analysis. Include:\n",
        "1. Explain what floodlight analysis reveals: where on the moderator continuum the effect is significant\n",
        "2. Report the exact J-N significance transition points (values of W where effect changes significance)\n",
        "3. Describe the region of significance: above/below what moderator value is the effect significant?\n",
        "4. Report the percentage of the sample in the significant vs non-significant region\n",
        "5. Discuss what the transition points mean theoretically\n",
        "6. Compare to traditional pick-a-point moderation (low/medium/high) — does floodlight add nuance?\n",
        "7. Write APA-style Results paragraph\n\n",
        "Formal academic prose (4-5 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Full CoMe Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k11()); return() }
      ctx <- tryCatch({
        cr <- come_result(); req(cr)
        jnd <- tryCatch(jn_data(), error=function(e) NULL)
        jn_txt <- if (!is.null(jnd)) paste0("\n\nJ-N data range: ", round(range(jnd$w, na.rm=TRUE), 3)) else ""
        paste0("CoMe FULL ANALYSIS\n", paste(head(capture.output(print(cr)), 80), collapse="\n"), jn_txt)
      }, error=function(e) "Please run CoMe Analysis first.")
      output$ai_output <- renderUI({ .ai_r11(call_gemini(paste0(
        "You are an expert in moderated mediation writing for a top-tier management journal.\n\n",
        "Task: Write a COMPREHENSIVE Complementary Mediation Results section. Include:\n",
        "1. Direct effect and its significance\n",
        "2. Indirect effect (mediation): a-path, b-path, a×b with bootstrapped CI\n",
        "3. Type of mediation (full/partial/none) with theoretical interpretation\n",
        "4. Moderation effect: interaction term, simple slopes at low/mean/high moderator\n",
        "5. Moderated mediation (index of moderated mediation and CI)\n",
        "6. Johnson-Neyman floodlight: significance regions on moderator continuum\n",
        "7. Theoretical narrative: what does the CoMe pattern mean for theory?\n\n",
        "Use APA 7 and Hayes (2018) standards. Formal academic prose (7-9 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k11()); return() }
      NULL # (use buttons above for specific analyses)
    }, ignoreInit = TRUE)

    # ══════════════════════════════════════════════════════════════════════

  })
}
