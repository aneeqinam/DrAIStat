# ── Module 12: Bibliometric Analysis (Advanced) ──────────────────────────────
# Full science mapping equivalent to biblioshiny
# Trends · Co-citation · Keyword Networks · Collaboration · Bradford · Lotka
# Dr. Aneeq Inam

mod12_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hero_html("📚", "Bibliometric Analysis",
              "Full science mapping: Trends · Co-citation · Keyword Networks · Collaboration · Bradford's & Lotka's Laws"),

    tags$div(style="background:#EBF4F7;padding:1rem;border-radius:8px;margin-bottom:1rem;",
      fluidRow(
        column(6,
          tags$b("📁 Supported formats:"),
          tags$ul(style="margin-bottom:0;margin-top:4px;",
            tags$li("Scopus → Export as CSV (select ALL fields + Cited References)"),
            tags$li("Web of Science → Plain text or Tab-delimited (Full Record + Cited Refs)"),
            tags$li("Lens.org / PubMed → CSV export"),
            tags$li("Any BibTeX (.bib) file")
          )
        ),
        column(6,
          tags$b("💡 Tips for best results:"),
          tags$ul(style="margin-bottom:0;margin-top:4px;",
            tags$li("Files up to 500 MB supported"),
            tags$li("Always include Cited References for co-citation networks"),
            tags$li("200+ records recommended for meaningful network visualisations"),
            tags$li("If auto-detect fails, select the format manually then click Reload")
          )
        )
      )
    ),

    fluidRow(
      column(5,
        fileInput(ns("file"), "Upload bibliometric file (.csv / .bib / .txt)",
                  accept=c(".csv",".bib",".txt"), width="100%")
      ),
      column(3, uiOutput(ns("db_select_ui"))),
      column(2, uiOutput(ns("fmt_select_ui"))),
      column(2, tags$div(style="margin-top:25px;", uiOutput(ns("reload_ui"))))
    ),

    uiOutput(ns("load_msg_ui")),
    uiOutput(ns("main_ui")),
  )
}

mod12_server <- function(id, gemini_key = reactive("")) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Selectors ────────────────────────────────────────────────────────────
    output$db_select_ui <- renderUI({
      req(input$file)
      selectInput(ns("db_type"), "Database source",
                  choices=c("Scopus"="scopus","Web of Science"="wos",
                            "Lens.org"="lens","PubMed"="pubmed"),
                  selected="scopus")
    })

    output$fmt_select_ui <- renderUI({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      sel <- if (ext=="bib") "bibtex" else "csv"
      selectInput(ns("fmt"), "File format",
                  choices=c("CSV"="csv","BibTeX"="bibtex","Plain text"="plaintext"),
                  selected=sel)
    })

    output$reload_ui <- renderUI({
      req(input$file)
      actionButton(ns("reload"), "↺ Reload", class="btn-default btn-sm")
    })

    # ── Data Loading ─────────────────────────────────────────────────────────
    bib_data <- reactive({
      req(input$file, input$db_type, input$fmt)
      input$reload
      withProgress(message="Loading bibliometric data…", value=0.3, {
        path <- input$file$datapath
        db   <- input$db_type
        fmt  <- input$fmt
        M <- tryCatch(convert2df(path, dbsource=db, format=fmt), error=function(e) NULL)
        if (is.null(M)) {
          for (f in c("csv","bibtex","plaintext")) {
            if (f==fmt) next
            M <- tryCatch(convert2df(path, dbsource=db, format=f), error=function(e) NULL)
            if (!is.null(M)) break
          }
        }
        setProgress(1)
        M
      })
    })

    output$load_msg_ui <- renderUI({
      req(input$file)
      M <- bib_data()
      if (is.null(M)) {
        tags$div(class="alert alert-danger",
          "⚠️ Could not load file. Check the database source and format, then click ↺ Reload.")
      } else {
        tags$div(class="alert alert-success", style="padding:8px 12px;",
          sprintf("✅ Loaded %d records successfully.", nrow(M)))
      }
    })

    bib_results <- reactive({
      req(bib_data())
      withProgress(message="Running bibliometric analysis…", {
        tryCatch(biblioAnalysis(bib_data(), sep=";"), error=function(e) NULL)
      })
    })

    # ── KPIs ─────────────────────────────────────────────────────────────────
    get_kpis <- reactive({
      req(bib_data(), bib_results())
      M   <- bib_data()
      res <- bib_results()
      n_docs    <- nrow(M)
      n_auth    <- tryCatch(length(res$Authors),  error=function(e) NA)
      n_sources <- tryCatch(length(res$Sources),  error=function(e) NA)
      tc   <- if ("TC" %in% names(M)) sum(M$TC, na.rm=TRUE) else NA
      avg_tc <- if (!is.na(tc) && n_docs>0) round(tc/n_docs,2) else NA
      collab_idx <- tryCatch({
        ac <- sapply(strsplit(M$AU,";"), length)
        round(mean(ac, na.rm=TRUE),2)
      }, error=function(e) NA)
      yr_range <- if ("PY" %in% names(M)) {
        yr <- range(M$PY, na.rm=TRUE)
        paste(yr[1],"–",yr[2])
      } else NA
      cagr <- tryCatch({
        ann_raw <- res$AnnualProduction
        if (inherits(ann_raw, "table") || is.numeric(ann_raw)) {
          ann <- data.frame(Year=as.integer(names(ann_raw)),
                            Articles=as.integer(ann_raw), stringsAsFactors=FALSE)
        } else {
          ann <- as.data.frame(ann_raw, stringsAsFactors=FALSE)
          if (ncol(ann)>=2) names(ann)[1:2] <- c("Year","Articles")
          ann$Year     <- suppressWarnings(as.integer(as.character(ann$Year)))
          ann$Articles <- suppressWarnings(as.integer(ann$Articles))
        }
        ann <- ann[!is.na(ann$Year) & !is.na(ann$Articles) & ann$Articles>0,]
        ann <- ann[order(ann$Year),]
        if (nrow(ann)>=2) {
          n_yrs <- max(ann$Year)-min(ann$Year)
          round((ann$Articles[nrow(ann)]/ann$Articles[1])^(1/n_yrs)-1,4)*100
        } else NA
      }, error=function(e) NA)
      list(n_docs=n_docs, n_auth=n_auth, n_sources=n_sources,
           tc=tc, avg_tc=avg_tc, collab_idx=collab_idx,
           yr_range=yr_range, cagr=cagr)
    })

    # ── Main UI ───────────────────────────────────────────────────────────────
    output$main_ui <- renderUI({
      req(bib_data())
      if (is.null(bib_data())) return(NULL)
      kpis <- get_kpis()
      cagr_label <- if (!is.na(kpis$cagr)) paste0(round(kpis$cagr,1),"%") else "N/A"
      tagList(
        fluidRow(
          valueBox(format(kpis$n_docs,big.mark=","),    "Documents",            icon=icon("file-alt"),    color="blue",   width=3),
          valueBox(format(kpis$n_auth,big.mark=","),    "Authors",              icon=icon("users"),       color="teal",   width=3),
          valueBox(format(kpis$n_sources,big.mark=","), "Sources / Journals",   icon=icon("newspaper"),   color="olive",  width=3),
          valueBox(kpis$yr_range,                       "Year Range",           icon=icon("calendar"),    color="navy",   width=3)
        ),
        fluidRow(
          valueBox(format(kpis$tc,big.mark=","),        "Total Citations",      icon=icon("quote-right"), color="maroon", width=3),
          valueBox(kpis$avg_tc,                         "Avg Citations / Paper", icon=icon("chart-bar"),  color="purple", width=3),
          valueBox(kpis$collab_idx,                     "Collaboration Index",  icon=icon("handshake"),   color="orange", width=3),
          valueBox(cagr_label,                          "Annual Growth (CAGR)", icon=icon("arrow-up"),    color="green",  width=3)
        ),

        tabBox(width=12,

          # ── Tab 1: Production Trends ────────────────────────────────────────
          tabPanel("📈 Publication Trends",
            fluidRow(
              column(3, selectInput(ns("trend_type"), "Chart type",
                                   choices=c("Area"="area","Bar"="bar","Line"="line"),
                                   selected="area")),
              column(3, checkboxInput(ns("show_growth"), "Show growth rate chart", value=TRUE)),
              column(3, br(),
                actionButton(ns("run_trend"), "📈 Plot Trend",
                             class="btn btn-primary btn-sm",
                             style="margin-top:4px; width:100%;"))
            ),
            plotlyOutput(ns("trend_plot"),  height="370px"),
            conditionalPanel(condition=paste0("input['",ns("show_growth"),"']===true"),
              plotlyOutput(ns("growth_plot"), height="230px")
            ),
            br(),
            DT::dataTableOutput(ns("trend_tbl"))
          ),

          # ── Tab 2: Authors ──────────────────────────────────────────────────
          tabPanel("👤 Authors",
            fluidRow(
              column(3, numericInput(ns("n_authors"), "Top N authors", value=20, min=5, max=100)),
              column(3, selectInput(ns("author_metric"), "Rank by",
                                   choices=c("Publications"="pubs","Total Citations"="tc"),
                                   selected="pubs"))
            ),
            plotlyOutput(ns("author_plot"), height="480px"),
            br(),
            DT::dataTableOutput(ns("author_tbl"))
          ),

          # ── Tab 3: Most Cited Papers ────────────────────────────────────────
          tabPanel("🏆 Most Cited Papers",
            fluidRow(column(3, numericInput(ns("n_cited"), "Show top N papers", value=25, min=5, max=200))),
            DT::dataTableOutput(ns("cited_tbl"))
          ),

          # ── Tab 4: Keywords ─────────────────────────────────────────────────
          tabPanel("🔑 Keywords",
            fluidRow(
              column(3, numericInput(ns("n_kw"), "Top N keywords", value=30, min=5, max=200)),
              column(3, selectInput(ns("kw_type"), "Keyword type",
                                   choices=c("Author Keywords (DE)"="DE",
                                             "Index Keywords (ID)"="ID",
                                             "Both combined"="both"),
                                   selected="DE"))
            ),
            plotlyOutput(ns("kw_plot"), height="520px"),
            br(),
            DT::dataTableOutput(ns("kw_tbl"))
          ),

          # ── Tab 5: Keyword Co-occurrence Network ─────────────────────────────
          tabPanel("🕸️ Keyword Network",
            tags$div(style="background:#FFF8E1;padding:.7rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              "Nodes = keywords; edges = papers where both appear together. Clusters reveal research themes."),
            fluidRow(
              column(3, numericInput(ns("kw_net_n"),   "Max keywords",   value=50, min=10, max=300)),
              column(3, numericInput(ns("kw_net_min"), "Min occurrences", value=3,  min=1)),
              column(3, selectInput(ns("kw_net_type"), "Layout",
                                   choices=c("Fruchterman-Reingold"="fruchterman",
                                             "Kamada-Kawai"="kamada",
                                             "MDS"="mds","Circular"="circle"),
                                   selected="fruchterman")),
              column(3, tags$div(style="margin-top:25px;",
                       actionButton(ns("run_kw_net"), "Generate Network", class="btn-primary")))
            ),
            withSpinner(plotOutput(ns("kw_net_plot"), height="620px")),
            br(),
            withSpinner(DT::dataTableOutput(ns("kw_net_tbl")))
          ),

          # ── Tab 6: Sources & Bradford's Law ─────────────────────────────────
          tabPanel("📰 Sources & Bradford's Law",
            tabsetPanel(
              tabPanel("Top Sources",
                fluidRow(column(3, numericInput(ns("n_journals"), "Top N sources", value=15, min=5, max=100))),
                plotlyOutput(ns("journal_plot"), height="420px"),
                br(),
                DT::dataTableOutput(ns("journal_tbl"))
              ),
              tabPanel("Bradford's Law",
                tags$div(style="background:#EBF4F7;padding:.75rem 1rem;border-radius:6px;margin-bottom:.75rem;",
                  tags$b("Bradford's Law:"), " Journals are divided into zones of equal productivity. ",
                  "Zone 1 (core) contains the fewest journals but the most articles. ",
                  "Useful for identifying the most impactful journals in your field."
                ),
                actionButton(ns("run_bradford"), "Run Bradford's Law Analysis", class="btn-info"),
                br(), br(),
                withSpinner(plotOutput(ns("bradford_plot"), height="420px")),
                br(),
                withSpinner(DT::dataTableOutput(ns("bradford_tbl")))
              )
            )
          ),

          # ── Tab 7: Country Analysis ──────────────────────────────────────────
          tabPanel("🌍 Country Analysis",
            tabsetPanel(
              tabPanel("Production by Country",
                plotlyOutput(ns("country_plot"), height="470px"),
                br(),
                DT::dataTableOutput(ns("country_tbl"))
              ),
              tabPanel("🗺️ World Map",
                fluidRow(
                  column(4, tags$div(style="margin-top:5px;",
                    actionButton(ns("run_country_map"), "🗺️ Generate World Map",
                                 class="btn btn-primary btn-sm",
                                 style="width:100%;"))),
                  column(8, tags$p(style="margin-top:8px; color:#888; font-size:0.88em;",
                    "Choropleth map of scientific production by country. Upload data and click Generate."))
                ),
                br(),
                plotlyOutput(ns("country_map"), height="500px"),
                br(),
                h4("International Collaboration Statistics",
                   style="margin-top:10px; color:#1A3A5C;"),
                DT::dataTableOutput(ns("collab_stats_tbl"))
              ),
              tabPanel("Country Collaboration Network",
                fluidRow(
                  column(3, numericInput(ns("country_n"), "Max countries", value=30, min=5, max=100)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_country_net"), "Generate Network", class="btn-primary")))
                ),
                withSpinner(plotOutput(ns("country_net_plot"), height="580px"))
              )
            )
          ),

          # ── Tab 8: Author Collaboration Network ──────────────────────────────
          tabPanel("👥 Author Collaboration",
            tags$div(style="background:#FFF8E1;padding:.7rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              "Nodes = authors; edges = papers co-authored together. Node size proportional to number of publications."),
            fluidRow(
              column(3, numericInput(ns("auth_net_n"), "Max authors", value=30, min=5, max=150)),
              column(3, tags$div(style="margin-top:25px;",
                       actionButton(ns("run_auth_net"), "Generate Network", class="btn-primary")))
            ),
            withSpinner(plotOutput(ns("auth_net_plot"), height="620px")),
            br(),
            withSpinner(DT::dataTableOutput(ns("auth_net_tbl")))
          ),

          # ── Tab 9: Co-Citation Analysis ──────────────────────────────────────
          tabPanel("🔄 Co-Citation Analysis",
            tabsetPanel(
              tabPanel("Reference Co-citation",
                tags$div(style="background:#F9F9F9;padding:.6rem 1rem;border-radius:6px;margin-bottom:.6rem;font-size:.9rem;",
                  "Papers are linked when they share cited references. Reveals intellectual structure of the field."),
                fluidRow(
                  column(3, numericInput(ns("cocite_n"),   "Max nodes",       value=50, min=10, max=200)),
                  column(3, numericInput(ns("cocite_min"), "Min co-citations", value=3,  min=1)),
                  column(3, selectInput(ns("cocite_type"), "Layout",
                                       choices=c("Kamada-Kawai"="kamada",
                                                 "Fruchterman-Reingold"="fruchterman",
                                                 "MDS"="mds"),
                                       selected="kamada")),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_cocite"), "Generate Network", class="btn-primary")))
                ),
                withSpinner(plotOutput(ns("cocite_plot"), height="620px")),
                br(),
                withSpinner(DT::dataTableOutput(ns("cocite_tbl")))
              ),
              tabPanel("Author Co-citation",
                tags$div(style="background:#F9F9F9;padding:.6rem 1rem;border-radius:6px;margin-bottom:.6rem;font-size:.9rem;",
                  "Authors are linked when they are both cited in the same papers. Reveals key intellectual contributors."),
                fluidRow(
                  column(3, numericInput(ns("auth_cocite_n"), "Max authors", value=30, min=10, max=150)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_auth_cocite"), "Generate Network", class="btn-primary")))
                ),
                withSpinner(plotOutput(ns("auth_cocite_plot"), height="580px"))
              ),
              tabPanel("Source Co-citation",
                tags$div(style="background:#F9F9F9;padding:.6rem 1rem;border-radius:6px;margin-bottom:.6rem;font-size:.9rem;",
                  "Journals are linked when they are both cited together. Shows the core literature of your field."),
                fluidRow(
                  column(3, numericInput(ns("src_cocite_n"), "Max sources", value=30, min=10, max=150)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_src_cocite"), "Generate Network", class="btn-primary")))
                ),
                withSpinner(plotOutput(ns("src_cocite_plot"), height="580px"))
              ),
              tabPanel("Bibliographic Coupling",
                tags$div(style="background:#F9F9F9;padding:.6rem 1rem;border-radius:6px;margin-bottom:.6rem;font-size:.9rem;",
                  "Papers sharing many references are coupled. Reveals current research fronts."),
                fluidRow(
                  column(3, numericInput(ns("coupling_n"), "Max papers", value=40, min=10, max=150)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_coupling"), "Generate Network", class="btn-primary")))
                ),
                withSpinner(plotOutput(ns("coupling_plot"), height="580px"))
              )
            )
          ),

          # ── Tab 10: Thematic Map ─────────────────────────────────────────────
          tabPanel("🗺️ Thematic Map",
            fluidRow(
              column(3, numericInput(ns("thematic_min"), "Min keyword freq", value=3, min=1)),
              column(3, selectInput(ns("thematic_field"), "Keyword field",
                                   choices=c("Author Keywords"="DE",
                                             "Index Keywords"="ID",
                                             "Title Words"="TI"),
                                   selected="DE")),
              column(3, numericInput(ns("thematic_n"), "Max keywords", value=500, min=50, max=2000)),
              column(3, tags$div(style="margin-top:25px;",
                       actionButton(ns("run_thematic"), "Generate Thematic Map", class="btn-primary")))
            ),
            withSpinner(plotOutput(ns("thematic_plot"), height="620px")),
            uiOutput(ns("thematic_interp"))
          ),

          # ── Tab 11: Lotka's Law ──────────────────────────────────────────────
          tabPanel("📐 Lotka's Law",
            tags$div(style="background:#EBF4F7;padding:.75rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              tags$b("Lotka's Law:"), " Describes author productivity in a scientific field. ",
              "Theoretical prediction: ~80% of papers come from ~20% of authors. ",
              "Beta ≈ 2 is the classic Lotka coefficient."
            ),
            actionButton(ns("run_lotka"), "Run Lotka's Law Analysis", class="btn-info"),
            br(), br(),
            withSpinner(plotOutput(ns("lotka_plot"), height="420px")),
            br(),
            withSpinner(verbatimTextOutput(ns("lotka_text")))
          ),

          # ── Tab 12: Historiograph ─────────────────────────────────────────────
          tabPanel("📜 Historiograph",
            tags$div(style="background:#EBF4F7;padding:.75rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              tags$b("Historiograph (Direct Citation Network):"), " Traces the intellectual lineage ",
              "of research by connecting papers through direct citation chains. Reveals the ",
              "pivotal works and evolutionary path of the field over time."
            ),
            fluidRow(
              column(3, numericInput(ns("histo_n"), "Top N papers", value=25, min=5, max=100)),
              column(3, numericInput(ns("histo_min_cite"), "Min citations", value=3, min=1)),
              column(3, tags$div(style="margin-top:25px;",
                       actionButton(ns("run_histo"), "📜 Generate Historiograph",
                                    class="btn btn-primary btn-sm", style="width:100%;")))
            ),
            withSpinner(plotOutput(ns("histo_plot"), height="640px")),
            br(),
            DT::dataTableOutput(ns("histo_tbl"))
          ),

          # ── Tab 13: Citation Analysis ─────────────────────────────────────────
          tabPanel("📊 Citation Analysis",
            tags$div(style="background:#EBF4F7;padding:.75rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              tags$b("Citation Metrics:"), " Tracks H-index growth, total citations per year, ",
              "citations per paper, and identifies citation bursts — papers with sudden spikes in scholarly attention."
            ),
            actionButton(ns("run_citations"), "📊 Compute Citation Metrics",
                         class="btn btn-primary btn-sm"),
            br(), br(),
            fluidRow(
              column(6, plotlyOutput(ns("cit_per_year_plot"), height="300px")),
              column(6, plotlyOutput(ns("h_index_plot"),       height="300px"))
            ),
            br(),
            h4("Top Cited Papers with Citation Burst Detection",
               style="color:#1A3A5C; margin-top:5px;"),
            DT::dataTableOutput(ns("citation_tbl"))
          ),

          # ── Tab 14: Conceptual Structure ──────────────────────────────────────
          tabPanel("🔬 Conceptual Structure",
            tags$div(style="background:#EBF4F7;padding:.75rem 1rem;border-radius:6px;margin-bottom:.75rem;",
              tags$b("Conceptual Structure Map (MCA):"), " Applies Multiple Correspondence Analysis ",
              "to keywords, revealing underlying intellectual clusters and conceptual groupings in the literature."
            ),
            fluidRow(
              column(3, numericInput(ns("cs_min_freq"), "Min keyword freq", value=3, min=1)),
              column(3, selectInput(ns("cs_field"), "Keyword field",
                                   choices=c("Author Keywords"="DE",
                                             "Index Keywords"="ID"),
                                   selected="DE")),
              column(3, numericInput(ns("cs_n_clust"), "Clusters", value=5, min=2, max=10)),
              column(3, tags$div(style="margin-top:25px;",
                       actionButton(ns("run_cs"), "🔬 Generate Map",
                                    class="btn btn-primary btn-sm", style="width:100%;")))
            ),
            withSpinner(plotOutput(ns("cs_plot"), height="600px")),
            br(),
            DT::dataTableOutput(ns("cs_clusters_tbl"))
          ),

          # ── Tab 15: Summary Statistics ───────────────────────────────────────
          tabPanel("📋 Summary Statistics",
            verbatimTextOutput(ns("bib_summary"))
          ),

          # ── Tab 16: Download All ─────────────────────────────────────────────
          tabPanel("📥 Download All",
            tags$h4("Export Results to Excel"),
            tags$p("Download a comprehensive Excel workbook with all analysis results in separate sheets."),
            tags$div(style="display:flex;gap:1rem;flex-wrap:wrap;margin-top:1rem;",
              downloadButton(ns("dl_all"),      "📊 Full Report — All Sheets (Excel)", class="btn-success btn-lg"),
              downloadButton(ns("dl_authors"),  "👤 Authors"),
              downloadButton(ns("dl_keywords"), "🔑 Keywords"),
              downloadButton(ns("dl_journals"), "📰 Journals"),
              downloadButton(ns("dl_cited"),    "🏆 Most Cited Papers"),
              downloadButton(ns("dl_summary"),  "📋 Annual Production")
            )
          ),

          # ── Tab 17: Dataset Comparison ───────────────────────────────────────
          tabPanel("🔀 Dataset Comparison",
            tags$div(style="background:#EBF4F7;padding:.9rem;border-radius:7px;margin-bottom:1rem;",
              tags$b("Full side-by-side comparison of two bibliometric datasets. "),
              "All analyses available for a single dataset are reproduced here for both. ",
              "Upload Dataset 2 below, label both, then click Run."
            ),
            fluidRow(
              column(5,
                tags$h5("Dataset 2 Upload"),
                fileInput(ns("file2"), "Upload 2nd bibliometric file (.csv/.bib/.txt)",
                          accept=c(".csv",".bib",".txt"), width="100%"),
                selectInput(ns("db2_type"), "Database (Dataset 2)",
                            choices=c("Scopus"="scopus","Web of Science"="wos","Lens.org"="lens"),
                            selected="scopus")
              ),
              column(4,
                tags$h5("Labels"),
                textInput(ns("ds1_label"), "Dataset 1 label", value="Dataset 1"),
                textInput(ns("ds2_label"), "Dataset 2 label", value="Dataset 2")
              ),
              column(3, br(), br(),
                numericInput(ns("cmp_top_n"), "Top N items", value=15, min=5, max=50),
                actionButton(ns("run_compare"), "🔀 Run Full Comparison",
                             class="btn-primary btn-block", style="margin-top:6px;")
              )
            ),
            uiOutput(ns("compare_ui"))
          ),

          # ── Tab 18: Time Horizon Analysis ───────────────────────────────────
          tabPanel("⏱️ Time Horizons",
            tags$div(style="background:#EBF4F7;padding:.9rem;border-radius:7px;margin-bottom:1rem;",
              tags$b("Split your dataset into time windows — ALL analyses reproduced per period. "),
              "Define 2–4 time slices (e.g. 2000–2010, 2011–2020, 2021–present). ",
              "Every analysis tab (trends, authors, keywords, sources, citations, etc.) is available per period."
            ),
            fluidRow(
              column(3, numericInput(ns("th_n_periods"), "Number of periods", value=3, min=2, max=4)),
              column(3, numericInput(ns("th_top_n"), "Top N items", value=15, min=5, max=50)),
              column(3, br(), actionButton(ns("th_set_periods"), "Set Period Ranges",
                                           class="btn-info btn-sm", style="margin-top:25px;"))
            ),
            uiOutput(ns("th_period_ui")),
            br(),
            actionButton(ns("run_th"), "⏱️ Analyse All Time Horizons", class="btn-primary"),
            br(), br(),
            uiOutput(ns("th_results_ui"))
          ),
          # ── Tab 19: Interactive visNetwork ───────────────────────────────────
          tabPanel("🕸️ Interactive Networks",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Interactive Network Visualizations"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Drag nodes, zoom and pan, hover for details. ",
                "Node size = degree centrality. Edge thickness = co-occurrence strength. ",
                "Run the corresponding analysis (Keyword Network / Author Collaboration) first.")),
            tabsetPanel(
              tabPanel("🔑 Keyword Co-occurrence",
                tags$p(style="font-size:.85rem;color:#555;margin:.5rem 0 .3rem;",
                  "First run 'Generate Network' in the Keyword Network tab. Then click below."),
                fluidRow(
                  column(3, numericInput(ns("vis_kw_n"), "Max keywords", value = 40, min = 10, max = 200)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_vis_kw"), "🕸️ Build Interactive Network", class="btn-primary")))
                ),
                withSpinner(visNetwork::visNetworkOutput(ns("kw_vis_net"), height = "580px"))
              ),
              tabPanel("👥 Author Collaboration",
                tags$p(style="font-size:.85rem;color:#555;margin:.5rem 0 .3rem;",
                  "First run 'Generate Network' in the Author Collaboration tab. Then click below."),
                fluidRow(
                  column(3, numericInput(ns("vis_auth_n"), "Max authors", value = 30, min = 5, max = 100)),
                  column(3, tags$div(style="margin-top:25px;",
                           actionButton(ns("run_vis_auth"), "🕸️ Build Interactive Network", class="btn-primary")))
                ),
                withSpinner(visNetwork::visNetworkOutput(ns("auth_vis_net"), height = "580px"))
              )
            )
          ),

          # ── Tab 20: Growth Heatmap ───────────────────────────────────────────
          tabPanel("📅 Growth Heatmap",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Annual Publication Growth Heatmap"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Each cell shows the number of publications in a given year and document-type group. ",
                "Darker cells = higher output. Useful for spotting growth spurts and document-type shifts.")),
            withSpinner(plotlyOutput(ns("growth_heatmap"), height = "480px"))
          ),

          # ── Tab 21: Lollipop Charts ──────────────────────────────────────────
          tabPanel("🍭 Lollipop Charts",
            tags$div(style="background:#EBF4F7;padding:.7rem 1rem;border-radius:6px;margin-bottom:.8rem;",
              tags$b("Ranked Lollipop Charts"),
              tags$p(style="margin:.3rem 0 0;font-size:.85rem;",
                "Interactive lollipop charts for top authors, sources, and countries. ",
                "Hover for counts; click legend items to highlight groups.")),
            tabsetPanel(
              tabPanel("👤 Top Authors",
                fluidRow(column(3, numericInput(ns("lp_n_au"), "Top N authors", value = 20, min = 5, max = 50))),
                withSpinner(plotlyOutput(ns("author_lollipop"), height = "500px"))
              ),
              tabPanel("📰 Top Sources",
                fluidRow(column(3, numericInput(ns("lp_n_so"), "Top N sources", value = 20, min = 5, max = 50))),
                withSpinner(plotlyOutput(ns("source_lollipop"), height = "500px"))
              ),
              tabPanel("🌍 Top Countries",
                fluidRow(column(3, numericInput(ns("lp_n_co"), "Top N countries", value = 20, min = 5, max = 50))),
                withSpinner(plotlyOutput(ns("country_lollipop"), height = "500px"))
              )
            )
          ),

          # \u2550\u2550 Knowledge Streams \u2014 Auto-Clustering \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550
          tabPanel("\ud83e\udde0 Knowledge Streams",
            tags$div(
              style = "background:linear-gradient(135deg,#EBF5FB,#EAF7F0);border-left:5px solid #2196A6;padding:.85rem 1.1rem;border-radius:8px;margin-bottom:1rem;",
              tags$h5(style="color:#1a5276;margin:0 0 .35rem;", "\ud83d\udd2c AI-Powered Knowledge Stream Detection"),
              tags$p(style="font-size:.82rem;margin:0;color:#2c3e50;",
                "Automatically discovers hidden research themes from keyword co-occurrence using community detection algorithms, ",
                "then sends each cluster to Groq AI for intelligent naming, description, and academic interpretation. ",
                tags$b("Upload bibliometric data and click the button to begin."))
            ),
            fluidRow(
              column(3,
                selectInput(ns("ks_method"), "\ud83d\udd27 Clustering Algorithm",
                  choices = c(
                    "Louvain \u2014 fast & accurate (recommended)" = "louvain",
                    "Leiden \u2014 high resolution"                = "leiden",
                    "Walktrap \u2014 random walk"                  = "walktrap",
                    "Fast Greedy \u2014 hierarchical"              = "fastgreedy",
                    "Edge Betweenness \u2014 bridge detection"     = "edge_betweenness"
                  ))
              ),
              column(2, numericInput(ns("ks_min_freq"), "Min keyword freq.", value = 3, min = 1, max = 20)),
              column(2, numericInput(ns("ks_n_top"),   "Top N keywords",    value = 80, min = 20, max = 300)),
              column(2, numericInput(ns("ks_n_kw"),    "Keywords per cluster (cards)", value = 10, min = 5, max = 20)),
              column(3, br(),
                actionButton(ns("run_ks"), "\ud83e\udde0 Detect Knowledge Streams",
                  class = "btn-primary btn-block",
                  style = "font-weight:bold;font-size:.92rem;height:38px;"))
            ),
            tags$div(style="margin-top:.5rem;",
              tags$small(style="color:#7f8c8d;",
                "\ud83d\udca1 Tip: After clustering, enter your Groq key in the sidebar and click ",
                tags$b('"\ud83e\udd16 AI Name & Describe"'), " for intelligent cluster labels and academic descriptions.")
            ),
            hr(style="margin:.7rem 0;"),
            uiOutput(ns("ks_results_ui"))
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
              column(4, actionButton(ns("ai_btn_trends"), "📈 Publication Trends",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_authors"), "👤 Author Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(4, actionButton(ns("ai_btn_kw"), "🔑 Keyword Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;"))
            ),
            fluidRow(
              column(6, actionButton(ns("ai_btn_cocite"), "🔄 Co-Citation Analysis",
                                     class="btn-info btn-block", style="margin-bottom:.5rem;")),
              column(6, actionButton(ns("ai_btn_all"), "📋 Full Bibliometric Summary",
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

    # ═══════════════════════════════════════════════════════════════════════════
    # OUTPUT RENDERERS
    # ═══════════════════════════════════════════════════════════════════════════

    # ── 1. Publication Trends ─────────────────────────────────────────────────
    # Computed directly from raw PY column — works with any bibliometrix version
    trend_data <- reactive({
      input$run_trend   # button click forces refresh
      req(bib_data())
      M <- bib_data()

      tryCatch({
        # PY = Publication Year column (standard field in bibliometrix)
        if (is.null(M$PY)) return(NULL)
        yrs <- suppressWarnings(as.integer(as.character(M$PY)))
        cur_yr <- as.integer(format(Sys.Date(), "%Y"))
        yrs <- yrs[!is.na(yrs) & yrs >= 1900 & yrs <= cur_yr + 1]
        if (length(yrs) == 0) return(NULL)
        df <- as.data.frame(table(yrs), stringsAsFactors = FALSE)
        names(df) <- c("Year", "Articles")
        df$Year     <- as.integer(as.character(df$Year))
        df$Articles <- as.integer(df$Articles)
        df <- df[!is.na(df$Year) & df$Articles > 0, ]
        df[order(df$Year), ]
      }, error = function(e) NULL)
    })

    output$trend_plot <- renderPlotly({
      req(trend_data())
      ann  <- trend_data()
      type <- if (!is.null(input$trend_type)) input$trend_type else "area"
      if (type=="bar") {
        p <- plot_ly(ann, x=~Year, y=~Articles, type="bar",
                     marker=list(color=TEAL, line=list(color=NAVY, width=0.5)))
      } else {
        fill_val <- if (type=="area") "tozeroy" else "none"
        p <- plot_ly(ann, x=~Year, y=~Articles, type="scatter", mode="lines+markers",
                     fill=fill_val, fillcolor=paste0(TEAL,"33"),
                     line=list(color=TEAL, width=2.5), marker=list(color=NAVY, size=7))
      }
      p |> layout(title="Annual Scientific Production",
                  xaxis=list(title="Year"), yaxis=list(title="Documents"),
                  plot_bgcolor="white", paper_bgcolor="white")
    })

    output$growth_plot <- renderPlotly({
      req(trend_data())
      ann <- trend_data()
      if (nrow(ann)<2) return(plotly_empty())
      ann <- ann[ann$Articles>0,]
      ann$Growth <- c(NA, round(diff(ann$Articles)/head(ann$Articles,-1)*100, 1))
      ann <- ann[!is.na(ann$Growth),]
      if (nrow(ann)==0) return(plotly_empty())
      cols <- ifelse(ann$Growth>=0, "#27AE60", "#E74C3C")
      plot_ly(ann, x=~Year, y=~Growth, type="bar",
              marker=list(color=cols)) |>
        layout(title="Annual Growth Rate (%)",
               xaxis=list(title="Year"), yaxis=list(title="Growth (%)"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$trend_tbl <- DT::renderDataTable({
      req(trend_data())
      ann <- trend_data()
      ann <- ann[order(-ann$Year),]
      ann$Cumulative    <- cumsum(ann$Articles)
      ann$Growth_pct    <- c(NA, round(diff(rev(ann$Articles))/head(rev(ann$Articles),-1)*100,1))
      tool_dt(ann)
    })

    # ── 2. Authors ────────────────────────────────────────────────────────────
    author_df_full <- reactive({
      req(bib_results())
      res <- bib_results()
      auth <- res$Authors
      if (is.null(auth) || length(auth)==0) return(NULL)
      df <- data.frame(Author=names(auth), Publications=as.integer(auth),
                       stringsAsFactors=FALSE)
      if ("TC" %in% names(bib_data())) {
        M <- bib_data()
        tc_agg <- tryCatch({
          rows <- lapply(seq_len(nrow(M)), function(i) {
            aus <- trimws(strsplit(M$AU[i],";")[[1]])
            if (length(aus)==0 || all(is.na(aus))) return(NULL)
            data.frame(Author=aus, TC=M$TC[i], stringsAsFactors=FALSE)
          })
          tmp <- do.call(rbind, Filter(Negate(is.null), rows))
          aggregate(TC ~ Author, data=tmp, FUN=sum)
        }, error=function(e) NULL)
        if (!is.null(tc_agg)) {
          df <- merge(df, tc_agg, by="Author", all.x=TRUE)
          names(df)[3] <- "Total_Citations"
        }
      }
      df
    })

    output$author_plot <- renderPlotly({
      req(author_df_full(), input$n_authors)
      df     <- author_df_full()
      metric <- if (!is.null(input$author_metric)) input$author_metric else "pubs"
      if (metric=="tc" && "Total_Citations" %in% names(df)) {
        df2 <- df[order(-df$Total_Citations, na.last=TRUE),]
        df2 <- head(df2, input$n_authors)
        df2 <- df2[order(df2$Total_Citations),]
        plot_ly(df2, y=~Author, x=~Total_Citations, type="bar", orientation="h",
                marker=list(color=AMBER, line=list(color=NAVY, width=0.5))) |>
          layout(title=paste("Top", input$n_authors, "Authors by Total Citations"),
                 yaxis=list(title=NULL, categoryorder="total ascending"),
                 xaxis=list(title="Total Citations"),
                 plot_bgcolor="white", paper_bgcolor="white")
      } else {
        df2 <- df[order(-df$Publications),]
        df2 <- head(df2, input$n_authors)
        df2 <- df2[order(df2$Publications),]
        plot_ly(df2, y=~Author, x=~Publications, type="bar", orientation="h",
                marker=list(color=TEAL, line=list(color=NAVY, width=0.5))) |>
          layout(title=paste("Top", input$n_authors, "Authors by Publications"),
                 yaxis=list(title=NULL, categoryorder="total ascending"),
                 xaxis=list(title="Publications"),
                 plot_bgcolor="white", paper_bgcolor="white")
      }
    })

    output$author_tbl <- DT::renderDataTable({
      req(author_df_full(), input$n_authors)
      df <- author_df_full()
      df <- df[order(-df$Publications),]
      df <- head(df, input$n_authors)
      if ("Total_Citations" %in% names(df)) names(df)[3] <- "Total Citations"
      tool_dt(df)
    })

    # ── 3. Most Cited Papers ──────────────────────────────────────────────────
    output$cited_tbl <- DT::renderDataTable({
      req(bib_data(), input$n_cited)
      M <- bib_data()
      if (!"TC" %in% names(M)) {
        return(DT::datatable(data.frame(
          Message="Citation data (TC column) not found. Use Scopus or WoS full export.")))
      }
      cols <- intersect(c("TI","AU","SO","PY","TC","DI"), names(M))
      df   <- M[,cols, drop=FALSE]
      df   <- df[order(-df$TC, na.last=TRUE),]
      df   <- head(df, input$n_cited)
      col_labels <- c(TI="Title",AU="Authors",SO="Source",PY="Year",
                      TC="Citations",DI="DOI")
      names(df) <- col_labels[names(df)]
      DT::datatable(df, rownames=FALSE, escape=FALSE,
        options=list(pageLength=10, scrollX=TRUE,
          columnDefs=list(list(width="320px", targets=0)))) |>
        DT::formatStyle("Citations", fontWeight="bold", color="#1A5276")
    })

    # ── 4. Keywords ───────────────────────────────────────────────────────────
    output$kw_plot <- renderPlotly({
      req(bib_results(), input$n_kw)
      res  <- bib_results()
      kwt  <- if (!is.null(input$kw_type)) input$kw_type else "DE"
      kw <- if (kwt=="ID") {
        head(res$ID, input$n_kw)
      } else if (kwt=="both") {
        de <- res$DE; id <- res$ID
        if (is.null(de)||length(de)==0) id
        else if (is.null(id)||length(id)==0) de
        else {
          all_kw <- c(de, id)
          sort(tapply(all_kw, names(all_kw), sum), decreasing=TRUE)[seq_len(min(input$n_kw, length(all_kw)))]
        }
      } else {
        kw0 <- head(res$DE, input$n_kw)
        if (is.null(kw0)||length(kw0)==0) head(res$ID, input$n_kw) else kw0
      }
      if (is.null(kw)||length(kw)==0) return(plotly_empty() |> layout(title="No keyword data available"))
      kw_df <- data.frame(Keyword=names(kw), Freq=as.integer(kw))
      kw_df <- kw_df[order(kw_df$Freq),]
      h <- max(420, nrow(kw_df)*22)
      plot_ly(kw_df, y=~Keyword, x=~Freq, type="bar", orientation="h",
              marker=list(color=AMBER, line=list(color=NAVY, width=0.5))) |>
        layout(title=paste("Top", input$n_kw, "Keywords"),
               yaxis=list(title=NULL, categoryorder="total ascending"),
               xaxis=list(title="Frequency"),
               plot_bgcolor="white", paper_bgcolor="white",
               height=h)
    })

    output$kw_tbl <- DT::renderDataTable({
      req(bib_results(), input$n_kw)
      res <- bib_results()
      de_df <- if (!is.null(res$DE)&&length(res$DE)>0)
                 data.frame(Keyword=names(res$DE), Frequency=as.integer(res$DE), Type="Author Keyword")
               else NULL
      id_df <- if (!is.null(res$ID)&&length(res$ID)>0)
                 data.frame(Keyword=names(res$ID), Frequency=as.integer(res$ID), Type="Index Keyword")
               else NULL
      kwt <- if (!is.null(input$kw_type)) input$kw_type else "DE"
      df  <- if (kwt=="DE") de_df
             else if (kwt=="ID") id_df
             else rbind(de_df, id_df)
      if (is.null(df)) return(DT::datatable(data.frame(Message="No keyword data available.")))
      tool_dt(head(df[order(-df$Frequency),], input$n_kw))
    })

    # ── 5. Keyword Co-occurrence Network ──────────────────────────────────────
    kw_net_matrix <- eventReactive(input$run_kw_net, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="co-occurrences",
                      network="keywords", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$kw_net_plot <- renderPlot({
      req(kw_net_matrix())
      mat <- kw_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red",cex=1.1); return()
      }
      n    <- if (!is.null(input$kw_net_n))   input$kw_net_n   else 50
      type <- if (!is.null(input$kw_net_type)) input$kw_net_type else "fruchterman"
      tryCatch(
        networkPlot(mat, n=n, Title="Keyword Co-occurrence Network",
                    type=type, size=TRUE, remove.multiple=FALSE,
                    labelsize=0.8, cluster="optimal", remove.isolates=TRUE),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    output$kw_net_tbl <- DT::renderDataTable({
      req(kw_net_matrix())
      mat <- kw_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) return(NULL)
      tryCatch({
        if (!is.matrix(mat) && !inherits(mat,"dgCMatrix")) return(NULL)
        m <- as.matrix(mat)
        deg <- rowSums(m > 0)
        df  <- data.frame(Keyword=names(deg), Degree=as.integer(deg))
        tool_dt(head(df[order(-df$Degree),], 50), "Top keywords by co-occurrence degree")
      }, error=function(e) NULL)
    })

    # ── 6. Sources & Bradford's Law ───────────────────────────────────────────
    output$journal_plot <- renderPlotly({
      req(bib_results(), input$n_journals)
      so <- head(bib_results()$Sources, input$n_journals)
      if (is.null(so)||length(so)==0) return(plotly_empty() |> layout(title="No source data"))
      so_df <- data.frame(Journal=names(so), Articles=as.integer(so))
      so_df <- so_df[order(so_df$Articles),]
      plot_ly(so_df, y=~Journal, x=~Articles, type="bar", orientation="h",
              marker=list(color=GREEN, line=list(color=NAVY, width=0.5))) |>
        layout(title=paste("Top", input$n_journals, "Sources / Journals"),
               yaxis=list(title=NULL, categoryorder="total ascending"),
               xaxis=list(title="Articles"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$journal_tbl <- DT::renderDataTable({
      req(bib_results(), input$n_journals)
      so <- head(bib_results()$Sources, input$n_journals)
      if (is.null(so)) return(NULL)
      df <- data.frame(Journal=names(so), Articles=as.integer(so))
      tool_dt(df[order(-df$Articles),])
    })

    bradford_result <- eventReactive(input$run_bradford, {
      req(bib_data())
      withProgress(message="Running Bradford's Law analysis…", {
        tryCatch(bradford(bib_data()), error=function(e) list(error=e$message))
      })
    })

    output$bradford_plot <- renderPlot({
      req(bradford_result())
      res <- bradford_result()
      if (!is.null(res$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",res$error),col="red"); return()
      }
      tryCatch(plot(res), error=function(e) {
        plot.new(); text(0.5,0.5,"Bradford plot unavailable",col="grey50")
      })
    })

    output$bradford_tbl <- DT::renderDataTable({
      req(bradford_result())
      res <- bradford_result()
      if (!is.null(res$error)) return(NULL)
      df <- tryCatch(res$table, error=function(e) NULL)
      if (is.null(df)) return(NULL)
      tool_dt(df, "Bradford's Law — Journal Zones")
    })

    # ── 7. Country Analysis ───────────────────────────────────────────────────
    country_counts <- reactive({
      req(bib_results())
      co <- bib_results()$CO
      if (is.null(co)||length(co)==0) {
        M <- bib_data()
        if ("AU_CO" %in% names(M)) {
          co_raw <- paste(M$AU_CO[!is.na(M$AU_CO)], collapse=";")
          co     <- trimws(unlist(strsplit(co_raw,";")))
          co     <- co[nchar(co)>0]
        }
      }
      if (is.null(co)||length(co)==0) return(NULL)
      df <- as.data.frame(table(co), stringsAsFactors=FALSE)
      names(df) <- c("Country","Articles")
      df[order(-df$Articles),]
    })

    output$country_plot <- renderPlotly({
      req(country_counts())
      co_df <- head(country_counts(), 25)
      if (is.null(co_df)) {
        return(plotly_empty() |> layout(title="Country data not available in this file"))
      }
      co_df <- co_df[order(co_df$Articles),]
      plot_ly(co_df, y=~Country, x=~Articles, type="bar", orientation="h",
              marker=list(color=NAVY)) |>
        layout(title="Top 25 Countries by Scientific Production",
               yaxis=list(categoryorder="total ascending"),
               xaxis=list(title="Articles"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$country_tbl <- DT::renderDataTable({
      req(country_counts())
      df <- country_counts()
      if (is.null(df)) return(DT::datatable(data.frame(Message="Country data not available.")))
      tool_dt(df)
    })

    # ── World Map ──────────────────────────────────────────────────────────────
    output$country_map <- renderPlotly({
      req(input$run_country_map, country_counts())
      df <- country_counts()
      if (is.null(df) || nrow(df) == 0)
        return(plotly_empty() |> layout(title="No country data available"))

      # Normalise country names to Title Case for plotly locationmode="country names"
      df$CountryClean <- tools::toTitleCase(tolower(trimws(df$Country)))

      plot_ly(df,
              locations   = ~CountryClean,
              z           = ~Articles,
              locationmode= "country names",
              type        = "choropleth",
              colorscale  = list(c(0,"#E8F4FD"), c(0.5,"#2196F3"), c(1,"#0D47A1")),
              colorbar    = list(title="Articles", thickness=15),
              text        = ~paste0("<b>", CountryClean, "</b><br>Articles: ", Articles),
              hovertemplate = "%{text}<extra></extra>") |>
        layout(
          title = list(text="<b>Global Scientific Production</b>",
                       font=list(size=16, color="#1A3A5C")),
          geo   = list(
            showframe      = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "#BBBBBB",
            showland       = TRUE,
            landcolor      = "#F5F5F5",
            showocean      = TRUE,
            oceancolor     = "#EAF4FB",
            showlakes      = FALSE,
            projection     = list(type="natural earth"),
            bgcolor        = "white"
          ),
          margin = list(l=0, r=0, t=50, b=10),
          paper_bgcolor = "white"
        )
    })

    output$collab_stats_tbl <- DT::renderDataTable({
      req(input$run_country_map, country_counts())
      df <- country_counts()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      # Build summary statistics
      total_countries <- nrow(df)
      total_articles  <- sum(df$Articles, na.rm=TRUE)
      top5 <- head(df, 5)$Country

      # If collaboration network has been computed, show collaboration pairs
      collab_pairs <- tryCatch({
        mat <- isolate(country_net_matrix())
        if (!is.null(mat) && !is.list(mat)) {
          m   <- as.matrix(mat)
          diag(m) <- 0
          idx <- which(m > 0, arr.ind=TRUE)
          if (nrow(idx) > 0) {
            pairs <- data.frame(
              Country_A     = rownames(m)[idx[,1]],
              Country_B     = colnames(m)[idx[,2]],
              Collaborations= m[idx],
              stringsAsFactors=FALSE
            )
            pairs <- pairs[pairs$Country_A < pairs$Country_B, ]
            pairs <- head(pairs[order(-pairs$Collaborations), ], 30)
            pairs
          } else NULL
        } else NULL
      }, error=function(e) NULL)

      if (!is.null(collab_pairs) && nrow(collab_pairs) > 0) {
        tool_dt(collab_pairs, "Top International Collaboration Pairs")
      } else {
        # Fall back to country production summary with % share
        df$Share_pct <- round(df$Articles / sum(df$Articles) * 100, 1)
        df$CountryClean <- tools::toTitleCase(tolower(df$Country))
        out <- data.frame(
          Country    = df$CountryClean,
          Articles   = df$Articles,
          Share_pct  = paste0(df$Share_pct, "%"),
          stringsAsFactors = FALSE
        )
        names(out) <- c("Country", "Articles", "Share (%)")
        tool_dt(head(out, 50), paste0("Production by Country — ",
                                       total_countries, " countries, ",
                                       total_articles, " total articles"))
      }
    })

    country_net_matrix <- eventReactive(input$run_country_net, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="collaboration",
                      network="countries", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$country_net_plot <- renderPlot({
      req(country_net_matrix())
      mat <- country_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n <- if (!is.null(input$country_n)) input$country_n else 30
      tryCatch(
        networkPlot(mat, n=n, Title="Country Collaboration Network",
                    type="sphere", size=TRUE, remove.multiple=FALSE,
                    labelsize=0.9, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    # ── 8. Author Collaboration Network ───────────────────────────────────────
    auth_net_matrix <- eventReactive(input$run_auth_net, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="collaboration",
                      network="authors", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$auth_net_plot <- renderPlot({
      req(auth_net_matrix())
      mat <- auth_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n <- if (!is.null(input$auth_net_n)) input$auth_net_n else 30
      tryCatch(
        networkPlot(mat, n=n, Title="Author Collaboration Network",
                    type="fruchterman", size=TRUE, remove.multiple=FALSE,
                    labelsize=0.75, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    output$auth_net_tbl <- DT::renderDataTable({
      req(auth_net_matrix())
      mat <- auth_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) return(NULL)
      tryCatch({
        m   <- as.matrix(mat)
        deg <- rowSums(m > 0)
        df  <- data.frame(Author=names(deg), Collaborations=as.integer(deg))
        tool_dt(head(df[order(-df$Collaborations),], 40),
                "Authors ranked by collaboration degree")
      }, error=function(e) NULL)
    })

    # ── 9. Co-Citation Networks ───────────────────────────────────────────────
    # Reference co-citation
    cocite_matrix <- eventReactive(input$run_cocite, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="co-citation",
                      network="references", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$cocite_plot <- renderPlot({
      req(cocite_matrix())
      mat <- cocite_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n    <- if (!is.null(input$cocite_n))   input$cocite_n   else 50
      type <- if (!is.null(input$cocite_type)) input$cocite_type else "kamada"
      tryCatch(
        networkPlot(mat, n=n, Title="Reference Co-citation Network",
                    type=type, size=TRUE, remove.multiple=FALSE,
                    labelsize=0.6, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    output$cocite_tbl <- DT::renderDataTable({
      req(cocite_matrix())
      mat <- cocite_matrix()
      if (is.list(mat) && !is.null(mat$error)) return(NULL)
      tryCatch({
        m   <- as.matrix(mat)
        deg <- rowSums(m > 0)
        df  <- data.frame(Reference=names(deg), Degree=as.integer(deg))
        tool_dt(head(df[order(-df$Degree),], 30), "Most central references")
      }, error=function(e) NULL)
    })

    # Author co-citation
    auth_cocite_matrix <- eventReactive(input$run_auth_cocite, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="co-citation",
                      network="authors", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$auth_cocite_plot <- renderPlot({
      req(auth_cocite_matrix())
      mat <- auth_cocite_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n <- if (!is.null(input$auth_cocite_n)) input$auth_cocite_n else 30
      tryCatch(
        networkPlot(mat, n=n, Title="Author Co-citation Network",
                    type="kamada", size=TRUE, remove.multiple=FALSE,
                    labelsize=0.8, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    # Source co-citation
    src_cocite_matrix <- eventReactive(input$run_src_cocite, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="co-citation",
                      network="sources", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$src_cocite_plot <- renderPlot({
      req(src_cocite_matrix())
      mat <- src_cocite_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n <- if (!is.null(input$src_cocite_n)) input$src_cocite_n else 30
      tryCatch(
        networkPlot(mat, n=n, Title="Source Co-citation Network",
                    type="kamada", size=TRUE, remove.multiple=FALSE,
                    labelsize=0.8, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    # Bibliographic coupling
    coupling_matrix <- eventReactive(input$run_coupling, {
      req(bib_data())
      tryCatch(
        biblioNetwork(bib_data(), analysis="coupling",
                      network="references", sep=";"),
        error=function(e) list(error=e$message)
      )
    })

    output$coupling_plot <- renderPlot({
      req(coupling_matrix())
      mat <- coupling_matrix()
      if (is.list(mat) && !is.null(mat$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",mat$error),col="red"); return()
      }
      n <- if (!is.null(input$coupling_n)) input$coupling_n else 40
      tryCatch(
        networkPlot(mat, n=n, Title="Bibliographic Coupling Network",
                    type="fruchterman", size=TRUE, remove.multiple=FALSE,
                    labelsize=0.7, cluster="optimal"),
        error=function(e) { plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red") }
      )
    })

    # ── 10. Thematic Map ──────────────────────────────────────────────────────
    thematic_result <- eventReactive(input$run_thematic, {
      req(bib_data())
      withProgress(message="Building thematic map…", {
        tryCatch({
          field <- if (!is.null(input$thematic_field)) input$thematic_field else "DE"
          minf  <- if (!is.null(input$thematic_min))   input$thematic_min   else 3
          n_kw  <- if (!is.null(input$thematic_n))     input$thematic_n     else 500
          thematicMap(bib_data(), field=field, n=n_kw,
                      minfreq=minf, stemming=FALSE,
                      size=0.5, n.labels=4, repel=TRUE)
        }, error=function(e) list(error=e$message))
      })
    })

    output$thematic_plot <- renderPlot({
      req(thematic_result())
      res <- thematic_result()
      if (!is.null(res$error)) {
        plot.new()
        text(0.5, 0.5,
             paste0("Could not generate thematic map:\n", res$error,
                    "\n\nTry reducing 'Min keyword freq' or increasing 'Max keywords'."),
             col="red", cex=0.9)
        return()
      }
      tryCatch(plot(res$map), error=function(e) {
        plot.new(); text(0.5,0.5,"Plot unavailable with current settings",col="grey50")
      })
    })

    output$thematic_interp <- renderUI({
      tags$div(style="background:#EBF4F7;padding:1rem;border-radius:8px;margin-top:1rem;",
        tags$h5("📖 How to read the Thematic Map (Cobo et al., 2011):"),
        fluidRow(
          column(6,
            tags$ul(
              tags$li(tags$b("Motor themes (top-right):"),
                " High centrality + high density → well-developed, important, and central topics"),
              tags$li(tags$b("Niche themes (top-left):"),
                " Low centrality + high density → highly developed but specialised / peripheral")
            )
          ),
          column(6,
            tags$ul(
              tags$li(tags$b("Emerging / declining (bottom-left):"),
                " Low centrality + low density → marginal, emerging, or declining topics"),
              tags$li(tags$b("Basic / transversal (bottom-right):"),
                " High centrality + low density → important foundational but underdeveloped topics")
            )
          )
        )
      )
    })

    # ── 11. Lotka's Law ───────────────────────────────────────────────────────
    lotka_result <- eventReactive(input$run_lotka, {
      req(bib_results())
      withProgress(message="Running Lotka's Law analysis…", {
        tryCatch(lotka(bib_results()), error=function(e) list(error=e$message))
      })
    })

    output$lotka_plot <- renderPlot({
      req(lotka_result())
      res <- lotka_result()
      if (!is.null(res$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",res$error),col="red"); return()
      }
      obs <- tryCatch(res$AuthorProd, error=function(e) NULL)
      if (is.null(obs)||nrow(obs)==0) {
        plot.new(); text(0.5,0.5,"No productivity data available",col="grey50"); return()
      }
      cn <- names(obs)
      x_col <- cn[1]; y_col <- cn[2]
      par(mar=c(5,5,3,2))
      tryCatch({
        plot(obs[[x_col]], obs[[y_col]],
             type="b", pch=19, col=NAVY, lwd=2,
             xlab="Number of Papers", ylab="Frequency of Authors",
             main="Lotka's Law — Author Productivity",
             log="xy", cex.lab=1.1)
        if (!is.null(res$Beta) && !is.null(res$C)) {
          xr   <- seq(min(obs[[x_col]]), max(obs[[x_col]]), length.out=200)
          y_th <- res$C / xr^res$Beta
          lines(xr, y_th, col=TEAL, lwd=2.5, lty=2)
          legend("topright",
                 legend=c("Observed","Theoretical (Lotka)"),
                 col=c(NAVY,TEAL), lwd=2,
                 pch=c(19,NA), lty=c(1,2), bty="n")
        }
      }, error=function(e) {
        plot.new(); text(0.5,0.5,paste("Plot error:",e$message),col="red")
      })
    })

    output$lotka_text <- renderPrint({
      req(lotka_result())
      res <- lotka_result()
      if (!is.null(res$error)) { cat("Error:", res$error, "\n"); return() }
      cat("══════════════════════════════════\n")
      cat("  Lotka's Law Analysis Results\n")
      cat("══════════════════════════════════\n\n")
      if (!is.null(res$Beta))    cat(sprintf("  Beta coefficient : %.4f\n", res$Beta))
      if (!is.null(res$C))       cat(sprintf("  C constant       : %.4f\n", res$C))
      if (!is.null(res$R2))      cat(sprintf("  R-squared        : %.4f\n", res$R2))
      if (!is.null(res$p.value)) {
        fit_msg <- if (res$p.value > 0.05) "✅ Fits Lotka's Law (p > 0.05)"
                   else                    "⚠️ Does not fit Lotka's Law (p ≤ 0.05)"
        cat(sprintf("  K-S p-value      : %.4f  %s\n", res$p.value, fit_msg))
      }
      cat("\n  Interpretation:\n")
      cat("  Beta ≈ 2 means ~25% of authors produce 50% of all papers.\n")
      cat("  Higher Beta = more concentration among prolific authors.\n\n")
      cat("  Author Productivity Table:\n")
      print(res$AuthorProd, row.names=FALSE)
    })

    # ── 12. Historiograph ─────────────────────────────────────────────────────
    histo_data <- eventReactive(input$run_histo, {
      req(bib_data())
      tryCatch({
        histResults <- histNetwork(bib_data(),
                                   min.citations = input$histo_min_cite,
                                   sep = ";")
        histResults
      }, error=function(e) list(error=e$message))
    })

    output$histo_plot <- renderPlot({
      req(histo_data())
      h <- histo_data()
      if (is.list(h) && !is.null(h$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",h$error),col="red"); return()
      }
      tryCatch({
        n <- if (!is.null(input$histo_n)) input$histo_n else 25
        histPlot(h, n=n, size=FALSE, remove.isolates=TRUE)
      }, error=function(e) {
        plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red")
      })
    })

    output$histo_tbl <- DT::renderDataTable({
      req(histo_data())
      h <- histo_data()
      if (is.list(h) && !is.null(h$error)) return(NULL)
      tryCatch({
        df <- h$histData
        if (!is.null(df) && nrow(df) > 0) {
          tool_dt(head(df[order(-df$citations_received, na.last=TRUE), ], 50),
                  "Historical Citation Network — Top Papers")
        } else NULL
      }, error=function(e) NULL)
    })

    # ── 13. Citation Analysis ─────────────────────────────────────────────────
    citation_data <- eventReactive(input$run_citations, {
      req(bib_results(), bib_data())
      tryCatch({
        res <- bib_results()
        M   <- bib_data()

        # Citations per year
        tc_per_year <- tryCatch({
          if (!is.null(M$PY) && !is.null(M$TC)) {
            agg <- aggregate(as.numeric(M$TC), by=list(Year=as.integer(M$PY)), FUN=sum, na.rm=TRUE)
            names(agg)[2] <- "TotalCitations"
            agg <- agg[!is.na(agg$Year), ]
            agg[order(agg$Year), ]
          } else NULL
        }, error=function(e) NULL)

        # Annual production (for CPP = citations per paper)
        ann_prod <- tryCatch({
          if (!is.null(M$PY) && !is.null(M$TC)) {
            cnt  <- aggregate(rep(1, nrow(M)), by=list(Year=as.integer(M$PY)), FUN=sum)
            tc2  <- aggregate(as.numeric(M$TC), by=list(Year=as.integer(M$PY)), FUN=sum, na.rm=TRUE)
            names(cnt)[2] <- "Papers"; names(tc2)[2] <- "Citations"
            merged <- merge(cnt, tc2, by="Year")
            merged$CPP <- round(merged$Citations / merged$Papers, 2)
            merged[order(merged$Year), ]
          } else NULL
        }, error=function(e) NULL)

        # H-index evolution
        h_evol <- tryCatch({
          if (!is.null(M$PY) && !is.null(M$TC)) {
            yrs <- sort(unique(as.integer(M$PY)), na.last=NA)
            yrs <- yrs[!is.na(yrs)]
            h_vals <- sapply(yrs, function(y) {
              tc_sub <- as.numeric(M$TC[as.integer(M$PY) <= y])
              tc_sub <- sort(tc_sub[!is.na(tc_sub)], decreasing=TRUE)
              if (length(tc_sub)==0) return(0L)
              as.integer(sum(tc_sub >= seq_along(tc_sub)))
            })
            data.frame(Year=yrs, H_index=h_vals)
          } else NULL
        }, error=function(e) NULL)

        # Top cited with burst flag (papers where TC > 2*mean)
        top_cited <- tryCatch({
          if (!is.null(M$TI) && !is.null(M$TC)) {
            tc_num <- as.numeric(M$TC)
            mean_tc <- mean(tc_num, na.rm=TRUE)
            df <- data.frame(
              Title    = M$TI,
              Year     = M$PY,
              Authors  = M$AU,
              Source   = if (!is.null(M$SO)) M$SO else NA,
              Citations= tc_num,
              Burst    = ifelse(tc_num > 2*mean_tc, "⚡ High Impact", ""),
              stringsAsFactors=FALSE
            )
            df <- df[!is.na(df$Citations), ]
            head(df[order(-df$Citations), ], 50)
          } else NULL
        }, error=function(e) NULL)

        list(tc_per_year=tc_per_year, ann_prod=ann_prod,
             h_evol=h_evol, top_cited=top_cited)
      }, error=function(e) list(error=e$message))
    })

    output$cit_per_year_plot <- renderPlotly({
      req(citation_data())
      d <- citation_data()
      if (is.list(d) && !is.null(d$error))
        return(plotly_empty() |> layout(title=d$error))
      ann <- d$ann_prod
      if (is.null(ann)) return(plotly_empty() |> layout(title="No citation data"))
      plot_ly(ann, x=~Year) |>
        add_bars(y=~Citations, name="Total Citations", marker=list(color=NAVY)) |>
        add_lines(y=~CPP, name="Cit/Paper", yaxis="y2",
                  line=list(color=AMBER, width=2)) |>
        layout(title="Citations per Year & per Paper",
               yaxis =list(title="Total Citations"),
               yaxis2=list(title="Cit/Paper", overlaying="y", side="right"),
               legend=list(orientation="h"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$h_index_plot <- renderPlotly({
      req(citation_data())
      d <- citation_data()
      if (is.list(d) && !is.null(d$error))
        return(plotly_empty() |> layout(title=d$error))
      h <- d$h_evol
      if (is.null(h)) return(plotly_empty() |> layout(title="No h-index data"))
      plot_ly(h, x=~Year, y=~H_index, type="scatter", mode="lines+markers",
              line=list(color=GREEN, width=2.5),
              marker=list(color=GREEN, size=6)) |>
        layout(title="H-index Evolution Over Time",
               xaxis=list(title="Year"),
               yaxis=list(title="H-index"),
               plot_bgcolor="white", paper_bgcolor="white")
    })

    output$citation_tbl <- DT::renderDataTable({
      req(citation_data())
      d <- citation_data()
      if (is.list(d) && !is.null(d$error)) return(NULL)
      tc <- d$top_cited
      if (is.null(tc)) return(NULL)
      tool_dt(tc, "Top Cited Papers (⚡ = citation burst > 2× field average)")
    })

    # ── 14. Conceptual Structure ──────────────────────────────────────────────
    cs_data <- eventReactive(input$run_cs, {
      req(bib_data())
      tryCatch({
        conceptualStructure(bib_data(),
                            field       = input$cs_field,
                            method      = "MCA",
                            minDegree   = input$cs_min_freq,
                            clust       = input$cs_n_clust,
                            stemming    = FALSE,
                            labelsize   = 12,
                            documents   = 20,
                            graph       = FALSE)
      }, error=function(e) list(error=e$message))
    })

    output$cs_plot <- renderPlot({
      req(cs_data())
      d <- cs_data()
      if (is.list(d) && !is.null(d$error)) {
        plot.new(); text(0.5,0.5,paste("Error:",d$error),col="red"); return()
      }
      tryCatch({
        if (!is.null(d$graph_terms)) {
          print(d$graph_terms)
        } else if (!is.null(d$graph_documents_Contrib)) {
          print(d$graph_documents_Contrib)
        } else {
          plot.new(); text(0.5,0.5,"No conceptual structure plot available",col="gray")
        }
      }, error=function(e) {
        plot.new(); text(0.5,0.5,paste("Error:",e$message),col="red")
      })
    })

    output$cs_clusters_tbl <- DT::renderDataTable({
      req(cs_data())
      d <- cs_data()
      if (is.list(d) && !is.null(d$error)) return(NULL)
      tryCatch({
        kw_clust <- d$km.res$cluster
        if (!is.null(kw_clust)) {
          df <- data.frame(
            Keyword = names(kw_clust),
            Cluster = as.integer(kw_clust),
            stringsAsFactors=FALSE
          )
          df <- df[order(df$Cluster, df$Keyword), ]
          tool_dt(df, "Keyword Clusters from Conceptual Structure Analysis")
        } else NULL
      }, error=function(e) NULL)
    })

    # ── 15. Summary Statistics ────────────────────────────────────────────────
    output$bib_summary <- renderPrint({
      req(bib_results())
      tryCatch(summary(bib_results(), k=15, pause=FALSE),
               error=function(e) cat("Summary unavailable:", e$message))
    })

    # ── 13. Downloads ─────────────────────────────────────────────────────────
    safe_df <- function(x) if (is.null(x)||nrow(x)==0) data.frame(Note="No data") else x

    output$dl_all <- downloadHandler(
      filename=function() paste0("Bibliometric_Full_Report_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_results(), bib_data())
        res <- bib_results()
        M   <- bib_data()
        kpis <- get_kpis()
        wb   <- openxlsx::createWorkbook()

        # Sheet 1: KPI Summary
        kpi_df <- data.frame(
          Metric=c("Total Documents","Total Authors","Total Sources/Journals",
                   "Total Citations","Avg Citations per Paper",
                   "Collaboration Index","Year Range","Annual Growth (CAGR %)"),
          Value=c(kpis$n_docs, kpis$n_auth, kpis$n_sources,
                  kpis$tc, kpis$avg_tc, kpis$collab_idx,
                  kpis$yr_range, kpis$cagr)
        )
        openxlsx::addWorksheet(wb, "KPI Summary")
        openxlsx::writeData(wb, "KPI Summary", kpi_df)

        # Sheet 2: Annual Production
        ann <- as.data.frame(res$AnnualProduction)
        if (!is.null(ann) && nrow(ann)>0) {
          names(ann) <- c("Year","Articles")
          openxlsx::addWorksheet(wb, "Annual Production")
          openxlsx::writeData(wb, "Annual Production", ann[order(ann$Year),])
        }

        # Sheet 3: Top Authors
        auth <- res$Authors
        if (!is.null(auth) && length(auth)>0) {
          a_df <- data.frame(Author=names(auth), Publications=as.integer(auth))
          openxlsx::addWorksheet(wb, "Top Authors")
          openxlsx::writeData(wb, "Top Authors", a_df[order(-a_df$Publications),])
        }

        # Sheet 4: Author Keywords
        de <- res$DE
        if (!is.null(de) && length(de)>0) {
          de_df <- data.frame(Keyword=names(de), Frequency=as.integer(de))
          openxlsx::addWorksheet(wb, "Author Keywords")
          openxlsx::writeData(wb, "Author Keywords", de_df[order(-de_df$Frequency),])
        }

        # Sheet 5: Index Keywords
        id <- res$ID
        if (!is.null(id) && length(id)>0) {
          id_df <- data.frame(Keyword=names(id), Frequency=as.integer(id))
          openxlsx::addWorksheet(wb, "Index Keywords")
          openxlsx::writeData(wb, "Index Keywords", id_df[order(-id_df$Frequency),])
        }

        # Sheet 6: Sources
        so <- res$Sources
        if (!is.null(so) && length(so)>0) {
          so_df <- data.frame(Source=names(so), Articles=as.integer(so))
          openxlsx::addWorksheet(wb, "Sources Journals")
          openxlsx::writeData(wb, "Sources Journals", so_df[order(-so_df$Articles),])
        }

        # Sheet 7: Countries
        co_df <- country_counts()
        if (!is.null(co_df) && nrow(co_df)>0) {
          openxlsx::addWorksheet(wb, "Countries")
          openxlsx::writeData(wb, "Countries", co_df)
        }

        # Sheet 8: Most Cited Papers
        if ("TC" %in% names(M)) {
          cols     <- intersect(c("TI","AU","SO","PY","TC","DI"), names(M))
          cited_df <- M[,cols,drop=FALSE]
          cited_df <- cited_df[order(-cited_df$TC, na.last=TRUE),]
          openxlsx::addWorksheet(wb, "Most Cited Papers")
          openxlsx::writeData(wb, "Most Cited Papers", head(cited_df,150))
        }

        # Sheet 9: Raw data (key columns, first 1000)
        raw_cols <- intersect(c("TI","AU","SO","PY","TC","DE","ID","AB","DI","UT"), names(M))
        openxlsx::addWorksheet(wb, "Raw Data")
        openxlsx::writeData(wb, "Raw Data", head(M[,raw_cols,drop=FALSE],1000))

        openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
      }
    )

    output$dl_summary <- downloadHandler(
      filename=function() paste0("Annual_Production_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_results())
        ann <- as.data.frame(bib_results()$AnnualProduction)
        if (!is.null(ann) && nrow(ann)>0) names(ann) <- c("Year","Articles")
        writexl::write_xlsx(list("Annual Production"=safe_df(ann)), file)
      }
    )

    output$dl_authors <- downloadHandler(
      filename=function() paste0("Authors_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_results())
        auth <- bib_results()$Authors
        df   <- if (!is.null(auth) && length(auth)>0)
                  data.frame(Author=names(auth), Publications=as.integer(auth))
                else data.frame(Note="No data")
        writexl::write_xlsx(list("Authors"=df[order(-df$Publications),]), file)
      }
    )

    output$dl_keywords <- downloadHandler(
      filename=function() paste0("Keywords_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_results())
        res    <- bib_results()
        sheets <- list()
        de <- res$DE
        id <- res$ID
        if (!is.null(de) && length(de)>0)
          sheets[["Author Keywords"]] <- data.frame(Keyword=names(de), Frequency=as.integer(de))
        if (!is.null(id) && length(id)>0)
          sheets[["Index Keywords"]] <- data.frame(Keyword=names(id), Frequency=as.integer(id))
        if (length(sheets)==0) sheets[["Keywords"]] <- data.frame(Note="No keyword data")
        writexl::write_xlsx(sheets, file)
      }
    )

    output$dl_journals <- downloadHandler(
      filename=function() paste0("Journals_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_results())
        so <- bib_results()$Sources
        df <- if (!is.null(so) && length(so)>0)
                data.frame(Source=names(so), Articles=as.integer(so))[order(-as.integer(so)),]
              else data.frame(Note="No data")
        writexl::write_xlsx(list("Sources"=df), file)
      }
    )

    output$dl_cited <- downloadHandler(
      filename=function() paste0("Most_Cited_Papers_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_data())
        M    <- bib_data()
        cols <- intersect(c("TI","AU","SO","PY","TC","DI"), names(M))
        df   <- M[,cols,drop=FALSE]
        if ("TC" %in% names(df)) df <- df[order(-df$TC, na.last=TRUE),]
        writexl::write_xlsx(list("Most Cited"=head(df,200)), file)
      }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # PURE HELPER FUNCTIONS — dataset-agnostic, used by comparison & time horizons
    # ══════════════════════════════════════════════════════════════════════════

    # Trend data frame from any M
    .trend_df <- function(M) {
      if (is.null(M)) return(NULL)
      yrs <- suppressWarnings(as.integer(as.character(M$PY)))
      yrs <- yrs[!is.na(yrs) & yrs >= 1900 & yrs <= as.integer(format(Sys.Date(),"%Y"))+1]
      if (length(yrs)==0) return(NULL)
      df <- as.data.frame(table(yrs)); names(df) <- c("Year","Articles")
      df$Year <- as.integer(as.character(df$Year)); df[order(df$Year),]
    }

    # Top-N authors
    .top_authors <- function(M, n=15) {
      if (is.null(M)||!"AU"%in%names(M)) return(NULL)
      au <- unlist(strsplit(paste(M$AU[!is.na(M$AU)],collapse=";"),";"))
      au <- trimws(au); au <- au[nchar(au)>0]
      if (length(au)==0) return(NULL)
      tbl <- sort(table(au),decreasing=TRUE)
      data.frame(Author=names(tbl)[1:min(n,length(tbl))],
                 Publications=as.integer(tbl)[1:min(n,length(tbl))],
                 stringsAsFactors=FALSE)
    }

    # Top-N keywords (col = "DE" or "ID")
    .top_kw <- function(M, col="DE", n=20) {
      if (is.null(M)||!col%in%names(M)) return(NULL)
      kw <- unlist(strsplit(toupper(paste(M[[col]][!is.na(M[[col]])],collapse=";")),";"))
      kw <- trimws(kw); kw <- kw[nchar(kw)>0 & kw!="NA"]
      if (length(kw)==0) return(NULL)
      tbl <- sort(table(kw),decreasing=TRUE)
      data.frame(Keyword=names(tbl)[1:min(n,length(tbl))],
                 Frequency=as.integer(tbl)[1:min(n,length(tbl))],
                 stringsAsFactors=FALSE)
    }

    # Top-N sources
    .top_sources <- function(M, n=15) {
      if (is.null(M)||!"SO"%in%names(M)) return(NULL)
      so <- M$SO[!is.na(M$SO)]; if (length(so)==0) return(NULL)
      tbl <- sort(table(so),decreasing=TRUE)
      data.frame(Source=names(tbl)[1:min(n,length(tbl))],
                 Articles=as.integer(tbl)[1:min(n,length(tbl))],
                 stringsAsFactors=FALSE)
    }

    # Top-N countries
    .top_countries <- function(M, n=15) {
      if (is.null(M)) return(NULL)
      col <- intersect(c("AU_CO","CO"), names(M))[1]
      if (is.na(col)) return(NULL)
      co <- unlist(strsplit(paste(M[[col]][!is.na(M[[col]])],collapse=";"),";"))
      co <- trimws(co); co <- co[nchar(co)>0]
      if (length(co)==0) return(NULL)
      tbl <- sort(table(co),decreasing=TRUE)
      data.frame(Country=names(tbl)[1:min(n,length(tbl))],
                 Articles=as.integer(tbl)[1:min(n,length(tbl))],
                 stringsAsFactors=FALSE)
    }

    # Most cited papers
    .cited_papers <- function(M, n=20) {
      if (is.null(M)||!"TC"%in%names(M)) return(NULL)
      cols <- intersect(c("TI","AU","SO","PY","TC","DI"),names(M))
      df <- M[,cols,drop=FALSE]; df <- df[order(-df$TC,na.last=TRUE),]
      head(df,n)
    }

    # Citation summary stats
    .cit_summary <- function(M, label="") {
      if (is.null(M)||!"TC"%in%names(M)) return(NULL)
      tc <- suppressWarnings(as.integer(M$TC)); tc <- tc[!is.na(tc)]
      if (length(tc)==0) return(NULL)
      tc_s <- sort(tc,decreasing=TRUE)
      h <- 0; for (i in seq_along(tc_s)) { if (tc_s[i]>=i) h<-i else break }
      data.frame(
        Metric=c("Dataset","Documents","Total Citations","Mean Cit/Paper",
                 "Median Citations","H-index","Max Citations","Papers with 0 cit"),
        Value=c(label, format(nrow(M),big.mark=","), format(sum(tc),big.mark=","),
                round(mean(tc),2), median(tc), h, max(tc), sum(tc==0)),
        stringsAsFactors=FALSE)
    }

    # Lotka's law frequency table
    .lotka_df <- function(M) {
      if (is.null(M)||!"AU"%in%names(M)) return(NULL)
      au <- unlist(strsplit(paste(M$AU[!is.na(M$AU)],collapse=";"),";"))
      au <- trimws(au); au <- au[nchar(au)>0]; if (length(au)==0) return(NULL)
      npubs <- as.integer(table(au))
      ft <- as.data.frame(table(npubs)); names(ft) <- c("N_Papers","N_Authors")
      ft$N_Papers <- as.integer(as.character(ft$N_Papers))
      ft$N_Authors <- as.integer(ft$N_Authors); ft
    }

    # Collaboration index
    .collab_idx <- function(M) {
      if (is.null(M)||!"AU"%in%names(M)) return(NA)
      ac <- sapply(strsplit(M$AU,";"), length)
      round(mean(ac,na.rm=TRUE),2)
    }

    # Summary stats row for one dataset
    .summary_row <- function(M, label) {
      if (is.null(M)) return(NULL)
      tc <- if("TC"%in%names(M)) suppressWarnings(as.integer(M$TC)) else integer(0)
      tc <- tc[!is.na(tc)]
      yrs <- suppressWarnings(as.integer(as.character(M$PY)))
      yrs <- yrs[!is.na(yrs)&yrs>1900]
      h <- 0
      if (length(tc)>0) {
        tc_s <- sort(tc,decreasing=TRUE)
        for (i in seq_along(tc_s)) { if (tc_s[i]>=i) h<-i else break }
      }
      data.frame(
        Metric=c("Label","Documents","Year Range","Total Citations",
                 "Avg Cit/Doc","H-index","Unique Authors","Unique Sources",
                 "Collaboration Index"),
        Value=c(label,
                format(nrow(M),big.mark=","),
                if(length(yrs)>1) paste(min(yrs),"–",max(yrs)) else "—",
                format(sum(tc),big.mark=","),
                if(length(tc)>0) round(mean(tc),2) else "—",
                h,
                format(length(unique(trimws(unlist(strsplit(paste(M$AU[!is.na(M$AU)],collapse=";"),";"))))),big.mark=","),
                format(length(unique(M$SO[!is.na(M$SO)])),big.mark=","),
                .collab_idx(M)),
        stringsAsFactors=FALSE)
    }

    # Build a side-by-side bar chart for two frequency tables
    .cmp_bar <- function(df1, df2, lbl1, lbl2, col_name, freq_name,
                          title="", color1=TEAL, color2=AMBER, height=420) {
      p <- plotly::plot_ly()
      if (!is.null(df1)&&nrow(df1)>0)
        p <- plotly::add_trace(p, data=df1, x=~get(freq_name), y=~reorder(get(col_name),get(freq_name)),
                               name=lbl1, type="bar", orientation="h",
                               marker=list(color=color1))
      if (!is.null(df2)&&nrow(df2)>0)
        p <- plotly::add_trace(p, data=df2, x=~get(freq_name), y=~reorder(get(col_name),get(freq_name)),
                               name=lbl2, type="bar", orientation="h",
                               marker=list(color=color2, opacity=0.85))
      p |> plotly::layout(title=title, barmode="group",
                          xaxis=list(title="Count"), yaxis=list(title=""),
                          plot_bgcolor="white", paper_bgcolor="white",
                          legend=list(x=0.7,y=0.05),
                          margin=list(l=200), height=height)
    }

    # ══════════════════════════════════════════════════════════════════════════
    # DATASET COMPARISON
    # ══════════════════════════════════════════════════════════════════════════

    bib_data2 <- reactive({
      req(input$file2)
      tryCatch({
        path <- input$file2$datapath
        name <- input$file2$name
        db   <- input$db2_type %||% "scopus"
        M2   <- bibliometrix::convert2df(path, dbsource=db,
                           format=if(tools::file_ext(name)=="bib") "bibtex" else "csv")
        M2
      }, error=function(e) NULL)
    })

    # Helper: extract key stats from a bibliometrix data frame
    bib_quick_stats <- function(M, label="Dataset") {
      if (is.null(M)) return(data.frame(Metric=character(), Value=character()))
      yrs <- suppressWarnings(as.integer(as.character(M$PY)))
      yrs <- yrs[!is.na(yrs) & yrs > 1900]
      tc  <- if (!is.null(M$TC)) suppressWarnings(as.integer(M$TC)) else integer(0)
      tc  <- tc[!is.na(tc)]
      au  <- if (!is.null(M$AU)) M$AU else character(0)
      so  <- if (!is.null(M$SO)) M$SO else character(0)
      # Compute h-index separately (avoids block expression inside c())
      h_idx <- 0
      if (length(tc) > 0) {
        tc_s <- sort(tc, decreasing=TRUE)
        for (i in seq_along(tc_s)) {
          if (tc_s[i] >= i) h_idx <- i else break
        }
      }
      data.frame(
        Metric = c("Dataset", "Documents", "Authors (unique)", "Sources/Journals",
                   "Year range", "Total citations", "Avg citations/doc",
                   "H-index (approx)"),
        Value  = c(label,
                   format(nrow(M), big.mark=","),
                   format(length(unique(unlist(strsplit(paste(au, collapse=";"), ";"))))),
                   format(length(unique(so)), big.mark=","),
                   if (length(yrs) > 0) paste(min(yrs), "-", max(yrs)) else "—",
                   format(sum(tc, na.rm=TRUE), big.mark=","),
                   if (length(tc) > 0) as.character(round(mean(tc, na.rm=TRUE), 1)) else "—",
                   as.character(h_idx)),
        stringsAsFactors=FALSE
      )
    }

    # Helper: top-N frequency table from semicolon-separated column
    top_n_freq <- function(M, col, n=15) {
      if (is.null(M) || !col %in% names(M)) return(NULL)
      items <- unlist(strsplit(toupper(paste(M[[col]][!is.na(M[[col]])], collapse=";")), ";"))
      items <- trimws(items)
      items <- items[nchar(items) > 0 & items != "NA"]
      if (length(items) == 0) return(NULL)
      tbl <- sort(table(items), decreasing=TRUE)
      data.frame(Item=names(tbl)[1:min(n,length(tbl))],
                 Freq=as.integer(tbl)[1:min(n,length(tbl))],
                 stringsAsFactors=FALSE)
    }

    output$compare_ui <- renderUI({
      req(input$run_compare)
      isolate({
        req(bib_data(), bib_data2())
        lbl1 <- input$ds1_label %||% "Dataset 1"
        lbl2 <- input$ds2_label %||% "Dataset 2"
        pal  <- c(TEAL, AMBER)
        tagList(
          tags$hr(),
          # Colour-coded headers
          fluidRow(
            column(6, tags$div(style=paste0("background:",pal[1],";color:white;padding:.5rem 1rem;border-radius:6px;font-weight:bold;"), lbl1)),
            column(6, tags$div(style=paste0("background:",pal[2],";color:white;padding:.5rem 1rem;border-radius:6px;font-weight:bold;"), lbl2))
          ),
          br(),
          tabsetPanel(
            # ── 1. Summary Stats ──────────────────────────────────────────────
            tabPanel("📊 Summary Stats",
              br(),
              fluidRow(
                column(6, DT::dataTableOutput(ns("cmp_sum1"))),
                column(6, DT::dataTableOutput(ns("cmp_sum2")))
              )
            ),
            # ── 2. Publication Trends ─────────────────────────────────────────
            tabPanel("📈 Publication Trends",
              br(),
              plotlyOutput(ns("cmp_trend_over"), height="360px"),
              fluidRow(
                column(6, tags$h5(lbl1), plotlyOutput(ns("cmp_growth1"), height="220px"),
                       DT::dataTableOutput(ns("cmp_trend_tbl1"))),
                column(6, tags$h5(lbl2), plotlyOutput(ns("cmp_growth2"), height="220px"),
                       DT::dataTableOutput(ns("cmp_trend_tbl2")))
              )
            ),
            # ── 3. Authors ────────────────────────────────────────────────────
            tabPanel("👤 Authors",
              br(),
              plotlyOutput(ns("cmp_au_bar"), height="440px"),
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_au_tbl1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_au_tbl2")))
              )
            ),
            # ── 4. Most Cited Papers ──────────────────────────────────────────
            tabPanel("🏆 Most Cited",
              br(),
              tags$h5(lbl1), DT::dataTableOutput(ns("cmp_cited1")),
              br(),
              tags$h5(lbl2), DT::dataTableOutput(ns("cmp_cited2"))
            ),
            # ── 5. Keywords ───────────────────────────────────────────────────
            tabPanel("🔑 Keywords",
              br(),
              plotlyOutput(ns("cmp_kw_bar"), height="440px"),
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_kw_tbl1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_kw_tbl2")))
              )
            ),
            # ── 6. Sources / Journals ─────────────────────────────────────────
            tabPanel("📰 Sources",
              br(),
              plotlyOutput(ns("cmp_so_bar"), height="440px"),
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_so_tbl1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_so_tbl2")))
              )
            ),
            # ── 7. Countries ──────────────────────────────────────────────────
            tabPanel("🌍 Countries",
              br(),
              plotlyOutput(ns("cmp_co_bar"), height="400px"),
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_co_tbl1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_co_tbl2")))
              )
            ),
            # ── 8. Citation Analysis ──────────────────────────────────────────
            tabPanel("📊 Citation Analysis",
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_cit1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_cit2")))
              )
            ),
            # ── 9. Lotka's Law ────────────────────────────────────────────────
            tabPanel("📐 Lotka's Law",
              br(),
              plotlyOutput(ns("cmp_lotka"), height="380px"),
              br(),
              fluidRow(
                column(6, tags$h5(lbl1), DT::dataTableOutput(ns("cmp_lotka_tbl1"))),
                column(6, tags$h5(lbl2), DT::dataTableOutput(ns("cmp_lotka_tbl2")))
              )
            ),
            # ── 10. Download ──────────────────────────────────────────────────
            tabPanel("📥 Download",
              br(),
              downloadButton(ns("dl_compare"), "📥 Download Full Comparison (Excel)",
                             class="btn-success btn-lg")
            )
          )
        )
      })
    })

    # ── Comparison server outputs ─────────────────────────────────────────────
    cmp_n <- reactive({ req(input$cmp_top_n); input$cmp_top_n })

    output$cmp_sum1 <- DT::renderDataTable({
      req(input$run_compare, bib_data())
      lbl <- input$ds1_label %||% "Dataset 1"
      tool_dt(.summary_row(bib_data(), lbl), paste("Summary:", lbl))
    })
    output$cmp_sum2 <- DT::renderDataTable({
      req(input$run_compare, bib_data2())
      lbl <- input$ds2_label %||% "Dataset 2"
      tool_dt(.summary_row(bib_data2(), lbl), paste("Summary:", lbl))
    })

    output$cmp_trend_over <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      lbl1 <- input$ds1_label %||% "Dataset 1"
      lbl2 <- input$ds2_label %||% "Dataset 2"
      d1 <- .trend_df(bib_data()); d2 <- .trend_df(bib_data2())
      p  <- plotly::plot_ly()
      if (!is.null(d1))
        p <- plotly::add_trace(p, data=d1, x=~Year, y=~Articles, name=lbl1,
                               type="scatter", mode="lines+markers",
                               line=list(color=TEAL, width=2.5), marker=list(size=6))
      if (!is.null(d2))
        p <- plotly::add_trace(p, data=d2, x=~Year, y=~Articles, name=lbl2,
                               type="scatter", mode="lines+markers",
                               line=list(color=AMBER, width=2.5, dash="dash"), marker=list(size=6))
      p |> plotly::layout(title="Annual Production — Overlay", xaxis=list(title="Year"),
                          yaxis=list(title="Documents"), plot_bgcolor="white", paper_bgcolor="white")
    })

    .growth_plot <- function(M, label, color) {
      d <- .trend_df(M); if (is.null(d)||nrow(d)<2) return(plotly_empty())
      d <- d[d$Articles>0,]
      d$Growth <- c(NA, round(diff(d$Articles)/head(d$Articles,-1)*100,1))
      d <- d[!is.na(d$Growth),]; if (nrow(d)==0) return(plotly_empty())
      cols <- ifelse(d$Growth>=0, "#27AE60","#E74C3C")
      plotly::plot_ly(d, x=~Year, y=~Growth, type="bar", marker=list(color=cols)) |>
        plotly::layout(title=paste("Growth Rate (%):", label),
                       xaxis=list(title=""), yaxis=list(title="Growth %"),
                       plot_bgcolor="white", paper_bgcolor="white")
    }
    output$cmp_growth1 <- renderPlotly({
      req(input$run_compare, bib_data())
      .growth_plot(bib_data(), input$ds1_label %||% "Dataset 1", TEAL)
    })
    output$cmp_growth2 <- renderPlotly({
      req(input$run_compare, bib_data2())
      .growth_plot(bib_data2(), input$ds2_label %||% "Dataset 2", AMBER)
    })

    .trend_tbl_out <- function(M) {
      d <- .trend_df(M); if (is.null(d)) return(NULL)
      d <- d[order(-d$Year),]; d$Cumulative <- rev(cumsum(rev(d$Articles)))
      tool_dt(d)
    }
    output$cmp_trend_tbl1 <- DT::renderDataTable({ req(input$run_compare,bib_data()); .trend_tbl_out(bib_data()) })
    output$cmp_trend_tbl2 <- DT::renderDataTable({ req(input$run_compare,bib_data2()); .trend_tbl_out(bib_data2()) })

    output$cmp_au_bar <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      n <- cmp_n()
      lbl1 <- input$ds1_label %||% "Dataset 1"
      lbl2 <- input$ds2_label %||% "Dataset 2"
      .cmp_bar(.top_authors(bib_data(),n), .top_authors(bib_data2(),n),
               lbl1, lbl2, "Author","Publications","Top Authors Comparison")
    })
    output$cmp_au_tbl1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .top_authors(bib_data(), cmp_n()); if(is.null(df)) return(NULL)
      tool_dt(df)
    })
    output$cmp_au_tbl2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .top_authors(bib_data2(), cmp_n()); if(is.null(df)) return(NULL)
      tool_dt(df)
    })

    output$cmp_cited1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .cited_papers(bib_data(), cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_cited2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .cited_papers(bib_data2(), cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$cmp_kw_bar <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      n <- cmp_n(); lbl1 <- input$ds1_label %||% "Dataset 1"; lbl2 <- input$ds2_label %||% "Dataset 2"
      .cmp_bar(.top_kw(bib_data(),"DE",n), .top_kw(bib_data2(),"DE",n),
               lbl1, lbl2, "Keyword","Frequency","Top Author Keywords Comparison")
    })
    output$cmp_kw_tbl1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .top_kw(bib_data(),"DE",cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_kw_tbl2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .top_kw(bib_data2(),"DE",cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$cmp_so_bar <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      n <- cmp_n(); lbl1 <- input$ds1_label %||% "Dataset 1"; lbl2 <- input$ds2_label %||% "Dataset 2"
      .cmp_bar(.top_sources(bib_data(),n), .top_sources(bib_data2(),n),
               lbl1, lbl2, "Source","Articles","Top Sources/Journals Comparison")
    })
    output$cmp_so_tbl1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .top_sources(bib_data(),cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_so_tbl2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .top_sources(bib_data2(),cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$cmp_co_bar <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      n <- cmp_n(); lbl1 <- input$ds1_label %||% "Dataset 1"; lbl2 <- input$ds2_label %||% "Dataset 2"
      .cmp_bar(.top_countries(bib_data(),n), .top_countries(bib_data2(),n),
               lbl1, lbl2, "Country","Articles","Country Production Comparison")
    })
    output$cmp_co_tbl1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .top_countries(bib_data(),cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_co_tbl2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .top_countries(bib_data2(),cmp_n()); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$cmp_cit1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      lbl <- input$ds1_label %||% "Dataset 1"
      df <- .cit_summary(bib_data(), lbl); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_cit2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      lbl <- input$ds2_label %||% "Dataset 2"
      df <- .cit_summary(bib_data2(), lbl); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$cmp_lotka <- renderPlotly({
      req(input$run_compare, bib_data(), bib_data2())
      lbl1 <- input$ds1_label %||% "Dataset 1"; lbl2 <- input$ds2_label %||% "Dataset 2"
      l1 <- .lotka_df(bib_data()); l2 <- .lotka_df(bib_data2())
      p  <- plotly::plot_ly()
      if (!is.null(l1))
        p <- plotly::add_trace(p, data=l1, x=~N_Papers, y=~N_Authors, name=lbl1,
                               type="scatter", mode="lines+markers",
                               line=list(color=TEAL, width=2), marker=list(size=6))
      if (!is.null(l2))
        p <- plotly::add_trace(p, data=l2, x=~N_Papers, y=~N_Authors, name=lbl2,
                               type="scatter", mode="lines+markers",
                               line=list(color=AMBER, width=2, dash="dash"), marker=list(size=6))
      p |> plotly::layout(title="Lotka's Law Comparison", xaxis=list(title="Articles Published"),
                          yaxis=list(title="Number of Authors", type="log"),
                          plot_bgcolor="white", paper_bgcolor="white")
    })
    output$cmp_lotka_tbl1 <- DT::renderDataTable({
      req(input$run_compare,bib_data())
      df <- .lotka_df(bib_data()); if(is.null(df)) return(NULL); tool_dt(df)
    })
    output$cmp_lotka_tbl2 <- DT::renderDataTable({
      req(input$run_compare,bib_data2())
      df <- .lotka_df(bib_data2()); if(is.null(df)) return(NULL); tool_dt(df)
    })

    output$dl_compare <- downloadHandler(
      filename=function() paste0("Bibliometric_Comparison_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(bib_data(), bib_data2())
        lbl1 <- input$ds1_label %||% "Dataset 1"
        lbl2 <- input$ds2_label %||% "Dataset 2"
        n    <- cmp_n()
        sheets <- list()
        s1 <- .summary_row(bib_data(), lbl1); s2 <- .summary_row(bib_data2(), lbl2)
        if (!is.null(s1) && !is.null(s2))
          sheets[["Summary"]] <- merge(s1, s2, by="Metric", all=TRUE,
                                       suffixes=c(paste0("_",lbl1), paste0("_",lbl2)))
        for (lbl_i in c(lbl1, lbl2)) {
          M_i <- if (lbl_i==lbl1) bib_data() else bib_data2()
          k <- .top_kw(M_i,"DE",n);   if(!is.null(k)) sheets[[paste("KW",lbl_i)]] <- k
          a <- .top_authors(M_i,n);    if(!is.null(a)) sheets[[paste("AU",lbl_i)]] <- a
          s <- .top_sources(M_i,n);    if(!is.null(s)) sheets[[paste("SO",lbl_i)]] <- s
          co <- .top_countries(M_i,n); if(!is.null(co)) sheets[[paste("CO",lbl_i)]] <- co
          ci <- .cit_summary(M_i,lbl_i); if(!is.null(ci)) sheets[[paste("Cit",lbl_i)]] <- ci
          lo <- .lotka_df(M_i);        if(!is.null(lo)) sheets[[paste("Lotka",lbl_i)]] <- lo
          cp <- .cited_papers(M_i,n);  if(!is.null(cp)) sheets[[paste("Cited",lbl_i)]] <- cp
        }
        writexl::write_xlsx(sheets, file)
      }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # TIME HORIZON ANALYSIS
    # ══════════════════════════════════════════════════════════════════════════

    TH_COLORS <- c(TEAL, AMBER, "#9C27B0", "#E53935")

    output$th_period_ui <- renderUI({
      req(input$th_set_periods)
      isolate({
        req(bib_data())
        yrs <- suppressWarnings(as.integer(as.character(bib_data()$PY)))
        yrs <- yrs[!is.na(yrs) & yrs > 1900]
        yr_min <- if(length(yrs)>0) min(yrs) else 2000
        yr_max <- if(length(yrs)>0) max(yrs) else as.integer(format(Sys.Date(),"%Y"))
        n <- input$th_n_periods %||% 3
        breaks <- round(seq(yr_min, yr_max, length.out=n+1))
        lapply(seq_len(n), function(i) {
          fluidRow(
            column(2, tags$div(style="padding-top:30px;", tags$b(paste0("Period ", i, ":")))),
            column(3, numericInput(ns(paste0("th_from_", i)), "From year",
                                   value=breaks[i], min=1900, max=2030, step=1)),
            column(3, numericInput(ns(paste0("th_to_", i)), "To year",
                                   value=breaks[i+1]-(if(i<n) 1 else 0),
                                   min=1900, max=2030, step=1)),
            column(4, textInput(ns(paste0("th_lbl_", i)), "Label",
                                value=paste0(breaks[i], "–", breaks[i+1]-(if(i<n) 1 else 0))))
          )
        })
      })
    })

    th_results <- eventReactive(input$run_th, {
      req(bib_data(), input$th_n_periods)
      M <- bib_data()
      n <- input$th_n_periods
      yrs_all <- suppressWarnings(as.integer(as.character(M$PY)))
      lapply(seq_len(n), function(i) {
        from_yr <- input[[paste0("th_from_", i)]]
        to_yr   <- input[[paste0("th_to_",   i)]]
        lbl     <- input[[paste0("th_lbl_",  i)]] %||% paste0("Period ", i)
        if (is.null(from_yr) || is.null(to_yr)) return(NULL)
        idx <- !is.na(yrs_all) & yrs_all >= from_yr & yrs_all <= to_yr
        sub <- M[idx, , drop=FALSE]
        list(
          label     = lbl,
          from      = from_yr,
          to        = to_yr,
          n         = nrow(sub),
          data      = sub,
          summary   = .summary_row(sub, lbl),
          trend     = .trend_df(sub),
          authors   = .top_authors(sub, 15),
          cited     = .cited_papers(sub, 20),
          kw_de     = .top_kw(sub, "DE", 15),
          sources   = .top_sources(sub, 15),
          countries = .top_countries(sub, 15),
          cit_sum   = .cit_summary(sub, lbl),
          lotka     = .lotka_df(sub)
        )
      })
    })

    # ── helper: multi-period grouped horizontal bar ───────────────────────
    .th_multi_bar <- function(named_dfs, col_name, freq_name,
                               title="", pal=TH_COLORS, height=420) {
      p <- plotly::plot_ly()
      for (i in seq_along(named_dfs)) {
        df <- named_dfs[[i]]
        nm <- names(named_dfs)[i]
        if (is.null(df) || nrow(df)==0) next
        p <- plotly::add_trace(p, data=df,
               x=~get(freq_name), y=~reorder(get(col_name), get(freq_name)),
               name=nm, type="bar", orientation="h",
               marker=list(color=pal[((i-1L) %% length(pal)) + 1L], opacity=0.85))
      }
      p |> plotly::layout(title=title, barmode="group",
                          xaxis=list(title="Count"), yaxis=list(title=""),
                          plot_bgcolor="white", paper_bgcolor="white",
                          legend=list(x=0.7, y=0.05),
                          margin=list(l=200), height=height)
    }

    # ── Summary Stats ─────────────────────────────────────────────────────
    output$th_sum_tbl <- DT::renderDataTable({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      if (length(res)==0) return(NULL)
      merged <- Reduce(function(a,b) merge(a,b,by="Metric",all=TRUE),
                       lapply(res, `[[`, "summary"))
      tool_dt(merged, "Summary Statistics by Time Period")
    })

    # ── Publication Trends ────────────────────────────────────────────────
    output$th_bar <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      df  <- data.frame(Period=sapply(res,`[[`,"label"),
                        Docs=sapply(res,`[[`,"n"), stringsAsFactors=FALSE)
      pal <- TH_COLORS[seq_len(nrow(df))]
      plotly::plot_ly(df, x=~Period, y=~Docs, type="bar",
                      marker=list(color=pal)) |>
        plotly::layout(title="Documents per Time Period",
                       xaxis=list(title=""), yaxis=list(title="Documents"),
                       plot_bgcolor="white", paper_bgcolor="white")
    })

    output$th_trends_over <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      p   <- plotly::plot_ly()
      for (i in seq_along(res)) {
        df_i <- res[[i]]$trend
        if (is.null(df_i) || nrow(df_i)==0) next
        p <- plotly::add_trace(p, data=df_i, x=~Year, y=~Articles,
               name=res[[i]]$label, type="scatter", mode="lines+markers",
               line=list(color=TH_COLORS[i], width=2.2),
               marker=list(size=5, color=TH_COLORS[i]))
      }
      p |> plotly::layout(title="Annual Output by Period",
                          xaxis=list(title="Year"), yaxis=list(title="Documents"),
                          plot_bgcolor="white", paper_bgcolor="white",
                          legend=list(x=0.02, y=0.98))
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_growth_", i)]] <- renderPlotly({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          r <- res[[i]]; df <- r$trend
          if (is.null(df) || nrow(df)<1) return(NULL)
          plotly::plot_ly(df, x=~Year, y=~Articles, type="bar",
                          marker=list(color=TH_COLORS[i])) |>
            plotly::layout(title=paste(r$label, "— Annual Output"),
                           xaxis=list(title="Year"), yaxis=list(title="Documents"),
                           plot_bgcolor="white", paper_bgcolor="white")
        })
        output[[paste0("th_trend_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$trend; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "Annual Output"))
        })
      })
    }

    # ── Authors ───────────────────────────────────────────────────────────
    output$th_au_bar <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      dfs <- setNames(lapply(res,`[[`,"authors"), sapply(res,`[[`,"label"))
      .th_multi_bar(dfs, "Author", "Publications",
                    "Top Authors by Period", height=460)
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_au_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$authors; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Top Authors"))
        })
      })
    }

    # ── Most Cited ────────────────────────────────────────────────────────
    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_cited_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$cited; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Most Cited Papers"))
        })
      })
    }

    # ── Keywords ──────────────────────────────────────────────────────────
    output$th_kw_bar <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      dfs <- setNames(lapply(res,`[[`,"kw_de"), sapply(res,`[[`,"label"))
      .th_multi_bar(dfs, "Keyword", "Frequency",
                    "Top Author Keywords (DE) by Period", height=460)
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_kw_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$kw_de; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Top Keywords"))
        })
      })
    }

    # ── Sources ───────────────────────────────────────────────────────────
    output$th_so_bar <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      dfs <- setNames(lapply(res,`[[`,"sources"), sapply(res,`[[`,"label"))
      .th_multi_bar(dfs, "Source", "Articles",
                    "Top Sources/Journals by Period", height=460)
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_so_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$sources; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Top Sources"))
        })
      })
    }

    # ── Countries ─────────────────────────────────────────────────────────
    output$th_co_bar <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      dfs <- setNames(lapply(res,`[[`,"countries"), sapply(res,`[[`,"label"))
      .th_multi_bar(dfs, "Country", "Articles",
                    "Top Countries by Period", height=460)
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_co_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$countries; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Top Countries"))
        })
      })
    }

    # ── Citation Analysis ─────────────────────────────────────────────────
    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_cit_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$cit_sum; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Citation Statistics"))
        })
      })
    }

    # ── Lotka's Law ───────────────────────────────────────────────────────
    output$th_lotka <- renderPlotly({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      p <- plotly::plot_ly()
      for (i in seq_along(res)) {
        df <- res[[i]]$lotka
        if (is.null(df) || nrow(df)==0) next
        p <- plotly::add_trace(p, data=df, x=~N_Papers, y=~N_Authors,
               name=res[[i]]$label, type="scatter", mode="lines+markers",
               line=list(color=TH_COLORS[i], width=2),
               marker=list(size=6, color=TH_COLORS[i]))
      }
      p |> plotly::layout(title="Lotka's Law — Scientific Productivity by Period",
                          xaxis=list(title="Papers per Author"),
                          yaxis=list(title="Number of Authors", type="log"),
                          plot_bgcolor="white", paper_bgcolor="white",
                          legend=list(x=0.7, y=0.98))
    })

    for (idx in 1:4) {
      local({
        i <- idx
        output[[paste0("th_lotka_tbl_", i)]] <- DT::renderDataTable({
          req(th_results())
          res <- Filter(Negate(is.null), th_results())
          if (i > length(res)) return(NULL)
          df <- res[[i]]$lotka; if (is.null(df)) return(NULL)
          tool_dt(df, paste(res[[i]]$label, "— Lotka Distribution"))
        })
      })
    }

    # ── Comprehensive Results UI ──────────────────────────────────────────
    output$th_results_ui <- renderUI({
      req(th_results())
      res <- Filter(Negate(is.null), th_results())
      if (length(res)==0) return(tags$p("No data found for specified periods."))
      np    <- length(res)
      col_w <- max(3L, floor(12L / np))
      # helper: generate per-period table columns
      per_cols <- function(prefix) {
        lapply(seq_len(np), function(i)
          column(col_w,
            tags$h5(style="color:#1A3A5C;font-weight:bold;", res[[i]]$label),
            DT::dataTableOutput(ns(paste0(prefix, "_", i)))))
      }
      tagList(
        tabsetPanel(type="tabs",
          tabPanel("📊 Summary Stats",
            br(),
            withSpinner(DT::dataTableOutput(ns("th_sum_tbl")))
          ),
          tabPanel("📈 Publication Trends",
            br(),
            tags$h4("Documents per Period"),
            withSpinner(plotlyOutput(ns("th_bar"), height="280px")),
            hr(),
            tags$h4("Annual Output — Overlay"),
            withSpinner(plotlyOutput(ns("th_trends_over"), height="340px")),
            hr(),
            tags$h4("Annual Output per Period"),
            fluidRow(
              lapply(seq_len(np), function(i)
                column(col_w,
                  tags$h5(style="color:#1A3A5C;font-weight:bold;", res[[i]]$label),
                  withSpinner(plotlyOutput(ns(paste0("th_growth_", i)), height="220px")),
                  DT::dataTableOutput(ns(paste0("th_trend_tbl_", i)))))
            )
          ),
          tabPanel("👤 Authors",
            br(),
            tags$h4("Top Authors — Grouped by Period"),
            withSpinner(plotlyOutput(ns("th_au_bar"), height="460px")),
            hr(),
            tags$h4("Author Rankings per Period"),
            fluidRow(per_cols("th_au_tbl"))
          ),
          tabPanel("🏆 Most Cited",
            br(),
            tags$h4("Most Cited Papers per Period"),
            fluidRow(per_cols("th_cited"))
          ),
          tabPanel("🔑 Keywords",
            br(),
            tags$h4("Top Keywords — Grouped by Period"),
            withSpinner(plotlyOutput(ns("th_kw_bar"), height="460px")),
            hr(),
            tags$h4("Keyword Rankings per Period"),
            fluidRow(per_cols("th_kw_tbl"))
          ),
          tabPanel("📰 Sources",
            br(),
            tags$h4("Top Sources — Grouped by Period"),
            withSpinner(plotlyOutput(ns("th_so_bar"), height="460px")),
            hr(),
            tags$h4("Source Rankings per Period"),
            fluidRow(per_cols("th_so_tbl"))
          ),
          tabPanel("🌍 Countries",
            br(),
            tags$h4("Top Countries — Grouped by Period"),
            withSpinner(plotlyOutput(ns("th_co_bar"), height="460px")),
            hr(),
            tags$h4("Country Rankings per Period"),
            fluidRow(per_cols("th_co_tbl"))
          ),
          tabPanel("📊 Citation Analysis",
            br(),
            tags$h4("Citation Statistics per Period"),
            fluidRow(per_cols("th_cit"))
          ),
          tabPanel("📐 Lotka's Law",
            br(),
            tags$h4("Scientific Productivity — Overlay"),
            withSpinner(plotlyOutput(ns("th_lotka"), height="380px")),
            hr(),
            tags$h4("Lotka Distribution per Period"),
            fluidRow(per_cols("th_lotka_tbl"))
          ),
          tabPanel("📥 Download",
            br(),
            tags$p("Download the full Time Horizon Report as an Excel workbook with one sheet per analysis per period."),
            br(),
            downloadButton(ns("dl_th"), "📥 Download Time Horizon Report (Excel)")
          )
        )
      )
    })

    output$dl_th <- downloadHandler(
      filename=function() paste0("Time_Horizons_", Sys.Date(), ".xlsx"),
      content=function(file) {
        req(th_results())
        res <- Filter(Negate(is.null), th_results())
        sheets <- list()
        merged <- Reduce(function(a,b) merge(a,b,by="Metric",all=TRUE),
                         lapply(res, `[[`, "summary"))
        sheets[["Summary"]] <- merged
        for (r in res) {
          lbl <- substr(gsub("[^A-Za-z0-9_]", "_", r$label), 1, 20)
          if (!is.null(r$trend))     sheets[[paste0("Trend_",     lbl)]] <- r$trend
          if (!is.null(r$authors))   sheets[[paste0("Authors_",   lbl)]] <- r$authors
          if (!is.null(r$cited))     sheets[[paste0("Cited_",     lbl)]] <- r$cited
          if (!is.null(r$kw_de))     sheets[[paste0("Keywords_",  lbl)]] <- r$kw_de
          if (!is.null(r$sources))   sheets[[paste0("Sources_",   lbl)]] <- r$sources
          if (!is.null(r$countries)) sheets[[paste0("Countries_", lbl)]] <- r$countries
          if (!is.null(r$cit_sum))   sheets[[paste0("Citations_", lbl)]] <- r$cit_sum
          if (!is.null(r$lotka))     sheets[[paste0("Lotka_",     lbl)]] <- r$lotka
        }
        writexl::write_xlsx(sheets, file)
      }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # 🧠 KNOWLEDGE STREAMS — Auto-Clustering + Groq AI Naming
    # ══════════════════════════════════════════════════════════════════════════

    ks_clusters_rv  <- reactiveVal(NULL)   # list: cluster assignments + keywords
    ks_ai_labels_rv <- reactiveVal(NULL)   # list of AI-generated names/descriptions

    # ── Run clustering when button clicked ─────────────────────────────────
    observeEvent(input$run_ks, {
      req(bib_data())
      withProgress(message = "🔍 Detecting knowledge streams...", value = 0, {

        tryCatch({
          if (!requireNamespace("igraph",   quietly = TRUE)) install.packages("igraph")
          library(igraph)

          M       <- bib_data()
          method  <- input$ks_method
          min_f   <- max(1L, as.integer(input$ks_min_freq))
          n_top   <- max(20L, as.integer(input$ks_n_top))

          setProgress(0.1, "Building keyword co-occurrence network...")

          # Extract keywords (prefer DE author keywords, fall back to ID)
          kw_col <- if (!is.null(M$DE) && any(nzchar(M$DE, keepNA=FALSE), na.rm=TRUE)) "DE"
                    else if (!is.null(M$ID) && any(nzchar(M$ID, keepNA=FALSE), na.rm=TRUE)) "ID"
                    else NULL
          if (is.null(kw_col)) {
            showNotification("No keyword column found (DE or ID). Please check your data.", type="warning")
            return()
          }

          # Build co-occurrence matrix via bibliometrix
          NetMatrix <- tryCatch(
            bibliometrix::biblioNetwork(M, analysis="co-occurrences",
              network="keywords", sep=";", n=n_top, shortlabel=FALSE),
            error = function(e) NULL
          )
          if (is.null(NetMatrix) || nrow(NetMatrix) == 0) {
            showNotification("Could not build keyword network. Try reducing min frequency.", type="warning")
            return()
          }

          setProgress(0.3, "Applying community detection...")

          # Filter by frequency
          kw_freq  <- rowSums(NetMatrix)
          keep     <- kw_freq >= min_f
          if (sum(keep) < 4) keep <- kw_freq >= 1
          NetMatrix <- NetMatrix[keep, keep]

          # Build igraph from adjacency matrix
          g <- igraph::graph_from_adjacency_matrix(NetMatrix, mode="undirected",
                                                   weighted=TRUE, diag=FALSE)
          g <- igraph::simplify(g, remove.multiple=TRUE, remove.loops=TRUE)

          # Remove isolated nodes
          isolated <- igraph::degree(g) == 0
          g <- igraph::delete_vertices(g, which(isolated))
          if (igraph::vcount(g) < 4) {
            showNotification("Network too sparse after filtering. Try lowering min frequency.", type="warning")
            return()
          }

          # Community detection
          comm <- switch(method,
            louvain       = igraph::cluster_louvain(g),
            leiden        = tryCatch(igraph::cluster_leiden(g, objective_function="modularity"),
                             error=function(e) igraph::cluster_louvain(g)),
            walktrap      = igraph::cluster_walktrap(g, steps=4),
            fastgreedy    = igraph::cluster_fast_greedy(g),
            edge_betweenness = igraph::cluster_edge_betweenness(g),
            igraph::cluster_louvain(g)
          )

          setProgress(0.5, "Extracting cluster keywords...")

          membership   <- igraph::membership(comm)
          n_clusters   <- igraph::communities(comm) |> length()
          modularity_v <- round(igraph::modularity(comm), 4)
          node_names   <- igraph::V(g)$name
          n_kw_show    <- as.integer(input$ks_n_kw)

          # Build per-cluster keyword list + frequency info
          clusters_list <- lapply(seq_len(n_clusters), function(k) {
            nodes_in_k  <- node_names[membership == k]
            if (length(nodes_in_k) == 0) return(NULL)
            # Frequency = row sum in original net for those nodes
            present     <- intersect(nodes_in_k, rownames(NetMatrix))
            freq_vec    <- sort(kw_freq[present], decreasing=TRUE)
            top_kws     <- names(head(freq_vec, n_kw_show))
            list(
              cluster_id  = k,
              keywords    = top_kws,
              all_nodes   = nodes_in_k,
              size        = length(nodes_in_k),
              freq_vec    = freq_vec
            )
          })
          clusters_list <- Filter(Negate(is.null), clusters_list)
          # Sort by size descending
          clusters_list <- clusters_list[order(sapply(clusters_list, `[[`, "size"), decreasing=TRUE)]
          # Re-assign sequential IDs after sort
          for (i in seq_along(clusters_list)) clusters_list[[i]]$cluster_id <- i

          setProgress(0.7, "Building temporal data...")

          # Temporal data: keywords × year co-occurrence
          py_col <- if (!is.null(M$PY)) M$PY else NULL
          kw_raw <- M[[kw_col]]
          temporal_df <- NULL
          if (!is.null(py_col) && !is.null(kw_raw)) {
            rows_t <- lapply(seq_len(nrow(M)), function(i) {
              yr  <- suppressWarnings(as.integer(py_col[i]))
              kws <- trimws(unlist(strsplit(as.character(kw_raw[i]), ";")))
              kws <- kws[nzchar(kws) & !is.na(kws)]
              if (is.na(yr) || length(kws)==0) return(NULL)
              data.frame(year=yr, keyword=toupper(kws), stringsAsFactors=FALSE)
            })
            temporal_df <- do.call(rbind, Filter(Negate(is.null), rows_t))
          }

          # Map each cluster's keywords to years
          cluster_year_df <- NULL
          if (!is.null(temporal_df)) {
            all_kw_cluster <- do.call(rbind, lapply(clusters_list, function(cl) {
              data.frame(keyword=toupper(cl$keywords),
                cluster=paste0("C", cl$cluster_id, ": (", cl$size, " kws)"),
                cluster_id=cl$cluster_id, stringsAsFactors=FALSE)
            }))
            merged <- merge(temporal_df, all_kw_cluster, by="keyword")
            if (nrow(merged)>0) {
              cluster_year_df <- aggregate(year ~ cluster + cluster_id, data=merged,
                FUN=function(x) length(x))
              names(cluster_year_df)[3] <- "count"
              # Full grid
              full_grid <- expand.grid(
                cluster    = unique(cluster_year_df$cluster),
                year       = seq(min(merged$year, na.rm=TRUE),
                                 max(merged$year, na.rm=TRUE)),
                stringsAsFactors=FALSE)
              cluster_year_df2 <- aggregate(
                cbind(count=rep(1, nrow(merged))) ~ cluster + year,
                data=merged, FUN=length)
              cluster_year_df <- merge(full_grid, cluster_year_df2, all.x=TRUE)
              cluster_year_df$count[is.na(cluster_year_df$count)] <- 0
            }
          }

          setProgress(0.9, "Storing results...")

          ks_clusters_rv(list(
            clusters     = clusters_list,
            graph        = g,
            membership   = membership,
            n_clusters   = n_clusters,
            modularity   = modularity_v,
            method       = method,
            temporal_df  = cluster_year_df,
            n_nodes      = igraph::vcount(g),
            n_edges      = igraph::ecount(g)
          ))
          ks_ai_labels_rv(NULL)  # clear previous AI labels

          setProgress(1)
          showNotification(
            sprintf("✅ Detected %d knowledge streams from %d keywords (Modularity = %.3f)",
                    n_clusters, igraph::vcount(g), modularity_v),
            type="message", duration=6)

        }, error = function(e) {
          showNotification(paste("Error:", e$message), type="error", duration=10)
        })
      })
    })

    # ── AI Name & Describe clusters ────────────────────────────────────────
    observeEvent(input$ks_ai_name, {
      req(ks_clusters_rv())
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) {
        showNotification("Please enter your Groq key in the sidebar first.", type="warning")
        return()
      }
      clusters <- ks_clusters_rv()$clusters
      withProgress(message = "🤖 Groq AI naming clusters...", value=0.2, {
        tryCatch({
          # Build a compact cluster summary for the prompt
          cluster_text <- paste(sapply(clusters, function(cl) {
            sprintf("Cluster %d (%d keywords): %s",
              cl$cluster_id, cl$size, paste(cl$keywords, collapse=", "))
          }), collapse="\n")

          prompt <- paste0(
            "You are an expert bibliometrician and research analyst. ",
            "I have identified ", length(clusters), " knowledge stream clusters ",
            "from a bibliometric keyword co-occurrence network analysis.\n\n",
            "For each cluster, I will provide the top keywords that define it. ",
            "Please provide:\n",
            "1. A concise academic NAME for the cluster (3-6 words max)\n",
            "2. A 2-sentence DESCRIPTION of the research theme\n",
            "3. The likely RESEARCH MATURITY: Emerging / Growing / Mature / Declining\n",
            "4. One KEY INSIGHT about this knowledge stream's importance\n\n",
            "Clusters:\n", cluster_text, "\n\n",
            "IMPORTANT: Respond in exactly this JSON format (array of objects):\n",
            '[{"id":1,"name":"...","description":"...","maturity":"...","insight":"..."},',
            '{"id":2,"name":"...","description":"...","maturity":"...","insight":"..."}]',
            "\n\nReturn ONLY the JSON array, no other text."
          )

          setProgress(0.5, "Waiting for Groq response...")
          raw <- call_gemini(prompt, api_key, model="llama-3.3-70b-versatile")

          # Parse JSON robustly
          json_str <- regmatches(raw, regexpr("\\[\\{.+\\}\\]", raw, perl=TRUE))
          if (length(json_str) == 0) {
            # Try to extract JSON even without perfect formatting
            json_str <- gsub("^[^\\[]*", "", raw)
            json_str <- gsub("[^\\]]*$", "", json_str)
          }

          labels <- tryCatch(
            jsonlite::fromJSON(json_str),
            error = function(e) NULL
          )

          if (is.null(labels) || !is.data.frame(labels)) {
            # Fallback: parse as best as we can
            labels <- data.frame(
              id=seq_along(clusters),
              name=paste("Stream", seq_along(clusters)),
              description="AI could not parse structured response. See raw output.",
              maturity="Unknown",
              insight=raw,
              stringsAsFactors=FALSE
            )
          }

          setProgress(0.9)
          ks_ai_labels_rv(labels)
          showNotification("✅ AI cluster names ready!", type="message", duration=4)

        }, error = function(e) {
          showNotification(paste("AI Error:", e$message), type="error")
        })
      })
    })

    # ── Results UI ─────────────────────────────────────────────────────────
    output$ks_results_ui <- renderUI({
      res <- ks_clusters_rv()
      if (is.null(res)) {
        return(tags$div(
          style="text-align:center;padding:3rem;color:#95a5a6;",
          tags$div(style="font-size:3rem;margin-bottom:.8rem;", "🧠"),
          tags$h4(style="color:#7f8c8d;", "No clusters yet"),
          tags$p("Upload your bibliometric data and click ", tags$b("Detect Knowledge Streams"), " above.")
        ))
      }

      ai_labels <- ks_ai_labels_rv()

      # Maturity colour helper
      maturity_color <- function(m) switch(tolower(m %||% ""),
        "emerging"="background:#D5F5E3;color:#1e8449;",
        "growing"="background:#D6EAF8;color:#1a5276;",
        "mature"="background:#FCF3CF;color:#7d6608;",
        "declining"="background:#FADBD8;color:#922b21;",
        "background:#F2F3F4;color:#555;"
      )

      palette <- c("#2196A6","#E74C3C","#27AE60","#F39C12","#8E44AD",
                   "#16A085","#C0392B","#D35400","#2980B9","#7D3C98",
                   "#117A65","#B7950B","#1F618D","#6E2F8A","#1ABC9C")

      tagList(
        # ── Header stats bar ────────────────────────────────────────────
        fluidRow(
          column(3,
            tags$div(style="background:#EBF5FB;border-radius:8px;padding:.7rem;text-align:center;",
              tags$div(style="font-size:1.8rem;font-weight:bold;color:#1a5276;", res$n_clusters),
              tags$div(style="font-size:.78rem;color:#555;", "Knowledge Streams")
            )
          ),
          column(3,
            tags$div(style="background:#EAF7F0;border-radius:8px;padding:.7rem;text-align:center;",
              tags$div(style="font-size:1.8rem;font-weight:bold;color:#1e8449;", res$n_nodes),
              tags$div(style="font-size:.78rem;color:#555;", "Keywords Mapped")
            )
          ),
          column(3,
            tags$div(style="background:#FEF9E7;border-radius:8px;padding:.7rem;text-align:center;",
              tags$div(style="font-size:1.8rem;font-weight:bold;color:#7d6608;", res$n_edges),
              tags$div(style="font-size:.78rem;color:#555;", "Co-occurrences")
            )
          ),
          column(3,
            tags$div(style="background:#F5EEF8;border-radius:8px;padding:.7rem;text-align:center;",
              tags$div(style="font-size:1.8rem;font-weight:bold;color:#6E2F8A;",
                sprintf("%.3f", res$modularity)),
              tags$div(style="font-size:.78rem;color:#555;", "Modularity (0–1)")
            )
          )
        ),

        tags$br(),

        # ── AI name button ───────────────────────────────────────────────
        fluidRow(
          column(4,
            actionButton(ns("ks_ai_name"), "🤖 AI Name & Describe All Clusters",
              class="btn-success btn-block", style="font-weight:bold;")
          ),
          column(8,
            tags$div(style="padding:.4rem 0;font-size:.8rem;color:#7f8c8d;",
              "Uses Groq AI (llama-3.3-70b) to intelligently name each cluster and identify its research theme, maturity stage, and key insight. ",
              tags$b("Requires Groq key in sidebar."))
          )
        ),

        tags$br(),

        # ── Cluster cards ────────────────────────────────────────────────
        tags$h5(style="color:#1a5276;border-bottom:2px solid #2196A6;padding-bottom:.3rem;",
          "📌 Knowledge Stream Clusters"),
        do.call(fluidRow, lapply(seq_along(res$clusters), function(i) {
          cl  <- res$clusters[[i]]
          col <- palette[((i-1) %% length(palette)) + 1]
          ai  <- if (!is.null(ai_labels) && i <= nrow(ai_labels)) ai_labels[i,] else NULL

          name_txt    <- if (!is.null(ai)) as.character(ai$name) else paste("Stream", cl$cluster_id)
          desc_txt    <- if (!is.null(ai)) as.character(ai$description) else "Click 'AI Name & Describe' for interpretation."
          maturity    <- if (!is.null(ai)) as.character(ai$maturity) else ""
          insight_txt <- if (!is.null(ai)) as.character(ai$insight) else ""
          mat_style   <- maturity_color(maturity)

          column(4,
            tags$div(
              style = sprintf(
                "border-left:5px solid %s;background:#fff;border-radius:8px;padding:.9rem 1rem;margin-bottom:.9rem;box-shadow:0 2px 6px rgba(0,0,0,.08);",
                col),
              # Header row
              tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:.4rem;",
                tags$span(
                  style=sprintf("background:%s;color:white;border-radius:12px;padding:.15rem .6rem;font-size:.7rem;font-weight:bold;",col),
                  sprintf("Cluster %d", cl$cluster_id)),
                if (nzchar(maturity))
                  tags$span(style=sprintf("border-radius:10px;padding:.12rem .55rem;font-size:.68rem;font-weight:bold;%s", mat_style), maturity)
              ),
              tags$div(style="font-weight:bold;font-size:.95rem;color:#1a1a2e;margin-bottom:.3rem;", name_txt),
              tags$div(style="font-size:.78rem;color:#555;margin-bottom:.5rem;", desc_txt),
              if (nzchar(insight_txt))
                tags$div(
                  style="background:#fffbf0;border-left:3px solid #f39c12;padding:.4rem .6rem;border-radius:4px;font-size:.76rem;color:#7d6608;margin-bottom:.5rem;",
                  tags$b("💡 "), insight_txt),
              tags$div(style="font-size:.74rem;color:#888;margin-top:.3rem;",
                sprintf("📊 %d keywords: ", cl$size),
                tags$span(style=sprintf("color:%s;font-weight:bold;",col),
                  paste(head(cl$keywords, 5), collapse=" · ")),
                if (cl$size > 5)
                  tags$span(style="color:#aaa;", sprintf(" + %d more", cl$size - 5))
              )
            )
          )
        })),

        # ── Network ──────────────────────────────────────────────────────
        tags$h5(style="color:#1a5276;border-bottom:2px solid #2196A6;padding-bottom:.3rem;margin-top:.5rem;",
          "🕸️ Interactive Cluster Network"),
        tags$p(style="font-size:.79rem;color:#7f8c8d;margin-bottom:.5rem;",
          "Node size = keyword frequency · Edge width = co-occurrence strength · Colour = knowledge stream · Hover for details."),
        withSpinner(visNetwork::visNetworkOutput(ns("ks_network"), height="560px")),

        tags$br(),

        # ── Temporal heatmap ─────────────────────────────────────────────
        if (!is.null(res$temporal_df) && nrow(res$temporal_df) > 0) tagList(
          tags$h5(style="color:#1a5276;border-bottom:2px solid #2196A6;padding-bottom:.3rem;",
            "📅 Knowledge Stream Evolution Over Time"),
          tags$p(style="font-size:.79rem;color:#7f8c8d;margin-bottom:.5rem;",
            "Shows how each research stream's activity has grown or declined over the years."),
          withSpinner(plotlyOutput(ns("ks_temporal"), height="350px"))
        ),

        tags$br(),

        # ── Cluster keyword table ─────────────────────────────────────────
        tags$h5(style="color:#1a5276;border-bottom:2px solid #2196A6;padding-bottom:.3rem;",
          "📋 Full Cluster Keyword Table"),
        DT::dataTableOutput(ns("ks_kw_table")),

        tags$br(),
        downloadButton(ns("ks_download"), "📥 Download Cluster Report (Excel)",
          class="btn-success", style="margin-bottom:1rem;")
      )
    })

    # ── Network output ──────────────────────────────────────────────────
    output$ks_network <- visNetwork::renderVisNetwork({
      res <- ks_clusters_rv(); req(res)
      ai_labels <- ks_ai_labels_rv()

      g          <- res$graph
      membership <- res$membership
      clusters   <- res$clusters
      palette    <- c("#2196A6","#E74C3C","#27AE60","#F39C12","#8E44AD",
                      "#16A085","#C0392B","#D35400","#2980B9","#7D3C98",
                      "#117A65","#B7950B","#1F618D","#6E2F8A","#1ABC9C")

      node_names <- igraph::V(g)$name
      kw_freq    <- rowSums(igraph::as_adjacency_matrix(g, attr="weight"))
      n_nodes    <- length(node_names)

      # Cluster index for each node (ordered by cluster size)
      sorted_ids <- sapply(clusters, `[[`, "cluster_id")
      mem_sorted <- match(membership, sorted_ids)
      if (any(is.na(mem_sorted))) mem_sorted[is.na(mem_sorted)] <- 1

      # Node labels from AI if available
      get_stream_name <- function(idx) {
        if (!is.null(ai_labels) && idx <= nrow(ai_labels))
          as.character(ai_labels$name[idx])
        else paste("Stream", idx)
      }

      node_color <- palette[((mem_sorted - 1) %% length(palette)) + 1]
      node_size  <- pmin(60, pmax(15, 8 + sqrt(kw_freq + 1) * 5))

      nodes <- data.frame(
        id    = seq_len(n_nodes),
        label = node_names,
        title = paste0("<b>", node_names, "</b><br>",
                       "Stream: ", sapply(mem_sorted, get_stream_name), "<br>",
                       "Freq: ", round(kw_freq, 1)),
        group = sapply(mem_sorted, get_stream_name),
        color = node_color,
        size  = node_size,
        font  = list(size=10, color="#1a1a2e"),
        borderWidth = 2,
        stringsAsFactors = FALSE
      )

      el <- igraph::as_data_frame(g, what="edges")
      edges <- data.frame(
        from  = match(el$from, node_names),
        to    = match(el$to,   node_names),
        width = pmin(8, pmax(0.5, log1p(el$weight) * 1.5)),
        color = list(color="#cccccc", opacity=0.6),
        title = paste0("co-occ: ", round(el$weight, 1)),
        stringsAsFactors = FALSE
      )

      visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visOptions(
          highlightNearest = list(enabled=TRUE, degree=1, labelOnly=FALSE),
          selectedBy       = list(variable="group", main="Filter by Stream"),
          nodesIdSelection = TRUE) |>
        visNetwork::visLayout(randomSeed=42) |>
        visNetwork::visPhysics(
          solver="forceAtlas2Based",
          forceAtlas2Based=list(gravitationalConstant=-80, springLength=100),
          stabilization=list(iterations=200)) |>
        visNetwork::visInteraction(hover=TRUE, tooltipDelay=100,
          navigationButtons=TRUE, zoomView=TRUE) |>
        visNetwork::visLegend(enabled=TRUE, position="right", width=0.18) |>
        visNetwork::visNodes(borderWidthSelected=4) |>
        visNetwork::visEdges(smooth=list(type="continuous"))
    })

    # ── Temporal heatmap ────────────────────────────────────────────────
    output$ks_temporal <- renderPlotly({
      res <- ks_clusters_rv(); req(res, res$temporal_df)
      df  <- res$temporal_df; req(nrow(df) > 0)
      ai_labels <- ks_ai_labels_rv()

      # Rename clusters with AI labels if available
      if (!is.null(ai_labels) && nrow(ai_labels) > 0) {
        old_nms <- unique(df$cluster)
        for (nm in old_nms) {
          idx <- as.integer(gsub(".*?(\\d+).*", "\\1", nm))
          if (!is.na(idx) && idx <= nrow(ai_labels))
            df$cluster[df$cluster == nm] <- paste0("C", idx, ": ", ai_labels$name[idx])
        }
      }

      # Filter to years with data
      yr_range <- range(df$year[df$count > 0], na.rm=TRUE)
      df <- df[df$year >= yr_range[1] & df$year <= yr_range[2], ]

      plotly::plot_ly(df,
        x = ~year, y = ~cluster, z = ~count, type="heatmap",
        colorscale=list(c(0,"#f0f3f4"), c(0.3,"#85c1e9"), c(0.7,"#2196A6"), c(1,"#1a5276")),
        hovertemplate="%{y}<br>Year: %{x}<br>Papers: %{z}<extra></extra>",
        showscale=TRUE, colorbar=list(title="Papers")) |>
        plotly::layout(
          title=list(text="Knowledge Stream Activity by Year", font=list(size=13,color="#1a5276")),
          xaxis=list(title="Year", tickformat="d", dtick=1),
          yaxis=list(title="", autorange="reversed", tickfont=list(size=10)),
          margin=list(l=220, b=50),
          plot_bgcolor="#fff", paper_bgcolor="#fff"
        )
    })

    # ── Keyword table ────────────────────────────────────────────────────
    output$ks_kw_table <- DT::renderDataTable({
      res <- ks_clusters_rv(); req(res)
      ai_labels <- ks_ai_labels_rv()
      rows <- lapply(res$clusters, function(cl) {
        ai  <- if (!is.null(ai_labels) && cl$cluster_id <= nrow(ai_labels)) ai_labels[cl$cluster_id,] else NULL
        nm  <- if (!is.null(ai)) as.character(ai$name) else paste("Stream", cl$cluster_id)
        mat <- if (!is.null(ai)) as.character(ai$maturity) else ""
        data.frame(
          Cluster     = cl$cluster_id,
          Stream_Name = nm,
          Maturity    = mat,
          Size_kws    = cl$size,
          Top_Keywords= paste(head(cl$keywords, 8), collapse=" · "),
          stringsAsFactors=FALSE
        )
      })
      df <- do.call(rbind, rows)
      tool_dt(df, "Knowledge Stream Cluster Summary")
    })

    # ── Download handler ─────────────────────────────────────────────────
    output$ks_download <- downloadHandler(
      filename = function() paste0("Knowledge_Streams_", format(Sys.time(),"%Y%m%d_%H%M"),".xlsx"),
      content  = function(file) {
        if (!requireNamespace("openxlsx", quietly=TRUE)) install.packages("openxlsx")
        res <- ks_clusters_rv(); req(res)
        ai_labels <- ks_ai_labels_rv()
        wb <- openxlsx::createWorkbook()

        # Sheet 1 — Cluster Summary
        sum_rows <- lapply(res$clusters, function(cl) {
          ai  <- if (!is.null(ai_labels) && cl$cluster_id <= nrow(ai_labels)) ai_labels[cl$cluster_id,] else NULL
          data.frame(
            Cluster_ID  = cl$cluster_id,
            Stream_Name = if(!is.null(ai)) as.character(ai$name) else paste("Stream",cl$cluster_id),
            Maturity    = if(!is.null(ai)) as.character(ai$maturity) else "",
            Description = if(!is.null(ai)) as.character(ai$description) else "",
            Key_Insight = if(!is.null(ai)) as.character(ai$insight) else "",
            N_Keywords  = cl$size,
            Top_Keywords= paste(cl$keywords, collapse="; "),
            stringsAsFactors=FALSE
          )
        })
        openxlsx::addWorksheet(wb, "Cluster_Summary")
        openxlsx::writeData(wb, "Cluster_Summary", do.call(rbind, sum_rows))

        # Sheet 2 — All keywords with cluster assignment
        kw_rows <- lapply(res$clusters, function(cl) {
          nm <- if (!is.null(ai_labels) && cl$cluster_id <= nrow(ai_labels))
            as.character(ai_labels$name[cl$cluster_id]) else paste("Stream",cl$cluster_id)
          data.frame(Keyword=cl$all_nodes, Cluster=cl$cluster_id,
            Stream=nm, stringsAsFactors=FALSE)
        })
        openxlsx::addWorksheet(wb, "Keyword_Assignments")
        openxlsx::writeData(wb, "Keyword_Assignments", do.call(rbind, kw_rows))

        # Sheet 3 — Temporal data
        if (!is.null(res$temporal_df) && nrow(res$temporal_df)>0) {
          openxlsx::addWorksheet(wb, "Temporal_Activity")
          openxlsx::writeData(wb, "Temporal_Activity", res$temporal_df)
        }

        openxlsx::saveWorkbook(wb, file, overwrite=TRUE)
      }
    )

    # ══ Groq AI Interpretation — Per-Analysis Buttons ══════════════════════

    .ai_r12 <- function(result) {
      tags$div(style="background:#f0f9ff; border-left:4px solid #2196A6; padding:1.2rem 1.4rem;
               border-radius:8px; line-height:1.8; margin-top:.5rem;",
        tags$b("\U0001f916 Groq AI Interpretation:"), tags$br(), tags$br(),
        tags$div(style="white-space:pre-wrap; font-size:.92rem;", result))
    }
    .ai_k12 <- function() tags$div(class="alert alert-warning", "\u26a0\ufe0f No Groq key in sidebar. ",
      tags$a("Get FREE key \u2192", href="https://console.groq.com/keys", target="_blank"))

    # 1. Publication Trends
    observeEvent(input$ai_btn_trends, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      ctx <- tryCatch({
        M <- bib_data(); req(M)
        yr_tbl <- table(M$PY)
        paste0("PUBLICATION TRENDS\nTotal documents: ", nrow(M),
               "\nYear range: ", min(M$PY, na.rm=TRUE), "–", max(M$PY, na.rm=TRUE),
               "\n\nDocuments per year:\n",
               paste(names(yr_tbl), yr_tbl, sep=": ", collapse="\n"))
      }, error=function(e) "Please upload bibliometric data first.")
      output$ai_output <- renderUI({ .ai_r12(call_gemini(paste0(
        "You are a bibliometrics expert writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of publication trend data. Include:\n",
        "1. Report total documents, time span, and average annual growth rate (AGR%)\n",
        "2. Identify key growth periods and potential catalysts (landmark papers, events, trends)\n",
        "3. Characterise the field trajectory: nascent, growing, mature, or declining?\n",
        "4. Identify citation peaks and what they reveal about field development\n",
        "5. Report h-index and citation metrics to contextualise the field's impact\n",
        "6. Compare growth to broader scientific output growth rates\n",
        "7. Provide a forward-looking statement about expected future growth\n",
        "8. Write APA-style Results paragraph for bibliometric publication trend analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 2. Author Analysis
    observeEvent(input$ai_btn_authors, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      ctx <- tryCatch({
        M <- bib_data(); req(M)
        auth_df <- tryCatch(author_df_full(), error=function(e) NULL)
        top_auth <- if (!is.null(auth_df)) head(auth_df[order(-auth_df$Articles),], 15) else NULL
        auth_txt <- if (!is.null(top_auth)) paste(capture.output(print(top_auth)), collapse="\n") else "Run author analysis"
        paste0("AUTHOR ANALYSIS\nTotal documents: ", nrow(M),
               "\n\nTop Authors by Publication Count:\n", auth_txt)
      }, error=function(e) "Please upload bibliometric data first.")
      output$ai_output <- renderUI({ .ai_r12(call_gemini(paste0(
        "You are a bibliometrics expert writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of author-level bibliometric data. Include:\n",
        "1. Identify the most prolific authors and their institutional affiliations\n",
        "2. Assess author productivity distribution: does it follow Lotka's Law (x^(-n))?\n",
        "3. Report top-cited authors and their h-index contributions to the field\n",
        "4. Identify core vs peripheral authors in the collaboration network\n",
        "5. Discuss geographic concentration: which countries/institutions dominate?\n",
        "6. Identify emerging vs established scholars based on trajectory\n",
        "7. Report international collaboration patterns\n",
        "8. Write APA-style Results paragraph for author-level bibliometric analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 3. Keyword Analysis
    observeEvent(input$ai_btn_kw, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      ctx <- tryCatch({
        M <- bib_data(); req(M)
        kw <- tryCatch({
          kw_data <- M[!is.na(M$DE) & M$DE != "", ]
          all_kw <- unlist(strsplit(kw_data$DE, ";\\s*"))
          all_kw <- trimws(toupper(all_kw))
          kw_freq <- sort(table(all_kw[nchar(all_kw) > 2]), decreasing=TRUE)
          head(kw_freq, 30)
        }, error=function(e) NULL)
        kw_txt <- if (!is.null(kw)) paste(names(kw), kw, sep=": ", collapse="\n") else "Run keyword analysis"
        paste0("KEYWORD ANALYSIS\n\nTop 30 Keywords by Frequency:\n", kw_txt)
      }, error=function(e) "Please upload bibliometric data first.")
      output$ai_output <- renderUI({ .ai_r12(call_gemini(paste0(
        "You are a bibliometrics expert writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of bibliometric keyword analysis. Include:\n",
        "1. Identify the most frequently used keywords and what they reveal about core themes\n",
        "2. Cluster keywords into thematic groups (theory, method, context, outcome)\n",
        "3. Identify keyword co-occurrence patterns and their implications for intellectual structure\n",
        "4. Trace the temporal evolution of keywords: which emerged recently vs historically dominant?\n",
        "5. Identify niche or emerging keywords that signal future research directions\n",
        "6. Distinguish author keywords vs index keywords — what does the difference reveal?\n",
        "7. Discuss what the keyword landscape reveals about disciplinary boundaries\n",
        "8. Write APA-style Results paragraph for keyword analysis\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 4. Co-Citation Analysis
    observeEvent(input$ai_btn_cocite, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      ctx <- tryCatch({
        M <- bib_data(); req(M)
        cc <- tryCatch(cocite_matrix(), error=function(e) NULL)
        cc_txt <- if (!is.null(cc)) paste("Matrix dimensions:", paste(dim(cc), collapse="x"), "\nTop co-cited pairs captured") else "Run co-citation analysis first"
        kpis <- get_kpis()
        paste0("CO-CITATION ANALYSIS\n", cc_txt,
               "\n\nField KPIs:\n", paste(capture.output(print(kpis)), collapse="\n"))
      }, error=function(e) "Please run Co-Citation analysis first.")
      output$ai_output <- renderUI({ .ai_r12(call_gemini(paste0(
        "You are an expert in science mapping and bibliometrics writing for a peer-reviewed journal.\n\n",
        "Task: DETAILED interpretation of co-citation analysis. Include:\n",
        "1. Explain co-citation analysis: two documents are co-cited when a third document cites both\n",
        "2. Identify the intellectual base: which foundational works are most co-cited?\n",
        "3. Identify co-citation clusters — what intellectual communities do they represent?\n",
        "4. Discuss seminal papers in each cluster and their theoretical contributions\n",
        "5. Describe the historical evolution of the intellectual structure\n",
        "6. Identify bridging references that link different intellectual communities\n",
        "7. Discuss implications for identifying research gaps and future directions\n",
        "8. Write APA-style Results paragraph using co-citation analysis conventions\n\n",
        "Formal academic prose (5-6 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    # 5. Full Bibliometric Summary
    observeEvent(input$ai_btn_all, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      ctx <- tryCatch({
        M <- bib_data(); req(M)
        kpis <- get_kpis()
        yr_tbl <- table(M$PY)
        paste0("BIBLIOMETRIC FULL ANALYSIS\n",
               "Total documents: ", nrow(M), "\nYears: ", min(M$PY,na.rm=TRUE), "–", max(M$PY,na.rm=TRUE),
               "\nSources: ", length(unique(M$SO)),
               "\n\nKPIs:\n", paste(capture.output(print(kpis)), collapse="\n"),
               "\n\nTop 10 cited years:\n",
               paste(names(tail(sort(yr_tbl),10)), tail(sort(yr_tbl),10), sep=": ", collapse="\n"))
      }, error=function(e) "Please upload bibliometric data first.")
      output$ai_output <- renderUI({ .ai_r12(call_gemini(paste0(
        "You are an expert bibliometrician writing for a top-tier management journal.\n\n",
        "Task: Write a COMPREHENSIVE bibliometric analysis Results section. Include:\n",
        "1. Descriptive overview: total documents, years, sources, citations, h-index\n",
        "2. Publication trend analysis: growth rate, key periods, trajectory\n",
        "3. Author analysis: top contributors, Lotka's Law, collaboration patterns\n",
        "4. Source analysis: top journals, Bradford's Law, core/peripheral outlets\n",
        "5. Keyword analysis: core themes, emerging topics, thematic evolution\n",
        "6. Co-citation analysis: intellectual base, foundational works, school clusters\n",
        "7. Country/institutional analysis: geographic distribution, collaboration networks\n",
        "8. Research gaps and future directions identified from the landscape\n\n",
        "Use APA 7. Formal academic prose (8-10 paragraphs).\n\nDATA:\n", ctx), api_key)) })
    })

    observeEvent(input$ai_btn, {
      api_key <- gemini_key()
      if (!nzchar(trimws(api_key %||% ""))) { output$ai_output <- renderUI(.ai_k12()); return() }
      NULL # (use buttons above for specific analyses)
    }, ignoreInit = TRUE)

    # ── Interactive visNetwork — Keyword Co-occurrence ────────────────────
    kw_vis_data <- eventReactive(input$run_vis_kw, {
      req(kw_net_matrix())
      mat <- kw_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) return(NULL)
      tryCatch({
        n_keep <- min(input$vis_kw_n %||% 40, nrow(mat))
        # Take top n keywords by total co-occurrence strength
        deg    <- rowSums(as.matrix(mat))
        top_kw <- names(sort(deg, decreasing = TRUE)[seq_len(n_keep)])
        sub_mat <- as.matrix(mat[top_kw, top_kw])

        g      <- igraph::graph_from_adjacency_matrix(sub_mat, mode="undirected",
                                                       weighted=TRUE, diag=FALSE)
        deg_c  <- igraph::degree(g)
        cl     <- igraph::cluster_louvain(g)
        pal    <- c(NAVY, TEAL, AMBER, GREEN, "#9B59B6", "#E74C3C", "#1ABC9C", "#F39C12")

        nodes_df <- data.frame(
          id    = igraph::V(g)$name,
          label = igraph::V(g)$name,
          value = as.numeric(deg_c) + 1,
          group = as.character(igraph::membership(cl)),
          title = paste0("<b>", igraph::V(g)$name, "</b><br>Degree: ", deg_c),
          color = pal[(igraph::membership(cl) %% length(pal)) + 1],
          stringsAsFactors = FALSE
        )

        el     <- igraph::as_edgelist(g)
        wts    <- igraph::E(g)$weight
        edges_df <- data.frame(
          from  = el[, 1],
          to    = el[, 2],
          value = as.numeric(wts),
          title = paste0("Co-occurrences: ", round(wts, 0)),
          stringsAsFactors = FALSE
        )
        list(nodes = nodes_df, edges = edges_df)
      }, error = function(e) NULL)
    })

    output$kw_vis_net <- visNetwork::renderVisNetwork({
      req(kw_vis_data())
      d <- kw_vis_data()
      if (is.null(d)) return(NULL)
      visNetwork::visNetwork(d$nodes, d$edges,
                             main = list(text = "Keyword Co-occurrence Network",
                                         style = paste0("color:", NAVY, ";font-size:16px;"))) |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visPhysics(stabilization = list(iterations = 200)) |>
        visNetwork::visLayout(randomSeed = 42) |>
        visNetwork::visInteraction(navigationButtons = TRUE, tooltipDelay = 100)
    })

    # ── Interactive visNetwork — Author Collaboration ─────────────────────
    auth_vis_data <- eventReactive(input$run_vis_auth, {
      req(auth_net_matrix())
      mat <- auth_net_matrix()
      if (is.list(mat) && !is.null(mat$error)) return(NULL)
      tryCatch({
        n_keep  <- min(input$vis_auth_n %||% 30, nrow(mat))
        deg     <- rowSums(as.matrix(mat))
        top_au  <- names(sort(deg, decreasing = TRUE)[seq_len(n_keep)])
        sub_mat <- as.matrix(mat[top_au, top_au])

        g      <- igraph::graph_from_adjacency_matrix(sub_mat, mode="undirected",
                                                       weighted=TRUE, diag=FALSE)
        deg_c  <- igraph::degree(g)
        cl     <- igraph::cluster_louvain(g)
        pal    <- c(TEAL, AMBER, NAVY, GREEN, "#9B59B6")

        nodes_df <- data.frame(
          id    = igraph::V(g)$name,
          label = igraph::V(g)$name,
          value = as.numeric(deg_c) + 1,
          group = as.character(igraph::membership(cl)),
          title = paste0("<b>", igraph::V(g)$name, "</b><br>Co-authors: ", deg_c),
          color = pal[(igraph::membership(cl) %% length(pal)) + 1],
          stringsAsFactors = FALSE
        )
        el     <- igraph::as_edgelist(g)
        wts    <- igraph::E(g)$weight
        edges_df <- data.frame(
          from  = el[, 1],
          to    = el[, 2],
          value = as.numeric(wts),
          title = paste0("Joint papers: ", round(wts, 0)),
          stringsAsFactors = FALSE
        )
        list(nodes = nodes_df, edges = edges_df)
      }, error = function(e) NULL)
    })

    output$auth_vis_net <- visNetwork::renderVisNetwork({
      req(auth_vis_data())
      d <- auth_vis_data()
      if (is.null(d)) return(NULL)
      visNetwork::visNetwork(d$nodes, d$edges,
                             main = list(text = "Author Collaboration Network",
                                         style = paste0("color:", NAVY, ";font-size:16px;"))) |>
        visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |>
        visNetwork::visPhysics(stabilization = list(iterations = 200)) |>
        visNetwork::visLayout(randomSeed = 42) |>
        visNetwork::visInteraction(navigationButtons = TRUE, tooltipDelay = 100)
    })

    # ── Growth Heatmap ────────────────────────────────────────────────────
    output$growth_heatmap <- renderPlotly({
      req(bib_data(), trend_data())
      M  <- bib_data()
      td <- trend_data()

      tryCatch({
        # Build year × document-type matrix
        yrs  <- suppressWarnings(as.integer(as.character(M$PY)))
        dt_col <- if (!is.null(M$DT)) as.character(M$DT) else rep("Article", nrow(M))
        dt_col[is.na(dt_col) | dt_col == ""] <- "Other"
        # Shorten long type names
        dt_col <- substr(dt_col, 1, 25)

        df_ht <- data.frame(Year = yrs, DocType = dt_col, stringsAsFactors = FALSE)
        df_ht <- df_ht[!is.na(df_ht$Year) & df_ht$Year >= 1990, ]
        if (nrow(df_ht) == 0) {
          # Fallback: simple year heatmap
          df_ht <- td
          df_ht$DocType <- "Articles"
          names(df_ht)[names(df_ht)=="Articles"] <- "n"
        } else {
          df_ht <- as.data.frame(table(df_ht$Year, df_ht$DocType), stringsAsFactors = FALSE)
          names(df_ht) <- c("Year", "DocType", "n")
          df_ht$Year <- as.integer(as.character(df_ht$Year))
          df_ht$n   <- as.integer(df_ht$n)
        }

        # Keep top-6 document types only
        top_dt <- names(sort(tapply(df_ht$n, df_ht$DocType, sum), decreasing = TRUE))[1:min(6, length(unique(df_ht$DocType)))]
        df_ht  <- df_ht[df_ht$DocType %in% top_dt, ]

        yr_vals  <- sort(unique(df_ht$Year))
        dt_vals  <- rev(top_dt)

        z_mat <- matrix(0, nrow = length(dt_vals), ncol = length(yr_vals),
                        dimnames = list(dt_vals, as.character(yr_vals)))
        for (i in seq_len(nrow(df_ht))) {
          ry <- as.character(df_ht$Year[i])
          rd <- df_ht$DocType[i]
          if (ry %in% colnames(z_mat) && rd %in% rownames(z_mat))
            z_mat[rd, ry] <- df_ht$n[i]
        }

        plot_ly(
          x         = yr_vals,
          y         = dt_vals,
          z         = z_mat,
          type      = "heatmap",
          colorscale = list(c(0, "#EBF4F7"), c(0.5, TEAL), c(1, NAVY)),
          hovertemplate = "Year: %{x}<br>Type: %{y}<br>Papers: %{z}<extra></extra>"
        ) |>
          layout(
            title = list(text = "Annual Publication Output by Document Type",
                         font = list(color = NAVY, size = 14)),
            xaxis = list(title = "Year", dtick = 5),
            yaxis = list(title = ""),
            plot_bgcolor  = "white",
            paper_bgcolor = "white"
          )
      }, error = function(e) {
        plotly_empty() |> layout(title = paste("Error:", e$message))
      })
    })

    # ── Lollipop Charts ───────────────────────────────────────────────────
    .make_lollipop <- function(df, x_col, y_col, title_txt, color) {
      df <- df[order(df[[x_col]]), ]
      df[[y_col]] <- factor(df[[y_col]], levels = df[[y_col]])
      plot_ly(df, x = df[[x_col]], y = df[[y_col]],
              type = "scatter", mode = "markers",
              marker = list(color = color, size = 11),
              hovertemplate = paste0("%{y}<br>", x_col, ": %{x}<extra></extra>"),
              showlegend = FALSE) |>
        add_segments(x = 0, xend = df[[x_col]], y = df[[y_col]], yend = df[[y_col]],
                     line = list(color = color, width = 1.8),
                     showlegend = FALSE, hoverinfo = "skip") |>
        layout(
          title = list(text = title_txt, font = list(color = NAVY, size = 14)),
          xaxis = list(title = x_col, zeroline = FALSE),
          yaxis = list(title = ""),
          plot_bgcolor  = "white",
          paper_bgcolor = "white",
          margin = list(l = 200)
        )
    }

    output$author_lollipop <- renderPlotly({
      req(bib_results())
      au <- bib_results()$Authors
      if (is.null(au) || length(au) == 0)
        return(plotly_empty() |> layout(title="No author data"))
      n  <- min(input$lp_n_au %||% 20, length(au))
      df <- data.frame(Articles = as.integer(head(sort(au, decreasing=TRUE), n)),
                       Author   = names(head(sort(au, decreasing=TRUE), n)),
                       stringsAsFactors = FALSE)
      .make_lollipop(df, "Articles", "Author",
                     paste0("Top ", n, " Authors by Publication Count"), NAVY)
    })

    output$source_lollipop <- renderPlotly({
      req(bib_results())
      so <- bib_results()$Sources
      if (is.null(so) || length(so) == 0)
        return(plotly_empty() |> layout(title="No source data"))
      n  <- min(input$lp_n_so %||% 20, length(so))
      df <- data.frame(Articles = as.integer(head(sort(so, decreasing=TRUE), n)),
                       Source   = names(head(sort(so, decreasing=TRUE), n)),
                       stringsAsFactors = FALSE)
      .make_lollipop(df, "Articles", "Source",
                     paste0("Top ", n, " Sources / Journals"), TEAL)
    })

    output$country_lollipop <- renderPlotly({
      req(bib_data())
      M <- bib_data()
      tryCatch({
        co_col <- if (!is.null(M$AU_CO)) M$AU_CO else if (!is.null(M$C1)) M$C1 else NULL
        if (is.null(co_col)) return(plotly_empty() |> layout(title="No country data"))
        countries <- unlist(strsplit(as.character(co_col), ";"))
        countries <- trimws(countries)
        countries <- countries[nzchar(countries) & !is.na(countries)]
        tbl <- sort(table(countries), decreasing = TRUE)
        n   <- min(input$lp_n_co %||% 20, length(tbl))
        df  <- data.frame(Articles = as.integer(head(tbl, n)),
                          Country  = names(head(tbl, n)),
                          stringsAsFactors = FALSE)
        .make_lollipop(df, "Articles", "Country",
                       paste0("Top ", n, " Countries by Publication Count"), AMBER)
      }, error = function(e) {
        plotly_empty() |> layout(title = paste("Error:", e$message))
      })
    })

    # ══════════════════════════════════════════════════════════════════════

  })
}
