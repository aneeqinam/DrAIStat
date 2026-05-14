# ============================================================
# Dr.AIStat — AI-Powered Statistical Analysis Tool (v3.0)
# Main Shiny Application
#
# Created by Dr. Aneeq Inam
# Assistant Professor, School of Business & Quality Management
# 
# ORCID: https://orcid.org/0000-0001-7682-2244
# Email: a.inam@hbmsu.ac.ae
#
# HOW TO RUN:
#   Option 1 — RStudio: open run.R → click "Source"
#   Option 2 — Console:  source("run.R")
#   Option 3 — Direct:   shiny::runApp(".")
#
# FIRST TIME: Run setup.R to install all packages.
# ============================================================

source("global.R")

# Load all analysis modules
for (f in list.files("modules", pattern="\\.R$", full.names=TRUE)) source(f)

# ════════════════════════════════════════════════════════════
#  UI
# ════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src="data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj48Y2lyY2xlIGN4PSIxMiIgY3k9IjEyIiByPSIxMSIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjIiLz48cGF0aCBkPSJNOCA4aDhNOCAxMmg0TTggMTZoNiIgc3Ryb2tlPSIjZmZmIiBzdHJva2Utd2lkdGg9IjIiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPjwvc3ZnPg==",
               height="22px", style="margin-right:6px; vertical-align:middle;"),
      tags$b("Dr.AIStat"),
      style = "font-size:1.05rem; color:white; letter-spacing:0.5px;"
    ),
    titleWidth = 280
  ),

  dashboardSidebar(
    width = 280,
    tags$style(HTML("
      .sidebar-menu > li > a { padding: 10px 15px; font-size: 0.88rem; }
      .sidebar-menu > li > a > .fa { width: 20px; }
      .shiny-input-container { margin-bottom: 0; }
      #global_path { font-size:.75rem; height:28px; padding:3px 6px; }
      #global_load_path { height:28px; padding:2px 8px; font-size:.75rem; }
      #global_file { margin-bottom:2px; }
      #global_file .btn { font-size:.72rem; padding:2px 6px; }
      #global_file input[type=text] { font-size:.72rem; }
    ")),
    sidebarMenu(
      id = "sidebar_menu",

      # ── Global Data Loader ─────────────────────────────────
      tags$div(style="padding:8px 15px 2px; color:#8AB4C4; font-size:.75rem; font-weight:bold; text-transform:uppercase;",
               "📁 Data Loader"),
      tags$div(style="padding:2px 15px 8px;",
        tags$div(style="background:#162d47; border-radius:8px; padding:10px;",
          # Option A: File browser
          tags$p(style="color:#aac4d8; font-size:.7rem; margin:0 0 3px; font-weight:bold;",
                 "Browse file:"),
          fileInput("global_file", label=NULL,
                    accept=c(".csv",".xlsx",".xls",".tsv",".rds"),
                    buttonLabel="📁 Browse", placeholder="No file",
                    width="100%"),
          # Option B: Path input
          tags$p(style="color:#aac4d8; font-size:.7rem; margin:4px 0 3px; font-weight:bold;",
                 "— or type / paste file path:"),
          tags$div(style="display:flex; gap:4px; align-items:flex-start;",
            textInput("global_path", label=NULL,
                      placeholder="C:/data/myfile.csv  or  ~/data/file.xlsx",
                      width="100%"),
            actionButton("global_load_path", "▶",
                         class="btn-primary",
                         title="Load from path",
                         style="flex-shrink:0; margin-top:0;")
          ),
          uiOutput("global_data_status"),
          # Clear button — only shown when data is loaded
          uiOutput("global_clear_btn")
        )
      ),

      tags$hr(style="border-color:#2A4A6C; margin:.3rem 1rem;"),

      tags$div(style = "padding:6px 15px 4px; color:#8AB4C4; font-size:.75rem; font-weight:bold; text-transform:uppercase;",
               "Quantitative Methods"),
      menuItem("🎨 Dr.AIStat Analytics Hub",    tabName="tab10", icon=NULL),
      menuItem("🤖 AI Research Assistant",       tabName="tab15", icon=NULL),

      tags$div(style = "padding:10px 15px 4px; color:#8AB4C4; font-size:.75rem; font-weight:bold; text-transform:uppercase;",
               "Specialised"),
      menuItem("📊 CoMe Analysis (A–E)",          tabName="tab11", icon=NULL),
      menuItem("📚 Bibliometric Analysis",        tabName="tab12", icon=NULL),
      menuItem("🔬 fsQCA Analysis",               tabName="tab13", icon=NULL),
      menuItem("📈 STATA-Style Methods",          tabName="tab14", icon=NULL),

      tags$hr(style="border-color:#2A4A6C; margin:.5rem 1rem;"),

      # ── Groq AI Key ────────────────────────────────────────
      tags$div(style="padding:4px 15px 2px; color:#8AB4C4; font-size:.75rem; font-weight:bold; text-transform:uppercase;",
               "🤖 AI Interpretation (Free)"),
      tags$div(style="padding:4px 15px 10px;",
        tags$div(style="background:#162d47; border-radius:8px; padding:10px;",
          tags$p(style="color:#aac4d8; font-size:.72rem; margin:0 0 5px; line-height:1.4;",
            HTML("Powered by <b>Groq AI</b> (100% free, no card needed). Get key in 30 sec:")),
          passwordInput("gemini_key", label=NULL,
                        placeholder="Paste Groq key (gsk_…)",
                        width="100%"),
          tags$a("🔑 Get FREE key → console.groq.com/keys",
                 href="https://console.groq.com/keys",
                 target="_blank",
                 style="color:#2196A6; font-size:.71rem; display:block; margin-top:3px;")
        )
      ),
      tags$hr(style="border-color:#2A4A6C; margin:.5rem 1rem;"),
      tags$div(style="padding:8px 15px; color:#8AB4C4; font-size:.72rem; line-height:1.4;",
        tags$b("Dr. Aneeq Inam"), tags$br(),
        "", tags$br(),
        "ORCID: 0000-0001-7682-2244"
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML(TOOL_CSS)),
      tags$title("Dr.AIStat — AI-Powered Statistical Analysis")
    ),

    tabItems(
      tabItem(tabName="tab10", mod10_ui("mod10")),
      tabItem(tabName="tab11", mod11_ui("mod11")),
      tabItem(tabName="tab12", mod12_ui("mod12")),
      tabItem(tabName="tab13", mod13_ui("mod13")),
      tabItem(tabName="tab14", mod14_ui("mod14")),
      tabItem(tabName="tab15", mod15_ui("mod15"))
    )
  )
)

# ════════════════════════════════════════════════════════════
#  SERVER
# ════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Shared API key ─────────────────────────────────────────
  gemini_key <- reactive(input$gemini_key %||% "")

  # ── Global Data Loader — File Browser ──────────────────────
  observeEvent(input$global_file, {
    req(input$global_file)
    tryCatch({
      df <- read_uploaded(input$global_file$datapath, input$global_file$name)
      if (!is.null(df)) {
        global_shared_data(as.data.frame(df))
        global_shared_name(input$global_file$name)
        showNotification(
          sprintf("✅ Loaded: %s  (%d rows × %d cols)",
                  input$global_file$name, nrow(df), ncol(df)),
          type = "message", duration = 4
        )
      }
    }, error = function(e) {
      showNotification(paste("Load error:", e$message), type="error", duration=6)
    })
  })

  # ── Global Data Loader — File Path ─────────────────────────
  observeEvent(input$global_load_path, {
    pth <- trimws(input$global_path %||% "")
    if (!nzchar(pth)) {
      showNotification("Please enter a file path first.", type="warning", duration=4)
      return()
    }
    tryCatch({
      df <- read_from_path(pth)
      if (!is.null(df)) {
        global_shared_data(as.data.frame(df))
        nm  <- basename(gsub("^['\"]|['\"]$", "", pth))
        global_shared_name(nm)
        showNotification(
          sprintf("✅ Loaded: %s  (%d rows × %d cols)", nm, nrow(df), ncol(df)),
          type = "message", duration = 4
        )
      }
    }, error = function(e) {
      showNotification(
        paste0("❌ ", e$message), type="error", duration=8
      )
    })
  })

  # ── Global Data Status Widget ───────────────────────────────
  output$global_data_status <- renderUI({
    df <- global_shared_data()
    nm <- global_shared_name()
    if (is.null(df)) {
      tags$p(style="color:#4a6a8a; font-size:.7rem; margin:4px 0 0; text-align:center;",
             "No dataset loaded")
    } else {
      tags$div(style="margin-top:5px; background:#0d1f33; border-radius:5px; padding:6px 8px;",
        tags$p(style="color:#4CAF50; font-size:.72rem; margin:0; font-weight:bold; word-break:break-all;",
               paste0("✅ ", nm)),
        tags$p(style="color:#7fb3c8; font-size:.69rem; margin:2px 0 0;",
               sprintf("%d rows × %d cols  |  available to all modules",
                       nrow(df), ncol(df)))
      )
    }
  })

  output$global_clear_btn <- renderUI({
    df <- global_shared_data()
    if (is.null(df)) return(NULL)
    tags$div(style="margin-top:4px; text-align:right;",
      actionButton("global_clear", "✕ Clear dataset",
                   class="btn-xs btn-default",
                   style="font-size:.68rem; padding:1px 6px; color:#aaa;")
    )
  })

  observeEvent(input$global_clear, {
    global_shared_data(NULL)
    global_shared_name("")
    showNotification("Dataset cleared.", type="message", duration=2)
  })

  # ── Module Servers ──────────────────────────────────────────
  mod10_server("mod10", gemini_key = gemini_key)
  mod11_server("mod11", gemini_key = gemini_key)
  mod12_server("mod12", gemini_key = gemini_key)
  mod13_server("mod13", gemini_key = gemini_key)
  mod14_server("mod14", gemini_key = gemini_key)
  mod15_server("mod15", gemini_key = gemini_key)
}

# ════════════════════════════════════════════════════════════
#  LAUNCH
# ════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
