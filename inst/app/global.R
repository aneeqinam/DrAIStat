# ============================================================
# global.R — Shared libraries, helpers, theme
# Dr.AIStat — AI-Powered Statistical Analysis Tool
# Dr. Aneeq Inam
# ============================================================

# Increase file upload limit to 500 MB (default is 5 MB)
options(shiny.maxRequestSize = 500 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(ggcorrplot)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(psych)
  library(GPArotation)
  library(moments)
  library(lavaan)
  library(semTools)
  library(semPlot)
  library(rstatix)
  library(car)
  library(lmtest)
  library(MASS)
  library(caret)
  library(randomForest)
  library(e1071)
  library(pROC)
  library(rpart)
  library(rpart.plot)
  library(cluster)
  library(factoextra)
  library(mclust)
  library(ltm)
  library(eRm)
  library(AER)
  library(boot)
  library(bibliometrix)
  library(stringr)
  library(scales)
  library(RColorBrewer)
  library(igraph)
  library(corrr)
  library(httr2)
  library(jsonlite)
})

# ── Advanced Visualisation packages (graceful fallback) ──────
for (.pkg in c("ggdist","ggridges","GGally","DiagrammeR",
               "visNetwork","ggalluvial","waffle",
               "patchwork","ggrepel","ggsignif")) {
  tryCatch(suppressPackageStartupMessages(
    do.call(library, list(.pkg, character.only = TRUE))),
    error = function(e) message(
      sprintf("Note: '%s' not installed — run install.R to enable all advanced plots.", .pkg))
  )
}

# Ranger (fast Random Forest) — graceful fallback
tryCatch(suppressPackageStartupMessages(library(ranger)),
         error = function(e)
           message("Note: ranger package not installed. Run install.R (needed for fsQCA What-If Simulator)."))

# QCA loaded separately — graceful fallback if not yet installed
tryCatch(suppressPackageStartupMessages(library(QCA)),
         error = function(e)
           message("Note: QCA package not installed. Run setup.R to install it (needed for fsQCA module)."))

# seminr (PLS-SEM / SmartPLS-equivalent) — graceful fallback
tryCatch(suppressPackageStartupMessages(library(seminr)),
         error = function(e)
           message("Note: seminr package not installed. Run install.packages('seminr') in RStudio (needed for PLS-SEM / SmartPLS Builder in SEM module)."))

# ── Null-coalescing operator (like Python's `or`) ──────────
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ── Colour palette ─────────────────────────────────────────
NAVY  <- "#1A3A5C"
TEAL  <- "#2196A6"
AMBER <- "#E07B39"
GREEN <- "#1E6438"
LIGHT <- "#F4F8FB"

# ── Shared CSS ──────────────────────────────────────────────
TOOL_CSS <- "
  body { font-family: 'Calibri', 'Segoe UI', sans-serif; background: #F4F8FB; }
  .content-wrapper, .right-side { background: #F4F8FB; }
  .box { border-radius: 10px; box-shadow: 0 2px 8px rgba(0,0,0,0.07); }
  .box-header { border-radius: 10px 10px 0 0; }
  .nav-tabs-custom .nav-tabs li.active a { border-top: 3px solid #2196A6; }
  .main-header .logo { background: #1A3A5C !important; font-weight: bold; }
  .main-header .navbar { background: #1A3A5C !important; }
  .skin-blue .main-sidebar { background: #1A3A5C; }
  .skin-blue .sidebar-menu > li.active > a,
  .skin-blue .sidebar-menu > li:hover > a { background: #2196A6; }
  .skin-blue .sidebar-menu > li > a { color: #E8F4F8; }
  .hero-box {
    background: linear-gradient(135deg, #1A3A5C 0%, #2196A6 100%);
    color: white; padding: 1.2rem 1.6rem; border-radius: 10px;
    margin-bottom: 1rem;
  }
  .hero-box h3 { margin: 0 0 0.3rem; color: white; font-size: 1.4rem; }
  .hero-box p  { margin: 0; opacity: 0.88; font-size: 0.88rem; }
  .sig-yes { color: #1E6438; font-weight: bold; }
  .sig-no  { color: #C0392B; font-weight: bold; }
  .creator-footer {
    background: linear-gradient(90deg, #1A3A5C, #2196A6);
    color: white; text-align: center; padding: 0.5rem;
    border-radius: 6px; margin-top: 1rem; font-size: 0.82rem;
  }
"

# ── Global Shared Dataset (accessible by ALL modules) ────────
# Set via the sidebar Data Loader; any module can read global_shared_data()
global_shared_data <- reactiveVal(NULL)
global_shared_name <- reactiveVal("")

# ── Shared helpers ──────────────────────────────────────────

# Read a file from a local filesystem path (CSV / XLSX / XLS / TSV / RDS)
read_from_path <- function(file_path) {
  if (is.null(file_path) || !nzchar(trimws(file_path))) return(NULL)
  # Strip surrounding quotes that some OS file managers add
  fp <- trimws(gsub("^['\"]|['\"]$", "", trimws(file_path)))
  fp <- path.expand(fp)
  if (!file.exists(fp)) stop(paste0("File not found: ", fp))
  ext <- tolower(tools::file_ext(fp))
  df <- switch(ext,
    csv  = utils::read.csv(fp, stringsAsFactors=FALSE, check.names=FALSE),
    tsv  = utils::read.table(fp, header=TRUE, sep="\t", stringsAsFactors=FALSE, check.names=FALSE),
    xlsx = as.data.frame(readxl::read_excel(fp)),
    xls  = as.data.frame(readxl::read_excel(fp)),
    rds  = readRDS(fp),
    stop(paste0("Unsupported format: .", ext, "  — use CSV, XLSX, XLS, TSV, or RDS"))
  )
  as.data.frame(df)
}

# Read uploaded file (xlsx or csv)
read_uploaded <- function(file_path, file_name) {
  ext <- tools::file_ext(file_name)
  tryCatch({
    if (tolower(ext) == "csv") {
      read.csv(file_path, stringsAsFactors=FALSE, check.names=FALSE)
    } else {
      as.data.frame(read_excel(file_path))
    }
  }, error = function(e) NULL)
}

# Get numeric columns
numeric_cols <- function(df) {
  names(df)[sapply(df, is.numeric)]
}

# Significance stars
sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*",
  ifelse(p < 0.10,  "†", ""))))
}

# APA p-value string
apa_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "< .001" else paste0("= ", sprintf("%.3f", p))
}

# Hero box HTML
hero_html <- function(icon, title, subtitle) {
  tags$div(class="hero-box",
    tags$h3(paste(icon, title)),
    tags$p(subtitle)
  )
}

# Creator footer
creator_footer <- function() {
  tags$div(class="creator-footer",
    HTML("<b>Dr.AIStat</b> &nbsp;|&nbsp;
          Created by <b>Dr. Aneeq Inam</b> &nbsp;|&nbsp;
          Assistant Professor &nbsp;|&nbsp;
          ORCID: 0000-0001-7682-2244 &nbsp;|&nbsp;
          a.inam@hbmsu.ac.ae")
  )
}

# Round all numeric columns in a data frame
round_df <- function(df, digits=4) {
  df[] <- lapply(df, function(x) if (is.numeric(x)) round(x, digits) else x)
  df
}

# Standard DT table
tool_dt <- function(df, caption=NULL) {
  datatable(df, caption=caption, rownames=FALSE,
            options=list(pageLength=15, scrollX=TRUE,
                         dom="Bfrtip", buttons=c("copy","csv","excel")),
            extensions="Buttons",
            class="compact stripe hover")
}

# ── Groq AI helper (100% FREE — no credit card needed) ───────────────────────
# Model: llama-3.1-8b-instant — Free: unlimited requests, fast inference
# Get your FREE key in 30 seconds at: https://console.groq.com/keys
# Uses OpenAI-compatible API format
call_gemini <- function(prompt, api_key,
                        model = "llama-3.1-8b-instant") {
  if (is.null(api_key) || !nzchar(trimws(api_key)))
    return("⚠️ No API key provided. Please enter your Groq API key in the sidebar.")
  url  <- "https://api.groq.com/openai/v1/chat/completions"
  body <- list(
    model       = model,
    messages    = list(list(role = "user", content = prompt)),
    temperature = 0.4,
    max_tokens  = 1500L
  )
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_headers(
        "Content-Type"  = "application/json",
        "Authorization" = paste("Bearer", trimws(api_key))
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    if (status != 200L) {
      err <- tryCatch(httr2::resp_body_json(resp), error = function(e) list())
      msg <- err$error$message %||% paste("HTTP", status)
      return(paste0("⚠️ Groq API error: ", msg,
                    "\n\nCommon fixes:\n",
                    "• Make sure you copied the full key from console.groq.com/keys\n",
                    "• The key should start with 'gsk_'\n",
                    "• Free tier: visit console.groq.com to check usage"))
    }
    result <- httr2::resp_body_json(resp)
    choices <- result$choices
    if (is.null(choices) || length(choices) == 0)
      return("⚠️ Groq returned no response. Try again in a moment.")
    choices[[1]]$message$content
  }, error = function(e) {
    paste0("⚠️ Connection error: ", e$message,
           "\n\nPlease check your internet connection and try again.")
  })
}
