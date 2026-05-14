#' Launch the Dr.AIStat Application
#'
#' Opens the Dr.AIStat AI-powered statistical analysis platform in your
#' default web browser. The application provides 15 analysis modules
#' covering the full quantitative research workflow.
#'
#' @param port Integer. Port to run the Shiny app on. Default is \code{3838}.
#'   Change if that port is in use.
#' @param browser Logical. Whether to automatically open the app in your
#'   default browser. Default is \code{TRUE}.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'
#' @details
#' \strong{Modules included:}
#' \enumerate{
#'   \item Descriptive Statistics
#'   \item Measurement Analysis (EFA / CFA / Reliability)
#'   \item Group Comparison Tests (t-test, ANOVA, MANOVA, chi-square)
#'   \item Regression Analysis (OLS, Hierarchical, Mediation)
#'   \item ML Classification (Decision Tree, Random Forest, SVM)
#'   \item Clustering Analysis (K-Means, Hierarchical, GMM)
#'   \item Item Response Theory (IRT: Rasch, 2PL)
#'   \item Endogeneity Tests (2SLS, Durbin-Wu-Hausman)
#'   \item Advanced Methods (Bayesian Regression, Network Analysis)
#'   \item SEM & Path Analysis (lavaan, PLS-SEM via seminr)
#'   \item CoMe — Conditional Mediation Analysis
#'   \item Bibliometric Analysis (bibliometrix)
#'   \item fsQCA — Fuzzy-Set Qualitative Comparative Analysis
#'   \item STATA-Style Methods (Panel Data, Survival, RDD, ARIMA)
#'   \item AI Research Assistant (Groq AI — free, no credit card needed)
#' }
#'
#' \strong{AI Interpretation:}
#' The app uses the free Groq API (llama-3.1-8b-instant model) for
#' AI-powered result interpretation. Obtain a free API key in 30 seconds
#' at \url{https://console.groq.com/keys} and paste it in the sidebar.
#'
#' \strong{Data formats supported:}
#' CSV, Excel (.xlsx, .xls), TSV, RDS. Upload via the sidebar Data Loader
#' or paste a local file path directly.
#'
#' @return Launches the Shiny application (invisible NULL).
#'
#' @author Dr. Aneeq Inam \email{a.inam@@hbmsu.ac.ae}
#'   \cr ORCID: \url{https://orcid.org/0000-0001-7682-2244}
#'   \cr Assistant Professor, 
#'
#' @references
#' \strong{CoMe Analysis — please cite:}
#' Cheah, J., Nitzl, C., Roldan, J., Cepeda-Carrion, G., & Gudergan, S.
#' (2021). A primer on the conditional mediation analysis in PLS-SEM.
#' \emph{SIGMIS Database, 52}(SI), 43-100.
#' \doi{10.1145/3505639.3505645}
#'
#' \strong{Bibliometric Analysis — please cite:}
#' Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for
#' comprehensive science mapping analysis.
#' \emph{Journal of Informetrics, 11}(4), 959-975.
#' \doi{10.1016/j.joi.2017.08.007}
#'
#' \strong{SEM / lavaan — please cite:}
#' Rosseel, Y. (2012). lavaan: An R package for structural equation modeling.
#' \emph{Journal of Statistical Software, 48}(2), 1-36.
#' \doi{10.18637/jss.v048.i02}
#'
#' \strong{fsQCA — please cite:}
#' Ragin, C.C. (2008). \emph{Redesigning Social Inquiry: Fuzzy Sets and Beyond.}
#' University of Chicago Press.
#'
#' @seealso \code{\link{draiststat_version}}
#'
#' @examples
#' \dontrun{
#' # Launch with default settings
#' run_draiststat()
#'
#' # Launch on a different port
#' run_draiststat(port = 4040)
#'
#' # Launch without opening browser (useful on servers)
#' run_draiststat(browser = FALSE)
#' }
#'
#' @export
run_draiststat <- function(port = 3838, browser = TRUE, ...) {
  app_dir <- system.file("app", package = "DrAIStat")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop(
      "Could not find the Dr.AIStat app directory.\n",
      "Try re-installing: devtools::install_github('aneeqinam/DrAIStat')",
      call. = FALSE
    )
  }

  message(
    "\n  ============================================================\n",
    "   Dr.AIStat v", draiststat_version(), " — AI-Powered Statistical Analysis\n",
    "   Dr. Aneeq Inam\n",
    "   ORCID: 0000-0001-7682-2244\n",
    "  ============================================================\n",
    "  Opening in browser at http://127.0.0.1:", port, "\n",
    "  Press Ctrl+C (or Escape in RStudio) to stop.\n\n"
  )

  shiny::runApp(
    appDir        = app_dir,
    port          = port,
    launch.browser = browser,
    ...
  )
}


#' Get the Dr.AIStat Package Version
#'
#' Returns the current version string of the DrAIStat package.
#'
#' @return A character string, e.g. \code{"3.0.0"}.
#'
#' @examples
#' draiststat_version()
#'
#' @export
draiststat_version <- function() {
  as.character(utils::packageVersion("DrAIStat"))
}
