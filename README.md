# DrAIStat — AI-Powered Statistical Analysis Tool

<p align="center">
  <img src="https://img.shields.io/badge/version-3.0.0-blue" alt="Version"/>
  <img src="https://img.shields.io/badge/R-%3E%3D4.1.0-276DC3?logo=r" alt="R version"/>
  <img src="https://img.shields.io/badge/license-MIT-green" alt="License"/>
  <img src="https://img.shields.io/badge/modules-15-orange" alt="Modules"/>
  <img src="https://img.shields.io/badge/AI-Groq%20(Free)-purple" alt="AI"/>
</p>

> **Dr.AIStat** is a comprehensive, AI-powered statistical analysis platform for researchers, built with R Shiny. It covers the full quantitative research workflow — from descriptive statistics to SEM, bibliometrics, and fsQCA — with free AI-powered result interpretation.

**Developed by Dr. Aneeq Inam**
Assistant Professor, School of Business & Quality Management

📧 a.inam@hbmsu.ac.ae | 🔗 [ORCID: 0000-0001-7682-2244](https://orcid.org/0000-0001-7682-2244)

---

## Installation

### Option 1 — Install from GitHub (Recommended)

```r
# Step 1: Install devtools if you don't have it
install.packages("devtools")

# Step 2: Install DrAIStat
devtools::install_github("aneeqinam/DrAIStat")

# Step 3: Launch the app
library(DrAIStat)
run_draiststat()
```

### Option 2 — Install dependencies first (for slow connections)

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "DT", "plotly", "ggplot2", "ggcorrplot", "readxl", "writexl",
  "openxlsx", "dplyr", "tidyr", "psych", "GPArotation", "moments",
  "lavaan", "semTools", "semPlot", "rstatix", "car", "lmtest",
  "MASS", "caret", "randomForest", "e1071", "pROC", "rpart",
  "rpart.plot", "cluster", "factoextra", "mclust", "ltm", "eRm",
  "AER", "boot", "bibliometrix", "stringr", "scales",
  "RColorBrewer", "igraph", "corrr", "httr2", "jsonlite"
))

devtools::install_github("aneeqinam/DrAIStat")
```

---

## Quick Start

```r
library(DrAIStat)

# Launch the app (opens in your browser)
run_draiststat()

# Launch on a different port
run_draiststat(port = 4040)

# Check version
draiststat_version()
```

---

## Analysis Modules (15 Total)

| # | Module | Key Methods |
|---|--------|-------------|
| 1 | 📊 **Descriptive Statistics** | psych::describe, correlations, Harman CMB test, distributions |
| 2 | 📏 **Measurement Analysis** | EFA, CFA (lavaan), Cronbach's α, ω, AVE, CR, HTMT |
| 3 | 📐 **Group Comparison Tests** | t-test, Mann-Whitney, ANOVA, Kruskal-Wallis, MANOVA, χ² |
| 4 | 📈 **Regression Analysis** | Simple, Multiple, Hierarchical OLS, β, VIF, ΔR², diagnostics |
| 5 | 🤖 **ML Classification** | Decision Tree, Random Forest, SVM, AUC, confusion matrix |
| 6 | 🔵 **Clustering Analysis** | K-Means (elbow), Hierarchical (dendrogram), GMM (mclust) |
| 7 | 📏 **Item Response Theory** | Rasch 1PL (eRm), 2PL (ltm), item fit, person-item map |
| 8 | 🔍 **Endogeneity Tests** | Durbin-Wu-Hausman, 2SLS (AER), Sargan-Hansen |
| 9 | ⚡ **Advanced Methods** | Bayesian regression (rstanarm), correlation network (igraph) |
| 10 | 🔗 **SEM & Path Analysis** | lavaan + semTools + semPlot, CFA, Full SEM, PLS-SEM (seminr) |
| 11 | 📊 **CoMe Analysis** | Conditional Mediation Models A–E, Bootstrap CI, CoMe Index (ω) |
| 12 | 📚 **Bibliometric Analysis** | bibliometrix, trends, co-citation, keyword co-occurrence, thematic map |
| 13 | 🔬 **fsQCA** | Calibration, truth table, necessary/sufficient conditions, QCA + SetMethods |
| 14 | 📈 **STATA-Style Methods** | Panel data (plm), survival analysis, RDD, ARIMA, VAR |
| 15 | 🤖 **AI Research Assistant** | Groq AI interpretation, hypothesis generation, APA writing support |

---

## AI Interpretation (Free)

Dr.AIStat uses the **Groq API** (llama-3.1-8b-instant model) for AI-powered result interpretation. It is **100% free** — no credit card required.

1. Visit [console.groq.com/keys](https://console.groq.com/keys) (takes ~30 seconds)
2. Create a free account and generate an API key (starts with `gsk_`)
3. Paste the key in the sidebar of the Dr.AIStat app

---

## Data Formats

Upload your data via the **sidebar Data Loader** or paste a local file path:

- **Excel** (.xlsx, .xls)
- **CSV** (.csv)
- **Tab-separated** (.tsv)
- **R data** (.rds)
- **Bibliometric exports**: Scopus CSV (all fields) or Web of Science plain text

---

## Citation

If you use Dr.AIStat in your research, please cite:

```
Inam, A. (2026). DrAIStat: AI-Powered Statistical Analysis Tool for Research
(R package version 3.0.0). .
https://github.com/aneeqinam/DrAIStat
```

BibTeX:
```bibtex
@software{Inam2026DrAIStat,
  author  = {Inam, Aneeq},
  title   = {{DrAIStat}: {AI}-Powered Statistical Analysis Tool for Research},
  year    = {2026},
  version = {3.0.0},
  url     = {https://github.com/aneeqinam/DrAIStat},
  note    = {R package}
}
```

### Module-Specific Citations

**CoMe Analysis:** Cheah, J., Nitzl, C., Roldán, J., Cepeda-Carrión, G., & Gudergan, S. (2021). A primer on the conditional mediation analysis in PLS-SEM. *SIGMIS Database, 52*(SI), 43–100. https://doi.org/10.1145/3505639.3505645

**Bibliometric Analysis:** Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool for comprehensive science mapping analysis. *Journal of Informetrics, 11*(4), 959–975. https://doi.org/10.1016/j.joi.2017.08.007

**SEM / CFA:** Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. *Journal of Statistical Software, 48*(2), 1–36. https://doi.org/10.18637/jss.v048.i02

**fsQCA:** Ragin, C.C. (2008). *Redesigning Social Inquiry: Fuzzy Sets and Beyond.* University of Chicago Press.

---

## System Requirements

- R ≥ 4.1.0 — [Download R](https://cran.r-project.org)
- RStudio (recommended) — [Download RStudio](https://posit.co/download/rstudio-desktop)
- Internet connection (for Groq AI interpretation; all analysis runs locally)

---

## License

MIT © 2026 Aneeq Inam

---

## Contact

**Dr. Aneeq Inam**
Assistant Professor, School of Business & Quality Management

📧 a.inam@hbmsu.ac.ae
🔗 [ORCID](https://orcid.org/0000-0001-7682-2244) | [DrAIStudio.com](https://draistudio.com)
