# ============================================================
# GBD Comprehensive Analysis Suite
# Package Installation Script
# Run this ONCE before launching the app
# ============================================================

# CRAN packages (required)
cran_required <- c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "shinycssloaders",
  "DT",
  "plotly",
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "readxl",       # Excel file upload support
  "stringr",
  "purrr",
  "tibble",
  "forecast",     # ARIMA, ETS, TBATS, NNETAR, Theta, Holt forecasting
  "segmented",    # Joinpoint regression
  "maps",         # World map polygons
  "RColorBrewer",
  "viridis",      # Colour scales
  "broom",
  "openxlsx",     # Excel export
  "scales",
  "data.table",   # Fast CSV loading (100 MB+ files)
  "patchwork",    # Multi-panel ggplot2 layouts
  "officer",      # Word export
  "flextable"
)

# CRAN packages (optional but recommended)
cran_optional <- c(
  "ggrepel",          # Non-overlapping labels
  "rnaturalearth",    # Better world maps
  "rnaturalearthdata",
  "sf",               # Spatial features
  "leaflet",          # Interactive maps
  "leaflet.extras",
  "quantreg",         # Quantile regression (SDI frontier analysis)
  "zip",              # ZIP file export (all figures bundle)
  "nordpred",         # Nordpred APC forecasting
  "Epi"               # Epidemiology functions
)

# Install required packages
message("Installing required packages...")
for (pkg in cran_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("  Installing: %s", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(sprintf("  Already installed: %s", pkg))
  }
}

# Install optional packages
message("\nInstalling optional packages (errors OK)...")
for (pkg in cran_optional) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch({
      install.packages(pkg, repos = "https://cloud.r-project.org")
      message(sprintf("  Installed: %s", pkg))
    }, error = function(e) {
      message(sprintf("  Skipped (not critical): %s", pkg))
    })
  } else {
    message(sprintf("  Already installed: %s", pkg))
  }
}

# Optional: BAPC (Bayesian APC — requires INLA)
message("\nNote: BAPC forecasting requires the INLA package.")
message("To install INLA (optional, advanced forecasting):")
message('  install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"))')
message('  install.packages("BAPC")  # After INLA')

message("\n✓ Installation complete. Run app.R to launch.")
