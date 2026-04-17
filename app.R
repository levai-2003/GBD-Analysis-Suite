# ============================================================
# GBD Comprehensive Analysis Suite
# app.R — Entry Point
# ============================================================
# Run:  shiny::runApp("path/to/gbd_shiny_app")
# ============================================================

source("global.R")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
