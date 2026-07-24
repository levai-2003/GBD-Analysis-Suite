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




rsconnect::setAccountInfo(
  name   = "bela2003",
  token  = "DEE23D52E60B55892D4FF4E7F0B1B70A",
  secret = "yJUadTUAK16RcBBpT8m5bcJ21s8/8bYSbQ6ArNeu"
)

rsconnect::deployApp(
  appDir      = "C:/Users/workstation/Desktop/shny app",
  appName     = "GBD-Analysis-Suite",
  forceUpdate = TRUE,
  lint        = FALSE
)