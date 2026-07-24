# ============================================================
# GBD Comprehensive Analysis Suite
# ui.R
# ============================================================

ui <- dashboardPage(
  skin = "black",

  # ---- HEADER ----
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/fluency/28/globe.png",
               style = "margin-right:8px; vertical-align:middle;"),
      "GBD Analysis Suite"
    ),
    titleWidth = 290
  ),

  # ---- SIDEBAR ----
  dashboardSidebar(
    width = 270,
    tags$div(style = "padding: 12px 15px 5px;",
      tags$p(style = "color:#aaa; font-size:11px; margin:0;",
             "Global Burden of Disease")
    ),
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("📁  Data Import",          tabName = "tab_data",     icon = icon("upload")),
      menuItem("🗺️  Distribution Maps",    tabName = "tab_maps",     icon = icon("map")),
      menuItem("📈  EAPC Analysis",        tabName = "tab_eapc",     icon = icon("chart-line")),
      menuItem("🔗  Joinpoint Regression", tabName = "tab_jp",       icon = icon("bezier-curve")),
      menuItem("🔬  Decomposition",        tabName = "tab_decomp",   icon = icon("layer-group")),
      menuItem("🔁  Descriptive APC",       tabName = "tab_apc",      icon = icon("circle-nodes")),
      menuItem("🔮  Forecasting",          tabName = "tab_forecast", icon = icon("forward")),
      menuItem("📊  Descriptive Stats",    tabName = "tab_desc",     icon = icon("table-cells")),
      menuItem("🌐  Frontier Analysis",    tabName = "tab_frontier",   icon = icon("flag")),
      menuItem("⚖️  Health Inequality",   tabName = "tab_inequality", icon = icon("scale-balanced")),
      menuItem("💾  Export & Methods",     tabName = "tab_export",   icon = icon("file-export"))
    ),
    tags$hr(style = "border-color:#444; margin:10px 15px;"),
    conditionalPanel(
      condition = "output.data_loaded == true",
      tags$div(style = "padding:0 15px;",
        tags$p(style = "color:#aaa; font-size:11px; margin-bottom:5px;",
               "⚙ GLOBAL FILTERS"),
        uiOutput("gf_measure"),
        uiOutput("gf_metric"),
        uiOutput("gf_sex"),
        uiOutput("gf_age")
      )
    )
  ),

  # ---- BODY ----
  dashboardBody(

    tags$head(
      tags$style(HTML("
        body, .content-wrapper { background:#f0f2f5; }
        .skin-black .main-header .logo,
        .skin-black .main-header .navbar { background:#1a1f2e !important; }
        .skin-black .main-sidebar     { background:#1a1f2e; }
        .skin-black .sidebar a        { color:#b8c1cc !important; }
        .skin-black .sidebar .active>a{ background:#2d3550 !important; color:#fff !important; }
        .skin-black .sidebar-menu>li:hover>a { background:#252b3b !important; }
        .box { border-radius:10px; box-shadow:0 2px 12px rgba(0,0,0,.08); border:none; }
        .box-header { border-radius:10px 10px 0 0; }
        .box.box-primary   > .box-header { background:#2C6FAC; color:#fff; }
        .box.box-info      > .box-header { background:#2980B9; color:#fff; }
        .box.box-success   > .box-header { background:#27AE60; color:#fff; }
        .box.box-warning   > .box-header { background:#D68910; color:#fff; }
        .box.box-danger    > .box-header { background:#C0392B; color:#fff; }
        .box.box-primary   > .box-header .box-title,
        .box.box-info      > .box-header .box-title,
        .box.box-success   > .box-header .box-title,
        .box.box-warning   > .box-header .box-title,
        .box.box-danger    > .box-header .box-title { color:#fff; }
        .page-header {
          background: linear-gradient(135deg, #1a1f2e 0%, #2C6FAC 100%);
          color:white; padding:14px 20px; border-radius:10px;
          margin-bottom:18px; display:flex; align-items:center; gap:10px;
        }
        .page-header h3 { margin:0; font-size:17px; font-weight:600; }
        .info-box { border-radius:10px; box-shadow:0 2px 10px rgba(0,0,0,.1); }
        .info-box-icon { border-radius:10px 0 0 10px; }
        .btn-run { background:#2C6FAC; color:#fff; border:none;
                   border-radius:7px; font-weight:600; width:100%; padding:9px; }
        .btn-run:hover { background:#1a5a97; color:#fff; }
        .shiny-spinner-output-container { min-height:50px; }
        .dataTables_wrapper { font-size:13px; }
        /* Fix plotly 0-width rendering inside box containers */
        .plotly.html-widget, .js-plotly-plot { width:100% !important; }
        .shiny-plot-output { width:100% !important; }
        .alert-data-needed {
          background:#EBF5FB; border-left:4px solid #2980B9;
          padding:12px 16px; border-radius:0 8px 8px 0;
          color:#1a5276; font-size:13px; margin:10px 0;
        }
        .method-pill {
          display:inline-block; background:#e8f4fd; color:#1a5276;
          border:1px solid #aad4f5; border-radius:20px;
          padding:3px 11px; font-size:12px; font-family:monospace; margin:2px;
        }
        .best-model-row { background:#d5f5e3 !important; font-weight:600; }
      "))
    ),

    tabItems(

      # ======================================================
      # TAB 1 — DATA IMPORT
      # ======================================================
      tabItem(tabName = "tab_data",
        div(class = "page-header",
          icon("upload", class = "fa-lg"),
          h3("Data Import & Preview")
        ),
        fluidRow(
          box(title = "Upload Data File", status = "primary", solidHeader = TRUE, width = 5,
            fileInput("file_gbd", NULL,
                      accept  = c(".csv", ".txt", ".xlsx", ".xls"),
                      placeholder = "Select CSV or Excel file…",
                      buttonLabel = "Browse"),
            fluidRow(
              column(6, selectInput("file_sep", "CSV Delimiter (ignored for Excel)",
                                    choices = c("Comma ,  " = ",",
                                                "Tab \\t"   = "\t",
                                                "Semicolon ;" = ";"),
                                    selected = ",")),
              column(6, checkboxInput("file_header", "Header row", TRUE))
            ),
            hr(style="margin:8px 0;"),
            actionButton("btn_load_test", "⚡ Load 'final data.xlsx' directly",
                         class = "btn-warning btn-block btn-sm"),
            hr(style="margin:8px 0;"),
            h5("Expected GHDx Columns", style="font-size:12px; color:#555;"),
            tags$ul(style="font-size:12px; color:#666; padding-left:18px;",
              tags$li("location_name, year"),
              tags$li("val, upper, lower"),
              tags$li("measure_name, metric_name"),
              tags$li("sex_name, age_name (optional)"),
              tags$li("cause_name (optional)")
            )
          ),
          box(title = "SDI Data (Optional)", status = "info", solidHeader = TRUE, width = 4,
            fileInput("file_sdi", NULL,
                      accept = ".csv",
                      placeholder = "SDI CSV…",
                      buttonLabel = "Browse"),
            hr(style="margin:8px 0;"),
            h5("Format:", style="font-size:12px; color:#555;"),
            tags$ul(style="font-size:12px; color:#666; padding-left:18px;",
              tags$li("location_name"),
              tags$li("year"),
              tags$li("sdi  (0–1 scale)")
            ),
            br(),
            downloadButton("dl_sdi_template", "SDI Template",
                           class = "btn-default btn-sm", icon = icon("download"))
          ),
          box(title = "Dataset Status", status = "success", solidHeader = TRUE, width = 3,
            uiOutput("data_status_panel")
          )
        ),
        fluidRow(
          box(title = "Summary", status = "primary", width = 12,
            uiOutput("data_summary_info_boxes")
          )
        ),
        fluidRow(
          box(title = "Data Preview (first 300 rows)", status = "primary", width = 12,
            withSpinner(DTOutput("tbl_preview"), color = APP_ACCENT)
          )
        )
      ),

      # ======================================================
      # TAB 2 — MAPS
      # ======================================================
      tabItem(tabName = "tab_maps",
        div(class = "page-header",
          icon("map", class = "fa-lg"),
          h3("Global Distribution Maps")
        ),
        fluidRow(
          box(title = "Map Controls", status = "primary", solidHeader = TRUE, width = 3,
            selectInput("map_type", "Display",
                       choices = c("ASR by Year"           = "asr_year",
                                   "EAPC Choropleth"       = "eapc",
                                   "% Change (start→end)" = "pct_change")),
            uiOutput("ui_map_year"),
            selectInput("map_palette", "Palette",
                       choices = c("YlOrRd","RdYlGn","Blues","Purples",
                                   "Viridis","Plasma","BuRd" = "RdBu")),
            checkboxInput("map_rev_palette", "Reverse palette", FALSE),
            hr(style="margin:8px 0;"),
            h5("Colour Scale", style="font-size:12px;"),
            uiOutput("ui_map_scale_info")
          ),
          box(title = "Choropleth Map", status = "primary", width = 9,
            withSpinner(plotlyOutput("plt_world_map", height = "480px"), color = APP_ACCENT)
          )
        ),
        fluidRow(
          box(title = "Country / Region Summary Table", status = "info", width = 12,
            withSpinner(DTOutput("tbl_map_summary"), color = APP_ACCENT)
          )
        )
      ),

      # ======================================================
      # TAB 3 — EAPC
      # ======================================================
      tabItem(tabName = "tab_eapc",
        div(class = "page-header",
          icon("chart-line", class = "fa-lg"),
          h3("EAPC — Estimated Annual Percentage Change")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_eapc_years"),
            uiOutput("ui_eapc_locs"),
            hr(style="margin:8px 0;"),
            tags$p("Formula:", style="font-size:12px; font-weight:600; margin-bottom:4px;"),
            div(class="method-pill", "ln(ASR) = α + β × year"),
            br(),
            div(class="method-pill", "EAPC = 100 × (e^β − 1)"),
            br(), br(),
            checkboxInput("eapc_mc",
                          "Propagate GBD uncertainty (Monte-Carlo)", value = FALSE),
            helpText(style="font-size:11px;",
                     "When checked and lower/upper bound columns are present, the EAPC confidence interval is obtained by Monte-Carlo simulation from the GBD uncertainty intervals (1000 draws) rather than the log-linear standard error."),
            br(),
            actionButton("btn_run_eapc", "▶  Calculate EAPC",
                        class = "btn-run", icon = icon("play"))
          ),
          column(9,
            fluidRow(
              infoBoxOutput("ibox_increasing", width = 4),
              infoBoxOutput("ibox_decreasing", width = 4),
              infoBoxOutput("ibox_stable",     width = 4)
            ),
            fluidRow(
              box(title = "EAPC Results Table", status = "primary", width = 12,
                withSpinner(DTOutput("tbl_eapc"), color = APP_ACCENT),
                br(),
                downloadButton("dl_eapc_csv", "Download CSV",
                               class = "btn-default btn-sm", icon = icon("download"))
              )
            )
          )
        ),
        fluidRow(
          box(title = "Forest Plot (Top 30 Locations by |EAPC|)", status = "info", width = 6,
            withSpinner(plotlyOutput("plt_eapc_forest", height = "500px"), color = APP_ACCENT)
          ),
          box(title = "EAPC vs Baseline ASR", status = "info", width = 6,
            withSpinner(plotlyOutput("plt_eapc_scatter_asr", height = "500px"), color = APP_ACCENT)
          )
        ),
        fluidRow(
          box(title = "EAPC vs SDI (Correlation Plot)", status = "warning", width = 12,
            conditionalPanel(
              condition = "output.has_sdi == false",
              div(class = "alert-data-needed",
                  icon("info-circle"), " Upload SDI data in the Data Import tab to enable this plot.")
            ),
            withSpinner(plotlyOutput("plt_eapc_sdi", height = "420px"), color = APP_ACCENT)
          )
        )
      ),

      # ======================================================
      # TAB 4 — JOINPOINT
      # ======================================================
      tabItem(tabName = "tab_jp",
        div(class = "page-header",
          icon("bezier-curve", class = "fa-lg"),
          h3("Joinpoint Regression Analysis")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_jp_loc"),
            uiOutput("ui_jp_sex"),
            radioButtons("jp_method", "Selection Method",
                         choices = c(
                           "Fixed (user-specified)" = "fixed",
                           "BIC (recommended)"      = "bic",
                           "AIC"                    = "aic",
                           "Permutation test"       = "permutation"
                         ), selected = "bic", inline = FALSE),
            conditionalPanel("input.jp_method == 'fixed'",
              sliderInput("jp_max", "Number of Joinpoints (fixed)",
                          min = 0, max = 5, value = 1, step = 1)
            ),
            helpText(style="font-size:11px;color:#27AE60;font-weight:bold;",
                     "BIC/AIC: automated selection. Fixed: user-specified N."),
            selectInput("jp_transform", "Model Type",
                        choices = c(
                          "Log / Multiplicative (NCI default)" = "log",
                          "Linear / Additive"                  = "linear",
                          "Square Root"                        = "sqrt"
                        ), selected = "log"),
            hr(style="margin:8px 0;"),
            actionButton("btn_run_jp", "▶  Run Joinpoint",
                        class = "btn-run", icon = icon("play"))
          ),
          column(9,
            box(title = "Trend Plot with Joinpoints", status = "primary", width = 12,
              withSpinner(plotlyOutput("plt_jp_trend", height = "450px"), color = APP_ACCENT)
            )
          )
        ),
        fluidRow(
          box(title = "Segment Summary (APC per segment)", status = "info", width = 6,
            withSpinner(DTOutput("tbl_jp_segments"), color = APP_ACCENT)
          ),
          box(title = "AAPC & Model Summary", status = "success", width = 6,
            verbatimTextOutput("txt_jp_summary")
          )
        ),
        fluidRow(
          box(title = "Multi-Location Trend Comparison", status = "warning", width = 12,
            uiOutput("ui_jp_multi_locs"),
            withSpinner(plotlyOutput("plt_jp_multi", height = "430px"), color = APP_ACCENT)
          )
        )
      ),

      # ======================================================
      # TAB 5 — DECOMPOSITION
      # ======================================================
      tabItem(tabName = "tab_decomp",
        div(class = "page-header",
          icon("layer-group", class = "fa-lg"),
          h3("Burden Decomposition Analysis")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_dc_locs"),
            uiOutput("ui_dc_start"),
            uiOutput("ui_dc_end"),
            hr(style="margin:8px 0;"),
            h5("Components:", style="font-size:12px; font-weight:600;"),
            tags$ul(style="font-size:12px; color:#555; padding-left:18px;",
              tags$li("Population growth"),
              tags$li("Population aging"),
              tags$li("Epidemiological change")
            ),
            helpText("Full DasGupta decomposition requires age-stratified data."),
            br(),
            actionButton("btn_run_dc", "▶  Run Decomposition",
                        class = "btn-run", icon = icon("play"))
          ),
          column(9,
            fluidRow(
              box(title = "% Change by Location", status = "primary", width = 7,
                withSpinner(plotlyOutput("plt_dc_bar", height = "400px"), color = APP_ACCENT)
              ),
              box(title = "Trend Direction", status = "info", width = 5,
                withSpinner(plotlyOutput("plt_dc_pie", height = "400px"), color = APP_ACCENT)
              )
            ),
            fluidRow(
              box(title = "Temporal Trends (Selected Locations)", status = "warning", width = 12,
                withSpinner(plotlyOutput("plt_dc_trends", height = "380px"), color = APP_ACCENT)
              )
            )
          )
        ),
        fluidRow(
          box(title = "Decomposition Results Table", status = "primary", width = 12,
            withSpinner(DTOutput("tbl_dc"), color = APP_ACCENT),
            br(),
            downloadButton("dl_dc_csv", "Download CSV",
                           class = "btn-default btn-sm", icon = icon("download"))
          )
        )
      ),

      # ======================================================
      # TAB 6 — FORECASTING (ENHANCED)
      # ======================================================
      tabItem(tabName = "tab_forecast",
        div(class = "page-header",
          icon("forward", class = "fa-lg"),
          h3("Burden Forecasting — Multiple Models")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_fc_loc"),
            selectInput("fc_method", "Primary Model",
                       choices = c(
                         "Auto-ARIMA"                  = "arima",
                         "ETS (Exponential Smoothing)" = "ets",
                         "TBATS"                       = "tbats",
                         "NNETAR (Neural Network AR)"  = "nnetar",
                         "Theta Method"                = "theta",
                         "Holt Linear Trend"           = "holt",
                         "Ensemble (ARIMA+ETS+NNETAR)" = "ensemble"
                       )),
            # ── Dual-model (GBD standard: ARIMA + ETS) ──────
            checkboxInput("fc_show_2nd", "Show comparison model (GBD style)",
                          value = TRUE),
            conditionalPanel("input.fc_show_2nd == true",
              selectInput("fc_method2", "Comparison Model",
                         choices = c(
                           "ETS (Exponential Smoothing)" = "ets",
                           "Auto-ARIMA"                  = "arima",
                           "TBATS"                       = "tbats",
                           "Theta Method"                = "theta",
                           "Holt Linear Trend"           = "holt"
                         ), selected = "ets")
            ),
            helpText(style="font-size:11px; color:#555;",
                     "GBD papers use ARIMA + ETS together. NNETAR/Ensemble ~30s"),
            sliderInput("fc_horizon", "Forecast Horizon (years)", 5, 30, 15, step = 5),
            selectInput("fc_ci", "Confidence Intervals",
                       choices = c("80% + 95%" = "both",
                                   "95% only"  = "95",
                                   "80% only"  = "80")),
            hr(style="margin:8px 0;"),
            actionButton("btn_run_fc", "▶  Run Forecast",
                        class = "btn-run", icon = icon("play")),
            br(), br(),
            actionButton("btn_compare_fc", "📊  Compare All Models",
                        class = "btn-warning btn-block",
                        icon = icon("chart-bar")),
            helpText(style="font-size:11px; color:#888;",
                     "Runs all 6 models + holdout accuracy")
          ),
          column(9,
            box(title = "Forecast Plot", status = "primary", width = 12,
              withSpinner(plotlyOutput("plt_fc_main", height = "450px"), color = APP_ACCENT)
            )
          )
        ),
        fluidRow(
          box(title = "Model Summary", status = "info", width = 5,
            verbatimTextOutput("txt_fc_model")
          ),
          box(title = "Forecast Table", status = "primary", width = 7,
            withSpinner(DTOutput("tbl_fc"), color = APP_ACCENT),
            br(),
            downloadButton("dl_fc_csv", "Download CSV",
                           class = "btn-default btn-sm", icon = icon("download"))
          )
        ),
        fluidRow(
          box(title = "Model Comparison — Holdout Accuracy (20% test set)",
              status = "success", solidHeader = TRUE, width = 6,
            helpText("Green row = best RMSE. Lower RMSE/MAE/MAPE = better."),
            withSpinner(DTOutput("tbl_fc_compare"), color = APP_ACCENT),
            br(),
            downloadButton("dl_compare_csv", "Download Comparison CSV",
                           class = "btn-default btn-sm", icon = icon("download"))
          ),
          box(title = "All-Model Forecast Comparison Plot",
              status = "success", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_fc_all_models", height = "380px"), color = APP_ACCENT)
          )
        ),
        fluidRow(
          box(title = "Multi-Location Forecast Comparison", status = "warning", width = 12,
            uiOutput("ui_fc_multi_locs"),
            withSpinner(plotlyOutput("plt_fc_multi", height = "430px"), color = APP_ACCENT)
          )
        ),

        # ── ARIMA + ETS dual panel (GBD publication style) ───────────────────
        fluidRow(
          div(class = "page-header", style = "margin:12px 15px 0;",
              icon("chart-area"), h4("ARIMA + ETS Dual Panel — Publication Style"))
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            helpText(style="font-size:12px; color:#555;",
                     "Runs both Auto-ARIMA and ETS simultaneously on the selected location. ",
                     "GBD papers typically report both models together."),
            uiOutput("ui_arima_ets_loc"),
            sliderInput("ae_horizon", "Forecast Horizon (years)", 5, 30, 15, step = 5),
            hr(style="margin:8px 0;"),
            actionButton("btn_run_arima_ets", "▶  Run ARIMA + ETS",
                        class = "btn-run", icon = icon("play")),
            br(), br(),
            downloadButton("dl_fig_arima_ets", "Download Dual Panel (PNG)",
                           class = "btn-primary btn-block btn-sm",
                           icon = icon("download"))
          ),
          column(9,
            box(title = "ARIMA + ETS — Side-by-Side (GBD Publication Figure)",
                status = "primary", width = 12,
              withSpinner(plotOutput("plt_arima_ets_dual", height = "560px", width = "100%"), color = APP_ACCENT)
            )
          )
        ),
        fluidRow(
          box(title = "ARIMA vs ETS — Accuracy Comparison",
              status = "info", solidHeader = TRUE, width = 6,
            withSpinner(DTOutput("tbl_arima_ets"), color = APP_ACCENT)
          ),
          box(title = "ARIMA vs ETS — Forecast Values Overlay",
              status = "info", width = 6,
            withSpinner(plotlyOutput("plt_arima_ets_overlay", height = "350px", width = "100%"), color = APP_ACCENT)
          )
        )
      ),

      # ======================================================
      # TAB 7 — DESCRIPTIVE STATS & HEATMAP
      # ======================================================
      tabItem(tabName = "tab_desc",
        div(class = "page-header",
          icon("table-cells", class = "fa-lg"),
          h3("Descriptive Statistics & Visual Exploration")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_desc_locs"),
            hr(style="margin:8px 0;"),
            actionButton("btn_run_desc", "▶  Compute Statistics",
                        class = "btn-run", icon = icon("play")),
            br(), br(),
            downloadButton("dl_desc_csv", "Download Table 1 (CSV)",
                           class = "btn-default btn-block btn-sm",
                           icon = icon("download"))
          ),
          column(9,
            box(title = "Table 1 — Descriptive Statistics by Location",
                status = "primary", width = 12,
              withSpinner(DTOutput("tbl_desc"), color = APP_ACCENT)
            )
          )
        ),
        fluidRow(
          box(title = "Heatmap — Rate by Year × Location",
              status = "info", solidHeader = TRUE, width = 8,
            withSpinner(plotlyOutput("plt_heatmap", height = "420px"), color = APP_ACCENT)
          ),
          box(title = "Heatmap Options", status = "info", width = 4,
            selectInput("heatmap_palette", "Colour Palette",
                       choices = c("Viridis" = "viridis",
                                   "Plasma"  = "plasma",
                                   "YlOrRd"  = "YlOrRd",
                                   "Blues"   = "Blues",
                                   "RdYlGn"  = "RdYlGn")),
            checkboxInput("heatmap_norm", "Normalize per location (0–1)", FALSE),
            helpText("Normalization highlights within-location change pattern."),
            br(),
            downloadButton("dl_fig_heatmap", "Download Heatmap (PNG)",
                           class = "btn-info btn-block btn-sm",
                           icon = icon("download"))
          )
        ),
        fluidRow(
          box(title = "Box Plot — Rate Distribution by Location",
              status = "warning", width = 6,
            withSpinner(plotlyOutput("plt_boxplot", height = "380px"), color = APP_ACCENT)
          ),
          box(title = "Sex Comparison (if sex breakdown available)",
              status = "warning", width = 6,
            withSpinner(plotlyOutput("plt_sex_compare", height = "380px"), color = APP_ACCENT),
            conditionalPanel(
              condition = "output.has_sex_breakdown == false",
              div(class = "alert-data-needed",
                  icon("info-circle"),
                  " Sex breakdown not available. Data shows Both sexes only.")
            )
          )
        ),
        fluidRow(
          box(title = "Temporal Trend Lines — All Selected Locations",
              status = "success", width = 12,
            withSpinner(plotlyOutput("plt_trend_lines", height = "380px"), color = APP_ACCENT)
          )
        ),

        # ── NEW: Butterfly chart (from reference GBD cardiovascular script) ──
        fluidRow(
          box(title = "🦋 Sex × Age Butterfly Chart (GBD Style)",
              status = "danger", solidHeader = TRUE, width = 8,
            withSpinner(plotlyOutput("plt_butterfly", height = "440px"), color = APP_ACCENT)
          ),
          box(title = "Butterfly Settings", status = "danger", width = 4,
            uiOutput("ui_butterfly_year"),
            uiOutput("ui_butterfly_loc"),
            helpText(style="font-size:12px;",
                     icon("info-circle"),
                     " Male bars extend right (positive), Female bars extend left (negative). ",
                     "Requires age-disaggregated sex data in your file."),
            br(),
            downloadButton("dl_fig_butterfly", "Download Butterfly (PNG)",
                           class="btn-danger btn-block btn-sm", icon=icon("download"))
          )
        ),

        # ── NEW: Age-stratified trend lines ──────────────────────────────────
        fluidRow(
          box(title = "📈 Age-Stratified Trend Lines (GBD Style)",
              status = "success", solidHeader = TRUE, width = 12,
            withSpinner(plotlyOutput("plt_age_trends", height = "420px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "Rate trends for each age group over time (1990–2023). ",
                     "Requires age-disaggregated rows in your data.")
          )
        ),

        # ── NEW: 3D Scatter (Year × Country × Rate) ──────────────────────────
        fluidRow(
          box(title = "🌐 3D Rate Surface — Year × Country × Rate (GBD Style)",
              status = "primary", solidHeader = TRUE, width = 12,
            withSpinner(plotlyOutput("plt_3d_scatter", height = "520px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "Interactive 3D view — drag to rotate, scroll to zoom. ",
                     "X = Year  |  Y = Country  |  Z = Age-standardized Rate")
          )
        ),

        # ── Treemap — Burden by Location ─────────────────────────────────────
        fluidRow(
          box(title = "🌳 Treemap — Burden Share by Location (GBD Style)",
              status = "success", solidHeader = TRUE, width = 7,
            withSpinner(plotlyOutput("plt_treemap", height = "440px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "Area = mean burden (Number); colour = age-standardised rate. ",
                     "Requires Number metric in data.")
          ),
          box(title = "Treemap Settings", status = "success", width = 5,
            uiOutput("ui_treemap_year"),
            selectInput("treemap_metric", "Colour by",
                       choices = c("Age-standardized Rate" = "rate",
                                   "EAPC (%/yr)"           = "eapc")),
            helpText(style="font-size:12px; color:#777;",
                     "EAPC colour requires EAPC analysis to be run first."),
            br(),
            downloadButton("dl_fig_treemap", "Download Treemap (PNG)",
                           class = "btn-success btn-block btn-sm",
                           icon = icon("download"))
          )
        ),

        # ── Arrow / Dumbbell diagram ──────────────────────────────────────────
        fluidRow(
          box(title = "🏹 Arrow Diagram — Burden Change (Baseline → Current) (GBD Style)",
              status = "warning", solidHeader = TRUE, width = 8,
            withSpinner(plotlyOutput("plt_arrow", height = "420px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "Each line connects the baseline-year rate (circle) to the end-year rate (triangle). ",
                     "Green = declining burden; Red = increasing burden.")
          ),
          box(title = "Arrow Diagram Settings", status = "warning", width = 4,
            uiOutput("ui_arrow_years"),
            br(),
            downloadButton("dl_fig_arrow", "Download Arrow Diagram (PNG)",
                           class = "btn-warning btn-block btn-sm",
                           icon = icon("download"))
          )
        )
      ),

      # ======================================================
      # TAB 8 — FRONTIER ANALYSIS
      # ======================================================
      tabItem(tabName = "tab_frontier",
        div(class = "page-header",
          icon("flag", class = "fa-lg"),
          h3("Health Frontier Analysis")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            sliderInput("frontier_pct", "Frontier Percentile (%)",
                       min = 5, max = 25, value = 10, step = 5),
            helpText("Lower % = stricter frontier (top performers only)"),
            sliderInput("frontier_span", "LOESS Smoothing Span",
                       min = 0.4, max = 0.9, value = 0.8, step = 0.1),
            helpText(style="font-size:11px;",
                     "Sensitivity control: vary span and percentile to check robustness of country classification."),
            hr(style="margin:8px 0;"),
            conditionalPanel(
              condition = "output.has_sdi == true",
              div(style="padding:8px; background:#d5f5e3; border-radius:6px; font-size:12px;",
                  icon("check-circle", style="color:#27AE60"),
                  " SDI data loaded — using SDI-based frontier")
            ),
            conditionalPanel(
              condition = "output.has_sdi == false",
              div(class = "alert-data-needed",
                  icon("info-circle"),
                  " No SDI data — using peer-group frontier (best performers as reference)")
            ),
            br(),
            actionButton("btn_run_frontier", "▶  Run Frontier Analysis",
                        class = "btn-run", icon = icon("play")),
            br(), br(),
            downloadButton("dl_frontier_csv", "Download Results (CSV)",
                           class = "btn-default btn-block btn-sm",
                           icon = icon("download"))
          ),
          column(9,
            conditionalPanel(
              condition = "output.has_sdi == true",
              box(title = "SDI vs ASR — Health Frontier (Loess Regression)",
                  status = "primary", width = 12,
                withSpinner(plotlyOutput("plt_frontier_sdi", height = "450px"), color = APP_ACCENT)
              )
            ),
            conditionalPanel(
              condition = "output.has_sdi == false",
              box(title = "Peer-Group Frontier — Best / Median / Worst Bands",
                  status = "primary", width = 12,
                withSpinner(plotlyOutput("plt_frontier_peer", height = "450px"), color = APP_ACCENT)
              )
            )
          )
        ),
        fluidRow(
          box(title = "Efficiency Scores & Excess Burden",
              status = "info", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_frontier_eff", height = "380px"), color = APP_ACCENT)
          ),
          box(title = "Frontier Results Table",
              status = "info", width = 6,
            withSpinner(DTOutput("tbl_frontier"), color = APP_ACCENT)
          )
        ),
        # ── Bump Chart — Country Rank Over Time ───────────────────────────────
        fluidRow(
          box(title = "📊 Bump Chart — Country Rank Over Time (GBD Style)",
              status = "warning", solidHeader = TRUE, width = 12,
            fluidRow(
              column(3,
                selectInput("bump_filter_type", "Filter Countries By",
                  choices = c("Top N Countries" = "topn",
                              "SDI Quintile"    = "sdi",
                              "Income Group"    = "income",
                              "Super-Region"    = "superregion",
                              "GBD Region"      = "region",
                              "Manual Select"   = "manual"),
                  selected = "topn")
              ),
              column(3,
                conditionalPanel("input.bump_filter_type == 'topn'",
                  numericInput("bump_top_n", "Top N Countries", value = 20, min = 3, max = 50, step = 1)
                ),
                conditionalPanel("input.bump_filter_type == 'sdi'",
                  selectInput("bump_sdi_val", "SDI Quintile",
                    choices = c("High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
                    selected = "High SDI")
                ),
                conditionalPanel("input.bump_filter_type == 'income'",
                  selectInput("bump_income_val", "Income Group",
                    choices = c("High","Upper-middle","Lower-middle","Low"),
                    selected = "High")
                ),
                conditionalPanel("input.bump_filter_type == 'superregion'",
                  selectInput("bump_sr_val", "Super-Region",
                    choices = c("High-income","Central Europe, Eastern Europe & Central Asia",
                                "Latin America & Caribbean","North Africa & Middle East",
                                "South Asia","Southeast Asia, East Asia & Oceania",
                                "Sub-Saharan Africa"),
                    selected = "High-income")
                ),
                conditionalPanel("input.bump_filter_type == 'region'",
                  uiOutput("ui_bump_region")
                ),
                conditionalPanel("input.bump_filter_type == 'manual'",
                  uiOutput("ui_bump_manual")
                )
              ),
              column(3,
                selectInput("bump_rank_by", "Rank By",
                  choices = c("Highest Rate (ASR)" = "highest",
                              "Lowest Rate (ASR)"  = "lowest"),
                  selected = "highest")
              ),
              column(3,
                sliderInput("bump_year_range", "Year Range",
                  min = 1990, max = 2023, value = c(1990, 2023), sep = "")
              )
            ),
            withSpinner(plotlyOutput("plt_bump", height = "500px", width = "100%"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "Rank 1 = highest age-standardised rate. ",
                     "Shows how each country's relative position changes over time.")
          )
        )
      ),

      # ======================================================
      # TAB APC — DESCRIPTIVE APC VISUALIZATION
      # ======================================================
      tabItem(tabName = "tab_apc",
        div(class = "page-header",
          icon("circle-nodes", class = "fa-lg"),
          h3("Descriptive APC Visualization")
        ),
        div(class = "alert alert-warning", style = "margin: 0 15px 10px 15px;",
            tags$strong("Important: "),
            "Outputs are descriptive marginal statistics only (marginal incidence rate ratios and linear period net drift). ",
            "No APC identifiability framework is implemented. ",
            "Do not interpret these results as formally estimated, causally separated age, period, or cohort effects. ",
            "For formal APC identification, use the NCI Age-Period-Cohort Web Tool."
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            uiOutput("ui_apc_loc"),
            helpText(style="font-size:12px; color:#555;",
                     icon("info-circle"),
                     " Requires age-disaggregated rows in your data (multiple age groups)."),
            hr(style="margin:8px 0;"),
            tags$p("Model:", style="font-size:12px; font-weight:600; margin-bottom:4px;"),
            div(class="method-pill", "log(Rate) ~ Age + Period + Cohort"),
            br(),
            div(class="method-pill", "IRR = exp(β)"),
            br(), br(),
            actionButton("btn_run_apc", "▶  Run Descriptive APC",
                        class = "btn-run", icon = icon("play"))
          ),
          column(9,
            fluidRow(
              infoBoxOutput("ibox_apc_obs",   width = 4),
              infoBoxOutput("ibox_apc_drift", width = 4),
              infoBoxOutput("ibox_apc_ages",  width = 4)
            ),
            fluidRow(
              box(title = "Age Effects (IRR by Age Group)", status = "primary", width = 4,
                withSpinner(plotlyOutput("plt_apc_age", height = "380px"), color = APP_ACCENT)
              ),
              box(title = "Period Effects (Net Drift)", status = "info", width = 4,
                withSpinner(plotlyOutput("plt_apc_period", height = "380px"), color = APP_ACCENT)
              ),
              box(title = "Cohort Effects (IRR by Birth Cohort)", status = "warning", width = 4,
                withSpinner(plotlyOutput("plt_apc_cohort", height = "380px"), color = APP_ACCENT)
              )
            )
          )
        ),
        fluidRow(
          box(title = "Descriptive APC Results Table", status = "primary", width = 12,
            withSpinner(DTOutput("tbl_apc"), color = APP_ACCENT),
            br(),
            downloadButton("dl_apc_csv", "Download APC Results (CSV)",
                           class = "btn-default btn-sm", icon = icon("download"))
          )
        )
      ),

      # ======================================================
      # TAB INEQUALITY — HEALTH INEQUALITY ANALYSIS
      # ======================================================
      tabItem(tabName = "tab_inequality",
        div(class = "page-header",
          icon("scale-balanced", class = "fa-lg"),
          h3("Health Inequality Analysis")
        ),
        fluidRow(
          box(title = "Settings", status = "primary", solidHeader = TRUE, width = 3,
            conditionalPanel(
              condition = "output.has_sdi == true",
              div(style="padding:8px; background:#d5f5e3; border-radius:6px; font-size:12px;",
                  icon("check-circle", style="color:#27AE60"),
                  " SDI data loaded — countries grouped by SDI quintile")
            ),
            conditionalPanel(
              condition = "output.has_sdi == false",
              div(class = "alert-data-needed",
                  icon("info-circle"),
                  " No SDI data — grouping countries by burden quintile (baseline year)")
            ),
            br(),
            helpText(style="font-size:12px; color:#555;",
                     "Compares trends between highest and lowest burden groups over time."),
            checkboxInput("sii_weighted",
                          "Population-weighted SII/RII (WLS)", value = FALSE),
            helpText(style="font-size:11px;",
                     "When checked, the slope index uses population-weighted least squares (requires Number + Rate metrics to derive population); otherwise unweighted OLS."),
            hr(style="margin:8px 0;"),
            actionButton("btn_run_ineq", "▶  Run Inequality Analysis",
                        class = "btn-run", icon = icon("play")),
            br(), br(),
            downloadButton("dl_ineq_csv", "Download Results (CSV)",
                           class = "btn-default btn-block btn-sm",
                           icon = icon("download"))
          ),
          column(9,
            box(title = "Rate Trends by Quintile Group", status = "primary", width = 12,
              withSpinner(plotlyOutput("plt_ineq_quintile", height = "420px"), color = APP_ACCENT)
            )
          )
        ),
        fluidRow(
          box(title = "Absolute Gap (High − Low Quintile) Over Time",
              status = "danger", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_ineq_gap", height = "360px"), color = APP_ACCENT)
          ),
          box(title = "Rate Ratio (High / Low Quintile) Over Time",
              status = "warning", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_ineq_ratio", height = "360px"), color = APP_ACCENT)
          )
        ),
        fluidRow(
          box(title = "Inequality Summary Table", status = "info", width = 12,
            withSpinner(DTOutput("tbl_ineq"), color = APP_ACCENT)
          )
        ),

        # ── SII / RII ────────────────────────────────────────────────────────
        fluidRow(
          box(title = "📐 Slope Index of Inequality (SII) Over Time",
              status = "primary", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_sii", height = "360px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "SII = slope of regression of rate on fractional rank. ",
                     "Positive = higher-burden locations have higher rates. ",
                     "Based on SDI rank if available, otherwise baseline-burden rank.")
          ),
          box(title = "📐 Relative Index of Inequality (RII) Over Time",
              status = "warning", solidHeader = TRUE, width = 6,
            withSpinner(plotlyOutput("plt_rii", height = "360px"), color = APP_ACCENT),
            helpText(style="font-size:12px; color:#555;",
                     "RII = SII / overall mean rate. ",
                     "Values > 1 indicate substantial relative inequality.")
          )
        )
      ),

      # ======================================================
      # TAB 9 — EXPORT & METHODS (ENHANCED)
      # ======================================================
      tabItem(tabName = "tab_export",
        div(class = "page-header",
          icon("file-export", class = "fa-lg"),
          h3("Export Results, Figures & Methods Text")
        ),
        fluidRow(
          box(title = "Excel Workbook (All Results)", status = "success",
              solidHeader = TRUE, width = 4,
            checkboxGroupInput("export_sheets", "Include sheets:",
                             choices = c(
                               "EAPC Results"         = "eapc",
                               "Joinpoint Segments"   = "jp",
                               "Decomposition"        = "dc",
                               "Forecast Values"      = "fc",
                               "Model Comparison"     = "model_comp",
                               "Descriptive Stats"    = "desc_stats",
                               "Frontier Analysis"    = "frontier",
                               "Raw Filtered Data"    = "raw"
                             ),
                             selected = c("eapc","jp","dc","fc","desc_stats")),
            br(),
            downloadButton("dl_excel", "Download Excel (.xlsx)",
                           class = "btn-success btn-block", icon = icon("file-excel"))
          ),
          box(title = "Figure Downloads", status = "info", solidHeader = TRUE, width = 4,
            selectInput("fig_format", "Format", choices = c("PNG" = "png", "PDF" = "pdf")),
            fluidRow(
              column(6, numericInput("fig_w", "Width (in)", 8, 4, 16, 0.5)),
              column(6, numericInput("fig_h", "Height (in)", 5, 3, 12, 0.5))
            ),
            numericInput("fig_dpi", "Resolution (DPI)", 300, 72, 600, 50),
            hr(style="margin:8px 0;"),
            downloadButton("dl_fig_eapc",     "EAPC Forest Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_jp",       "Joinpoint Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_fc",       "Forecast Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_arima_ets2","ARIMA+ETS Dual Panel",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_heatmap2", "Heatmap",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_frontier2","Frontier Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_map",      "World Map",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_apc",      "APC Model Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_ineq",     "Health Inequality Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_sii",      "SII / RII Plot",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_treemap2", "Treemap",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_bump",     "Bump Chart",
                           class = "btn-info btn-block btn-sm"), br(),
            downloadButton("dl_fig_arrow2",   "Arrow Diagram",
                           class = "btn-info btn-block btn-sm")
          ),
          box(title = "Auto-Generated Methods Text", status = "warning",
              solidHeader = TRUE, width = 4,
            checkboxInput("methods_include_frontier",
                         "Include Frontier Analysis section", TRUE),
            actionButton("btn_gen_methods", "Generate Methods",
                        icon = icon("file-alt"), class = "btn-warning btn-block"),
            br(), br(),
            verbatimTextOutput("txt_methods", placeholder = TRUE),
            br(),
            conditionalPanel(
              condition = "output.methods_ready == true",
              downloadButton("dl_methods", "Download (.txt)",
                             class = "btn-default btn-sm", icon = icon("download"))
            )
          )
        )
      )

    ) # end tabItems
  )   # end dashboardBody
)     # end dashboardPage
