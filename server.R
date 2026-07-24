# ============================================================
# GBD Comprehensive Analysis Suite
# server.R
# ============================================================

server <- function(input, output, session) {

  # ==========================================================
  # HELPER: TOP + BOTTOM N COUNTRIES (performance guard)
  # Returns up to n_top highest + n_bot lowest countries by
  # mean rate. Used across all multi-country plots.
  # ==========================================================
  top_bottom_locs <- function(df, n_top = 20, n_bot = 20,
                               rate_col = "val") {
    if (is.null(df) || nrow(df) == 0) return(character(0))
    summ <- df %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(avg = mean(.data[[rate_col]], na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(avg))
    n <- nrow(summ)
    top  <- summ$location_name[seq_len(min(n_top, n))]
    bot  <- summ$location_name[seq_len(min(n_bot, n)) + max(0, n - min(n_bot, n))]
    unique(c(top, bot))
  }

  # ==========================================================
  # REACTIVE: DATA LOADING & PARSING
  # ==========================================================

  rv <- reactiveValues(
    raw_df       = NULL,
    sdi_df       = NULL,
    eapc_res     = NULL,
    jp_res       = NULL,
    dc_res       = NULL,
    dc_res_dg    = NULL,   # Das Gupta 3-component (NULL if age-stratified data unavailable)
    fc_res       = NULL,
    fc_res2      = NULL,   # second model (GBD dual-model comparison)
    model_comp   = NULL,
    desc_res     = NULL,
    frontier_res = NULL,
    apc_res      = NULL,   # Age-Period-Cohort model
    ineq_res     = NULL,   # Health inequality analysis
    methods_txt  = NULL
  )

  # Load GBD file
  observeEvent(input$file_gbd, {
    req(input$file_gbd)
    tryCatch({
      ext <- tolower(tools::file_ext(input$file_gbd$name))
      df <- if (ext %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file_gbd$datapath, sheet = 1,
                           na = c("NA", "", "N/A", "<NA>"))
      } else {
        read.csv(input$file_gbd$datapath,
                 header = input$file_header,
                 sep    = input$file_sep,
                 stringsAsFactors = FALSE,
                 na.strings = c("NA", "", "N/A", "<NA>"))
      }
      df <- as.data.frame(df)
      df <- parse_gbd_columns(df)
      rv$raw_df <- df
      showNotification(
        paste0("✓ Loaded ", nrow(df), " rows × ", ncol(df), " columns"),
        type = "message", duration = 4
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Quick-load button — searches for data file relative to app directory, then Desktop
  observeEvent(input$btn_load_test, {
    app_dir  <- getwd()
    desktop  <- file.path(path.expand("~"), "..", "Desktop")  # works cross-platform
    # Search order: app folder first, then Desktop
    candidates <- c(
      file.path(app_dir, "GBD_Global_204_Data.csv"),
      file.path(app_dir, "final data.xlsx"),
      file.path(app_dir, "final_data.xlsx"),
      file.path(desktop, "GBD_Global_204_Data.csv"),
      file.path(desktop, "final data.xlsx")
    )
    test_path <- Filter(file.exists, candidates)[1]
    if (is.na(test_path) || length(test_path) == 0) {
      showNotification("No data file found. Please upload a file using Browse.",
                       type = "error", duration = 5)
      return()
    }
    tryCatch({
      df <- if (grepl("\\.csv$", test_path, ignore.case = TRUE)) {
        as.data.frame(data.table::fread(test_path, na.strings = c("NA", "", "N/A", "<NA>"),
                                        data.table = FALSE))
      } else {
        as.data.frame(readxl::read_excel(test_path, sheet = 1,
                                         na = c("NA", "", "N/A", "<NA>")))
      }
      df <- parse_gbd_columns(df)
      rv$raw_df <- df
      # Auto-load matching SDI file from same folder
      sdi_candidates <- c(
        file.path(app_dir, "GBD_Global_204_SDI.csv"),
        file.path(app_dir, "SDI.csv"),
        file.path(desktop, "GBD_Global_204_SDI.csv")
      )
      sdi_path <- Filter(file.exists, sdi_candidates)[1]
      if (!is.na(sdi_path) && length(sdi_path) > 0 && file.exists(sdi_path)) {
        sdi <- as.data.frame(data.table::fread(sdi_path, data.table = FALSE))
        names(sdi) <- tolower(names(sdi))
        rv$sdi_df <- sdi
        n_countries <- length(unique(df$location_name))
        showNotification(
          paste0("✓ Loaded ", format(nrow(df), big.mark=","), " rows | ",
                 n_countries, " countries | SDI auto-loaded ✓"),
          type = "message", duration = 6
        )
      } else {
        showNotification(
          paste0("✓ Loaded ", format(nrow(df), big.mark=","), " rows × ", ncol(df), " columns"),
          type = "message", duration = 4
        )
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Load SDI file
  observeEvent(input$file_sdi, {
    req(input$file_sdi)
    tryCatch({
      sdi <- read.csv(input$file_sdi$datapath, stringsAsFactors = FALSE)
      names(sdi) <- tolower(names(sdi))
      rv$sdi_df <- sdi
      showNotification("✓ SDI data loaded", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("SDI error:", e$message), type = "error")
    })
  })

  # Boolean reactives for conditional UI
  output$data_loaded <- reactive({ !is.null(rv$raw_df) })
  output$has_sdi     <- reactive({ !is.null(rv$sdi_df) })
  output$has_sex_breakdown <- reactive({
    df <- rv$raw_df
    if (is.null(df) || !"sex_name" %in% names(df)) return(FALSE)
    snames <- tolower(unique(df$sex_name))
    # Data uses "Males"/"Females" — tolower → "males"/"females"
    # Use startsWith-style grepl so both "male"/"males" and "female"/"females" match
    any(grepl("^male|^female", snames))
  })
  outputOptions(output, "data_loaded",      suspendWhenHidden = FALSE)
  outputOptions(output, "has_sdi",          suspendWhenHidden = FALSE)
  outputOptions(output, "has_sex_breakdown",suspendWhenHidden = FALSE)

  # ==========================================================
  # GLOBAL FILTER UIs
  # ==========================================================

  output$gf_measure <- renderUI({
    df <- rv$raw_df; req(df)
    if (!"measure_name" %in% names(df)) return(NULL)
    selectInput("gf_measure", "Measure",
               choices  = unique(df$measure_name),
               selected = unique(df$measure_name)[1],
               selectize = FALSE)
  })

  output$gf_metric <- renderUI({
    df <- rv$raw_df; req(df)
    if (!"metric_name" %in% names(df)) return(NULL)
    choices <- unique(df$metric_name)
    default <- choices[grep("rate", tolower(choices))][1] %||% choices[1]
    selectInput("gf_metric", "Metric", choices = choices, selected = default, selectize = FALSE)
  })

  output$gf_sex <- renderUI({
    df <- rv$raw_df; req(df)
    if (!"sex_name" %in% names(df)) return(NULL)
    choices <- unique(df$sex_name)
    default <- choices[grep("both|total", tolower(choices))][1] %||% choices[1]
    selectInput("gf_sex", "Sex", choices = choices, selected = default, selectize = FALSE)
  })

  output$gf_age <- renderUI({
    df <- rv$raw_df; req(df)
    if (!"age_name" %in% names(df)) return(NULL)
    choices <- unique(df$age_name)
    default <- choices[grep("standardized|age-standard", tolower(choices))][1] %||% choices[1]
    selectInput("gf_age", "Age Group", choices = choices, selected = default, selectize = FALSE)
  })

  # Filtered data
  filt_data <- reactive({
    df <- rv$raw_df; req(df)
    if (!is.null(input$gf_measure) && "measure_name" %in% names(df))
      df <- df %>% filter(measure_name == input$gf_measure)
    if (!is.null(input$gf_metric) && "metric_name" %in% names(df))
      df <- df %>% filter(metric_name == input$gf_metric)
    if (!is.null(input$gf_sex) && "sex_name" %in% names(df))
      df <- df %>% filter(sex_name == input$gf_sex)
    if (!is.null(input$gf_age) && "age_name" %in% names(df))
      df <- df %>% filter(age_name == input$gf_age)
    df
  })

  # ==========================================================
  # TAB 1 — DATA IMPORT
  # ==========================================================

  output$data_status_panel <- renderUI({
    df <- rv$raw_df
    if (is.null(df)) {
      return(div(style = "text-align:center; padding:20px; color:#999;",
                 icon("cloud-upload-alt", class="fa-3x"), br(), br(),
                 "No data loaded yet"))
    }
    cols_found <- intersect(
      c("location_name","year","val","measure_name","metric_name","sex_name","age_name"),
      names(df)
    )
    tags$div(
      tags$p(icon("check-circle", style="color:#27AE60"), strong("GBD File Loaded")),
      tags$p(style="font-size:12px;",
             icon("table"), sprintf(" %s rows × %s cols",
                                    format(nrow(df),big.mark=","), ncol(df))),
      tags$p(style="font-size:12px;",
             icon("columns"), sprintf(" Columns matched: %d / 7", length(cols_found))),
      if (!is.null(rv$sdi_df))
        tags$p(style="font-size:12px;color:#27AE60;",
               icon("check"), " SDI data loaded")
    )
  })

  output$data_summary_info_boxes <- renderUI({
    df <- rv$raw_df
    if (is.null(df)) {
      return(div(class="alert-data-needed",
                 icon("info-circle"), " Upload a GHDx file to begin."))
    }
    n_locs <- if ("location_name" %in% names(df)) length(unique(df$location_name)) else "?"
    n_yrs  <- if ("year"          %in% names(df)) length(unique(df$year))          else "?"
    yr_rng <- if ("year"          %in% names(df)) paste(range(df$year,na.rm=T),collapse="–") else "N/A"
    n_meas <- if ("measure_name"  %in% names(df)) length(unique(df$measure_name))  else "?"

    fluidRow(
      infoBox("Locations", n_locs,                         icon=icon("globe"),        color="blue",   fill=TRUE, width=3),
      infoBox("Year Range", paste0(n_yrs," (",yr_rng,")"), icon=icon("calendar-alt"), color="green",  fill=TRUE, width=3),
      infoBox("Total Rows", format(nrow(df),big.mark=","), icon=icon("table"),        color="purple", fill=TRUE, width=3),
      infoBox("Measures",   n_meas,                        icon=icon("chart-bar"),    color="teal",   fill=TRUE, width=3)
    )
  })

  output$tbl_preview <- renderDT({
    df <- rv$raw_df; req(df)
    datatable(head(df, 300), rownames=FALSE,
             options=list(scrollX=TRUE, pageLength=15, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons")
  })

  output$dl_sdi_template <- downloadHandler(
    filename = "SDI_template.csv",
    content  = function(f) {
      write.csv(data.frame(
        location_name = c("Global","Egypt","Saudi Arabia","United States of America"),
        year          = c(2021, 2021, 2021, 2021),
        sdi           = c(0.66, 0.58, 0.80, 0.93)
      ), f, row.names=FALSE)
    }
  )

  # ==========================================================
  # TAB 2 — MAPS
  # ==========================================================

  output$ui_map_year <- renderUI({
    df <- filt_data(); req(df, "year" %in% names(df))
    yrs <- sort(unique(df$year))
    sliderInput("map_year", "Year", min=min(yrs), max=max(yrs),
               value=max(yrs), step=1, sep="",
               animate=animationOptions(1200))
  })

  output$ui_map_scale_info <- renderUI({
    switch(input$map_type,
      "asr_year"   = tags$small("Higher = greater burden", style="color:#666;"),
      "eapc"       = tags$small("Red = increasing, Green = decreasing", style="color:#666;"),
      "pct_change" = tags$small("% change from start to end year", style="color:#666;")
    )
  })

  output$plt_world_map <- renderPlotly({
    df  <- filt_data(); req(df, world_map_df)
    wm  <- world_map_df

    if (input$map_type == "eapc") {
      eapc_df <- df %>%
        filter(!is.na(val), val > 0) %>%
        group_by(location_name) %>%
        arrange(year) %>%
        summarise(
          eapc = tryCatch({
            m <- lm(log(val) ~ year)
            100 * (exp(coef(m)["year"]) - 1)
          }, error = function(e) NA_real_),
          .groups = "drop"
        ) %>%
        mutate(region = map_gbd_names(location_name))

      map_df    <- left_join(wm, eapc_df, by="region", relationship="many-to-many")
      fill_var  <- "eapc"
      scale_lim <- max(abs(map_df$eapc), na.rm=TRUE) * 1.05
      fill_scale <- scale_fill_gradient2(
        low="darkgreen", mid="lightyellow", high="darkred",
        midpoint=0, limits=c(-scale_lim, scale_lim),
        name="EAPC (%)", na.value="#D5D8DC"
      )
      title_txt      <- paste("EAPC:", paste(range(df$year,na.rm=T), collapse="–"))
      tooltip_suffix <- "%"

    } else if (input$map_type == "pct_change") {
      req(nrow(df)>0)
      yr_range <- range(df$year, na.rm=TRUE)
      pct_df <- df %>%
        filter(year %in% yr_range, !is.na(val)) %>%
        group_by(location_name) %>%
        summarise(
          pct = 100 * (val[year==yr_range[2]][1] - val[year==yr_range[1]][1]) /
                      val[year==yr_range[1]][1],
          .groups="drop"
        ) %>%
        mutate(region = map_gbd_names(location_name))

      map_df    <- left_join(wm, pct_df, by="region", relationship="many-to-many")
      fill_var  <- "pct"
      scale_lim <- max(abs(map_df$pct), na.rm=TRUE)*1.05
      fill_scale <- scale_fill_gradient2(
        low="darkgreen", mid="lightyellow", high="darkred",
        midpoint=0, limits=c(-scale_lim, scale_lim),
        name="% Change", na.value="#D5D8DC"
      )
      title_txt      <- paste0("% Change: ", yr_range[1], " → ", yr_range[2])
      tooltip_suffix <- "%"

    } else {
      req(input$map_year)
      yr_data <- df %>%
        filter(year == input$map_year) %>%
        mutate(region = map_gbd_names(location_name)) %>%
        select(region, val)

      map_df   <- left_join(wm, yr_data, by="region", relationship="many-to-many")
      fill_var <- "val"
      pal <- if (input$map_palette == "Viridis") {
        viridis::viridis(9)
      } else if (input$map_palette == "Plasma") {
        viridis::plasma(9)
      } else {
        brewer.pal(9, input$map_palette)
      }
      if (input$map_rev_palette) pal <- rev(pal)
      fill_scale <- scale_fill_gradientn(colors=pal, name="Value", na.value="#D5D8DC")
      title_txt      <- paste("Year:", input$map_year)
      tooltip_suffix <- ""
    }

    p <- ggplot(map_df,
                aes(x=long, y=lat, group=group,
                    fill=.data[[fill_var]],
                    text=paste0(region, "\nValue: ",
                                round(.data[[fill_var]], 2), tooltip_suffix))) +
      geom_polygon(color="white", linewidth=0.1) +
      fill_scale +
      theme_void(base_size=11) +
      theme(legend.position="right",
            plot.background=element_rect(fill="white",color=NA),
            plot.title=element_text(size=13,face="bold")) +
      labs(title=title_txt)

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             margin=list(l=0,r=0,t=30,b=0))
  })

  output$tbl_map_summary <- renderDT({
    df <- filt_data(); req(df, "location_name" %in% names(df))
    smry <- df %>%
      filter(!is.na(val)) %>%
      group_by(location_name) %>%
      arrange(year) %>%
      summarise(
        Period         = paste(range(year), collapse="–"),
        `Latest Value` = round(last(val), 3),
        `Earliest Value` = round(first(val), 3),
        `% Change`     = round(100*(last(val)-first(val))/first(val), 1),
        .groups="drop"
      ) %>%
      arrange(desc(`Latest Value`))
    datatable(smry, rownames=FALSE,
             options=list(pageLength=15, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons")
  })

  # ==========================================================
  # TAB 3 — EAPC
  # ==========================================================

  output$ui_eapc_years <- renderUI({
    df <- filt_data(); req(df, "year" %in% names(df))
    yrs <- sort(unique(df$year))
    sliderInput("eapc_yr", "Year Range", min=min(yrs), max=max(yrs),
               value=c(min(yrs), max(yrs)), step=1, sep="")
  })

  output$ui_eapc_locs <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    selectizeInput("eapc_locs", "Locations (blank = all)",
                  choices=locs, multiple=TRUE, selected=NULL,
                  options=list(placeholder="All locations (default)"))
  })

  observeEvent(input$btn_run_eapc, {
    df <- filt_data(); req(df)
    df2 <- df
    if (!is.null(input$eapc_yr))
      df2 <- df2 %>% filter(year >= input$eapc_yr[1], year <= input$eapc_yr[2])
    if (!is.null(input$eapc_locs) && length(input$eapc_locs) > 0)
      df2 <- df2 %>% filter(location_name %in% input$eapc_locs)

    withProgress(message="Calculating EAPC…", value=0, {
      rv$eapc_res <- calc_eapc_all(df2, mc = isTRUE(input$eapc_mc))
      incProgress(1)
    })
    showNotification("✓ EAPC calculation complete", type="message", duration=3)
  })

  eapc_with_sdi <- reactive({
    res <- rv$eapc_res; req(res)
    sdi <- rv$sdi_df

    # Also pull region metadata from raw data if available
    raw <- rv$raw_df
    meta_cols <- c("region", "super_region", "income_group", "sdi_quintile")
    if (!is.null(raw)) {
      avail <- meta_cols[meta_cols %in% names(raw)]
      if (length(avail) > 0) {
        meta <- raw %>%
          dplyr::select(location_name, dplyr::all_of(avail)) %>%
          dplyr::distinct(location_name, .keep_all = TRUE)
        res <- dplyr::left_join(res, meta, by = "location_name")
      }
    }

    if (is.null(sdi)) return(res)

    # Also pull region metadata from SDI file if not already in res
    sdi_meta_cols <- meta_cols[meta_cols %in% names(sdi)]
    sdi_sum <- sdi %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(
        sdi = mean(sdi, na.rm = TRUE),
        dplyr::across(dplyr::any_of(sdi_meta_cols),
                      ~ dplyr::first(na.omit(.))),
        .groups = "drop"
      )
    # Only add SDI meta cols not already in res
    already_have <- names(res)
    sdi_sum <- sdi_sum %>%
      dplyr::select(location_name, sdi,
                    dplyr::any_of(sdi_meta_cols[!sdi_meta_cols %in% already_have]))
    dplyr::left_join(res, sdi_sum, by = "location_name")
  })

  output$ibox_increasing <- renderInfoBox({
    n <- if (!is.null(rv$eapc_res)) sum(rv$eapc_res$trend=="Increasing",na.rm=T) else 0
    infoBox("Increasing", n, icon=icon("arrow-trend-up"), color="red",   fill=TRUE)
  })
  output$ibox_decreasing <- renderInfoBox({
    n <- if (!is.null(rv$eapc_res)) sum(rv$eapc_res$trend=="Decreasing",na.rm=T) else 0
    infoBox("Decreasing", n, icon=icon("arrow-trend-down"), color="green", fill=TRUE)
  })
  output$ibox_stable <- renderInfoBox({
    n <- if (!is.null(rv$eapc_res)) sum(rv$eapc_res$trend=="Stable",na.rm=T) else 0
    infoBox("Stable",     n, icon=icon("minus"),            color="yellow", fill=TRUE)
  })

  output$tbl_eapc <- renderDT({
    res <- eapc_with_sdi(); req(res)
    disp <- res %>%
      mutate(`EAPC (95% CI)` = paste0(round(eapc,2),"% (",
                                       round(eapc_lower,2),", ",round(eapc_upper,2),")")) %>%
      select(Location=location_name, `Start Year`=start_year, `End Year`=end_year,
             `Baseline ASR`=start_asr, `Final ASR`=end_asr, `EAPC (95% CI)`,
             `P-value`=p_value, `R²`=r_squared, Trend=trend) %>%
      {if ("sdi" %in% names(res)) mutate(., SDI=round(res$sdi,3)) else .}

    datatable(disp, rownames=FALSE,
             options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("Trend",
                  backgroundColor=styleEqual(
                    c("Increasing","Decreasing","Stable"),
                    c("#FDEDEC","#EAFAF1","#F4F6F7")))
  })

  output$plt_eapc_forest <- renderPlotly({
    res <- rv$eapc_res; req(res)
    pdat <- res %>%
      filter(!is.na(eapc)) %>%
      slice_max(abs(eapc), n=20) %>%
      arrange(eapc) %>%
      mutate(location_name=factor(location_name, levels=location_name))

    p <- ggplot(pdat,
                aes(x=eapc, y=location_name, color=trend,
                    text=paste0(location_name,"\nEAPC: ",round(eapc,2),
                               "%\n95% CI: (",round(eapc_lower,2),", ",round(eapc_upper,2),")"))) +
      geom_vline(xintercept=0, linetype="dashed", color="gray50", linewidth=0.5) +
      geom_errorbar(aes(xmin=eapc_lower, xmax=eapc_upper),
                   width=0.35, linewidth=0.5, orientation="y") +
      geom_point(size=2.5) +
      scale_color_manual(values=TREND_COLORS) +
      labs(x="EAPC (%)", y=NULL, color="Trend") +
      theme_minimal(base_size=10) +
      theme(legend.position="bottom", axis.text.y=element_text(size=9))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             margin=list(l=10,r=10,t=10,b=30))
  })

  output$plt_eapc_scatter_asr <- renderPlotly({
    res <- eapc_with_sdi(); req(res)
    pdat <- res %>% dplyr::filter(!is.na(eapc))
    req(nrow(pdat) >= 2)

    # --- Manual LM fit + 95% CI (native plot_ly — avoids heavy ggplotly + geom_smooth) ---
    pred_df <- tryCatch({
      fit_lm <- lm(eapc ~ start_asr, data = pdat)
      x_seq  <- seq(min(pdat$start_asr, na.rm = TRUE),
                    max(pdat$start_asr, na.rm = TRUE), length.out = 120)
      pd     <- as.data.frame(predict(fit_lm,
                                       newdata  = data.frame(start_asr = x_seq),
                                       interval = "confidence", level = 0.95))
      pd$x   <- x_seq
      pd
    }, error = function(e) NULL)

    has_region <- "region" %in% names(pdat) && !all(is.na(pdat$region))
    color_col  <- if (has_region) as.character(pdat$region) else as.character(pdat$trend)
    color_col[is.na(color_col)] <- "Other"
    col_map    <- if (has_region) GBD_REGION_COLORS else TREND_COLORS

    label_df <- dplyr::bind_rows(
      pdat %>% dplyr::slice_max(eapc, n = 6),
      pdat %>% dplyr::slice_min(eapc, n = 6)
    ) %>% dplyr::distinct(location_name, .keep_all = TRUE)

    hover_text <- paste0(
      "<b>", pdat$location_name, "</b>",
      "<br>Baseline ASR: ", round(pdat$start_asr, 2),
      "<br>EAPC: ", round(pdat$eapc, 2), "%",
      if (has_region) paste0("<br>Region: ", pdat$region) else ""
    )

    p <- plot_ly()

    if (!is.null(pred_df)) {
      p <- p %>%
        add_ribbons(x = pred_df$x, ymin = pred_df$lwr, ymax = pred_df$upr,
                    fillcolor = "rgba(174,214,241,0.35)",
                    line = list(color = "transparent"),
                    showlegend = FALSE, hoverinfo = "none", name = "95% CI") %>%
        add_lines(x = pred_df$x, y = pred_df$fit,
                  line = list(color = "black", width = 1.8),
                  showlegend = FALSE, hoverinfo = "none", name = "Linear fit")
    }

    for (grp in sort(unique(color_col))) {
      idx     <- which(color_col == grp)
      grp_col <- if (grp %in% names(col_map)) col_map[[grp]] else "#888888"
      p <- p %>%
        add_markers(x      = pdat$start_asr[idx],
                    y      = pdat$eapc[idx],
                    name   = grp,
                    marker = list(color = grp_col, size = 8,
                                  opacity = 0.85, line = list(width = 0)),
                    text      = hover_text[idx],
                    hoverinfo = "text")
    }

    annots <- lapply(seq_len(nrow(label_df)), function(i) {
      list(x = label_df$start_asr[i], y = label_df$eapc[i],
           text      = label_df$location_name[i],
           showarrow = TRUE, arrowhead = 0, arrowsize = 0.5,
           arrowcolor = "gray70", arrowwidth = 0.8,
           font      = list(size = 9, color = "black"),
           bgcolor   = "rgba(255,255,255,0.75)", borderpad = 2,
           ax = 25, ay = -20)
    })

    p %>% layout(
      shapes = list(
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = 0, y1 = 0, yref = "y",
             line = list(dash = "dash", color = "gray55", width = 1))
      ),
      annotations   = annots,
      xaxis         = list(title = "Baseline ASPR (per 100,000)",
                           gridcolor = "#eeeeee", zeroline = FALSE),
      yaxis         = list(title = "EAPC (% per year)",
                           gridcolor = "#eeeeee", zeroline = FALSE),
      title         = list(text = "EAPC vs Baseline Rate \u2014 GBD Style",
                           font = list(size = 14)),
      paper_bgcolor = "white", plot_bgcolor = "white",
      legend        = list(font = list(size = 9), orientation = "h",
                           xanchor = "center", x = 0.5, y = -0.18)
    )
  })

  output$plt_eapc_sdi <- renderPlotly({
    res <- eapc_with_sdi()
    if (is.null(res) || !"sdi" %in% names(res)) return(NULL)
    pdat <- res %>% dplyr::filter(!is.na(eapc), !is.na(sdi))
    req(nrow(pdat) >= 4)

    ct    <- cor.test(pdat$sdi, pdat$eapc, use = "complete.obs")
    r_val <- round(ct$estimate, 2)
    ci    <- round(ct$conf.int, 2)
    p_val <- if (ct$p.value < 0.001) "< 0.001" else round(ct$p.value, 3)
    corr_text <- paste0("R = ", r_val, " (", ci[1], " to ", ci[2], "),  p = ", p_val)

    # Manual loess fit — native plot_ly avoids heavy ggplotly + geom_smooth
    lo_fit <- tryCatch({
      lo    <- loess(eapc ~ sdi, data = pdat, span = 0.75)
      x_seq <- seq(min(pdat$sdi, na.rm = TRUE),
                   max(pdat$sdi, na.rm = TRUE), length.out = 200)
      y_hat <- predict(lo, newdata = data.frame(sdi = x_seq))
      ok    <- !is.na(y_hat)
      list(x = x_seq[ok], y = y_hat[ok])
    }, error = function(e) NULL)

    has_region <- "region" %in% names(pdat) && !all(is.na(pdat$region))
    color_col  <- if (has_region) as.character(pdat$region) else as.character(pdat$trend)
    color_col[is.na(color_col)] <- "Other"
    col_map    <- if (has_region) GBD_REGION_COLORS else TREND_COLORS

    n_label  <- min(10, nrow(pdat))
    label_df <- dplyr::bind_rows(
      pdat %>% dplyr::slice_max(eapc, n = n_label %/% 2),
      pdat %>% dplyr::slice_min(eapc, n = n_label %/% 2)
    ) %>% dplyr::distinct(location_name, .keep_all = TRUE)

    hover_text <- paste0(
      "<b>", pdat$location_name, "</b>",
      "<br>SDI: ", round(pdat$sdi, 3),
      "<br>EAPC: ", round(pdat$eapc, 2), "%",
      if (has_region) paste0("<br>Region: ", pdat$region) else ""
    )

    p <- plot_ly()

    if (!is.null(lo_fit)) {
      p <- p %>%
        add_lines(x = lo_fit$x, y = lo_fit$y,
                  line = list(color = "black", width = 2),
                  showlegend = FALSE, hoverinfo = "none", name = "LOESS")
    }

    for (grp in sort(unique(color_col))) {
      idx     <- which(color_col == grp)
      grp_col <- if (grp %in% names(col_map)) col_map[[grp]] else "#888888"
      p <- p %>%
        add_markers(x      = pdat$sdi[idx],
                    y      = pdat$eapc[idx],
                    name   = grp,
                    marker = list(color = grp_col, size = 8,
                                  opacity = 0.85, line = list(width = 0)),
                    text      = hover_text[idx],
                    hoverinfo = "text")
    }

    country_annots <- lapply(seq_len(nrow(label_df)), function(i) {
      list(x = label_df$sdi[i], y = label_df$eapc[i],
           text      = label_df$location_name[i],
           showarrow = TRUE, arrowhead = 0, arrowsize = 0.5,
           arrowcolor = "gray70", arrowwidth = 0.8,
           font      = list(size = 9, color = "black"),
           bgcolor   = "rgba(255,255,255,0.75)", borderpad = 2,
           ax = 25, ay = -20)
    })

    corr_annot <- list(
      x         = min(pdat$sdi, na.rm = TRUE) + 0.01,
      y         = max(pdat$eapc, na.rm = TRUE),
      text      = corr_text,
      showarrow = FALSE,
      xanchor   = "left", yanchor = "top",
      font      = list(size = 11, color = "black"),
      bgcolor   = "rgba(255,255,255,0.8)"
    )

    p %>% layout(
      shapes = list(
        list(type = "line", x0 = 0, x1 = 1, xref = "paper",
             y0 = 0, y1 = 0, yref = "y",
             line = list(dash = "dash", color = "gray60", width = 1))
      ),
      annotations   = c(country_annots, list(corr_annot)),
      xaxis         = list(title = "Socio-Demographic Index (SDI)",
                           gridcolor = "#eeeeee", zeroline = FALSE),
      yaxis         = list(title = "EAPC (% per year)",
                           gridcolor = "#eeeeee", zeroline = FALSE),
      title         = list(text = "EAPC vs SDI \u2014 GBD Style",
                           font = list(size = 14)),
      paper_bgcolor = "white", plot_bgcolor = "white",
      legend        = list(font = list(size = 9), orientation = "h",
                           xanchor = "center", x = 0.5, y = -0.18)
    )
  })

  output$dl_eapc_csv <- downloadHandler(
    filename = function() paste0("EAPC_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(rv$eapc_res, f, row.names=FALSE)
  )

  # ==========================================================
  # TAB 4 — JOINPOINT
  # ==========================================================

  output$ui_jp_loc <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    def  <- grep("global|world", tolower(locs), value=TRUE)[1] %||% locs[1]
    selectInput("jp_loc", "Location", choices=locs, selected=def)
  })

  output$ui_jp_sex <- renderUI({
    df <- rv$raw_df; req(df)
    if (!"sex_name" %in% names(df)) return(NULL)
    selectInput("jp_sex", "Sex (override)",
               choices=c("(Use global filter)"="__global__", unique(df$sex_name)))
  })

  observeEvent(input$btn_run_jp, {
    df <- filt_data(); req(df, input$jp_loc)
    df2 <- df
    if (!is.null(input$jp_sex) && input$jp_sex != "__global__" &&
        "sex_name" %in% names(df2))
      df2 <- df2 %>% filter(sex_name == input$jp_sex)

    loc_df <- df2 %>%
      filter(location_name==input$jp_loc, !is.na(val), val>0) %>%
      arrange(year)

    jp_meth <- input$jp_method %||% "bic"
    # For auto methods (bic/aic/permutation) the slider is hidden — use max=5
    # For fixed, use the slider value directly
    n_jp    <- if (jp_meth == "fixed") as.integer(input$jp_max %||% 1) else 5L
    withProgress(message = paste0("Joinpoint [", toupper(jp_meth), "]: ", input$jp_loc, " …"), {
      rv$jp_res <- calc_joinpoint(
        loc_df$year, loc_df$val,
        max_jp    = n_jp,
        method    = jp_meth,
        transform = input$jp_transform %||% "log"
      )
      incProgress(1)
    })

    if (is.null(rv$jp_res))
      showNotification("Joinpoint failed (insufficient data?)", type="warning")
    else
      showNotification(paste0("✓ ", rv$jp_res$n_joinpoints, " joinpoint(s) detected"),
                       type="message", duration=3)
  })

  output$plt_jp_trend <- renderPlotly({
    result <- rv$jp_res; df <- filt_data()
    req(result, df, input$jp_loc)
    loc_df <- df %>% filter(location_name==input$jp_loc, !is.na(val)) %>% arrange(year)

    p <- ggplot(loc_df, aes(x=year, y=val)) +
      geom_point(size=2, color="#2C6FAC", alpha=0.75)

    if (!is.null(result$fitted))
      p <- p + geom_line(data=result$fitted, aes(x=year, y=fitted_val),
                        color="#E74C3C", linewidth=1.3)

    if (length(result$joinpoints)>0)
      for (jp in result$joinpoints)
        p <- p + geom_vline(xintercept=jp, linetype="dashed",
                            color="#8E44AD", linewidth=0.7, alpha=0.8)

    if (nrow(result$segments)>0) {
      segs <- result$segments
      for (i in seq_len(nrow(segs))) {
        mid_yr <- mean(c(segs$start_year[i], segs$end_year[i]))
        mid_y  <- loc_df %>%
          filter(abs(year-mid_yr)==min(abs(year-mid_yr))) %>%
          pull(val) %>% first() * 1.05
        p <- p + annotate("text", x=mid_yr, y=mid_y,
                          label=paste0("APC=",segs$apc[i],"%"),
                          size=3, color="#8E44AD", fontface="bold")
      }
    }

    p <- p +
      labs(x="Year", y="Age-Standardized Rate",
           title=paste0(input$jp_loc, " — ", result$n_joinpoints,
                        " joinpoint(s) | AAPC: ", result$aapc, "%")) +
      theme_minimal(base_size=12)

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$tbl_jp_segments <- renderDT({
    result <- rv$jp_res; req(result)
    disp <- result$segments %>%
      mutate(`Start Year`  = round(start_year),
             `End Year`    = round(end_year),
             `APC (95% CI)` = paste0(round(apc,2)," (",round(apc_lower,2),", ",round(apc_upper,2),")"),
             Significant   = ifelse(significant,"Yes *","No")) %>%
      select(`Start Year`, `End Year`, `APC (95% CI)`, Significant)

    datatable(disp, rownames=FALSE,
             options=list(dom="t", paging=FALSE)) %>%
      formatStyle("Significant",
                  backgroundColor=styleEqual(c("Yes *","No"), c("#D5F5E3","white")))
  })

  output$txt_jp_summary <- renderText({
    result <- rv$jp_res; req(result)
    segs <- result$segments
    method_lbl <- switch(result$method %||% "bic",
      "bic"         = "BIC auto-select",
      "aic"         = "AIC auto-select",
      "permutation" = "Permutation test (NCI, n=199)",
      "fixed"       = paste0("Fixed N=", result$n_joinpoints)
    )
    transform_lbl <- switch(result$transform %||% "log",
      "log"    = "Log/Multiplicative",
      "linear" = "Linear/Additive",
      "sqrt"   = "Square Root"
    )
    paste0(
      "Location:       ", input$jp_loc, "\n",
      "Method:         ", method_lbl, "\n",
      "Model type:     ", transform_lbl, "\n",
      "Joinpoints:     ", result$n_joinpoints, "\n",
      if (length(result$joinpoints)>0)
        paste0("Breakpoints:    ", paste(round(result$joinpoints), collapse=", "), "\n")
      else "",
      "AAPC:           ", result$aapc, "% per year\n\n",
      "Interpretation:\n",
      ifelse(result$aapc > 0,
             paste0("Overall increasing trend of ", abs(result$aapc), "% per year."),
             paste0("Overall decreasing trend of ", abs(result$aapc), "% per year.")),
      "\n\nSegments:\n",
      paste(apply(segs, 1, function(r) {
        paste0("  ", round(as.numeric(r["start_year"])), "–",
               round(as.numeric(r["end_year"])),
               ": APC = ", r["apc"], "%",
               ifelse(as.logical(r["significant"])," *",""))
      }), collapse="\n")
    )
  })

  output$ui_jp_multi_locs <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    selectizeInput("jp_multi_locs", "Select Locations (max 8)",
                  choices=locs, multiple=TRUE, selected=head(locs,5),
                  options=list(maxItems=8))
  })

  output$plt_jp_multi <- renderPlotly({
    df <- filt_data(); req(df, input$jp_multi_locs)
    pdat <- df %>%
      filter(location_name %in% input$jp_multi_locs, !is.na(val)) %>%
      arrange(location_name, year)
    p <- ggplot(pdat, aes(x=year, y=val, color=location_name)) +
      geom_line(linewidth=0.8) +
      geom_point(size=1.4, alpha=0.5) +
      labs(x="Year", y="Rate", color="Location") +
      theme_minimal(base_size=11) +
      theme(legend.position="right")
    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # ==========================================================
  # TAB 5 — DECOMPOSITION
  # ==========================================================

  output$ui_dc_locs <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    selectizeInput("dc_locs", "Locations",
                  choices=locs, multiple=TRUE, selected=head(locs,8),
                  options=list(maxItems=20))
  })

  output$ui_dc_start <- renderUI({
    df <- filt_data(); req(df)
    yrs <- sort(unique(df$year))
    selectInput("dc_start", "Start Year", choices=yrs, selected=min(yrs))
  })

  output$ui_dc_end <- renderUI({
    df <- filt_data(); req(df)
    yrs <- sort(unique(df$year))
    selectInput("dc_end", "End Year", choices=yrs, selected=max(yrs))
  })

  observeEvent(input$btn_run_dc, {
    # Decomposition needs BOTH Number + Rate metrics — bypass the metric filter.
    # Apply only sex and age group filters so Kitagawa can access counts + rates.
    df <- rv$raw_df; req(df, input$dc_locs, input$dc_start, input$dc_end)
    if (!is.null(input$gf_sex) && "sex_name" %in% names(df))
      df <- df %>% dplyr::filter(sex_name == input$gf_sex)
    if (!is.null(input$gf_age) && "age_name" %in% names(df))
      df <- df %>% dplyr::filter(age_name == input$gf_age)
    dc_df <- df %>% filter(location_name %in% input$dc_locs)
    withProgress(message="Running decomposition…", {
      # Run Kitagawa 2-component (always attempts)
      rv$dc_res <- calc_decomp_simple(dc_df,
                                      start_year=as.integer(input$dc_start),
                                      end_year  =as.integer(input$dc_end))
      # Attempt Das Gupta 3-component (returns NULL if age-stratified data unavailable)
      rv$dc_res_dg <- calc_decomp_dasgupta(dc_df,
                                           start_year=as.integer(input$dc_start),
                                           end_year  =as.integer(input$dc_end))
      incProgress(1)
    })
    method_msg <- if (!is.null(rv$dc_res_dg))
      "✓ Decomposition complete (Kitagawa + Das Gupta available)" else
      "✓ Decomposition complete (Kitagawa 2-component)"
    showNotification(method_msg, type="message", duration=4)
  })

  output$plt_dc_bar <- renderPlotly({
    # Prefer 3-component Das Gupta if available; fall back to 2-component Kitagawa
    use_dg <- !is.null(rv$dc_res_dg) && nrow(rv$dc_res_dg) > 0
    res <- if (use_dg) rv$dc_res_dg else rv$dc_res
    req(res)

    if (use_dg) {
      # 3-component Das Gupta stacked bar
      comp_long <- res %>%
        dplyr::arrange(abs_change) %>%
        tidyr::pivot_longer(
          cols = c(pop_growth, aging, epid_change),
          names_to  = "component",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          component = dplyr::case_when(
            component == "pop_growth"  ~ "Population Growth",
            component == "aging"       ~ "Population Aging",
            component == "epid_change" ~ "Epidemiological Change",
            TRUE ~ component
          ),
          component = factor(component,
            levels = c("Population Growth","Population Aging","Epidemiological Change"))
        )

      p <- ggplot(comp_long,
                 aes(x=reorder(location, abs_change), y=value,
                     fill=component,
                     text=paste0(location,"\n",component,":\n",round(value,1)))) +
        geom_col(position="stack") +
        geom_hline(yintercept=0, color="black", linewidth=0.5) +
        scale_fill_manual(
          values = c("Population Growth"      = "#3498DB",
                     "Population Aging"       = "#E67E22",
                     "Epidemiological Change" = "#27AE60")
        ) +
        coord_flip() +
        labs(x=NULL, y="Change in cases", fill=NULL,
             title=paste0("Das Gupta 3-component decomposition: ", input$dc_start, " → ", input$dc_end),
             subtitle="Population Growth + Population Aging + Epidemiological Change") +
        theme_minimal(base_size=10) +
        theme(legend.position="bottom",
              plot.title=element_text(face="bold"))

    } else if (isTRUE(res$has_components[1]) && !all(is.na(res$pop_growth))) {
      # 2-component Kitagawa stacked bar
      comp_long <- res %>%
        dplyr::arrange(abs_change) %>%
        tidyr::pivot_longer(
          cols = c(pop_growth, epid_change),
          names_to  = "component",
          values_to = "value"
        ) %>%
        dplyr::mutate(
          component = dplyr::case_when(
            component == "pop_growth"  ~ "Population Growth",
            component == "epid_change" ~ "Epidemiological Change",
            TRUE ~ component
          ),
          component = factor(component,
            levels = c("Population Growth","Epidemiological Change"))
        )

      p <- ggplot(comp_long,
                 aes(x=reorder(location, abs_change), y=value,
                     fill=component,
                     text=paste0(location,"\n",component,":\n",round(value,1)))) +
        geom_col(position="stack") +
        geom_hline(yintercept=0, color="black", linewidth=0.5) +
        scale_fill_manual(
          values = c("Population Growth"      = "#3498DB",
                     "Epidemiological Change" = "#27AE60")
        ) +
        coord_flip() +
        labs(x=NULL, y="Change in cases", fill=NULL,
             title=paste0("Kitagawa 2-component decomposition: ", input$dc_start, " → ", input$dc_end),
             subtitle="Population Growth + Epidemiological Change (age-stratified data not provided; upload age-specific rates for 3-component Das Gupta)") +
        theme_minimal(base_size=10) +
        theme(legend.position="bottom",
              plot.title=element_text(face="bold"))

    } else {
      # Fallback: simple % change bar (no Number metric available)
      p <- ggplot(res %>% arrange(pct_change),
                 aes(x=reorder(location,pct_change), y=pct_change, fill=direction,
                     text=paste0(location,"\n",round(pct_change,1),"%"))) +
        geom_col() +
        geom_hline(yintercept=0, color="black", linewidth=0.5) +
        scale_fill_manual(values=c("Increasing"="#E74C3C","Decreasing"="#27AE60")) +
        coord_flip() +
        labs(x=NULL, y="% Change in Rate", fill=NULL,
             title=paste(input$dc_start, "→", input$dc_end),
             subtitle="Upload data with Number + Rate metrics for decomposition into components") +
        theme_minimal(base_size=10) +
        theme(legend.position="bottom")
    }

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$plt_dc_pie <- renderPlotly({
    res <- rv$dc_res; req(res)
    cnt <- table(res$direction)
    plot_ly(labels=names(cnt), values=as.vector(cnt), type="pie",
            marker=list(colors=c("#E74C3C","#27AE60")),
            textinfo="label+percent") %>%
      layout(title="Direction of Change", paper_bgcolor="white",
             legend=list(orientation="h"))
  })

  output$plt_dc_trends <- renderPlotly({
    df <- filt_data(); req(df, input$dc_locs)
    pdat <- df %>% filter(location_name %in% input$dc_locs, !is.na(val)) %>%
      arrange(location_name, year)
    p <- ggplot(pdat, aes(x=year, y=val, color=location_name)) +
      geom_line(linewidth=0.8) +
      geom_point(size=1.4, alpha=0.5) +
      geom_vline(xintercept=c(as.integer(input$dc_start), as.integer(input$dc_end)),
                linetype="dashed", color="gray40", linewidth=0.6) +
      labs(x="Year", y="Rate", color="Location") +
      theme_minimal(base_size=11)
    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$tbl_dc <- renderDT({
    # Show Das Gupta if available, else Kitagawa
    use_dg <- !is.null(rv$dc_res_dg) && nrow(rv$dc_res_dg) > 0
    res <- if (use_dg) rv$dc_res_dg else rv$dc_res
    req(res)

    # Pick columns based on which method was used
    cols_show <- intersect(
      c("location","start_year","end_year","start_val","end_val",
        "abs_change","pct_change",
        "pop_growth","aging","epid_change",
        "pct_pop_growth","pct_aging","pct_epid",
        "direction","decomp_method"),
      names(res)
    )
    disp <- res[, cols_show, drop=FALSE]
    # Friendly column names
    rename_map <- c(
      "pop_growth"     = "Pop. Growth",
      "aging"          = "Pop. Aging",
      "epid_change"    = "Epid. Change",
      "pct_pop_growth" = "% Pop. Growth",
      "pct_aging"      = "% Aging",
      "pct_epid"       = "% Epid.",
      "decomp_method"  = "Method"
    )
    for (k in names(rename_map))
      if (k %in% names(disp)) names(disp)[names(disp) == k] <- rename_map[[k]]

    datatable(disp, rownames=FALSE,
             options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("pct_change",
                  color=styleInterval(0, c("#1E8449","#C0392B")),
                  fontWeight="bold")
  })

  output$dl_dc_csv <- downloadHandler(
    filename = function() {
      suffix <- if (!is.null(rv$dc_res_dg)) "DasGupta" else "Kitagawa"
      paste0("Decomposition_", suffix, "_", Sys.Date(), ".csv")
    },
    content  = function(f) {
      out <- if (!is.null(rv$dc_res_dg)) rv$dc_res_dg else rv$dc_res
      write.csv(out, f, row.names=FALSE)
    }
  )

  # ==========================================================
  # TAB 6 — FORECASTING (ENHANCED)
  # ==========================================================

  output$ui_fc_loc <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    def  <- grep("global|world", tolower(locs), value=TRUE)[1] %||% locs[1]
    selectInput("fc_loc", "Location", choices=locs, selected=def)
  })

  output$ui_fc_multi_locs <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    selectizeInput("fc_multi_locs", "Compare (max 6)",
                  choices=locs, multiple=TRUE, selected=head(locs,3),
                  options=list(maxItems=6))
  })

  # Helper to dispatch a model by key
  run_fc_model <- function(key, years, rates, h) {
    switch(key,
      "arima"    = calc_arima_forecast(  years, rates, h=h),
      "ets"      = calc_ets_forecast(    years, rates, h=h),
      "tbats"    = calc_tbats_forecast(  years, rates, h=h),
      "nnetar"   = calc_nnetar_forecast( years, rates, h=h),
      "theta"    = calc_theta_forecast(  years, rates, h=h),
      "holt"     = calc_holt_forecast(   years, rates, h=h),
      "ensemble" = calc_ensemble_forecast(years, rates, h=h),
      calc_arima_forecast(years, rates, h=h)
    )
  }

  # ==========================================================
  # ARIMA + ETS DUAL PANEL
  # ==========================================================

  output$ui_arima_ets_loc <- renderUI({
    df <- filt_data()
    if (is.null(df) || !"location_name" %in% names(df)) return(NULL)
    locs <- sort(unique(df$location_name))
    selectInput("ae_loc", "Location", choices = locs, selected = locs[1])
  })

  ae_results <- reactiveValues(arima = NULL, ets = NULL)

  observeEvent(input$btn_run_arima_ets, {
    df <- filt_data(); req(df, input$ae_loc)
    loc_df <- df %>%
      dplyr::filter(location_name == input$ae_loc, !is.na(val), val > 0) %>%
      dplyr::arrange(year)
    req(nrow(loc_df) >= 5)
    h <- input$ae_horizon %||% 15

    withProgress(message = paste("Running ARIMA + ETS:", input$ae_loc, "…"), value = 0, {
      ae_results$arima <- calc_arima_forecast(loc_df$year, loc_df$val, h = h)
      incProgress(0.5)
      ae_results$ets   <- calc_ets_forecast(loc_df$year, loc_df$val, h = h)
      incProgress(0.5)
    })
    showNotification("✓ ARIMA + ETS complete", type = "message", duration = 3)
  })

  # GBD Fig 8 style: red circles = actual, black circles = forecast,
  # dashed vertical line at forecast start, blue CI ribbon, panel labels (A)/(B)
  make_arima_ets_panel <- function(arima_res, ets_res, loc) {

    make_single <- function(res, tag, y_label = "ASPR (per 100,000 persons)") {
      hist <- res$historical
      fc   <- res$forecast
      last_hist_yr <- max(hist$year)

      # Combine for x-axis range
      all_yrs <- c(hist$year, fc$year)

      # Build data frames with Type label
      hist_df <- data.frame(year = hist$year, rate = hist$rate, Type = "Actual")
      fc_df   <- data.frame(year = fc$year,   rate = fc$point,  Type = "Forecast")
      pts_df  <- dplyr::bind_rows(hist_df, fc_df)

      ggplot() +
        # 95% CI ribbon (light blue)
        geom_ribbon(data = fc,
                    aes(x = year, ymin = lower_95, ymax = upper_95),
                    fill = "#AED6F1", alpha = 0.55) +
        # 80% CI ribbon (slightly darker)
        geom_ribbon(data = fc,
                    aes(x = year, ymin = lower_80, ymax = upper_80),
                    fill = "#5DADE2", alpha = 0.30) +
        # Actual line (red)
        geom_line(data = hist_df, aes(x = year, y = rate),
                  color = "#E74C3C", linewidth = 0.8) +
        # Forecast line (black)
        geom_line(data = fc_df, aes(x = year, y = rate),
                  color = "black", linewidth = 0.8) +
        # Actual points — red open circles (GBD style)
        geom_point(data = hist_df, aes(x = year, y = rate, color = Type, shape = Type),
                   size = 2.2, stroke = 0.8) +
        # Forecast points — black filled circles
        geom_point(data = fc_df, aes(x = year, y = rate, color = Type, shape = Type),
                   size = 2.2, stroke = 0.8) +
        # Dashed vertical divider
        geom_vline(xintercept = last_hist_yr + 0.5,
                   linetype = "dashed", color = "gray50", linewidth = 0.7) +
        scale_color_manual(name = "Type",
                           values = c("Actual" = "#E74C3C", "Forecast" = "black")) +
        scale_shape_manual(name = "Type",
                           values = c("Actual" = 1, "Forecast" = 16)) +
        scale_x_continuous(breaks = seq(floor(min(all_yrs)/5)*5,
                                        ceiling(max(all_yrs)/5)*5, by = 10)) +
        labs(x     = "Year",
             y     = y_label,
             title = paste0("(", tag, ")  ", res$model_label)) +
        theme_gbd(base_size = 11) +
        theme(legend.position  = c(0.85, 0.92),
              legend.key.size  = unit(0.5, "cm"),
              plot.title       = element_text(face = "bold", size = 12, hjust = 0))
    }

    p1 <- make_single(arima_res, "A")
    p2 <- make_single(ets_res,   "B")

    if (requireNamespace("patchwork", quietly = TRUE)) {
      patchwork::wrap_plots(p1, p2, ncol = 1) +
        patchwork::plot_annotation(
          title    = paste0("Forecast — ", loc),
          subtitle = "Shaded area: 95% CI (light) and 80% CI (dark)  |  Dashed line: forecast start",
          theme    = theme(plot.title    = element_text(face = "bold", size = 13),
                           plot.subtitle = element_text(size = 9.5, color = "#555"),
                           plot.background = element_rect(fill = "white", color = NA))
        )
    } else {
      p1
    }
  }

  output$plt_arima_ets_dual <- renderPlot({
    req(ae_results$arima, ae_results$ets)
    tryCatch(
      make_arima_ets_panel(ae_results$arima, ae_results$ets, input$ae_loc %||% ""),
      error = function(e) {
        ggplot() +
          annotate("text", x=0.5, y=0.5, label=paste("Render error:", e$message),
                   size=4, color="red") +
          theme_void()
      }
    )
  }, height = 560, width = function() max(session$clientData$output_plt_arima_ets_dual_width, 600))

  output$plt_arima_ets_overlay <- renderPlotly({
    req(ae_results$arima, ae_results$ets)
    ar   <- ae_results$arima
    et   <- ae_results$ets
    hist <- ar$historical
    last_yr <- max(hist$year)

    plotly::plot_ly() %>%
      # Observed historical line
      plotly::add_trace(
        data = hist, x = ~year, y = ~rate,
        type = "scatter", mode = "lines+markers",
        name = "Observed",
        line   = list(color = "#2C3E50", width = 2),
        marker = list(color = "#2C3E50", size = 5, symbol = "circle"),
        hovertemplate = paste0("<b>Observed</b><br>Year: %{x}<br>Rate: %{y:.3f}<extra></extra>")
      ) %>%
      # ETS forecast — drawn first (bottom layer)
      plotly::add_trace(
        data = et$forecast, x = ~year, y = ~point,
        type = "scatter", mode = "lines+markers",
        name = et$model_label,
        line   = list(color = "#E74C3C", width = 4, dash = "dot"),
        marker = list(color = "#E74C3C", size = 8, symbol = "diamond"),
        hovertemplate = paste0("<b>", et$model_label, "</b><br>Year: %{x}<br>Forecast: %{y:.3f}<extra></extra>")
      ) %>%
      # ARIMA forecast — drawn on top
      plotly::add_trace(
        data = ar$forecast, x = ~year, y = ~point,
        type = "scatter", mode = "lines+markers",
        name = ar$model_label,
        line   = list(color = "#2980B9", width = 3, dash = "dash"),
        marker = list(color = "#2980B9", size = 7, symbol = "circle"),
        hovertemplate = paste0("<b>", ar$model_label, "</b><br>Year: %{x}<br>Forecast: %{y:.3f}<extra></extra>")
      ) %>%
      # Dashed vertical divider at forecast start
      plotly::add_segments(
        x = last_yr + 0.5, xend = last_yr + 0.5,
        y = min(hist$rate, na.rm=TRUE) * 0.95,
        yend = max(hist$rate, na.rm=TRUE) * 1.05,
        line = list(color = "gray60", dash = "dash", width = 1.5),
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%
      plotly::layout(
        title = list(
          text = paste0("<b>ARIMA vs ETS Overlay — ", input$ae_loc %||% "", "</b>"),
          font = list(size = 13)
        ),
        xaxis  = list(title = "Year", showgrid = TRUE, gridcolor = "#ebebeb"),
        yaxis  = list(title = "Age-Standardized Rate (per 100,000)",
                      showgrid = TRUE, gridcolor = "#ebebeb"),
        legend = list(orientation = "h", x = 0, y = -0.25,
                      font = list(size = 11)),
        paper_bgcolor = "white", plot_bgcolor = "white",
        hovermode = "x unified"
      )
  })

  output$tbl_arima_ets <- renderDT({
    req(ae_results$arima, ae_results$ets)
    ar <- ae_results$arima
    et <- ae_results$ets
    tbl <- data.frame(
      Model = c(ar$model_label, et$model_label),
      AIC   = c(ar$aic,  et$aic),
      AICc  = c(ar$aicc, et$aicc),
      BIC   = c(ar$bic,  et$bic)
    )
    datatable(tbl, rownames = FALSE,
              options = list(dom = "t", paging = FALSE)) %>%
      formatStyle("AIC",
                  background = styleColorBar(range(tbl$AIC, na.rm = TRUE), "#D6EAF8"),
                  backgroundSize = "100% 80%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle(0, fontWeight = "bold")
  })

  output$dl_fig_arima_ets <- downloadHandler(
    filename = function() paste0("ARIMA_ETS_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      req(ae_results$arima, ae_results$ets)
      p <- make_arima_ets_panel(ae_results$arima, ae_results$ets, input$ae_loc %||% "")
      ggsave_safe(f, p, (input$fig_w %||% 8) * 1.5, input$fig_h %||% 5, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_arima_ets2 <- downloadHandler(
    filename = function() paste0("ARIMA_ETS_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      req(ae_results$arima, ae_results$ets)
      p <- make_arima_ets_panel(ae_results$arima, ae_results$ets, input$ae_loc %||% "")
      ggsave_safe(f, p, (input$fig_w %||% 8) * 1.5, input$fig_h %||% 5, input$fig_dpi %||% 300)
    }
  )

  # Run forecast(s) — primary + optional 2nd model (GBD standard: ARIMA + ETS)
  observeEvent(input$btn_run_fc, {
    df <- filt_data(); req(df, input$fc_loc)
    loc_df <- df %>%
      filter(location_name==input$fc_loc, !is.na(val), val>0) %>%
      arrange(year)

    n_models <- if (isTRUE(input$fc_show_2nd) &&
                    !is.null(input$fc_method2) &&
                    input$fc_method2 != input$fc_method) 2 else 1

    withProgress(message=paste("Forecasting:", input$fc_loc, "…"),
                 value=0, {
      rv$fc_res <- run_fc_model(input$fc_method, loc_df$year, loc_df$val,
                                h=input$fc_horizon)
      incProgress(0.6)

      if (n_models == 2) {
        rv$fc_res2 <- run_fc_model(input$fc_method2, loc_df$year, loc_df$val,
                                   h=input$fc_horizon)
      } else {
        rv$fc_res2 <- NULL
      }
      incProgress(0.4)
    })

    if (is.null(rv$fc_res))
      showNotification("Forecast failed — check data", type="warning")
    else {
      msg <- paste0("✓ ", rv$fc_res$model_label)
      if (!is.null(rv$fc_res2))
        msg <- paste0(msg, "  +  ", rv$fc_res2$model_label)
      showNotification(msg, type="message", duration=4)
    }
  })

  # Compare all models
  observeEvent(input$btn_compare_fc, {
    df <- filt_data(); req(df, input$fc_loc)
    loc_df <- df %>%
      filter(location_name==input$fc_loc, !is.na(val), val>0) %>%
      arrange(year)

    withProgress(message="Comparing all models (this may take ~30s)…", value=0, {
      rv$model_comp <- compare_forecast_models(loc_df$year, loc_df$val)
      incProgress(1)
    })
    showNotification("✓ Model comparison complete", type="message", duration=3)
  })

  output$plt_fc_main <- renderPlotly({
    result <- rv$fc_res; req(result)
    hist   <- result$historical
    fc     <- result$forecast
    res2   <- rv$fc_res2   # may be NULL

    # Colours: primary = navy, secondary = red (Lancet palette)
    COL1 <- "#00468B"   # ARIMA / primary
    COL2 <- "#ED0000"   # ETS   / secondary

    p <- ggplot() +
      geom_line(data=hist, aes(x=year, y=rate,
                               text=paste0("Year: ",year,"\nObserved: ",round(rate,4))),
               color="#2C3E50", linewidth=1.1) +
      geom_point(data=hist, aes(x=year, y=rate), color="#2C3E50", size=1.5)

    # Primary model ribbons + line
    if (input$fc_ci %in% c("both","95"))
      p <- p + geom_ribbon(data=fc, aes(x=year, ymin=lower_95, ymax=upper_95),
                           fill=COL1, alpha=0.12)
    if (input$fc_ci %in% c("both","80"))
      p <- p + geom_ribbon(data=fc, aes(x=year, ymin=lower_80, ymax=upper_80),
                           fill=COL1, alpha=0.22)
    p <- p +
      geom_line(data=fc, aes(x=year, y=point,
                             text=paste0("Year: ",year,"\n",result$model_label,": ",
                                        round(point,4))),
               color=COL1, linewidth=1.2, linetype="dashed")

    # Optional second model (GBD comparison)
    if (!is.null(res2)) {
      fc2 <- res2$forecast
      if (input$fc_ci %in% c("both","95"))
        p <- p + geom_ribbon(data=fc2, aes(x=year, ymin=lower_95, ymax=upper_95),
                             fill=COL2, alpha=0.10)
      if (input$fc_ci %in% c("both","80"))
        p <- p + geom_ribbon(data=fc2, aes(x=year, ymin=lower_80, ymax=upper_80),
                             fill=COL2, alpha=0.18)
      p <- p +
        geom_line(data=fc2, aes(x=year, y=point,
                               text=paste0("Year: ",year,"\n",res2$model_label,": ",
                                          round(point,4))),
                 color=COL2, linewidth=1.2, linetype="dotdash")
    }

    # Divider + label
    p <- p +
      geom_vline(xintercept=max(hist$year)+0.5,
                linetype="dotted", color="gray50", linewidth=0.7) +
      annotate("text", x=max(hist$year)+0.8, y=max(hist$rate)*0.99,
               label="Forecast →", hjust=0, size=3.2, color="gray40")

    title_txt <- paste0(input$fc_loc, " — ", result$model_label)
    if (!is.null(res2)) title_txt <- paste0(title_txt, "  vs  ", res2$model_label)
    title_txt <- paste0(title_txt, " | Horizon: ", input$fc_horizon, " years")

    p <- p +
      labs(x="Year", y="Age-Standardized Rate (per 100 000)", title=title_txt) +
      theme_minimal(base_size=12) +
      theme(plot.title=element_text(face="bold", size=11))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             legend=list(orientation="h", y=-0.15))
  })

  output$txt_fc_model <- renderPrint({
    result <- rv$fc_res; req(result)
    cat("Model:", result$model_label, "\n")
    cat(sprintf("AIC: %s  |  AICc: %s  |  BIC: %s\n",
                ifelse(is.na(result$aic),"N/A",result$aic),
                ifelse(is.na(result$aicc),"N/A",result$aicc),
                ifelse(is.na(result$bic),"N/A",result$bic)))
    cat("\n")
    if (!is.null(result$model)) tryCatch(print(result$model), error=function(e) NULL)
  })

  output$tbl_fc <- renderDT({
    result <- rv$fc_res; req(result)
    disp <- result$forecast %>%
      mutate(across(where(is.numeric), ~round(.,4))) %>%
      rename(Year=year, `Point Forecast`=point,
             `80% Lower`=lower_80, `80% Upper`=upper_80,
             `95% Lower`=lower_95, `95% Upper`=upper_95)
    datatable(disp, rownames=FALSE,
             options=list(dom="t", paging=FALSE, scrollX=TRUE))
  })

  output$dl_fc_csv <- downloadHandler(
    filename = function() paste0("Forecast_",input$fc_method,"_",Sys.Date(),".csv"),
    content  = function(f) write.csv(rv$fc_res$forecast, f, row.names=FALSE)
  )

  # Model comparison table
  output$tbl_fc_compare <- renderDT({
    comp <- rv$model_comp
    if (is.null(comp)) return(datatable(data.frame(Note="Click 'Compare All Models' to run.")))
    best_row <- which(comp$Best == TRUE)  # 1-based row index
    # Build JS row-highlight callback: DT rows are 0-indexed in JS
    row_cb <- if (length(best_row) > 0) {
      js_rows <- paste(best_row - 1L, collapse=",")
      JS(sprintf(
        "function(row, data, index) { if ([%s].indexOf(index) >= 0) $(row).css('background-color','#D5F5E3'); }",
        js_rows
      ))
    } else JS("function(row, data, index) {}")

    datatable(comp %>% select(-Best), rownames=FALSE,
             options=list(dom="t", paging=FALSE, scrollX=TRUE,
                          rowCallback=row_cb)) %>%
      formatStyle("RMSE",
                  backgroundColor=styleEqual(min(comp$RMSE, na.rm=TRUE), "#D5F5E3"))
  })

  output$dl_compare_csv <- downloadHandler(
    filename = function() paste0("ModelComparison_",Sys.Date(),".csv"),
    content  = function(f) write.csv(rv$model_comp, f, row.names=FALSE)
  )

  # All-models comparison forecast plot
  output$plt_fc_all_models <- renderPlotly({
    df <- filt_data(); req(df, input$fc_loc)
    loc_df <- df %>%
      filter(location_name==input$fc_loc, !is.na(val), val>0) %>%
      arrange(year)
    if (nrow(loc_df) < 5) return(NULL)

    h <- input$fc_horizon %||% 15
    model_fns <- list(
      "ARIMA"   = function() calc_arima_forecast(loc_df$year, loc_df$val, h),
      "ETS"     = function() calc_ets_forecast(  loc_df$year, loc_df$val, h),
      "TBATS"   = function() calc_tbats_forecast( loc_df$year, loc_df$val, h),
      "Theta"   = function() calc_theta_forecast( loc_df$year, loc_df$val, h),
      "Holt"    = function() calc_holt_forecast(  loc_df$year, loc_df$val, h)
    )

    # Only run if button was clicked (use fc_res as trigger to avoid auto-execution)
    req(input$btn_run_fc > 0 || input$btn_compare_fc > 0)

    # Collect all model forecasts into a single data frame so aes(color=Model) works correctly
    all_fc <- bind_rows(lapply(names(model_fns), function(nm) {
      fc_m <- tryCatch(model_fns[[nm]](), error=function(e) NULL)
      if (is.null(fc_m)) return(NULL)
      fc_m$forecast %>% mutate(Model = nm)
    }))

    p <- ggplot()
    p <- p + geom_line(data=loc_df, aes(x=year, y=val), color="#2C3E50", linewidth=1.2)
    p <- p + geom_point(data=loc_df, aes(x=year, y=val), color="#2C3E50", size=1.5)

    if (nrow(all_fc) > 0) {
      p <- p + geom_line(data=all_fc,
                        aes(x=year, y=point, color=Model),
                        linewidth=0.9, linetype="dashed")
    }

    p <- p +
      geom_vline(xintercept=max(loc_df$year)+0.5,
                linetype="dotted", color="gray50") +
      scale_color_brewer(palette="Set1", name="Model") +
      labs(x="Year", y="Rate", title=paste0(input$fc_loc, " — All Models")) +
      theme_minimal(base_size=11)

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Multi-location comparison
  output$plt_fc_multi <- renderPlotly({
    df <- filt_data(); req(df, input$fc_multi_locs)
    locs <- input$fc_multi_locs

    hist <- df %>% filter(location_name %in% locs, !is.na(val)) %>%
      arrange(location_name, year)

    fc_list <- lapply(locs, function(loc) {
      ld <- hist %>% filter(location_name==loc, val>0) %>% arrange(year)
      if (nrow(ld) < 5) return(NULL)
      fc <- tryCatch(
        calc_arima_forecast(ld$year, ld$val, h=input$fc_horizon %||% 15),
        error=function(e) NULL
      )
      if (is.null(fc)) return(NULL)
      mutate(fc$forecast, location_name=loc)
    }) %>% bind_rows()

    p <- ggplot() +
      geom_line(data=hist,    aes(x=year, y=val, color=location_name), linewidth=0.8) +
      geom_point(data=hist,   aes(x=year, y=val, color=location_name), size=1.2, alpha=0.5)

    if (nrow(fc_list)>0) {
      p <- p +
        geom_line(data=fc_list,   aes(x=year, y=point, color=location_name),
                 linewidth=0.8, linetype="dashed") +
        geom_ribbon(data=fc_list,
                   aes(x=year, ymin=lower_95, ymax=upper_95, fill=location_name),
                   alpha=0.08)
    }

    p <- p +
      labs(x="Year", y="Rate", color="Location", fill="Location") +
      theme_minimal(base_size=11)

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # ==========================================================
  # TAB 7 — DESCRIPTIVE STATS & HEATMAP
  # ==========================================================

  output$ui_desc_locs <- renderUI({
    df <- filt_data(); req(df)
    locs <- sort(unique(df$location_name))
    selectizeInput("desc_locs", "Locations (blank = auto top 20)",
                  choices=locs, multiple=TRUE, selected=NULL,
                  options=list(maxItems=20, placeholder="Auto: Top 10 + Bottom 10 by rate"))
  })

  observeEvent(input$btn_run_desc, {
    df <- filt_data(); req(df)
    df2 <- df
    if (!is.null(input$desc_locs) && length(input$desc_locs)>0)
      df2 <- df2 %>% filter(location_name %in% input$desc_locs)

    withProgress(message="Computing descriptive statistics…", {
      rv$desc_res <- calc_descriptive_stats(df2)
      incProgress(1)
    })
    showNotification("✓ Descriptive statistics computed", type="message", duration=3)
  })

  output$tbl_desc <- renderDT({
    res <- rv$desc_res
    if (is.null(res)) return(datatable(data.frame(Note="Click 'Compute Statistics' to run.")))
    datatable(res, rownames=FALSE,
             options=list(pageLength=25, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("% Change",
                  color=styleInterval(0, c("#1E8449","#C0392B")),
                  fontWeight="bold")
  })

  output$dl_desc_csv <- downloadHandler(
    filename = function() paste0("Table1_DescriptiveStats_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(rv$desc_res, f, row.names=FALSE)
  )

  # Heatmap
  output$plt_heatmap <- renderPlotly({
    df <- filt_data(); req(df, "location_name" %in% names(df))

    locs_sel <- if (!is.null(input$desc_locs) && length(input$desc_locs)>0)
      input$desc_locs else {
        rate_df <- df %>% dplyr::filter(metric_name == "Rate")
        if (nrow(rate_df) == 0) rate_df <- df
        top_bottom_locs(rate_df, n_top = 10, n_bot = 10)
      }

    heat_df <- df %>%
      filter(location_name %in% locs_sel, !is.na(val)) %>%
      group_by(location_name, year) %>%
      summarise(val = mean(val, na.rm=TRUE), .groups="drop")

    if (isTRUE(input$heatmap_norm)) {
      heat_df <- heat_df %>%
        group_by(location_name) %>%
        mutate(val = (val - min(val, na.rm=TRUE)) /
                     pmax(max(val,na.rm=TRUE) - min(val,na.rm=TRUE), 1e-9)) %>%
        ungroup()
    }

    heat_wide <- heat_df %>%
      pivot_wider(names_from=year, values_from=val) %>%
      column_to_rownames("location_name") %>%
      as.matrix()

    pal_fn <- switch(input$heatmap_palette %||% "viridis",
      "viridis" = viridis::viridis,
      "plasma"  = viridis::plasma,
      function(n) brewer.pal(min(n,9), input$heatmap_palette %||% "YlOrRd")
    )

    plot_ly(
      x = colnames(heat_wide),
      y = rownames(heat_wide),
      z = heat_wide,
      type = "heatmap",
      colorscale = list(
        list(0, pal_fn(10)[1]),
        list(0.5, pal_fn(10)[5]),
        list(1, pal_fn(10)[10])
      ),
      hovertemplate = "%{y}<br>Year: %{x}<br>Value: %{z:.3f}<extra></extra>"
    ) %>%
      layout(
        title  = "Rate Heatmap (Year × Location)",
        xaxis  = list(title="Year"),
        yaxis  = list(title=NULL),
        paper_bgcolor="white",
        margin = list(l=140, b=60)
      )
  })

  # Box plot
  output$plt_boxplot <- renderPlotly({
    df <- filt_data(); req(df)
    locs_sel <- if (!is.null(input$desc_locs) && length(input$desc_locs)>0)
      input$desc_locs else {
        rate_df <- df %>% dplyr::filter(metric_name == "Rate")
        if (nrow(rate_df) == 0) rate_df <- df
        top_bottom_locs(rate_df, n_top = 10, n_bot = 10)
      }

    pdat <- df %>% filter(location_name %in% locs_sel, !is.na(val))

    p <- ggplot(pdat, aes(x=reorder(location_name, val, FUN=median),
                          y=val, fill=location_name)) +
      geom_boxplot(alpha=0.75, outlier.size=1.5, show.legend=FALSE) +
      coord_flip() +
      labs(x=NULL, y="Rate", title="Rate Distribution by Location") +
      theme_minimal(base_size=11) +
      scale_fill_brewer(palette="Set2")

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Sex comparison
  output$plt_sex_compare <- renderPlotly({
    df <- rv$raw_df; req(df)
    if (!"sex_name" %in% names(df)) return(NULL)

    sexes <- tolower(unique(df$sex_name))
    # Data uses "Males"/"Females" — use grepl to match both singular and plural forms
    if (!any(grepl("^male|^female", sexes))) return(NULL)

    # Apply metric/age filters but not sex filter
    df2 <- df
    if (!is.null(input$gf_metric) && "metric_name" %in% names(df2))
      df2 <- df2 %>% filter(metric_name == input$gf_metric)
    if (!is.null(input$gf_age) && "age_name" %in% names(df2))
      df2 <- df2 %>% filter(age_name == input$gf_age)

    locs_sel <- if (!is.null(input$desc_locs) && length(input$desc_locs)>0)
      input$desc_locs else top_bottom_locs(df2, n_top = 10, n_bot = 10)

    pdat <- df2 %>%
      filter(location_name %in% locs_sel,
             grepl("^male|^female", sex_name, ignore.case=TRUE),
             !is.na(val)) %>%
      mutate(sex_label = ifelse(grepl("^female", sex_name, ignore.case=TRUE),
                                "Female", "Male")) %>%
      group_by(location_name, year, sex_label) %>%
      summarise(val=mean(val,na.rm=TRUE), .groups="drop")

    if (nrow(pdat)==0) return(NULL)

    p <- ggplot(pdat, aes(x=year, y=val, color=sex_label, linetype=location_name)) +
      geom_line(linewidth=0.8) +
      geom_point(size=1.2, alpha=0.6) +
      scale_color_manual(values=c("Male"="#3498DB","Female"="#E74C3C")) +
      labs(x="Year", y="Rate", color="Sex", linetype="Location",
           title="Male vs Female Rate Comparison") +
      theme_minimal(base_size=11)

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Temporal trend lines
  output$plt_trend_lines <- renderPlotly({
    df <- filt_data(); req(df)
    locs_sel <- if (!is.null(input$desc_locs) && length(input$desc_locs)>0)
      input$desc_locs else {
        rate_df <- df %>% dplyr::filter(metric_name == "Rate")
        if (nrow(rate_df) == 0) rate_df <- df
        top_bottom_locs(rate_df, n_top = 10, n_bot = 10)
      }

    pdat <- df %>%
      filter(location_name %in% locs_sel, !is.na(val)) %>%
      arrange(location_name, year)

    p <- ggplot(pdat, aes(x=year, y=val, color=location_name,
                          text=paste0(location_name,"\nYear: ",year,"\nValue: ",round(val,3)))) +
      geom_line(linewidth=0.9) +
      geom_point(size=1.5, alpha=0.6) +
      geom_smooth(aes(group=location_name), method="loess", se=FALSE,
                 linewidth=0.5, linetype="dotted", alpha=0.5) +
      labs(x="Year", y="Rate", color="Location",
           title="Temporal Trends — All Selected Locations") +
      theme_minimal(base_size=11) +
      theme(legend.position="right")

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # ── Butterfly chart UI helpers ────────────────────────────────────────
  output$ui_butterfly_year <- renderUI({
    df <- rv$raw_df; req(df)
    yrs <- sort(unique(df$year))
    sliderInput("butterfly_year", "Year", min=min(yrs), max=max(yrs),
                value=max(yrs), step=1, sep="")
  })
  output$ui_butterfly_loc <- renderUI({
    df <- rv$raw_df; req(df)
    locs <- sort(unique(df$location_name))
    selectInput("butterfly_loc", "Location",
                choices = c("All (overlay)" = "all", setNames(locs, locs)),
                selected = "all")
  })

  # ── Butterfly chart (Male vs Female × Age group) ─────────────────────
  output$plt_butterfly <- renderPlotly({
    df <- rv$raw_df; req(df, input$butterfly_year)

    # Need sex-disaggregated + age-disaggregated data
    # Accept "Male"/"Female" or "Males"/"Females"
    has_sex <- "sex_name" %in% names(df) &&
               any(grepl("^male|^female", df$sex_name, ignore.case=TRUE))
    has_age <- "age_name" %in% names(df) &&
               any(!grepl("standardized|All ages", df$age_name, ignore.case=TRUE))

    if (!has_sex || !has_age) {
      return(plotly_empty() %>%
        layout(title=list(
          text="Age-disaggregated sex data not found in this dataset.\nLoad a file with Male/Female rows and multiple age groups.",
          font=list(size=13, color="#888")),
          paper_bgcolor="white"))
    }

    # Build butterfly data — handle "Male"/"Female" or "Males"/"Females"
    m_col <- if ("metric_name" %in% names(df)) "metric_name" else NULL
    pdat <- df %>%
      filter(grepl("^male|^female", sex_name, ignore.case=TRUE),
             year == input$butterfly_year) %>%
      { if (!is.null(m_col)) filter(., metric_name == "Rate") else . } %>%
      filter(!grepl("standardized|All ages", age_name, ignore.case=TRUE),
             !is.na(val)) %>%
      { if (!is.null(input$butterfly_loc) && input$butterfly_loc != "all")
          filter(., location_name == input$butterfly_loc) else . } %>%
      mutate(sex_clean = ifelse(grepl("^female", sex_name, ignore.case=TRUE),
                                "Female", "Male")) %>%
      group_by(age_name, sex_clean) %>%
      summarise(val = mean(val, na.rm=TRUE), .groups="drop") %>%
      mutate(
        # Female → negative (left side), Male → positive (right side)
        val_plot  = ifelse(sex_clean == "Female", -val, val),
        sex_name  = sex_clean,   # keep consistent name for fill
        # Sort age groups
        age_order = suppressWarnings(as.numeric(gsub("[^0-9].*","", age_name)))
      ) %>%
      arrange(age_order)

    if (nrow(pdat) == 0) {
      return(plotly_empty() %>%
        layout(title=list(text="No data for selected year/location.",
                          font=list(size=13,color="#888")),
               paper_bgcolor="white"))
    }

    age_lvls <- unique(pdat$age_name[order(pdat$age_order)])
    pdat$age_f <- factor(pdat$age_name, levels=age_lvls)

    p <- ggplot(pdat, aes(x=val_plot, y=age_f, fill=sex_name,
                          text=paste0(sex_name," | Age: ",age_name,
                                      "\nRate: ",round(abs(val_plot),4)))) +
      geom_col(width=0.7, alpha=0.85) +
      geom_vline(xintercept=0, colour="black", linewidth=0.5) +
      scale_fill_manual(values=c("Male"="#00468B","Female"="#ED0000",
                                 "Males"="#00468B","Females"="#ED0000"),
                        name="Sex") +
      scale_x_continuous(labels=function(x) format(abs(x), scientific=FALSE)) +
      labs(x="Age-specific death rate (per 100 000)",
           y="Age Group",
           title=paste0("Sex × Age Butterfly — ",
                        input$butterfly_year,
                        if (!is.null(input$butterfly_loc) && input$butterfly_loc!="all")
                          paste0(" | ",input$butterfly_loc) else ""),
           caption="← Female  |  Male →") +
      theme_minimal(base_size=11) +
      theme(legend.position="top",
            plot.caption=element_text(hjust=0.5, colour="grey40"))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white",
             legend=list(orientation="h", y=1.05))
  })

  # Butterfly download
  output$dl_fig_butterfly <- downloadHandler(
    filename = function() paste0("Butterfly_",input$butterfly_year,"_",Sys.Date(),".png"),
    content  = function(f) {
      df <- rv$raw_df; req(df, input$butterfly_year)
      pdat <- df %>%
        filter(grepl("^male|^female", sex_name, ignore.case=TRUE),
               year==input$butterfly_year) %>%
        { if ("metric_name" %in% names(.)) filter(., metric_name=="Rate") else . } %>%
        filter(!grepl("standardized|All ages", age_name, ignore.case=TRUE),
               !is.na(val)) %>%
        mutate(sex_clean=ifelse(grepl("^female",sex_name,ignore.case=TRUE),"Female","Male")) %>%
        group_by(age_name,sex_clean) %>%
        summarise(val=mean(val,na.rm=TRUE),.groups="drop") %>%
        mutate(val_plot=ifelse(sex_clean=="Female",-val,val),
               age_order=suppressWarnings(as.numeric(gsub("[^0-9].*","",age_name)))) %>%
        arrange(age_order) %>%
        mutate(age_f=factor(age_name,levels=unique(age_name)))

      p <- ggplot(pdat, aes(x=val_plot,y=age_f,fill=sex_clean)) +
        geom_col(width=0.7,alpha=0.85) +
        geom_vline(xintercept=0,colour="black",linewidth=0.5) +
        scale_fill_manual(values=c("Male"="#00468B","Female"="#ED0000"),name="Sex") +
        scale_x_continuous(labels=function(x) format(abs(x),scientific=FALSE)) +
        labs(x="Age-specific death rate (per 100 000)",y="Age Group",
             title=paste0("Sex × Age Butterfly — ",input$butterfly_year),
             caption="← Female  |  Male →") +
        theme_classic(base_size=11) +
        theme(legend.position="top")
      ggsave(f, p, width=10, height=6, dpi=input$fig_dpi %||% 300, bg="white")
    }
  )

  # ── Age-stratified trend lines ─────────────────────────────────────────
  output$plt_age_trends <- renderPlotly({
    df <- rv$raw_df; req(df)

    has_age <- "age_name" %in% names(df) &&
               any(!grepl("standardized|All ages", df$age_name, ignore.case=TRUE))

    if (!has_age) {
      return(plotly_empty() %>%
        layout(title=list(
          text="Age-disaggregated data not found.\nLoad a file with multiple age-group rows.",
          font=list(size=13,color="#888")),
          paper_bgcolor="white"))
    }

    m_col <- if ("metric_name" %in% names(df)) "metric_name" else NULL
    s_col <- if ("sex_name"    %in% names(df)) "sex_name"    else NULL

    pdat <- df %>%
      { if (!is.null(m_col)) filter(., metric_name == "Rate")   else . } %>%
      { if (!is.null(s_col)) filter(., grepl("^both", sex_name, ignore.case=TRUE)) else . } %>%
      filter(!grepl("standardized|All ages", age_name, ignore.case=TRUE),
             !is.na(val)) %>%
      group_by(age_name, year) %>%
      summarise(val=mean(val,na.rm=TRUE),.groups="drop") %>%
      mutate(age_order=as.numeric(gsub("[^0-9].*","",age_name))) %>%
      arrange(age_order, year)

    if (nrow(pdat) < 2) {
      return(plotly_empty() %>%
        layout(title=list(text="No age-group data for current filters.",
                          font=list(size=13,color="#888")),
               paper_bgcolor="white"))
    }

    n_ages  <- length(unique(pdat$age_name))
    pal_use <- if (n_ages <= 9) RColorBrewer::brewer.pal(max(3,n_ages),"Set1")[1:n_ages] else
                 viridis::viridis(n_ages)

    p <- ggplot(pdat, aes(x=year, y=val, colour=age_name, group=age_name,
                          text=paste0("Age: ",age_name,
                                      "\nYear: ",year,"\nRate: ",round(val,4)))) +
      geom_line(linewidth=0.9) +
      geom_point(size=1.4, alpha=0.7) +
      scale_colour_manual(values=pal_use, name="Age group") +
      scale_x_continuous(breaks=seq(1990,2025,5)) +
      labs(x="Year", y="Rate (per 100 000)",
           title="Age-Stratified Trends — GBD Style",
           subtitle="Each line = one age group (Both sexes, age-specific rate)") +
      theme_minimal(base_size=11) +
      theme(legend.position="right")

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # ── 3D Scatter — Year × Country × Rate ────────────────────────────────
  output$plt_3d_scatter <- renderPlotly({
    df <- filt_data(); req(df)

    # Cap at top 20 countries by mean rate
    top20 <- df %>% filter(!is.na(val)) %>%
      group_by(location_name) %>%
      summarise(avg = mean(val, na.rm=TRUE), .groups="drop") %>%
      slice_max(avg, n=20) %>% pull(location_name)

    pdat <- df %>%
      filter(!is.na(val), location_name %in% top20) %>%
      arrange(location_name, year)

    loc_names <- sort(unique(pdat$location_name))
    pdat$loc_num <- as.numeric(factor(pdat$location_name, levels=loc_names))

    loc_colors <- c("#00468B","#ED0000","#42B540","#0099B4","#925E9F","#AD002A")
    col_map    <- setNames(loc_colors[seq_along(loc_names)], loc_names)

    plot_ly(pdat,
            x = ~year,
            y = ~loc_num,
            z = ~val,
            color       = ~location_name,
            colors      = loc_colors[seq_along(loc_names)],
            type        = "scatter3d",
            mode        = "lines+markers",
            marker      = list(size=3, opacity=0.8),
            line        = list(width=3),
            text        = ~paste0(location_name,
                                  "\nYear: ", year,
                                  "\nRate: ", round(val,4)),
            hoverinfo   = "text") %>%
      layout(
        paper_bgcolor = "white",
        scene = list(
          xaxis = list(title="Year",
                       tickvals=seq(1990,2025,5),
                       backgroundcolor="rgb(245,245,245)",
                       gridcolor="white", showbackground=TRUE),
          yaxis = list(title="Country",
                       tickvals=seq_along(loc_names),
                       ticktext=loc_names,
                       backgroundcolor="rgb(245,245,245)",
                       gridcolor="white", showbackground=TRUE),
          zaxis = list(title="Rate (per 100k)",
                       backgroundcolor="rgb(245,245,245)",
                       gridcolor="white", showbackground=TRUE),
          aspectmode="manual",
          aspectratio=list(x=1.5, y=0.8, z=0.8),
          camera=list(eye=list(x=1.5, y=-1.8, z=0.8))
        ),
        title = list(
          text="3D Rate Surface — Year × Country × Age-Standardized Rate",
          font=list(size=13, color="#2C3E50")
        ),
        legend = list(title=list(text="Country"))
      )
  })

  # Heatmap figure download
  output$dl_fig_heatmap <- downloadHandler(
    filename = function() paste0("Heatmap_", Sys.Date(), ".png"),
    content  = function(f) {
      df <- filt_data(); req(df)
      heat_df <- df %>%
        filter(!is.na(val)) %>%
        group_by(location_name, year) %>%
        summarise(val=mean(val,na.rm=TRUE), .groups="drop")

      p <- ggplot(heat_df, aes(x=factor(year), y=location_name, fill=val)) +
        geom_tile(color="white") +
        scale_fill_viridis_c(name="Rate") +
        labs(x="Year", y=NULL, title="Rate Heatmap") +
        theme_minimal(base_size=10) +
        theme(axis.text.x=element_text(angle=45, hjust=1, size=8))
      ggsave(f, p, width=input$fig_w, height=input$fig_h,
             dpi=input$fig_dpi %||% 300)
    }
  )

  # ==========================================================
  # TAB 8 — FRONTIER ANALYSIS
  # ==========================================================

  observeEvent(input$btn_run_frontier, {
    df <- filt_data(); req(df)
    withProgress(message="Running frontier analysis…", {
      rv$frontier_res <- calc_frontier_analysis(
        df,
        sdi_df       = rv$sdi_df,
        frontier_pct = (input$frontier_pct %||% 10) / 100,
        loess_span   = (input$frontier_span %||% 0.8)
      )
      incProgress(1)
    })
    showNotification("✓ Frontier analysis complete", type="message", duration=3)
  })

  output$plt_frontier_sdi <- renderPlotly({
    res <- rv$frontier_res
    req(res, res$type == "sdi")
    dat  <- res$data
    fline <- res$frontier_line

    p <- ggplot() +
      geom_line(data=fline, aes(x=sdi, y=frontier_val),
               color="#27AE60", linewidth=1.3, linetype="dashed") +
      geom_point(data=dat,
                aes(x=sdi, y=mean_val, color=status,
                    text=paste0(location_name,
                               "\nSDI: ",round(sdi,3),
                               "\nMean Rate: ",round(mean_val,3),
                               "\nEfficiency: ",efficiency_pct,"%")),
                size=3.5, alpha=0.85) +
      scale_color_manual(values=c("Near Frontier"="#27AE60","Above Frontier"="#E74C3C")) +
      annotate("text", x=min(fline$sdi)*1.02, y=max(fline$frontier_val)*0.98,
               label=paste0("Frontier (", input$frontier_pct,"th pctl)"),
               hjust=0, size=3.5, color="#27AE60") +
      labs(x="Socio-Demographic Index (SDI)", y="Mean Age-Standardized Rate",
           color="Status", title="Health Frontier: SDI vs ASR") +
      theme_minimal(base_size=12)

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$plt_frontier_peer <- renderPlotly({
    res <- rv$frontier_res
    req(res, res$type == "peer")
    dat    <- res$data
    yearly <- res$yearly

    # Get one of the locations to show (first alphabetically)
    first_loc <- sort(unique(dat$location_name))[1]
    loc_dat   <- dat %>% filter(location_name == first_loc)

    p <- ggplot() +
      geom_ribbon(data=yearly, aes(x=year, ymin=frontier_val, ymax=worst_val),
                 fill="#EBF5FB", alpha=0.5) +
      geom_ribbon(data=yearly, aes(x=year, ymin=frontier_val, ymax=median_val),
                 fill="#D5F5E3", alpha=0.5) +
      geom_line(data=yearly, aes(x=year, y=frontier_val, color="Best (frontier)"),
               linewidth=1.2) +
      geom_line(data=yearly, aes(x=year, y=median_val, color="Median"),
               linewidth=1.2, linetype="dashed") +
      geom_line(data=yearly, aes(x=year, y=worst_val, color="Worst"),
               linewidth=1.2, linetype="dotted") +
      geom_line(data=dat,
               aes(x=year, y=val, group=location_name, color=location_name),
               linewidth=0.8, alpha=0.85) +
      scale_color_manual(
        values=c("Best (frontier)"="#27AE60","Median"="#F39C12",
                 "Worst"="#E74C3C",
                 setNames(viridis::viridis(length(unique(dat$location_name))),
                          sort(unique(dat$location_name))))
      ) +
      labs(x="Year", y="Rate", color=NULL,
           title=paste0("Peer-Group Frontier — ",input$frontier_pct,"th Percentile")) +
      theme_minimal(base_size=12)

    ggplotly(p) %>% layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$plt_frontier_eff <- renderPlotly({
    res <- rv$frontier_res; req(res)

    if (res$type == "sdi") {
      dat <- res$data %>% arrange(desc(excess_burden))
      p <- ggplot(dat,
                 aes(x=reorder(location_name, excess_burden),
                     y=excess_burden, fill=status,
                     text=paste0(location_name,
                                "\nExcess Burden: ",round(excess_burden,3),
                                "\nEfficiency: ",efficiency_pct,"%"))) +
        geom_col() +
        scale_fill_manual(values=c("Near Frontier"="#27AE60","Above Frontier"="#E74C3C")) +
        coord_flip() +
        labs(x=NULL, y="Excess Burden (above frontier)", fill=NULL,
             title="Excess Burden vs Frontier") +
        theme_minimal(base_size=11)
    } else {
      latest_yr <- max(res$data$year, na.rm=TRUE)
      dat <- res$ranking
      p <- ggplot(dat,
                 aes(x=reorder(location_name, efficiency_pct),
                     y=efficiency_pct, fill=status,
                     text=paste0(location_name,
                                "\nEfficiency: ",efficiency_pct,"%",
                                "\nRank: ",rank," of ",nrow(dat)))) +
        geom_col() +
        geom_hline(yintercept=100, color="gray40", linetype="dashed") +
        scale_fill_manual(values=c("Near Frontier"="#27AE60","Above Frontier"="#E74C3C")) +
        coord_flip() +
        labs(x=NULL, y="Efficiency Score (%)", fill=NULL,
             title=paste0("Efficiency Scores — ", latest_yr)) +
        theme_minimal(base_size=11)
    }
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  output$tbl_frontier <- renderDT({
    res <- rv$frontier_res
    if (is.null(res)) return(datatable(data.frame(Note="Click 'Run Frontier Analysis'.")))

    if (res$type == "sdi") {
      disp <- res$data %>%
        select(Location=location_name, SDI=sdi,
               `Mean Rate`=mean_val, `Frontier Rate`=frontier_val,
               `Excess Burden`=excess_burden, `% Excess`=pct_excess,
               `Efficiency %`=efficiency_pct, Status=status)
    } else {
      disp <- res$ranking %>%
        select(Location=location_name, Rank=rank,
               `Latest Rate`=val, `Frontier Rate`=frontier_val,
               `Excess Burden`=excess_burden, `Efficiency %`=efficiency_pct, Status=status)
    }

    datatable(disp, rownames=FALSE,
             options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("Status",
                  backgroundColor=styleEqual(
                    c("Near Frontier","Above Frontier"),
                    c("#D5F5E3","#FDEDEC")))
  })


  output$dl_frontier_csv <- downloadHandler(
    filename = function() paste0("Frontier_", Sys.Date(), ".csv"),
    content  = function(f) {
      res <- rv$frontier_res
      if (is.null(res)) return(write.csv(data.frame(), f))
      write.csv(res$data, f, row.names=FALSE)
    }
  )

  # ==========================================================
  # TAB 9 — EXPORT & METHODS (ENHANCED)
  # ==========================================================

  output$dl_excel <- downloadHandler(
    filename = function() paste0("GBD_Analysis_", Sys.Date(), ".xlsx"),
    content  = function(f) {
      wb     <- createWorkbook()
      sheets <- input$export_sheets

      if ("eapc" %in% sheets && !is.null(rv$eapc_res)) {
        addWorksheet(wb, "EAPC Results"); writeData(wb, "EAPC Results", rv$eapc_res)
      }
      if ("jp" %in% sheets && !is.null(rv$jp_res)) {
        addWorksheet(wb, "Joinpoint"); writeData(wb, "Joinpoint", rv$jp_res$segments)
      }
      if ("dc" %in% sheets && !is.null(rv$dc_res)) {
        addWorksheet(wb, "Decomposition_Kitagawa")
        writeData(wb, "Decomposition_Kitagawa", rv$dc_res)
        if (!is.null(rv$dc_res_dg)) {
          addWorksheet(wb, "Decomposition_DasGupta")
          writeData(wb, "Decomposition_DasGupta", rv$dc_res_dg)
        }
      }
      if ("fc" %in% sheets && !is.null(rv$fc_res)) {
        addWorksheet(wb, "Forecast"); writeData(wb, "Forecast", rv$fc_res$forecast)
      }
      if ("model_comp" %in% sheets && !is.null(rv$model_comp)) {
        addWorksheet(wb, "Model Comparison"); writeData(wb, "Model Comparison", rv$model_comp)
      }
      if ("desc_stats" %in% sheets && !is.null(rv$desc_res)) {
        addWorksheet(wb, "Descriptive Stats"); writeData(wb, "Descriptive Stats", rv$desc_res)
      }
      if ("frontier" %in% sheets && !is.null(rv$frontier_res)) {
        addWorksheet(wb, "Frontier Analysis")
        writeData(wb, "Frontier Analysis", rv$frontier_res$data)
      }
      if (!is.null(rv$apc_res)) {
        addWorksheet(wb, "APC Model")
        writeData(wb, "APC Model", rv$apc_res$age_df)
      }
      if (!is.null(rv$ineq_res)) {
        addWorksheet(wb, "Health Inequality")
        writeData(wb, "Health Inequality", rv$ineq_res$gap_df)
      }
      if ("raw" %in% sheets && !is.null(filt_data())) {
        addWorksheet(wb, "Filtered Data"); writeData(wb, "Filtered Data", head(filt_data(),5000))
      }
      saveWorkbook(wb, f, overwrite=TRUE)
    }
  )

  # --- Figure downloads ---
  ggsave_safe <- function(f, p, w, h, dpi) {
    ggsave(f, p, width=w, height=h, dpi=dpi, bg="white")
  }

  output$dl_fig_eapc <- downloadHandler(
    filename = function() paste0("EAPC_Forest_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      res <- rv$eapc_res; req(res)
      p <- ggplot(
        res %>% filter(!is.na(eapc)) %>% slice_max(abs(eapc),n=30) %>%
          arrange(eapc) %>% mutate(location_name=factor(location_name,levels=location_name)),
        aes(x=eapc, y=location_name, color=trend)
      ) +
        geom_vline(xintercept=0, linetype="dashed", color="gray50") +
        geom_errorbar(aes(xmin=eapc_lower, xmax=eapc_upper),
                     width=0.35, orientation="y") +
        geom_point(size=2.5) +
        scale_color_manual(values=TREND_COLORS) +
        labs(x="EAPC (%)", y=NULL, color="Trend") +
        theme_minimal(base_size=11)
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_jp <- downloadHandler(
    filename = function() paste0("Joinpoint_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      result <- rv$jp_res; df <- filt_data()
      req(result, df, input$jp_loc)
      loc_df <- df %>% filter(location_name==input$jp_loc, !is.na(val)) %>% arrange(year)
      p <- ggplot(loc_df, aes(x=year, y=val)) +
        geom_point(size=2, color="#2C6FAC", alpha=0.75) +
        { if (!is.null(result$fitted))
            geom_line(data=result$fitted, aes(x=year, y=fitted_val),
                     color="#E74C3C", linewidth=1.3) } +
        { if (length(result$joinpoints)>0)
            geom_vline(xintercept=result$joinpoints, linetype="dashed",
                      color="#8E44AD", linewidth=0.7) } +
        labs(x="Year", y="Age-Standardized Rate",
             title=paste0(input$jp_loc, " — Joinpoint Regression")) +
        theme_minimal(base_size=12)
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_fc <- downloadHandler(
    filename = function() paste0("Forecast_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      result <- rv$fc_res; req(result)
      PAPER_BLUE <- "#2980B9"
      p <- ggplot() +
        # 95% CI shaded area (paper style: blue shaded region)
        geom_ribbon(data=result$forecast,
                   aes(x=year, ymin=lower_95, ymax=upper_95),
                   fill=PAPER_BLUE, alpha=0.20) +
        # Historical observed line (dark)
        geom_line(data=result$historical, aes(x=year, y=rate),
                 color="#2C3E50", linewidth=1.2) +
        geom_point(data=result$historical, aes(x=year, y=rate),
                  color="#2C3E50", size=1.8, alpha=0.8) +
        # Forecast line — single solid blue (paper style)
        geom_line(data=result$forecast, aes(x=year, y=point),
                 color=PAPER_BLUE, linewidth=1.3) +
        # Forecast/history divider
        geom_vline(xintercept=max(result$historical$year)+0.5,
                  linetype="dotted", color="gray60", linewidth=0.6) +
        annotate("text", x=max(result$historical$year)+1,
                 y=max(c(result$historical$rate, result$forecast$upper_95), na.rm=TRUE)*0.97,
                 label="Forecast →", hjust=0, size=3.5, color="gray40") +
        labs(x="Year", y="Age-Standardized Rate (per 100 000)",
             title=paste0(input$fc_loc %||% "", " — ", result$model_label),
             caption="Blue line: predicted value  |  Blue area: 95% CI") +
        theme_classic(base_size=12) +
        theme(
          plot.title   = element_text(face="bold", size=12),
          plot.caption = element_text(color="gray50", size=9),
          axis.line    = element_line(color="black", linewidth=0.4),
          panel.grid.major.y = element_line(color="#f0f0f0", linewidth=0.4),
          panel.grid.major.x = element_blank()
        )
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_heatmap2 <- downloadHandler(
    filename = function() paste0("Heatmap_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      df <- filt_data(); req(df)
      heat_df <- df %>%
        filter(!is.na(val)) %>%
        group_by(location_name, year) %>%
        summarise(val=mean(val,na.rm=TRUE), .groups="drop")
      p <- ggplot(heat_df, aes(x=factor(year), y=location_name, fill=val)) +
        geom_tile(color="white") +
        scale_fill_viridis_c(name="Rate") +
        labs(x="Year", y=NULL, title="Rate Heatmap") +
        theme_minimal(base_size=10) +
        theme(axis.text.x=element_text(angle=45, hjust=1, size=7))
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_frontier2 <- downloadHandler(
    filename = function() paste0("Frontier_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      res <- rv$frontier_res; req(res)
      if (res$type == "sdi") {
        p <- ggplot() +
          geom_line(data=res$frontier_line,
                   aes(x=sdi, y=frontier_val), color="#27AE60", linewidth=1.3, linetype="dashed") +
          geom_point(data=res$data,
                    aes(x=sdi, y=mean_val, color=status, label=location_name),
                    size=3) +
          scale_color_manual(values=c("Near Frontier"="#27AE60","Above Frontier"="#E74C3C")) +
          labs(x="SDI", y="Mean Rate", title="Health Frontier", color="Status") +
          theme_minimal(base_size=12)
      } else {
        p <- ggplot(res$data, aes(x=year, y=val, color=location_name)) +
          geom_line(linewidth=0.9) +
          geom_line(data=res$yearly, aes(x=year, y=frontier_val),
                   color="#27AE60", linewidth=1.5, linetype="dashed", inherit.aes=FALSE) +
          labs(x="Year", y="Rate", color="Location", title="Peer-Group Frontier") +
          theme_minimal(base_size=12)
      }
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_map <- downloadHandler(
    filename = function() paste0("WorldMap_", Sys.Date(), ".", input$fig_format),
    content  = function(f) {
      df <- filt_data(); req(df, world_map_df)
      map_yr <- input$map_year %||% max(df$year, na.rm=TRUE)
      yr_data <- df %>% filter(year == map_yr) %>%
        mutate(region=map_gbd_names(location_name)) %>%
        select(region, val)
      map_df <- left_join(world_map_df, yr_data, by="region", relationship="many-to-many")
      p <- ggplot(map_df, aes(x=long, y=lat, group=group, fill=val)) +
        geom_polygon(color="white", linewidth=0.15) +
        scale_fill_gradientn(
          colours  = c("#FFFFCC","#FFEDA0","#FED976","#FEB24C",
                       "#FD8D3C","#FC4E2A","#E31A1C","#BD0026","#800026"),
          na.value = "#D5D8DC",
          name     = "Rate\n(per 100k)",
          guide    = guide_colorbar(barwidth=0.8, barheight=8,
                                    title.position="top", title.hjust=0.5)
        ) +
        coord_fixed(1.3) +
        theme_void(base_size=11) +
        labs(title=paste0("Age-Standardized Rate — ", map_yr)) +
        theme(
          plot.background  = element_rect(fill="white", color=NA),
          plot.title       = element_text(face="bold", size=13, hjust=0.5),
          legend.position  = "right",
          legend.title     = element_text(size=9),
          plot.margin      = margin(5,5,5,5)
        )
      ggsave_safe(f, p, input$fig_w*1.5, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  # --- Methods text ---
  observeEvent(input$btn_gen_methods, {
    df <- rv$raw_df
    yr_range <- if (!is.null(df) && "year" %in% names(df))
      paste(range(df$year,na.rm=T), collapse="–") else "1990–2021"
    n_locs <- if (!is.null(df) && "location_name" %in% names(df))
      length(unique(df$location_name)) else "N"
    rv$methods_txt <- generate_methods_text(
      year_range    = yr_range,
      n_locations   = n_locs,
      fc_method     = toupper(input$fc_method %||% "ARIMA"),
      max_jp        = input$jp_max %||% 3,
      has_frontier  = isTRUE(input$methods_include_frontier)
    )
  })

  output$methods_ready <- reactive({ !is.null(rv$methods_txt) })
  outputOptions(output, "methods_ready", suspendWhenHidden = FALSE)

  output$txt_methods <- renderText({ rv$methods_txt %||% "(Click Generate Methods)" })

  output$dl_methods <- downloadHandler(
    filename = function() paste0("Methods_", Sys.Date(), ".txt"),
    content  = function(f) writeLines(rv$methods_txt, f)
  )

  # ==========================================================
  # TAB APC — AGE-PERIOD-COHORT MODEL
  # ==========================================================

  output$ui_apc_loc <- renderUI({
    df <- rv$raw_df; req(df)
    locs <- sort(unique(df$location_name))
    def  <- grep("global|world", tolower(locs), value=TRUE)[1] %||% locs[1]
    selectInput("apc_loc", "Location", choices=locs, selected=def)
  })

  observeEvent(input$btn_run_apc, {
    # Use raw_df so all age groups are present (filt_data() removes non-standardized rows)
    df_raw <- rv$raw_df; req(df_raw, input$apc_loc)
    # Still apply measure/metric/sex filters but NOT age filter
    df_apc <- df_raw
    if (!is.null(input$gf_measure) && "measure_name" %in% names(df_apc))
      df_apc <- df_apc %>% filter(measure_name == input$gf_measure)
    if (!is.null(input$gf_metric) && "metric_name" %in% names(df_apc))
      df_apc <- df_apc %>% filter(metric_name == input$gf_metric)
    withProgress(message=paste("APC model:", input$apc_loc, "…"), value=0, {
      rv$apc_res <- calc_apc_model(df_apc, loc=input$apc_loc)
      incProgress(1)
    })
    if (is.null(rv$apc_res))
      showNotification("APC model requires age-disaggregated data (multiple age groups, multiple years).",
                       type="warning", duration=5)
    else
      showNotification(paste0("✓ APC model complete — ", rv$apc_res$n_obs, " observations"),
                       type="message", duration=3)
  })

  output$ibox_apc_obs <- renderInfoBox({
    n <- if (!is.null(rv$apc_res)) rv$apc_res$n_obs else "—"
    infoBox("Observations", n, icon=icon("database"), color="blue", fill=TRUE)
  })
  output$ibox_apc_drift <- renderInfoBox({
    d <- if (!is.null(rv$apc_res) && !is.na(rv$apc_res$net_drift))
      paste0(rv$apc_res$net_drift, "% / yr") else "—"
    col <- if (!is.null(rv$apc_res) && !is.na(rv$apc_res$net_drift) &&
               rv$apc_res$net_drift < 0) "green" else "red"
    infoBox("Net Drift (Period)", d, icon=icon("arrow-trend-up"), color=col, fill=TRUE)
  })
  output$ibox_apc_ages <- renderInfoBox({
    n <- if (!is.null(rv$apc_res)) nrow(rv$apc_res$age_df) else "—"
    infoBox("Age Groups", n, icon=icon("users"), color="purple", fill=TRUE)
  })

  # Age effects plot
  output$plt_apc_age <- renderPlotly({
    res <- rv$apc_res
    if (is.null(res)) {
      return(plotly_empty() %>%
        layout(title=list(text="Click '▶ Run APC Model' to compute.",
                          font=list(size=12,color="#888")), paper_bgcolor="white"))
    }
    adf <- res$age_df
    p <- ggplot(adf, aes(x=age_mid, y=irr,
                         text=paste0("Age: ", age_mid,
                                     "\nIRR: ", round(irr, 3)))) +
      geom_hline(yintercept=1, linetype="dashed", color="gray50", linewidth=0.6) +
      geom_col(aes(fill=irr > 1), width=4, alpha=0.85, show.legend=FALSE) +
      geom_line(color="#2980B9", linewidth=0.8) +
      geom_point(color="#2980B9", size=2) +
      scale_fill_manual(values=c("TRUE"="#E74C3C","FALSE"="#27AE60")) +
      scale_x_continuous(breaks=seq(0,100,10)) +
      labs(x="Age (years)", y="IRR",
           title="Age Effects",
           subtitle=paste0("Location: ", res$location)) +
      theme_minimal(base_size=10) +
      theme(plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Period effects plot
  output$plt_apc_period <- renderPlotly({
    res <- rv$apc_res
    if (is.null(res)) {
      return(plotly_empty() %>%
        layout(title=list(text="Run APC Model first.",
                          font=list(size=12,color="#888")), paper_bgcolor="white"))
    }
    pdf <- res$period_df
    drift_lbl <- if (!is.na(res$net_drift))
      paste0("Net drift: ", res$net_drift, "% / yr") else "Net drift: N/A"

    p <- ggplot(pdf, aes(x=period, y=irr,
                         text=paste0("Year: ", period, "\nIRR: ", round(irr, 3)))) +
      geom_hline(yintercept=1, linetype="dashed", color="gray50", linewidth=0.6) +
      geom_ribbon(aes(ymin=pmin(irr,1), ymax=pmax(irr,1)),
                 fill="#2980B9", alpha=0.15) +
      geom_line(color="#2980B9", linewidth=1.2) +
      geom_point(color="#2980B9", size=2.5) +
      annotate("text", x=min(pdf$period)+1, y=max(pdf$irr)*0.97,
               label=drift_lbl, hjust=0, size=3.2, color="#2C3E50") +
      labs(x="Year (Period)", y="IRR",
           title="Period Effects (Net Drift)",
           subtitle="IRR relative to overall mean rate") +
      theme_minimal(base_size=10) +
      theme(plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Cohort effects plot
  output$plt_apc_cohort <- renderPlotly({
    res <- rv$apc_res
    if (is.null(res)) {
      return(plotly_empty() %>%
        layout(title=list(text="Run APC Model first.",
                          font=list(size=12,color="#888")), paper_bgcolor="white"))
    }
    cdf <- res$cohort_df
    if (is.null(cdf) || nrow(cdf) < 3) {
      return(plotly_empty() %>%
        layout(title=list(text="Insufficient cohort data.",
                          font=list(size=12,color="#888")), paper_bgcolor="white"))
    }
    p <- ggplot(cdf, aes(x=cohort, y=irr,
                         text=paste0("Birth Cohort: ", cohort,
                                     "\nIRR: ", round(irr, 3)))) +
      geom_hline(yintercept=1, linetype="dashed", color="gray50", linewidth=0.6) +
      geom_line(color="#E67E22", linewidth=1.2) +
      geom_point(aes(color=irr > 1), size=2.2, show.legend=FALSE) +
      scale_color_manual(values=c("TRUE"="#E74C3C","FALSE"="#27AE60")) +
      labs(x="Birth Cohort (year)", y="IRR",
           title="Cohort Effects",
           subtitle="IRR by birth cohort relative to mean") +
      theme_minimal(base_size=10) +
      theme(plot.title=element_text(face="bold"))
    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # APC combined table
  output$tbl_apc <- renderDT({
    res <- rv$apc_res
    if (is.null(res))
      return(datatable(data.frame(Note="Click '▶ Run APC Model' to compute results.")))

    age_tbl <- res$age_df %>%
      dplyr::transmute(
        Type = "Age Effect",
        Label = paste0(age_mid, " yrs"),
        `Mean Rate` = round(mean_rate, 4),
        IRR = round(irr, 4)
      )
    period_tbl <- res$period_df %>%
      dplyr::transmute(
        Type = "Period Effect",
        Label = as.character(period),
        `Mean Rate` = round(mean_rate, 4),
        IRR = round(irr, 4)
      )
    cohort_tbl <- if (!is.null(res$cohort_df) && nrow(res$cohort_df) > 0)
      res$cohort_df %>%
        dplyr::transmute(
          Type = "Cohort Effect",
          Label = as.character(cohort),
          `Mean Rate` = round(mean_rate, 4),
          IRR = round(irr, 4)
        )
    else data.frame()

    disp <- bind_rows(age_tbl, period_tbl, cohort_tbl)
    datatable(disp, rownames=FALSE,
             filter="top",
             options=list(pageLength=20, scrollX=TRUE, dom="Bfrtip",
                          buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("IRR",
                  background=styleColorBar(range(disp$IRR,na.rm=TRUE), "#AED6F1"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat",
                  backgroundPosition="center")
  })

  output$dl_apc_csv <- downloadHandler(
    filename = function() paste0("APC_", input$apc_loc, "_", Sys.Date(), ".csv"),
    content  = function(f) {
      res <- rv$apc_res
      if (is.null(res)) return(write.csv(data.frame(), f))
      age_t <- res$age_df %>% dplyr::mutate(type="age")
      per_t <- res$period_df %>% dplyr::mutate(type="period")
      write.csv(bind_rows(age_t, per_t), f, row.names=FALSE)
    }
  )

  # ==========================================================
  # TAB INEQUALITY — HEALTH INEQUALITY ANALYSIS
  # ==========================================================

  observeEvent(input$btn_run_ineq, {
    df <- filt_data(); req(df)
    withProgress(message="Running health inequality analysis…", value=0, {
      rv$ineq_res <- calc_health_inequality(df, sdi_df=rv$sdi_df)
      incProgress(1)
    })
    if (is.null(rv$ineq_res))
      showNotification("Need ≥4 locations across multiple years.",
                       type="warning", duration=4)
    else
      showNotification("✓ Health inequality analysis complete", type="message", duration=3)
  })

  # Quintile time series plot
  output$plt_ineq_quintile <- renderPlotly({
    res <- rv$ineq_res
    if (is.null(res)) {
      return(plotly_empty() %>%
        layout(title=list(text="Click '▶ Run Inequality Analysis' to compute.",
                          font=list(size=12,color="#888")), paper_bgcolor="white"))
    }
    qts <- res$quintile_ts
    group_colors <- c(
      "Low SDI"       = "#27AE60", "Low burden"       = "#27AE60",
      "Low-Mid SDI"   = "#82E0AA", "Low-Mid burden"   = "#82E0AA",
      "Middle SDI"    = "#F39C12", "Middle burden"    = "#F39C12",
      "Mid-High SDI"  = "#F1948A", "Mid-High burden"  = "#F1948A",
      "High SDI"      = "#E74C3C", "High burden"      = "#E74C3C"
    )
    avail <- unique(qts$group_label)
    col_use <- group_colors[avail]
    col_use[is.na(col_use)] <- viridis::viridis(sum(is.na(col_use)))

    p <- ggplot(qts, aes(x=year, y=mean_rate, color=group_label, group=group_label,
                         text=paste0(group_label,
                                     "\nYear: ", year,
                                     "\nMean Rate: ", round(mean_rate, 3),
                                     "\nN locations: ", n_locs))) +
      geom_line(linewidth=1.2) +
      geom_point(size=2.2, alpha=0.8) +
      scale_color_manual(values=col_use) +
      labs(x="Year", y="Mean Age-Standardized Rate",
           color=if(res$type=="sdi") "SDI Quintile" else "Burden Group",
           title="Health Inequality — Rate Trends by Quintile Group",
           subtitle=paste0("Grouping method: ",
                           ifelse(res$type=="sdi","SDI quintile","Burden quintile (baseline year)"))) +
      theme_minimal(base_size=12) +
      theme(legend.position="right", plot.title=element_text(face="bold"))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Absolute gap over time
  output$plt_ineq_gap <- renderPlotly({
    res <- rv$ineq_res
    if (is.null(res)) return(NULL)
    gdf <- res$gap_df

    p <- ggplot(gdf, aes(x=year, y=gap,
                         text=paste0("Year: ", year,
                                     "\nAbsolute Gap: ", round(gap, 3),
                                     "\nHigh: ", round(high_q,3),
                                     "  Low: ", round(low_q,3)))) +
      geom_area(fill="#E74C3C", alpha=0.15) +
      geom_line(color="#E74C3C", linewidth=1.3) +
      geom_point(color="#E74C3C", size=2) +
      labs(x="Year", y="Absolute Gap (High − Low)",
           title="Absolute Gap Over Time",
           subtitle="Difference between highest and lowest quintile group") +
      theme_minimal(base_size=11) +
      theme(plot.title=element_text(face="bold"))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Rate ratio over time
  output$plt_ineq_ratio <- renderPlotly({
    res <- rv$ineq_res
    if (is.null(res)) return(NULL)
    gdf <- res$gap_df

    p <- ggplot(gdf, aes(x=year, y=ratio,
                         text=paste0("Year: ", year,
                                     "\nRate Ratio: ", round(ratio, 2),
                                     "x"))) +
      geom_hline(yintercept=1, linetype="dashed", color="gray50") +
      geom_area(fill="#E67E22", alpha=0.15) +
      geom_line(color="#E67E22", linewidth=1.3) +
      geom_point(color="#E67E22", size=2) +
      labs(x="Year", y="Rate Ratio (High / Low)",
           title="Relative Inequality Over Time",
           subtitle="Rate ratio: highest / lowest quintile group") +
      theme_minimal(base_size=11) +
      theme(plot.title=element_text(face="bold"))

    ggplotly(p, tooltip="text") %>%
      layout(paper_bgcolor="white", plot_bgcolor="white")
  })

  # Inequality summary table
  output$tbl_ineq <- renderDT({
    res <- rv$ineq_res
    if (is.null(res))
      return(datatable(data.frame(Note="Click '▶ Run Inequality Analysis'.")))

    disp <- res$gap_df %>%
      dplyr::mutate(
        across(c(high_q, low_q, gap), ~round(., 4)),
        ratio = round(ratio, 3)
      ) %>%
      dplyr::rename(
        Year       = year,
        `High Quintile` = high_q,
        `Low Quintile`  = low_q,
        `Absolute Gap`  = gap,
        `Rate Ratio`    = ratio
      )
    datatable(disp, rownames=FALSE,
             options=list(pageLength=20, dom="Bfrtip", buttons=c("copy","csv")),
             extensions="Buttons") %>%
      formatStyle("Absolute Gap",
                  background=styleColorBar(range(disp$`Absolute Gap`,na.rm=TRUE), "#FADBD8"),
                  backgroundSize="100% 80%", backgroundRepeat="no-repeat",
                  backgroundPosition="center") %>%
      formatStyle("Rate Ratio",
                  color=styleInterval(1, c("#27AE60","#E74C3C")),
                  fontWeight="bold")
  })

  output$dl_ineq_csv <- downloadHandler(
    filename = function() paste0("HealthInequality_", Sys.Date(), ".csv"),
    content  = function(f) {
      res <- rv$ineq_res
      if (is.null(res)) return(write.csv(data.frame(), f))
      write.csv(res$gap_df, f, row.names=FALSE)
    }
  )

  # ==========================================================
  # EXPORT FIGURE DOWNLOADS — APC & INEQUALITY (paper style)
  # ==========================================================

  output$dl_fig_apc <- downloadHandler(
    filename = function() paste0("APC_Model_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      res <- rv$apc_res; req(res)
      requireNamespace("patchwork", quietly=TRUE)

      p1 <- ggplot(res$age_df, aes(x=age_mid, y=irr)) +
        geom_hline(yintercept=1, linetype="dashed", color="gray50") +
        geom_col(aes(fill=irr>1), width=4, alpha=0.85, show.legend=FALSE) +
        geom_line(color="#2980B9", linewidth=0.8) +
        scale_fill_manual(values=c("TRUE"="#E74C3C","FALSE"="#27AE60")) +
        labs(x="Age (years)", y="IRR", title="(A) Age Effects") +
        theme_classic(base_size=10) +
        theme(plot.title=element_text(face="bold"))

      p2 <- ggplot(res$period_df, aes(x=period, y=irr)) +
        geom_hline(yintercept=1, linetype="dashed", color="gray50") +
        geom_ribbon(aes(ymin=pmin(irr,1), ymax=pmax(irr,1)), fill="#2980B9", alpha=0.15) +
        geom_line(color="#2980B9", linewidth=1.2) +
        geom_point(color="#2980B9", size=2) +
        labs(x="Year (Period)", y="IRR",
             title=paste0("(B) Period Effects\nNet drift: ", res$net_drift, "% / yr")) +
        theme_classic(base_size=10) +
        theme(plot.title=element_text(face="bold"))

      p3 <- ggplot(res$cohort_df, aes(x=cohort, y=irr)) +
        geom_hline(yintercept=1, linetype="dashed", color="gray50") +
        geom_line(color="#E67E22", linewidth=1.2) +
        geom_point(aes(color=irr>1), size=2, show.legend=FALSE) +
        scale_color_manual(values=c("TRUE"="#E74C3C","FALSE"="#27AE60")) +
        labs(x="Birth Cohort", y="IRR", title="(C) Cohort Effects") +
        theme_classic(base_size=10) +
        theme(plot.title=element_text(face="bold"))

      if (requireNamespace("patchwork", quietly=TRUE)) {
        p_all <- patchwork::wrap_plots(p1, p2, p3, ncol=3) +
          patchwork::plot_annotation(
            title    = paste0("Age-Period-Cohort Model — ", res$location),
            subtitle = paste0("N observations: ", res$n_obs),
            theme    = theme(plot.title=element_text(face="bold", size=13))
          )
      } else {
        p_all <- p1  # fallback if patchwork not available
      }
      ggsave_safe(f, p_all, input$fig_w*1.4, input$fig_h,
                  input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_ineq <- downloadHandler(
    filename = function() paste0("HealthInequality_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      res <- rv$ineq_res; req(res)
      qts <- res$quintile_ts
      gdf <- res$gap_df

      group_colors <- c(
        "Low SDI"       = "#27AE60", "Low burden"       = "#27AE60",
        "Low-Mid SDI"   = "#82E0AA", "Low-Mid burden"   = "#82E0AA",
        "Middle SDI"    = "#F39C12", "Middle burden"    = "#F39C12",
        "Mid-High SDI"  = "#F1948A", "Mid-High burden"  = "#F1948A",
        "High SDI"      = "#E74C3C", "High burden"      = "#E74C3C"
      )
      avail    <- unique(qts$group_label)
      col_use  <- group_colors[avail]
      col_use[is.na(col_use)] <- viridis::viridis(sum(is.na(col_use)))

      p1 <- ggplot(qts, aes(x=year, y=mean_rate,
                             color=group_label, group=group_label)) +
        geom_line(linewidth=1.2) +
        geom_point(size=1.8, alpha=0.8) +
        scale_color_manual(values=col_use) +
        labs(x="Year", y="Mean Age-Standardized Rate",
             color=if(res$type=="sdi") "SDI Quintile" else "Burden Group",
             title="(A) Rate Trends by Quintile Group") +
        theme_classic(base_size=11) +
        theme(legend.position="bottom", plot.title=element_text(face="bold"))

      p2 <- ggplot(gdf, aes(x=year, y=gap)) +
        geom_area(fill="#E74C3C", alpha=0.15) +
        geom_line(color="#E74C3C", linewidth=1.2) +
        labs(x="Year", y="Absolute Gap",
             title="(B) Absolute Gap (High − Low)") +
        theme_classic(base_size=11) +
        theme(plot.title=element_text(face="bold"))

      if (requireNamespace("patchwork", quietly=TRUE)) {
        p_all <- patchwork::wrap_plots(p1, p2, ncol=2, widths=c(2,1)) +
          patchwork::plot_annotation(
            title    = "Health Inequality Analysis",
            subtitle = paste0("Grouping: ",
                              ifelse(res$type=="sdi","SDI quintile","Burden quintile")),
            theme    = theme(plot.title=element_text(face="bold", size=13))
          )
      } else {
        p_all <- p1
      }
      ggsave_safe(f, p_all, input$fig_w*1.3, input$fig_h,
                  input$fig_dpi %||% 300)
    }
  )

  # ==========================================================
  # SLOPE INDEX OF INEQUALITY (SII / RII)
  # ==========================================================

  sii_res <- reactive({
    res <- rv$ineq_res
    if (is.null(res)) return(NULL)
    df <- filt_data(); req(df)
    calc_sii(df, sdi_df = rv$sdi_df, weighted = isTRUE(input$sii_weighted))
  })

  output$plt_sii <- renderPlotly({
    sr <- sii_res()
    if (is.null(sr)) {
      return(plotly_empty() %>%
               layout(title = list(text = "Run Inequality Analysis first",
                                   font = list(size = 14))))
    }
    sdf <- sr$sii_df
    p <- ggplot(sdf, aes(x = year, y = sii,
                          text = paste0("Year: ", year,
                                        "\nSII: ", round(sii, 3),
                                        "\nMean Rate: ", round(mean_rate, 2)))) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      geom_ribbon(aes(ymin = pmin(sii, 0), ymax = pmax(sii, 0)),
                  fill = "#2980B9", alpha = 0.15) +
      geom_line(color = "#2980B9", linewidth = 1.3) +
      geom_point(color = "#2980B9", size = 2.5) +
      labs(x = "Year", y = "SII (per 100 000)",
           title = paste0("Slope Index of Inequality (SII) — ", sr$rank_type, " rank"),
           subtitle = paste0("N locations: ", sr$n_locs,
                             "  |  Positive = higher-burden locations have higher rates")) +
      theme_classic(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "white", plot_bgcolor = "white")
  })

  output$plt_rii <- renderPlotly({
    sr <- sii_res()
    if (is.null(sr)) {
      return(plotly_empty() %>%
               layout(title = list(text = "Run Inequality Analysis first",
                                   font = list(size = 14))))
    }
    sdf <- sr$sii_df %>% dplyr::filter(!is.na(rii))
    p <- ggplot(sdf, aes(x = year, y = rii,
                          text = paste0("Year: ", year,
                                        "\nRII: ", round(rii, 3)))) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray80") +
      geom_ribbon(aes(ymin = pmin(rii, 1), ymax = pmax(rii, 1)),
                  fill = "#E67E22", alpha = 0.15) +
      geom_line(color = "#E67E22", linewidth = 1.3) +
      geom_point(color = "#E67E22", size = 2.5) +
      labs(x = "Year", y = "RII",
           title = paste0("Relative Index of Inequality (RII) — ", sr$rank_type, " rank"),
           subtitle = "RII = predicted rate at lowest-SDI rank / highest-SDI rank (Kunst-Mackenbach)  |  Values > 1 = greater burden in lower-SDI populations") +
      theme_classic(base_size = 11) +
      theme(plot.title = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(paper_bgcolor = "white", plot_bgcolor = "white")
  })

  output$dl_fig_sii <- downloadHandler(
    filename = function() paste0("SII_RII_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      sr <- sii_res(); req(sr)
      sdf <- sr$sii_df
      p1 <- ggplot(sdf, aes(x = year, y = sii)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
        geom_ribbon(aes(ymin = pmin(sii,0), ymax = pmax(sii,0)),
                    fill = "#2980B9", alpha = 0.18) +
        geom_line(color = "#2980B9", linewidth = 1.3) +
        geom_point(color = "#2980B9", size = 2) +
        labs(x = "Year", y = "SII", title = "(A) Slope Index of Inequality") +
        theme_classic(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
      p2 <- ggplot(sdf %>% dplyr::filter(!is.na(rii)), aes(x = year, y = rii)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray60") +
        geom_ribbon(aes(ymin = pmin(rii,1), ymax = pmax(rii,1)),
                    fill = "#E67E22", alpha = 0.18) +
        geom_line(color = "#E67E22", linewidth = 1.3) +
        geom_point(color = "#E67E22", size = 2) +
        labs(x = "Year", y = "RII", title = "(B) Relative Index of Inequality") +
        theme_classic(base_size = 11) +
        theme(plot.title = element_text(face = "bold"))
      if (requireNamespace("patchwork", quietly = TRUE)) {
        p_all <- patchwork::wrap_plots(p1, p2, ncol = 2) +
          patchwork::plot_annotation(
            title    = paste0("Health Inequality Indices (", sr$rank_type, " rank)"),
            subtitle = paste0("N locations: ", sr$n_locs),
            theme    = theme(plot.title = element_text(face = "bold", size = 13))
          )
      } else {
        p_all <- p1
      }
      ggsave_safe(f, p_all, input$fig_w * 1.3, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  # ==========================================================
  # TREEMAP — Burden share by location
  # ==========================================================

  output$ui_treemap_year <- renderUI({
    df <- filt_data()
    if (is.null(df) || !"year" %in% names(df)) return(NULL)
    yrs <- sort(unique(df$year))
    selectInput("treemap_year", "Reference Year",
                choices  = yrs,
                selected = max(yrs))
  })

  output$plt_treemap <- renderPlotly({
    df <- filt_data(); req(df)
    yr_sel <- as.integer(input$treemap_year %||% max(df$year, na.rm = TRUE))

    # Prefer Number metric for area; Rate for colour
    num_df <- df %>% dplyr::filter(metric_name == "Number", year == yr_sel)
    if ("age_name" %in% names(num_df)) {
      asr_n <- num_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
      if (nrow(asr_n) > 0) num_df <- asr_n
    }
    rate_df <- df %>% dplyr::filter(metric_name == "Rate", year == yr_sel)
    if ("age_name" %in% names(rate_df)) {
      asr_r <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
      if (nrow(asr_r) > 0) rate_df <- asr_r
    }

    if (nrow(num_df) == 0) {
      # Fallback: use rate as area too
      tm_data <- rate_df %>%
        dplyr::group_by(location_name) %>%
        dplyr::summarise(burden = mean(val, na.rm = TRUE),
                         rate   = mean(val, na.rm = TRUE), .groups = "drop")
      area_label <- "Rate"
    } else {
      tm_data <- num_df %>%
        dplyr::group_by(location_name) %>%
        dplyr::summarise(burden = mean(val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::left_join(
          rate_df %>% dplyr::group_by(location_name) %>%
            dplyr::summarise(rate = mean(val, na.rm = TRUE), .groups = "drop"),
          by = "location_name"
        ) %>%
        dplyr::mutate(rate = dplyr::coalesce(rate, burden))
      area_label <- "Number (Cases)"
    }

    # Limit to top 20 by burden for performance + readability
    tm_data <- tm_data %>%
      dplyr::arrange(dplyr::desc(burden)) %>%
      dplyr::slice_head(n = 20)

    eapc_col <- NULL
    if (!is.null(input$treemap_metric) && input$treemap_metric == "eapc" &&
        is.null(rv$eapc_res)) {
      showNotification("Run EAPC Analysis first to colour by EAPC.", type = "warning", duration = 4)
    }
    if (!is.null(input$treemap_metric) && input$treemap_metric == "eapc" &&
        !is.null(rv$eapc_res)) {
      eapc_col <- rv$eapc_res %>%
        dplyr::select(location_name, eapc) %>%
        dplyr::distinct()
      tm_data <- tm_data %>% dplyr::left_join(eapc_col, by = "location_name")
    }

    color_var <- if (!is.null(input$treemap_metric) && input$treemap_metric == "eapc" &&
                     "eapc" %in% names(tm_data)) "eapc" else "rate"
    color_vals <- tm_data[[color_var]]
    color_label <- if (color_var == "eapc") "EAPC (%/yr)" else "ASR (per 100k)"

    plotly::plot_ly(
      data   = tm_data,
      type   = "treemap",
      labels = ~location_name,
      parents= ~"",
      values = ~burden,
      text   = ~paste0(location_name,
                       "\n", area_label, ": ", round(burden, 1),
                       "\n", color_label, ": ", round(.data[[color_var]], 3)),
      hoverinfo = "text",
      marker = list(
        colors   = color_vals,
        colorscale = list(
          list(0, "#FFFFCC"), list(0.25, "#FEB24C"),
          list(0.5, "#FC4E2A"), list(0.75, "#BD0026"),
          list(1,   "#67000D")
        ),
        showscale = TRUE,
        colorbar  = list(title = color_label)
      )
    ) %>%
      plotly::layout(
        title     = list(text = paste0("Meningitis Burden by Location (", yr_sel, ")"),
                         font = list(size = 14, color = "#1a1f2e")),
        paper_bgcolor = "white"
      )
  })

  output$dl_fig_treemap <- downloadHandler(
    filename = function() paste0("Treemap_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      # Use ggplot2 fallback for static PNG (plotly treemap can't be saved easily)
      df <- filt_data(); req(df)
      yr_sel <- as.integer(input$treemap_year %||% max(df$year, na.rm = TRUE))
      rate_df <- df %>% dplyr::filter(metric_name == "Rate", year == yr_sel)
      if ("age_name" %in% names(rate_df)) {
        asr_r <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
        if (nrow(asr_r) > 0) rate_df <- asr_r
      }
      tm_data <- rate_df %>%
        dplyr::group_by(location_name) %>%
        dplyr::summarise(rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(rate))

      # Simple bar chart as static export fallback for treemap
      p <- ggplot(tm_data, aes(x = reorder(location_name, rate), y = rate, fill = rate)) +
        geom_col(alpha = 0.9) +
        coord_flip() +
        scale_fill_gradientn(
          colours = c("#FFFFCC","#FEB24C","#FC4E2A","#BD0026","#67000D"),
          name    = "ASR"
        ) +
        labs(x = NULL, y = "Age-Standardized Rate (per 100 000)",
             title    = paste0("Burden by Location — ", yr_sel),
             subtitle = "Colour and height = age-standardised rate") +
        theme_classic(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "right")
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_treemap2 <- downloadHandler(
    filename = function() paste0("Treemap_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      df <- filt_data(); req(df)
      yr_sel <- as.integer(input$treemap_year %||% max(df$year, na.rm = TRUE))
      rate_df <- df %>% dplyr::filter(metric_name == "Rate", year == yr_sel)
      if ("age_name" %in% names(rate_df)) {
        asr_r <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
        if (nrow(asr_r) > 0) rate_df <- asr_r
      }
      tm_data <- rate_df %>%
        dplyr::group_by(location_name) %>%
        dplyr::summarise(rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(rate))
      p <- ggplot(tm_data, aes(x = reorder(location_name, rate), y = rate, fill = rate)) +
        geom_col(alpha = 0.9) +
        coord_flip() +
        scale_fill_gradientn(
          colours = c("#FFFFCC","#FEB24C","#FC4E2A","#BD0026","#67000D"),
          name    = "ASR"
        ) +
        labs(x = NULL, y = "Age-Standardized Rate (per 100 000)",
             title    = paste0("Burden by Location — ", yr_sel),
             subtitle = "Colour and height = age-standardised rate") +
        theme_classic(base_size = 12) +
        theme(plot.title = element_text(face = "bold"))
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  # ==========================================================
  # BUMP CHART — Country rank over time
  # ==========================================================

  # Dynamic UI: region list
  output$ui_bump_region <- renderUI({
    df <- rv$raw_df
    regions <- if (!is.null(df) && "region" %in% names(df))
                 sort(unique(df$region)) else character(0)
    selectInput("bump_region_val", "GBD Region", choices = regions,
                selected = regions[1])
  })

  # Dynamic UI: manual country picker
  output$ui_bump_manual <- renderUI({
    df <- rv$raw_df
    locs <- if (!is.null(df)) sort(unique(df$location_name)) else character(0)
    selectizeInput("bump_manual_locs", "Select Countries",
                   choices = locs, selected = locs[seq_len(min(10, length(locs)))],
                   multiple = TRUE,
                   options = list(maxItems = 20, placeholder = "Pick countries (max 20)..."))
  })

  # Helper: build bump data
  bump_data <- reactive({
    df <- filt_data(); req(df)

    # Use ASR if available, else all Rate rows
    rate_df <- df %>% dplyr::filter(metric_name == "Rate")
    if ("age_name" %in% names(rate_df)) {
      asr <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
      if (nrow(asr) > 0) rate_df <- asr
    }
    req(nrow(rate_df) > 0)

    # Year filter
    yr <- input$bump_year_range %||% c(1990, 2023)
    rate_df <- rate_df %>% dplyr::filter(year >= yr[1], year <= yr[2])

    # Summarise to country × year mean rate
    summ <- rate_df %>%
      dplyr::group_by(year, location_name) %>%
      dplyr::summarise(mean_rate = mean(val, na.rm = TRUE), .groups = "drop")

    # Add metadata columns if available
    meta_cols <- c("super_region","region","income_group","sdi_quintile")
    avail <- meta_cols[meta_cols %in% names(rate_df)]
    if (length(avail) > 0) {
      meta <- rate_df %>%
        dplyr::select(location_name, dplyr::all_of(avail)) %>%
        dplyr::distinct(location_name, .keep_all = TRUE)
      summ <- dplyr::left_join(summ, meta, by = "location_name")
    }

    # Apply filter type
    ftype <- input$bump_filter_type %||% "topn"
    if (ftype == "topn") {
      n <- as.integer(input$bump_top_n %||% 15)
      # Rank by top N globally (highest or lowest mean rate across all years)
      if ((input$bump_rank_by %||% "highest") == "highest") {
        top_locs <- summ %>%
          dplyr::group_by(location_name) %>%
          dplyr::summarise(avg = mean(mean_rate, na.rm=TRUE), .groups="drop") %>%
          dplyr::top_n(n, avg) %>% dplyr::pull(location_name)
      } else {
        top_locs <- summ %>%
          dplyr::group_by(location_name) %>%
          dplyr::summarise(avg = mean(mean_rate, na.rm=TRUE), .groups="drop") %>%
          dplyr::top_n(n, -avg) %>% dplyr::pull(location_name)
      }
      summ <- summ %>% dplyr::filter(location_name %in% top_locs)
    } else if (ftype == "sdi" && "sdi_quintile" %in% names(summ)) {
      summ <- summ %>% dplyr::filter(sdi_quintile == (input$bump_sdi_val %||% "High SDI"))
    } else if (ftype == "income" && "income_group" %in% names(summ)) {
      summ <- summ %>% dplyr::filter(income_group == (input$bump_income_val %||% "High"))
    } else if (ftype == "superregion" && "super_region" %in% names(summ)) {
      summ <- summ %>% dplyr::filter(super_region == (input$bump_sr_val %||% "High-income"))
    } else if (ftype == "region" && "region" %in% names(summ)) {
      summ <- summ %>% dplyr::filter(region == (input$bump_region_val %||% ""))
    } else if (ftype == "manual") {
      sel <- input$bump_manual_locs
      if (!is.null(sel) && length(sel) > 0)
        summ <- summ %>% dplyr::filter(location_name %in% sel)
    }

    req(nrow(summ) > 0)

    # Cap at top 20 countries (by mean rate) to avoid overlap — applies to all filter types
    rank_by <- input$bump_rank_by %||% "highest"
    n_cap <- 20L
    locs_ranked <- summ %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(avg = mean(mean_rate, na.rm = TRUE), .groups = "drop")
    if (rank_by == "highest") {
      locs_keep <- locs_ranked %>% dplyr::top_n(n_cap, avg) %>% dplyr::pull(location_name)
    } else {
      locs_keep <- locs_ranked %>% dplyr::top_n(n_cap, -avg) %>% dplyr::pull(location_name)
    }
    summ <- summ %>% dplyr::filter(location_name %in% locs_keep)

    # Compute ranks within each year
    summ %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(rank = if (rank_by == "highest")
                             rank(-mean_rate, ties.method = "min")
                           else
                             rank(mean_rate, ties.method = "min"),
                   n_locs = dplyr::n()) %>%
      dplyr::ungroup()
  })

  output$plt_bump <- renderPlotly({
    rank_df <- bump_data(); req(rank_df)

    locs  <- unique(rank_df$location_name)
    n_loc <- length(locs)
    pal   <- if (n_loc <= 8)  RColorBrewer::brewer.pal(max(3, n_loc), "Set2") else
             if (n_loc <= 12) RColorBrewer::brewer.pal(12, "Paired") else
               viridis::turbo(n_loc)
    loc_colors <- setNames(pal[seq_along(locs)], locs)

    rank_label <- if ((input$bump_rank_by %||% "highest") == "highest")
                    "Rank 1 = Highest Rate" else "Rank 1 = Lowest Rate"

    # Build as native plotly to avoid ggplotly scale_y_reverse double-flip bug
    fig <- plotly::plot_ly()
    for (loc in locs) {
      d <- rank_df %>% dplyr::filter(location_name == loc)
      fig <- fig %>% plotly::add_trace(
        data = d, x = ~year, y = ~rank,
        type = "scatter", mode = "lines+markers",
        name = loc,
        line   = list(color = loc_colors[[loc]], width = 1.8),
        marker = list(color = loc_colors[[loc]], size  = 5),
        text   = ~paste0("<b>", location_name, "</b>",
                         "<br>Year: ", year,
                         "<br>Rank: #", rank,
                         "<br>Rate: ", round(mean_rate, 2)),
        hoverinfo = "text",
        showlegend = TRUE
      )
    }
    fig %>% plotly::layout(
      title = list(
        text = paste0("<b>Country Ranking Over Time — Meningitis Burden</b><br>",
                      "<sup>", rank_label, " | ", min(rank_df$year), "–",
                      max(rank_df$year), "</sup>"),
        font = list(size = 13)
      ),
      xaxis = list(title = "Year",
                   tickvals = seq(min(rank_df$year), max(rank_df$year), by = 5),
                   showgrid = TRUE, gridcolor = "#eeeeee"),
      yaxis = list(title = rank_label,
                   autorange = "reversed",
                   tickvals  = seq_len(n_loc),
                   ticktext  = paste0("#", seq_len(n_loc)),
                   showgrid  = TRUE, gridcolor = "#eeeeee"),
      legend = list(font = list(size = 10), orientation = "v"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      hovermode = "closest"
    )
  })

  output$dl_fig_bump <- downloadHandler(
    filename = function() paste0("BumpChart_Rank_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      rank_df <- bump_data(); req(rank_df)
      locs  <- unique(rank_df$location_name)
      n_loc <- length(locs)
      pal   <- if (n_loc <= 8)  RColorBrewer::brewer.pal(max(3, n_loc), "Set2") else
               if (n_loc <= 12) RColorBrewer::brewer.pal(12, "Paired") else
                 viridis::turbo(n_loc)
      rank_label <- if ((input$bump_rank_by %||% "highest") == "highest")
                      "Rank 1 = Highest Rate" else "Rank 1 = Lowest Rate"
      p <- ggplot(rank_df,
                  aes(x = year, y = rank, group = location_name,
                      color = location_name)) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 3) +
        scale_y_reverse(breaks = seq_len(n_loc), labels = paste0("#", seq_len(n_loc))) +
        scale_color_manual(values = setNames(pal[seq_along(locs)], locs)) +
        scale_x_continuous(breaks = seq(min(rank_df$year), max(rank_df$year), by = 5)) +
        labs(x = "Year", y = rank_label, color = "Country",
             title    = "Country Ranking Over Time — Meningitis Burden",
             subtitle = paste0(rank_label, " | Age-standardized rate | ",
                               min(rank_df$year), "–", max(rank_df$year))) +
        theme_classic(base_size = 12) +
        theme(legend.position = "right", plot.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, hjust = 1))
      ggsave_safe(f, p, input$fig_w * 1.2, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  # ==========================================================
  # ARROW / DUMBBELL DIAGRAM — Burden change baseline → end
  # ==========================================================

  output$ui_arrow_years <- renderUI({
    df <- filt_data()
    if (is.null(df) || !"year" %in% names(df)) return(NULL)
    yrs <- sort(unique(df$year))
    tagList(
      selectInput("arrow_yr_start", "Baseline Year",
                  choices  = yrs,
                  selected = min(yrs)),
      selectInput("arrow_yr_end", "End Year",
                  choices  = yrs,
                  selected = max(yrs))
    )
  })

  arrow_data <- reactive({
    df <- filt_data(); req(df)
    yr_s <- as.integer(input$arrow_yr_start %||% min(df$year, na.rm = TRUE))
    yr_e <- as.integer(input$arrow_yr_end   %||% max(df$year, na.rm = TRUE))
    rate_df <- df %>% dplyr::filter(metric_name == "Rate")
    if ("age_name" %in% names(rate_df)) {
      asr <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
      if (nrow(asr) > 0) rate_df <- asr
    }
    if (nrow(rate_df) == 0) return(NULL)
    start_df <- rate_df %>%
      dplyr::filter(year == yr_s) %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(start_rate = mean(val, na.rm = TRUE), .groups = "drop")
    end_df <- rate_df %>%
      dplyr::filter(year == yr_e) %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(end_rate = mean(val, na.rm = TRUE), .groups = "drop")
    merged <- dplyr::inner_join(start_df, end_df, by = "location_name") %>%
      dplyr::mutate(
        change    = end_rate - start_rate,
        pct_chg   = (end_rate - start_rate) / pmax(start_rate, 1e-9) * 100,
        direction = ifelse(change < 0, "Declining", "Increasing")
      ) %>%
      dplyr::arrange(start_rate)
    list(data = merged, yr_s = yr_s, yr_e = yr_e)
  })

  output$plt_arrow <- renderPlotly({
    ad <- arrow_data()
    if (is.null(ad) || nrow(ad$data) == 0) {
      return(plotly_empty() %>%
               layout(title = list(text = "No Rate data available", font = list(size = 14))))
    }
    d <- ad$data
    # Top 20 declining (biggest absolute decrease) + top 20 increasing
    dec <- d %>% dplyr::filter(direction == "Declining") %>%
      dplyr::arrange(change) %>% dplyr::slice_head(n = 20)
    inc <- d %>% dplyr::filter(direction == "Increasing") %>%
      dplyr::arrange(dplyr::desc(change)) %>% dplyr::slice_head(n = 20)
    d <- dplyr::bind_rows(dec, inc) %>%
      dplyr::arrange(start_rate)
    locs <- d$location_name
    dir_colors <- c("Declining" = "#27AE60", "Increasing" = "#E74C3C")

    fig <- plotly::plot_ly()
    for (i in seq_len(nrow(d))) {
      row <- d[i, ]
      col <- dir_colors[row$direction]
      fig <- fig %>% plotly::add_segments(
        x    = row$start_rate, xend = row$end_rate,
        y    = row$location_name, yend = row$location_name,
        line = list(color = col, width = 2.5),
        showlegend = FALSE
      )
    }
    fig <- fig %>%
      plotly::add_markers(
        data  = d,
        x     = ~start_rate, y = ~location_name,
        name  = paste("Baseline (", ad$yr_s, ")"),
        marker= list(symbol = "circle", size = 11, color = "#2C3E50"),
        text  = ~paste0(location_name, "\nBaseline (", ad$yr_s, "): ", round(start_rate, 2)),
        hoverinfo = "text"
      ) %>%
      plotly::add_markers(
        data  = d,
        x     = ~end_rate, y = ~location_name,
        name  = paste("End (", ad$yr_e, ")"),
        marker = list(symbol = "triangle-right", size = 12,
                      color  = ~direction,
                      colorscale = list(c(0, "#27AE60"), c(1, "#E74C3C"))),
        text  = ~paste0(location_name,
                        "\nEnd (", ad$yr_e, "): ", round(end_rate, 2),
                        "\nChange: ", ifelse(change >= 0, "+", ""), round(change, 2),
                        " (", round(pct_chg, 1), "%)"),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        title      = list(text = paste0("Top 20 Declining + Top 20 Increasing Countries: ",
                                        ad$yr_s, " \u2192 ", ad$yr_e),
                          font = list(size = 13, color = "#1a1f2e")),
        xaxis      = list(title = "Age-Standardized Rate (per 100 000)"),
        yaxis      = list(title = "", tickfont = list(size = 10)),
        legend     = list(orientation = "h", y = -0.12),
        height     = max(400, nrow(d) * 22),
        paper_bgcolor = "white", plot_bgcolor = "white"
      )
    fig
  })

  make_arrow_plot <- function(d, yr_s, yr_e) {
    d <- d %>% dplyr::arrange(start_rate) %>%
      dplyr::mutate(location_name = factor(location_name, levels = location_name))
    ggplot(d) +
      geom_segment(
        aes(x = start_rate, xend = end_rate,
            y = location_name, yend = location_name,
            color = direction),
        linewidth = 1.5,
        arrow = grid::arrow(length = grid::unit(0.25, "cm"), type = "closed")
      ) +
      geom_point(aes(x = start_rate, y = location_name), color = "#2C3E50", size = 3.5) +
      scale_color_manual(values = c("Declining" = "#27AE60", "Increasing" = "#E74C3C"),
                         name   = "Trend") +
      labs(x       = "Age-Standardized Rate (per 100 000)",
           y       = NULL,
           title   = paste0("Burden Change: ", yr_s, " \u2192 ", yr_e),
           subtitle= "Circle = baseline rate; arrowhead = end-year rate") +
      theme_classic(base_size = 12) +
      theme(legend.position   = "bottom",
            plot.title        = element_text(face = "bold"))
  }

  output$dl_fig_arrow <- downloadHandler(
    filename = function() paste0("Arrow_Diagram_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      ad <- arrow_data(); req(ad)
      p  <- make_arrow_plot(ad$data, ad$yr_s, ad$yr_e)
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

  output$dl_fig_arrow2 <- downloadHandler(
    filename = function() paste0("Arrow_Diagram_", Sys.Date(), ".", input$fig_format %||% "png"),
    content  = function(f) {
      ad <- arrow_data(); req(ad)
      p  <- make_arrow_plot(ad$data, ad$yr_s, ad$yr_e)
      ggsave_safe(f, p, input$fig_w, input$fig_h, input$fig_dpi %||% 300)
    }
  )

} # end server
