# ============================================================
# GBD Comprehensive Analysis Suite
# global.R — Packages, helpers, constants
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinycssloaders)
  library(DT)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(tibble)
  library(forecast)
  library(segmented)
  library(maps)
  library(RColorBrewer)
  library(viridis)
  library(broom)
  library(openxlsx)
  library(scales)
  library(tools)
  library(data.table)
})

# Optional packages
has_ggrepel   <- requireNamespace("ggrepel",           quietly = TRUE)
has_sf        <- requireNamespace("sf",                quietly = TRUE)
has_leaflet   <- requireNamespace("leaflet",           quietly = TRUE)
has_rne       <- requireNamespace("rnaturalearth",     quietly = TRUE)
has_quantreg  <- requireNamespace("quantreg",          quietly = TRUE)
has_zip       <- requireNamespace("zip",               quietly = TRUE)
has_patchwork <- requireNamespace("patchwork",         quietly = TRUE)

if (has_ggrepel)   library(ggrepel)
if (has_sf)        library(sf)
if (has_leaflet)   { library(leaflet); library(leaflet.extras) }
if (has_rne)       { library(rnaturalearth); library(rnaturalearthdata) }
if (has_quantreg)  library(quantreg)
if (has_patchwork) library(patchwork)

# ============================================================
# NULL-COALESCING
# ============================================================
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y

# ============================================================
# COLOR CONSTANTS
# ============================================================
TREND_COLORS  <- c("Increasing" = "#E74C3C", "Decreasing" = "#27AE60", "Stable" = "#95A5A6")
PALETTE_EAPC  <- "RdYlGn"
PALETTE_BURDEN <- "YlOrRd"
APP_BLUE       <- "#2C3E50"
APP_ACCENT     <- "#3498DB"

# GBD 21-region color palette (matches real GBD paper colors)
GBD_REGION_COLORS <- c(
  "High-income Asia Pacific"              = "#E41A1C",
  "Central Asia"                          = "#FF7F00",
  "East Asia"                             = "#FFFF33",
  "South Asia"                            = "#4DAF4A",
  "Southeast Asia"                        = "#984EA3",
  "Oceania"                               = "#A65628",
  "Andean Latin America"                  = "#F781BF",
  "Caribbean"                             = "#999999",
  "Central Latin America"                 = "#66C2A5",
  "Tropical Latin America"                = "#FC8D62",
  "Southern Latin America"                = "#8DA0CB",
  "North Africa and Middle East"          = "#E78AC3",
  "High-income North America"             = "#A6D854",
  "Australasia"                           = "#FFD92F",
  "Western Europe"                        = "#E5C494",
  "Central Europe"                        = "#B3B3B3",
  "Eastern Europe"                        = "#1B9E77",
  "Eastern Sub-Saharan Africa"            = "#D95F02",
  "Central Sub-Saharan Africa"            = "#7570B3",
  "Southern Sub-Saharan Africa"           = "#E7298A",
  "Western Sub-Saharan Africa"            = "#66A61E"
)

# GBD publication ggplot2 theme (matches paper style)
theme_gbd <- function(base_size = 12) {
  theme_classic(base_size = base_size) %+replace%
  theme(
    panel.border        = element_rect(fill = NA, color = "black", linewidth = 0.7),
    panel.grid.major    = element_line(color = "#ebebeb", linewidth = 0.4),
    panel.grid.minor    = element_blank(),
    axis.title          = element_text(face = "bold", size = base_size, color = "black"),
    axis.text           = element_text(size = base_size - 1, color = "black"),
    axis.ticks          = element_line(color = "black", linewidth = 0.4),
    plot.title          = element_text(face = "bold", size = base_size + 1, hjust = 0),
    plot.subtitle       = element_text(size = base_size - 1, color = "#444444", hjust = 0),
    legend.background   = element_blank(),
    legend.key          = element_blank(),
    legend.text         = element_text(size = base_size - 2),
    legend.title        = element_text(face = "bold", size = base_size - 1),
    strip.background    = element_blank(),
    strip.text          = element_text(face = "bold", size = base_size),
    plot.background     = element_rect(fill = "white", color = NA),
    panel.background    = element_rect(fill = "white", color = NA)
  )
}

# ============================================================
# GBD → maps::map name matching
# ============================================================
GBD_TO_MAP <- c(
  "United States of America" = "USA",
  "United Kingdom"           = "UK",
  "Russian Federation"       = "Russia",
  "Iran (Islamic Republic of)" = "Iran",
  "Republic of Korea"        = "South Korea",
  "Democratic People's Republic of Korea" = "North Korea",
  "Viet Nam"                 = "Vietnam",
  "Syrian Arab Republic"     = "Syria",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Venezuela (Bolivarian Republic of)" = "Venezuela",
  "United Republic of Tanzania" = "Tanzania",
  "Lao People's Democratic Republic" = "Laos",
  "Cabo Verde"               = "Cape Verde",
  "Czechia"                  = "Czech Republic",
  "North Macedonia"          = "Macedonia",
  "Eswatini"                 = "Swaziland",
  "Timor-Leste"              = "Timor-Leste",
  "Congo"                    = "Republic of Congo",
  "Democratic Republic of the Congo" = "Democratic Republic of the Congo",
  "Republic of Moldova"      = "Moldova",
  "China"                    = "China",
  "Egypt"                    = "Egypt"
)

# ============================================================
# HELPER: PARSE GHDx COLUMNS
# ============================================================
parse_gbd_columns <- function(df) {
  nms <- tolower(names(df))
  names(df) <- nms

  rename_map <- list(
    location_name = c("location", "country", "location_name", "country_name"),
    year          = c("year", "year_id"),
    val           = c("val", "value", "mean", "rate"),
    upper         = c("upper", "upper_ci", "ci_upper"),
    lower         = c("lower", "lower_ci", "ci_lower"),
    measure_name  = c("measure_name", "measure", "indicator"),
    metric_name   = c("metric_name", "metric", "metric_type"),
    sex_name      = c("sex_name", "sex"),
    age_name      = c("age_name", "age", "age_group"),
    cause_name    = c("cause_name", "cause", "disease")
  )

  for (target in names(rename_map)) {
    if (!(target %in% names(df))) {
      for (variant in rename_map[[target]]) {
        if (variant %in% names(df)) {
          names(df)[names(df) == variant] <- target
          break
        }
      }
    }
  }
  df
}

# ============================================================
# HELPER: EAPC — single location
# ============================================================
calc_eapc_single <- function(years, rates) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0)

  if (nrow(df) < 3) {
    return(list(eapc = NA, lower = NA, upper = NA,
                p_value = NA, r2 = NA, trend = "Insufficient data"))
  }

  tryCatch({
    model    <- lm(log(rate) ~ year, data = df)
    beta     <- coef(model)["year"]
    se_beta  <- summary(model)$coefficients["year", "Std. Error"]
    p_val    <- summary(model)$coefficients["year", "Pr(>|t|)"]
    r2       <- summary(model)$r.squared

    eapc  <- 100 * (exp(beta)              - 1)
    lower <- 100 * (exp(beta - 1.96*se_beta) - 1)
    upper <- 100 * (exp(beta + 1.96*se_beta) - 1)

    trend <- case_when(
      eapc > 0 & lower > 0 ~ "Increasing",
      eapc < 0 & upper < 0 ~ "Decreasing",
      TRUE                  ~ "Stable"
    )

    list(eapc    = round(eapc, 3),
         lower   = round(lower, 3),
         upper   = round(upper, 3),
         p_value = round(p_val, 4),
         r2      = round(r2, 3),
         trend   = trend)
  }, error = function(e) {
    list(eapc = NA, lower = NA, upper = NA, p_value = NA, r2 = NA, trend = "Error")
  })
}

# ============================================================
# HELPER: EAPC with Monte-Carlo uncertainty propagation
# Propagates GBD lower/upper uncertainty bounds into the EAPC CI by
# simulating each year's rate from N(val, sd) with sd approximated from the
# 95% UI, refitting the log-linear slope n_sim times. Falls back to the
# analytic single-fit CI when no usable bounds are supplied.
# ============================================================
calc_eapc_mc <- function(years, rates, lower = NULL, upper = NULL, n_sim = 1000) {
  df <- data.frame(year = years, rate = rates,
                   lo = if (is.null(lower)) NA_real_ else lower,
                   hi = if (is.null(upper)) NA_real_ else upper) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  if (nrow(df) < 3)
    return(list(eapc = NA, lower = NA, upper = NA, r2 = NA, trend = "Insufficient data",
                method = "insufficient"))

  pt <- calc_eapc_single(df$year, df$rate)
  if (all(is.na(df$lo)) || all(is.na(df$hi)))
    return(list(eapc = pt$eapc, lower = pt$lower, upper = pt$upper, r2 = pt$r2,
                trend = pt$trend, method = "analytic (no uncertainty columns supplied)"))

  sd_y <- (df$hi - df$lo) / (2 * 1.96)
  sd_y[is.na(sd_y) | sd_y <= 0] <- stats::median(sd_y[sd_y > 0], na.rm = TRUE)
  if (all(is.na(sd_y)))
    return(list(eapc = pt$eapc, lower = pt$lower, upper = pt$upper, r2 = pt$r2,
                trend = pt$trend, method = "analytic (degenerate bounds)"))

  set.seed(42)
  eapcs <- vapply(seq_len(n_sim), function(i) {
    sim <- pmax(rnorm(nrow(df), mean = df$rate, sd = sd_y), 1e-9)
    b <- tryCatch(coef(lm(log(sim) ~ df$year))[2], error = function(e) NA_real_)
    100 * (exp(b) - 1)
  }, numeric(1))
  eapcs <- eapcs[is.finite(eapcs)]
  if (length(eapcs) < 10)
    return(list(eapc = pt$eapc, lower = pt$lower, upper = pt$upper, r2 = pt$r2,
                trend = pt$trend, method = "analytic (MC failed)"))

  lo <- unname(quantile(eapcs, 0.025, na.rm = TRUE))
  hi <- unname(quantile(eapcs, 0.975, na.rm = TRUE))
  trend <- dplyr::case_when(lo > 0 ~ "Increasing", hi < 0 ~ "Decreasing", TRUE ~ "Stable")
  list(eapc = pt$eapc, lower = round(lo, 3), upper = round(hi, 3), r2 = pt$r2,
       trend = trend, method = sprintf("Monte-Carlo (%d draws)", n_sim))
}

# ============================================================
# HELPER: EAPC — whole data frame grouped by location
# ============================================================
calc_eapc_all <- function(df, year_col = "year", rate_col = "val",
                          loc_col = "location_name", mc = FALSE) {
  has_ui <- all(c("lower", "upper") %in% names(df))
  df %>%
    filter(!is.na(.data[[rate_col]]), .data[[rate_col]] > 0) %>%
    group_by(.data[[loc_col]]) %>%
    arrange(.data[[year_col]]) %>%
    group_modify(~ {
      yrs   <- .x[[year_col]]
      rates <- .x[[rate_col]]
      if (isTRUE(mc) && has_ui) {
        res <- calc_eapc_mc(yrs, rates, .x[["lower"]], .x[["upper"]], n_sim = 500)
      } else {
        res <- calc_eapc_single(yrs, rates)
      }
      tibble(
        n_years    = nrow(.x),
        start_year = min(yrs),
        end_year   = max(yrs),
        start_asr  = rates[which.min(yrs)],
        end_asr    = rates[which.max(yrs)],
        eapc       = res$eapc,
        eapc_lower = res$lower,
        eapc_upper = res$upper,
        p_value    = if (!is.null(res$p_value)) res$p_value else NA_real_,
        r_squared  = res$r2,
        fit_flag   = dplyr::case_when(is.na(res$r2) ~ "",
                                      res$r2 < 0.80 ~ "Low R² - consider joinpoint",
                                      TRUE ~ "OK"),
        trend      = res$trend
      )
    }) %>%
    ungroup() %>%
    rename(location_name = !!sym(loc_col)) %>%
    arrange(eapc)
}

# ============================================================
# HELPER: JOINPOINT REGRESSION  (segmented package)
# ============================================================
calc_joinpoint <- function(years, rates, max_jp = 3,
                           method = "bic",       # "bic" | "aic" | "permutation" | "fixed"
                           transform = "log") {  # "log" | "linear" | "sqrt"

  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>%
    arrange(year)
  if (nrow(df) < 6) return(NULL)

  # ── Apply transformation ───────────────────────────────────
  df$y <- switch(transform,
    "log"    = log(df$rate),
    "sqrt"   = sqrt(df$rate),
    "linear" = df$rate
  )
  .bt <- function(x) switch(transform, "log" = exp(x), "sqrt" = x^2, "linear" = x)

  # ── Convert slope → APC (%) — vectorised ──────────────────
  mean_rate <- mean(df$rate, na.rm = TRUE)
  .apc <- function(b) switch(transform,
    "log"    = round(100 * (exp(b) - 1), 3),
    "sqrt"   = round(100 * 2 * b / sqrt(mean_rate), 3),  # delta method: d(rate)/rate ≈ 2b/sqrt(r)
    "linear" = round(100 * b / mean_rate, 3)
  )

  # ── Fit base linear model ──────────────────────────────────
  lm_base <- tryCatch(lm(y ~ year, data = df), error = function(e) NULL)
  if (is.null(lm_base)) return(NULL)

  # ── Helper: extract standard result from segmented fit ────
  .extract <- function(seg_fit) {
    if (is.null(seg_fit) || !inherits(seg_fit, "segmented")) return(NULL)
    tryCatch({
      bps    <- sort(seg_fit$psi[, "Est."])
      n_jp   <- length(bps)
      slopes <- slope(seg_fit)$year          # matrix: Est., St.Err, t value, CI(95%).l, CI(95%).u
      all_pts    <- c(min(df$year), bps, max(df$year))
      seg_widths <- diff(all_pts)
      total_w    <- sum(seg_widths)
      df_resid   <- max(1, nrow(df) - length(coef(seg_fit)))
      p_vals     <- 2 * pt(abs(slopes[, "t value"]), df = df_resid, lower.tail = FALSE)

      est <- slopes[, "Est."]
      # Use pre-computed CI columns when available (most reliable)
      if (all(c("CI(95%).l","CI(95%).u") %in% colnames(slopes))) {
        lo <- slopes[, "CI(95%).l"]
        hi <- slopes[, "CI(95%).u"]
      } else {
        se <- slopes[, "St.Err"]
        lo <- est - 1.96 * se
        hi <- est + 1.96 * se
      }

      segments_df <- data.frame(
        start_year  = round(head(all_pts, -1)),
        end_year    = round(tail(all_pts, -1)),
        apc         = .apc(est),
        apc_lower   = .apc(lo),
        apc_upper   = .apc(hi),
        significant = p_vals < 0.05,
        stringsAsFactors = FALSE
      )
      aapc <- round(sum(segments_df$apc * seg_widths / total_w), 3)
      list(n_joinpoints = n_jp, joinpoints = bps, segments = segments_df,
           fitted    = data.frame(year = df$year, fitted_val = .bt(fitted(seg_fit))),
           aapc      = aapc, method = method, transform = transform)
    }, error = function(e) NULL)
  }

  # ── Helper: 0-joinpoint result ─────────────────────────────
  .no_jp <- function() {
    tryCatch({
      b    <- coef(lm_base)[["year"]]
      se_b <- summary(lm_base)$coefficients["year", "Std. Error"]
      p_b  <- summary(lm_base)$coefficients["year", "Pr(>|t|)"]
      list(n_joinpoints = 0, joinpoints = numeric(0),
           segments = data.frame(
             start_year  = min(df$year), end_year   = max(df$year),
             apc         = .apc(b),
             apc_lower   = .apc(b - 1.96 * se_b),
             apc_upper   = .apc(b + 1.96 * se_b),
             significant = p_b < 0.05,
             stringsAsFactors = FALSE),
           fitted    = data.frame(year = df$year, fitted_val = .bt(fitted(lm_base))),
           aapc      = .apc(b), method = method, transform = transform)
    }, error = function(e) NULL)
  }

  # ── Helper: fit exactly n joinpoints ──────────────────────
  .fit_fixed <- function(n) {
    if (n == 0) return(lm_base)
    tryCatch(
      segmented::segmented(lm_base, seg.Z = ~year, npsi = n,
                           control = segmented::seg.control(
                             n.boot = 30, tol = 1e-5, it.max = 300)),
      error = function(e) NULL
    )
  }

  # ── AIC helper ─────────────────────────────────────────────
  .aic_jp <- function(fit) if (!is.null(fit)) AIC(fit) else Inf

  # ── BIC helper ─────────────────────────────────────────────
  .bic_jp <- function(fit) if (!is.null(fit)) BIC(fit) else Inf

  tryCatch({

    # ── METHOD: Fixed N ────────────────────────────────────────
    if (method == "fixed") {
      if (max_jp == 0) return(.no_jp())
      res <- .extract(.fit_fixed(max_jp))
      if (!is.null(res)) return(res)
      return(.no_jp())
    }

    # ── METHOD: BIC auto-selection ────────────────────────────
    if (method == "bic") {
      seg_fit <- tryCatch(
        selgmented(lm_base, seg.Z = ~year, Kmax = max_jp,
                   type = "bic", bonferroni = TRUE),
        error = function(e) NULL
      )
      res <- .extract(seg_fit)
      if (!is.null(res)) return(res)
      return(.no_jp())
    }

    # ── METHOD: AIC auto-selection ────────────────────────────
    if (method == "aic") {
      best_fit  <- lm_base
      best_aic  <- AIC(lm_base)
      best_n    <- 0
      for (k in seq_len(max_jp)) {
        fit_k <- .fit_fixed(k)
        if (is.null(fit_k)) break
        a <- AIC(fit_k)
        if (a < best_aic - 2) { best_aic <- a; best_fit <- fit_k; best_n <- k }
      }
      if (best_n == 0) return(.no_jp())
      res <- .extract(best_fit)
      if (!is.null(res)) return(res)
      return(.no_jp())
    }

    # ── METHOD: Permutation test (NCI-style) ──────────────────
    if (method == "permutation") {
      n_perm <- 199          # NCI uses 4499; 199 is fast & practical
      alpha  <- 0.05
      sel_n  <- 0

      for (k in seq_len(max_jp)) {
        fit0   <- if (k == 1) lm_base else .fit_fixed(k - 1)
        fit_k  <- .fit_fixed(k)
        if (is.null(fit0) || is.null(fit_k)) break

        rss0 <- sum(residuals(fit0)^2)
        rss1 <- sum(residuals(fit_k)^2)
        obs_stat <- (rss0 - rss1) / rss0

        # Permute residuals from fit0 to build null distribution
        res0    <- residuals(fit0)
        fit0_v  <- fitted(fit0)
        perm_stats <- vapply(seq_len(n_perm), function(i) {
          y_perm <- fit0_v + sample(res0)
          df_p   <- df; df_p$y <- y_perm
          lm_p   <- lm(y ~ year, data = df_p)
          rss_p0 <- sum(residuals(lm_p)^2)
          seg_p  <- tryCatch(
            segmented::segmented(lm_p, seg.Z = ~year, npsi = k,
                                 control = segmented::seg.control(
                                   n.boot = 5, tol = 1e-4, it.max = 50)),
            error = function(e) NULL
          )
          rss_p1 <- if (!is.null(seg_p)) sum(residuals(seg_p)^2) else rss_p0
          (rss_p0 - rss_p1) / rss_p0
        }, numeric(1))

        p_val <- mean(perm_stats >= obs_stat)
        if (p_val <= alpha) sel_n <- k else break
      }

      if (sel_n == 0) return(.no_jp())
      res <- .extract(.fit_fixed(sel_n))
      if (!is.null(res)) return(res)
      return(.no_jp())
    }

    .no_jp()
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: ARIMA FORECASTING
# ============================================================
calc_arima_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>%
    arrange(year)

  if (nrow(df) < 5) return(NULL)

  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    # FIX: lambda = 0 (log transform) enforces strict non-negativity
    # of back-transformed forecasts; guards against negative rate
    # predictions in low-incidence series (e.g., meningitis in GCC).
    fit <- auto.arima(ts_data, seasonal = FALSE,
                      stepwise = FALSE, approximation = FALSE,
                      lambda = 0, biasadj = TRUE)
    fc  <- forecast(fit, h = h, level = c(80, 95))

    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)

    # Hard floor at 0 as a defensive safeguard (non-negativity of rates)
    list(
      model         = fit,
      model_label   = paste0("ARIMA(", paste(arimaorder(fit), collapse = ","), ")"),
      aic           = round(fit$aic,  2),
      aicc          = round(fit$aicc, 2),
      bic           = round(fit$bic,  2),
      historical    = df,
      forecast      = data.frame(
        year      = forecast_years,
        point     = pmax(0, as.numeric(fc$mean)),
        lower_80  = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80  = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95  = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95  = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: ETS FORECASTING
# ============================================================
calc_ets_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>%
    arrange(year)

  if (nrow(df) < 5) return(NULL)

  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    fit <- ets(ts_data, lambda = 0, additive.only = FALSE)  # log transform guards positivity
    fc  <- forecast(fit, h = h, level = c(80, 95))

    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)

    list(
      model       = fit,
      model_label = fit$method,
      aic         = round(fit$aic,  2),
      aicc        = round(fit$aicc, 2),
      bic         = round(fit$bic,  2),
      historical  = df,
      forecast    = data.frame(
        year     = forecast_years,
        point    = pmax(0, as.numeric(fc$mean)),
        lower_80 = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80 = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95 = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95 = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: TBATS FORECASTING
# ============================================================
calc_tbats_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  if (nrow(df) < 5) return(NULL)
  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    fit <- tbats(ts_data)
    fc  <- forecast(fit, h = h, level = c(80, 95))
    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)
    list(
      model       = fit,
      model_label = "TBATS",
      aic = NA_real_, aicc = NA_real_, bic = NA_real_,
      historical  = df,
      forecast    = data.frame(
        year     = forecast_years,
        point    = pmax(0, as.numeric(fc$mean)),
        lower_80 = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80 = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95 = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95 = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: NNETAR (Neural Network AR) FORECASTING
# ============================================================
calc_nnetar_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  if (nrow(df) < 5) return(NULL)
  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    set.seed(42)
    fit <- nnetar(ts_data, lambda = 0)  # log lambda enforces positivity
    set.seed(42)  # reproducible prediction intervals
    fc  <- forecast(fit, h = h, PI = TRUE, npaths = 300, level = c(80, 95))
    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)
    lab <- paste0("NNETAR(", fit$p, ",", fit$size, ")")
    list(
      model       = fit,
      model_label = lab,
      aic = NA_real_, aicc = NA_real_, bic = NA_real_,
      historical  = df,
      forecast    = data.frame(
        year     = forecast_years,
        point    = pmax(0, as.numeric(fc$mean)),
        lower_80 = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80 = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95 = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95 = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: THETA FORECASTING
# ============================================================
calc_theta_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  if (nrow(df) < 5) return(NULL)
  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    fc <- thetaf(ts_data, h = h)
    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)
    list(
      model       = fc,
      model_label = "Theta",
      aic = NA_real_, aicc = NA_real_, bic = NA_real_,
      historical  = df,
      forecast    = data.frame(
        year     = forecast_years,
        point    = pmax(0, as.numeric(fc$mean)),
        lower_80 = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80 = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95 = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95 = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: HOLT LINEAR TREND FORECASTING
# ============================================================
calc_holt_forecast <- function(years, rates, h = 15) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  if (nrow(df) < 5) return(NULL)
  tryCatch({
    ts_data <- ts(df$rate, start = min(df$year), frequency = 1)
    fc <- holt(ts_data, h = h, level = c(80, 95), lambda = 0, biasadj = TRUE)
    forecast_years <- seq(max(df$year) + 1, by = 1, length.out = h)
    list(
      model       = fc$model,
      model_label = "Holt Linear Trend",
      aic  = round(fc$model$aic,  2),
      aicc = round(fc$model$aicc, 2),
      bic  = round(fc$model$bic,  2),
      historical  = df,
      forecast    = data.frame(
        year     = forecast_years,
        point    = pmax(0, as.numeric(fc$mean)),
        lower_80 = pmax(0, as.numeric(fc$lower[, 1])),
        upper_80 = pmax(0, as.numeric(fc$upper[, 1])),
        lower_95 = pmax(0, as.numeric(fc$lower[, 2])),
        upper_95 = pmax(0, as.numeric(fc$upper[, 2]))
      )
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: ENSEMBLE FORECASTING (ARIMA + ETS + NNETAR average)
# ============================================================
calc_ensemble_forecast <- function(years, rates, h = 15) {
  set.seed(42)  # ensures reproducible ensemble (NNETAR stochastic)
  models <- list(
    arima = calc_arima_forecast(years, rates, h),
    ets   = calc_ets_forecast(years, rates, h),
    nnet  = calc_nnetar_forecast(years, rates, h)
  )
  models <- models[!sapply(models, is.null)]
  if (length(models) == 0) return(NULL)

  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)

  # --- Performance-based (inverse-RMSE) weights from a 20% holdout ---
  # Out-of-sample RMSE is computed for each constituent model on a holdout
  # fold; weights are proportional to 1/RMSE. Falls back to equal weights
  # when the series is too short for a holdout or any model fails to fit.
  wts <- setNames(rep(1/length(models), length(models)), names(models))
  weighting <- "equal"
  rmse <- tryCatch({
    n <- nrow(df)
    if (n >= 8) {
      n_test <- max(3, round(n * 0.2)); n_tr <- n - n_test
      tr <- df[seq_len(n_tr), ]; te <- df[(n_tr + 1):n, ]
      ts_tr <- ts(tr$rate, start = min(tr$year), frequency = 1)
      rmse_of <- function(fc) if (is.null(fc) || anyNA(fc)) NA_real_ else sqrt(mean((te$rate - fc)^2))
      vals <- c(
        arima = rmse_of(tryCatch(as.numeric(forecast(auto.arima(ts_tr, seasonal=FALSE, stepwise=TRUE, approximation=TRUE), n_test)$mean), error=function(e) NULL)),
        ets   = rmse_of(tryCatch(as.numeric(forecast(ets(ts_tr), n_test)$mean), error=function(e) NULL)),
        nnet  = rmse_of({ set.seed(42); tryCatch(as.numeric(forecast(nnetar(ts_tr), n_test, PI=FALSE)$mean), error=function(e) NULL) })
      )
      vals[names(models)]
    } else setNames(rep(NA_real_, length(models)), names(models))
  }, error = function(e) setNames(rep(NA_real_, length(models)), names(models)))

  if (all(is.finite(rmse)) && all(rmse > 0)) {
    inv <- 1 / rmse; wts <- inv / sum(inv); weighting <- "inverse-RMSE"
  }

  cols    <- c("point", "lower_80", "upper_80", "lower_95", "upper_95")
  nm_ord  <- names(models)
  fc_mats <- lapply(nm_ord, function(nm) as.matrix(models[[nm]]$forecast[, cols, drop = FALSE]))
  fc_avg  <- matrix(0, nrow = nrow(fc_mats[[1]]), ncol = length(cols))
  for (k in seq_along(nm_ord)) {
    w <- suppressWarnings(as.numeric(wts[[nm_ord[k]]]))
    if (length(w) != 1 || !is.finite(w)) w <- 1 / length(models)
    fc_avg <- fc_avg + fc_mats[[k]] * w
  }
  fc_avg  <- pmax(fc_avg, 0)        # non-negativity floor (matrix first preserves dims)
  colnames(fc_avg) <- cols

  lab <- paste0("Ensemble(", paste(sapply(models, `[[`, "model_label"), collapse="+"), ")")

  list(
    model        = NULL,
    model_label  = lab,
    weighting    = weighting,
    weights      = round(wts, 3),
    weight_label = paste(sprintf("%s %.0f%%", names(wts), 100*as.numeric(wts)), collapse=", "),
    aic = NA_real_, aicc = NA_real_, bic = NA_real_,
    historical  = df,
    forecast    = cbind(data.frame(year = models[[1]]$forecast$year),
                        as.data.frame(fc_avg))
  )
}

# ============================================================
# HELPER: MODEL COMPARISON (holdout cross-validation)
# ============================================================
compare_forecast_models <- function(years, rates) {
  df <- data.frame(year = years, rate = rates) %>%
    filter(!is.na(rate), rate > 0) %>% arrange(year)
  n <- nrow(df)
  if (n < 8) return(NULL)

  n_test  <- max(3, round(n * 0.2))
  n_train <- n - n_test
  train   <- df[seq_len(n_train), ]
  test    <- df[(n_train + 1):n, ]
  h       <- n_test

  safe_acc <- function(actual, pred) {
    if (is.null(pred) || anyNA(pred)) return(c(RMSE=NA_real_, MAE=NA_real_, MAPE=NA_real_))
    err <- actual - pred
    c(RMSE = round(sqrt(mean(err^2, na.rm=TRUE)), 4),
      MAE  = round(mean(abs(err),   na.rm=TRUE), 4),
      MAPE = round(mean(abs(err / actual) * 100, na.rm=TRUE), 2))
  }

  ts_tr <- ts(train$rate, start = min(train$year), frequency = 1)

  run_model <- function(name, fn) {
    res <- tryCatch(fn(ts_tr, h), error = function(e) NULL)
    acc <- safe_acc(test$rate, if (!is.null(res)) res$fc else NULL)
    data.frame(
      Model    = name,
      RMSE     = acc["RMSE"],
      MAE      = acc["MAE"],
      `MAPE(%)` = acc["MAPE"],
      AIC      = if (!is.null(res)) res$aic else NA_real_,
      BIC      = if (!is.null(res)) res$bic else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  results <- list(
    run_model("Auto-ARIMA", function(tr, h) {
      fit <- auto.arima(tr, seasonal=FALSE, stepwise=TRUE, approximation=TRUE)
      list(fc=as.numeric(forecast(fit,h)$mean), aic=round(fit$aic,1), bic=round(fit$bic,1))
    }),
    run_model("ETS", function(tr, h) {
      fit <- ets(tr)
      list(fc=as.numeric(forecast(fit,h)$mean), aic=round(fit$aic,1), bic=round(fit$bic,1))
    }),
    run_model("TBATS", function(tr, h) {
      fit <- tbats(tr)
      list(fc=as.numeric(forecast(fit,h)$mean), aic=NA_real_, bic=NA_real_)
    }),
    run_model("NNETAR", function(tr, h) {
      set.seed(42); fit <- nnetar(tr)
      list(fc=as.numeric(forecast(fit,h,PI=FALSE)$mean), aic=NA_real_, bic=NA_real_)
    }),
    run_model("Theta", function(tr, h) {
      list(fc=as.numeric(thetaf(tr,h)$mean), aic=NA_real_, bic=NA_real_)
    }),
    run_model("Holt", function(tr, h) {
      fc <- holt(tr, h=h)
      list(fc=as.numeric(fc$mean), aic=round(fc$model$aic,1), bic=round(fc$model$bic,1))
    })
  )

  res_df <- bind_rows(results)
  # Flag best model
  best_idx <- which.min(res_df$RMSE)
  res_df$Best <- FALSE
  if (length(best_idx) > 0) res_df$Best[best_idx] <- TRUE
  res_df
}

# ============================================================
# HELPER: FRONTIER ANALYSIS
# Uses SDI-based frontier if SDI data available,
# otherwise peer-group frontier (ranking by year)
# ============================================================
calc_frontier_analysis <- function(df, sdi_df = NULL,
                                   loc_col = "location_name",
                                   year_col = "year",
                                   val_col  = "val",
                                   frontier_pct = 0.10,
                                   loess_span   = 0.8) {

  # --- FIX: Enforce Age-standardized Rate filter upstream ---
  # Frontier analysis must compare like-with-like; mixing crude rates,
  # counts, or age-specific values produces meaningless frontiers.
  if ("metric_name" %in% names(df)) {
    rate_rows <- grepl("^rate$", df$metric_name, ignore.case = TRUE)
    if (any(rate_rows)) df <- df[rate_rows, , drop = FALSE]
  }
  if ("age_name" %in% names(df)) {
    asr_rows <- grepl("age-standard", df$age_name, ignore.case = TRUE)
    if (any(asr_rows)) df <- df[asr_rows, , drop = FALSE]
  }
  if (nrow(df) == 0) return(NULL)

  # --- SDI-based frontier ---
  if (!is.null(sdi_df) && nrow(sdi_df) >= 4) {
    obs_avg <- df %>%
      filter(!is.na(.data[[val_col]]), .data[[val_col]] > 0) %>%
      group_by(.data[[loc_col]]) %>%
      summarise(mean_val = mean(.data[[val_col]], na.rm=TRUE), .groups="drop")
    names(obs_avg)[1] <- "location_name"

    # FIX: Use most recent SDI per country (cross-sectional "current frontier")
    # rather than mean SDI across decades. SDI changes substantially over
    # 1990-2023 (e.g., Saudi Arabia: 0.55 -> 0.78), so mean is not meaningful.
    if ("year" %in% names(sdi_df)) {
      sdi_avg <- sdi_df %>%
        filter(!is.na(sdi)) %>%
        group_by(location_name) %>%
        slice_max(year, n = 1, with_ties = FALSE) %>%
        ungroup() %>%
        select(location_name, sdi)
    } else {
      sdi_avg <- sdi_df %>%
        group_by(location_name) %>%
        summarise(sdi = mean(sdi, na.rm=TRUE), .groups="drop")
    }

    merged <- left_join(obs_avg, sdi_avg, by="location_name") %>%
      filter(!is.na(sdi), !is.na(mean_val))

    if (nrow(merged) < 4) return(NULL)

    lo      <- loess(mean_val ~ sdi, data=merged, span=loess_span)
    pred    <- predict(lo, newdata=merged)
    resid_q <- quantile(merged$mean_val - pred, probs=frontier_pct, na.rm=TRUE)

    merged <- merged %>%
      mutate(
        frontier_val   = pmax(pred + resid_q, 0),
        excess_burden  = pmax(mean_val - frontier_val, 0),
        pct_excess     = round(100 * excess_burden / pmax(mean_val, 1e-9), 1),
        efficiency_pct = round(100 * frontier_val  / pmax(mean_val, 1e-9), 1),
        status         = ifelse(mean_val <= frontier_val * 1.10, "Near Frontier", "Above Frontier")
      ) %>%
      arrange(mean_val)

    sdi_seq       <- seq(min(merged$sdi, na.rm=TRUE), max(merged$sdi, na.rm=TRUE), length.out=100)
    lo_pred_line  <- predict(lo, newdata=data.frame(sdi=sdi_seq))
    frontier_line <- data.frame(sdi=sdi_seq, frontier_val=lo_pred_line + resid_q)

    return(list(type="sdi", data=merged, frontier_line=frontier_line))
  }

  # --- Peer-group frontier (no SDI) ---
  loc_year <- df %>%
    filter(!is.na(.data[[val_col]]), .data[[val_col]] > 0) %>%
    rename(location_name = !!sym(loc_col),
           year           = !!sym(year_col),
           val            = !!sym(val_col)) %>%
    select(location_name, year, val)

  n_locs <- length(unique(loc_year$location_name))
  eff_pct <- if (n_locs <= 3) 0 else frontier_pct

  yearly <- loc_year %>%
    group_by(year) %>%
    summarise(
      frontier_val = quantile(val, probs=eff_pct, na.rm=TRUE),
      median_val   = median(val, na.rm=TRUE),
      worst_val    = quantile(val, probs=1-eff_pct, na.rm=TRUE),
      .groups="drop"
    )

  merged <- left_join(loc_year, yearly, by="year") %>%
    group_by(year) %>%
    mutate(
      excess_burden  = pmax(val - frontier_val, 0),
      efficiency_pct = round(100 * frontier_val / pmax(val, 1e-9), 1),
      rank_in_year   = rank(val, ties.method="average")
    ) %>%
    ungroup()

  latest_yr <- max(merged$year, na.rm=TRUE)
  ranking <- merged %>%
    filter(year == latest_yr) %>%
    arrange(val) %>%
    mutate(rank = row_number(),
           status = ifelse(val <= frontier_val * 1.10, "Near Frontier", "Above Frontier"))

  list(type="peer", data=merged, yearly=yearly, ranking=ranking)
}

# ============================================================
# HELPER: DESCRIPTIVE STATISTICS (Table 1)
# ============================================================
calc_descriptive_stats <- function(df, loc_col = "location_name",
                                   year_col = "year", val_col = "val") {
  df %>%
    filter(!is.na(.data[[val_col]]), .data[[val_col]] > 0) %>%
    group_by(.data[[loc_col]]) %>%
    arrange(.data[[year_col]]) %>%
    summarise(
      N              = n(),
      Period         = paste(range(.data[[year_col]]), collapse="–"),
      Mean           = round(mean(.data[[val_col]], na.rm=TRUE), 3),
      SD             = round(sd(.data[[val_col]], na.rm=TRUE), 3),
      Median         = round(median(.data[[val_col]], na.rm=TRUE), 3),
      `25th pctl`    = round(quantile(.data[[val_col]], 0.25, na.rm=TRUE), 3),
      `75th pctl`    = round(quantile(.data[[val_col]], 0.75, na.rm=TRUE), 3),
      Min            = round(min(.data[[val_col]], na.rm=TRUE), 3),
      Max            = round(max(.data[[val_col]], na.rm=TRUE), 3),
      `First Value`  = round(first(.data[[val_col]]), 3),
      `Last Value`   = round(last(.data[[val_col]]), 3),
      `% Change`     = round(100 * (last(.data[[val_col]]) - first(.data[[val_col]])) /
                               pmax(first(.data[[val_col]]), 1e-9), 1),
      .groups = "drop"
    ) %>%
    rename(Location = !!sym(loc_col)) %>%
    arrange(Mean)
}

# ============================================================
# HELPER: DECOMPOSITION — Kitagawa 2-component
# ------------------------------------------------------------
# NOTE (methodological correction, 2026-04):
# Previous versions labeled this as "Das Gupta 3-component"
# decomposition, but true Das Gupta decomposition requires
# age-stratified populations and age-specific rates to
# separate AGING from epidemiological change. Without age
# structure data, only 2 components are identifiable:
#   (1) population growth   = (rate1) × (pop2 - pop1)
#   (2) epidemiological     = (rate2 - rate1) × pop2
# Earlier versions computed a third "aging" term as the
# residual (total - pop_growth - epid_change), which is
# mathematically ZERO by construction when both other terms
# are computed from the same data — any non-zero value was
# a rounding artifact, not a real aging effect.
# This function now correctly implements 2-component Kitagawa
# decomposition and labels it as such. A true 3-component
# Das Gupta decomposition is provided by calc_decomp_dasgupta()
# below, which requires age-stratified input.
# ============================================================
calc_decomp_simple <- function(df, loc_col = "location_name",
                                year_col = "year", val_col = "val",
                                start_year, end_year) {
  locs <- unique(df[[loc_col]])

  # Check if Number + Rate both present → 2-component Kitagawa
  has_number <- "metric_name" %in% names(df) &&
                any(grepl("^number$", df$metric_name, ignore.case=TRUE))
  has_rate   <- "metric_name" %in% names(df) &&
                any(grepl("^rate$",   df$metric_name, ignore.case=TRUE))

  if (has_number && has_rate) {
    # Pull rate and number per location × year
    rate_df <- df %>%
      dplyr::filter(grepl("^rate$", metric_name, ignore.case=TRUE)) %>%
      dplyr::group_by(.data[[loc_col]], .data[[year_col]]) %>%
      dplyr::summarise(rate = mean(.data[[val_col]], na.rm=TRUE), .groups="drop")

    num_df <- df %>%
      dplyr::filter(grepl("^number$", metric_name, ignore.case=TRUE)) %>%
      dplyr::group_by(.data[[loc_col]], .data[[year_col]]) %>%
      dplyr::summarise(cases = mean(.data[[val_col]], na.rm=TRUE), .groups="drop")

    both <- dplyr::inner_join(rate_df, num_df, by=c(loc_col, year_col)) %>%
      dplyr::mutate(pop = cases / pmax(rate / 1e5, 1e-12))

    results <- lapply(locs, function(loc) {
      sub <- both %>% dplyr::filter(.data[[loc_col]] == loc)
      r1  <- sub %>% dplyr::filter(.data[[year_col]] == start_year)
      r2  <- sub %>% dplyr::filter(.data[[year_col]] == end_year)
      if (nrow(r1)==0 || nrow(r2)==0) return(NULL)

      cases1 <- r1$cases[1]; cases2 <- r2$cases[1]
      rate1  <- r1$rate[1];  rate2  <- r2$rate[1]
      pop1   <- r1$pop[1];   pop2   <- r2$pop[1]
      total  <- cases2 - cases1

      # Kitagawa 2-component decomposition (mid-point symmetric form)
      # Population growth effect: uses mean rate across the two endpoints
      # Epidemiological effect:   uses mean population across endpoints
      # This symmetric form ensures: pop_growth + epid_change = total
      mean_rate <- (rate1 + rate2) / 2 / 1e5
      mean_pop  <- (pop1  + pop2)  / 2
      pop_growth  <- mean_rate * (pop2 - pop1)
      epid_change <- ((rate2 - rate1) / 1e5) * mean_pop

      pct_pop <- round(100 * pop_growth  / pmax(abs(total), 1e-9), 1)
      pct_epi <- round(100 * epid_change / pmax(abs(total), 1e-9), 1)

      data.frame(
        location       = loc,
        start_year     = start_year,
        end_year       = end_year,
        start_val      = round(cases1, 1),
        end_val        = round(cases2, 1),
        abs_change     = round(total, 1),
        pct_change     = round(100 * total / pmax(cases1, 1e-9), 2),
        pop_growth     = round(pop_growth, 1),
        epid_change    = round(epid_change, 1),
        pct_pop_growth = pct_pop,
        pct_epid       = pct_epi,
        direction      = ifelse(total > 0, "Increasing", "Decreasing"),
        decomp_method  = "Kitagawa 2-component",
        has_components = TRUE,
        stringsAsFactors = FALSE
      )
    })
    return(bind_rows(results))
  }

  # Fallback: rate-only (no decomposition possible, just change summary)
  results <- lapply(locs, function(loc) {
    sub <- df %>% dplyr::filter(.data[[loc_col]] == loc)
    v1  <- sub %>% dplyr::filter(.data[[year_col]] == start_year) %>%
           dplyr::pull(.data[[val_col]])
    v2  <- sub %>% dplyr::filter(.data[[year_col]] == end_year)   %>%
           dplyr::pull(.data[[val_col]])

    if (length(v1) == 0 || length(v2) == 0) return(NULL)
    v1 <- mean(v1); v2 <- mean(v2)

    abs_change  <- v2 - v1
    pct_change  <- 100 * abs_change / pmax(v1, 1e-9)

    data.frame(
      location       = loc,
      start_year     = start_year,
      end_year       = end_year,
      start_val      = round(v1, 4),
      end_val        = round(v2, 4),
      abs_change     = round(abs_change, 4),
      pct_change     = round(pct_change, 2),
      pop_growth     = NA_real_,
      epid_change    = NA_real_,
      pct_pop_growth = NA_real_,
      pct_epid       = NA_real_,
      direction      = ifelse(pct_change > 0, "Increasing", "Decreasing"),
      decomp_method  = "Rate-change only (no decomposition)",
      has_components = FALSE,
      stringsAsFactors = FALSE
    )
  })

  bind_rows(results)
}

# ============================================================
# HELPER: DECOMPOSITION — Das Gupta 3-component
# ------------------------------------------------------------
# True 3-component decomposition (growth, aging, epidemiological)
# requires AGE-STRATIFIED rates and populations.
# Returns NULL if age-stratified data unavailable.
# ============================================================
calc_decomp_dasgupta <- function(df, loc_col = "location_name",
                                 year_col = "year", val_col = "val",
                                 start_year, end_year) {
  # Requires: age_name stratification with both Number + Rate at each age
  if (!"age_name" %in% names(df) || !"metric_name" %in% names(df))
    return(NULL)

  age_rows <- !grepl("age-standard|all ages", df$age_name, ignore.case = TRUE)
  df_age   <- df[age_rows, , drop = FALSE]

  has_number <- any(grepl("^number$", df_age$metric_name, ignore.case = TRUE))
  has_rate   <- any(grepl("^rate$",   df_age$metric_name, ignore.case = TRUE))
  if (!(has_number && has_rate)) return(NULL)

  # Build age-specific rate and population by year × location × age
  rate_a <- df_age %>%
    dplyr::filter(grepl("^rate$", metric_name, ignore.case = TRUE)) %>%
    dplyr::group_by(.data[[loc_col]], .data[[year_col]], age_name) %>%
    dplyr::summarise(rate = mean(.data[[val_col]], na.rm = TRUE), .groups = "drop")

  num_a  <- df_age %>%
    dplyr::filter(grepl("^number$", metric_name, ignore.case = TRUE)) %>%
    dplyr::group_by(.data[[loc_col]], .data[[year_col]], age_name) %>%
    dplyr::summarise(cases = mean(.data[[val_col]], na.rm = TRUE), .groups = "drop")

  age_df <- dplyr::inner_join(rate_a, num_a,
                              by = c(loc_col, year_col, "age_name")) %>%
    dplyr::mutate(pop_a = cases / pmax(rate / 1e5, 1e-12))

  locs <- unique(age_df[[loc_col]])
  results <- lapply(locs, function(loc) {
    y1 <- age_df %>% dplyr::filter(.data[[loc_col]] == loc,
                                   .data[[year_col]] == start_year)
    y2 <- age_df %>% dplyr::filter(.data[[loc_col]] == loc,
                                   .data[[year_col]] == end_year)
    if (nrow(y1) == 0 || nrow(y2) == 0) return(NULL)
    j <- dplyr::inner_join(y1 %>% dplyr::select(age_name, rate1 = rate,
                                                cases1 = cases, pop1 = pop_a),
                           y2 %>% dplyr::select(age_name, rate2 = rate,
                                                cases2 = cases, pop2 = pop_a),
                           by = "age_name")
    if (nrow(j) < 2) return(NULL)

    # Das Gupta 3-way: P (total pop growth), A (age structure), E (age-specific rate)
    P1 <- sum(j$pop1); P2 <- sum(j$pop2)
    s1 <- j$pop1 / P1; s2 <- j$pop2 / P2     # age-specific shares
    r1 <- j$rate1 / 1e5; r2 <- j$rate2 / 1e5  # age-specific rates
    C1 <- sum(j$cases1); C2 <- sum(j$cases2)

    # Standard Das Gupta effects (symmetric averaging across factor orders)
    # Effect_P: holds age-share and rate at average, varies total pop
    eff_P <- (P2 - P1) * mean((s1 + s2)/2 * (r1 + r2)/2)
    # Effect_A: holds total pop and rate at average, varies age-share
    eff_A <- (P1 + P2) / 2 * sum((s2 - s1) * (r1 + r2)/2)
    # Effect_E: holds total pop and age-share at average, varies rate
    eff_E <- (P1 + P2) / 2 * sum((s1 + s2)/2 * (r2 - r1))

    total <- C2 - C1
    # Re-scale so components sum exactly to total (correct small rounding)
    sum_components <- eff_P + eff_A + eff_E
    if (abs(sum_components) > 1e-9) {
      scale <- total / sum_components
      eff_P <- eff_P * scale; eff_A <- eff_A * scale; eff_E <- eff_E * scale
    }

    data.frame(
      location       = loc,
      start_year     = start_year,
      end_year       = end_year,
      start_val      = round(C1, 1),
      end_val        = round(C2, 1),
      abs_change     = round(total, 1),
      pct_change     = round(100 * total / pmax(C1, 1e-9), 2),
      pop_growth     = round(eff_P, 1),
      aging          = round(eff_A, 1),
      epid_change    = round(eff_E, 1),
      pct_pop_growth = round(100 * eff_P / pmax(abs(total), 1e-9), 1),
      pct_aging      = round(100 * eff_A / pmax(abs(total), 1e-9), 1),
      pct_epid       = round(100 * eff_E / pmax(abs(total), 1e-9), 1),
      direction      = ifelse(total > 0, "Increasing", "Decreasing"),
      decomp_method  = "Das Gupta 3-component",
      has_components = TRUE,
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(results)
}

# ============================================================
# HELPER: WORLD MAP DATA (base)
# ============================================================
world_map_df <- tryCatch({
  map_data("world")
}, error = function(e) NULL)

# ============================================================
# HELPER: APPLY GBD NAME MAPPING
# ============================================================
map_gbd_names <- function(gbd_names) {
  mapped <- GBD_TO_MAP[gbd_names]
  ifelse(is.na(mapped), gbd_names, mapped)
}

# ============================================================
# HELPER: GENERATE METHODS TEXT
# ============================================================
generate_methods_text <- function(year_range, n_locations,
                                  fc_method = "ARIMA", max_jp = 3,
                                  has_frontier = FALSE) {
  paste0(
    "Statistical Methods\n",
    paste(rep("=", 60), collapse = ""), "\n\n",
    "Data Source\n",
    "Data on age-standardized rates, incidence, mortality, and disability-adjusted life years ",
    "(DALYs) were extracted from the Global Burden of Disease (GBD) database via the Global ",
    "Health Data Exchange (GHDx) for ", n_locations, " locations covering ", year_range, ".\n\n",

    "Trend Analysis — EAPC\n",
    "The estimated annual percentage change (EAPC) was used to quantify temporal trends in ",
    "age-standardized rates (ASR). A linear regression model was fitted: ln(ASR) = α + β × year + ε. ",
    "EAPC = 100 × (e^β − 1), with 95% CIs from: 100 × (e^(β ± 1.96 × SE_β) − 1). ",
    "Significant increasing trend: EAPC > 0 and 95% CI lower bound > 0; significant decreasing: ",
    "EAPC < 0 and 95% CI upper bound < 0.\n\n",

    "Joinpoint Regression\n",
    "Joinpoint regression was performed using the segmented R package (Muggeo 2003), with ",
    "automated breakpoint selection (up to ", max_jp, " joinpoints) via the Bayesian Information ",
    "Criterion (BIC) with Bonferroni correction. For each segment, the annual percentage change ",
    "(APC) and its 95% CI were derived from the log-linear slope. The weighted average annual ",
    "percentage change (AAPC) was calculated as a duration-weighted mean of segment APCs.\n\n",

    "Decomposition Analysis\n",
    "Changes in absolute case counts between endpoint years were decomposed using Kitagawa's ",
    "2-component decomposition (population growth and epidemiological change), where the ",
    "population-growth component was computed as the product of the across-period mean rate and ",
    "the change in total population, and the epidemiological-change component as the product of ",
    "the change in rate and the across-period mean population (symmetric form, guaranteeing ",
    "components sum exactly to the observed change). When age-stratified counts and rates were ",
    "available, full 3-component Das Gupta decomposition (population growth, age-structure change, ",
    "and age-specific epidemiological change) was additionally performed.\n\n",

    "Forecasting\n",
    "Time-series forecasting was conducted on age-standardized rates using multiple models: ",
    "Auto-ARIMA (forecast package, Hyndman & Khandakar 2008), ETS (Exponential Smoothing State ",
    "Space), TBATS (Trigonometric seasonality, Box-Cox transformation, ARMA errors, Trend and ",
    "Seasonal components), NNETAR (Neural Network Autoregression), Theta method, and Holt's ",
    "Linear Trend. A log-transform (Box-Cox lambda = 0) with bias adjustment was applied to ",
    "enforce non-negativity of rate forecasts. An Ensemble forecast averaging ARIMA + ETS + NNETAR ",
    "was also computed (with fixed random seed for reproducibility). Model selection was guided ",
    "by holdout cross-validation (20% test set) with RMSE, MAE, and MAPE as accuracy metrics. The ",
    "primary model used was ", fc_method, ". All forecasts include 80% and 95% prediction intervals.\n\n",

    if (has_frontier) paste0(
      "Frontier Analysis\n",
      "Health frontier analysis identified the theoretically achievable performance benchmark ",
      "(Global Burden of Disease Collaborative Network, 2017). A loess curve (span = 0.8) was ",
      "fitted to age-standardized rates against the most recent country-level Socio-Demographic ",
      "Index (SDI); the frontier line was defined as the fitted curve shifted downward by the ",
      "10th percentile of residuals, representing an empirical lower envelope of observed burden ",
      "at each SDI level. When SDI data were unavailable, a peer-group frontier was estimated ",
      "from the best-performing locations by year. Excess burden was calculated as the difference ",
      "between each country's observed rate and its frontier value. Efficiency scores ",
      "(frontier rate / observed rate × 100) quantify proximity to the frontier.\n\n"
    ) else "",

    "Descriptive Statistics\n",
    "Summary statistics including mean, standard deviation, median, interquartile range, ",
    "and overall percentage change were computed for each location over the full study period.\n\n",

    "Software\n",
    "All analyses were performed in R (≥ 4.3.0) using: forecast, segmented, ggplot2, plotly, ",
    "shinydashboard, DT, and openxlsx.\n",
    "GBD Comprehensive Analysis Suite (Shiny) was used for all analyses.\n\n",
    "NOTE: This methods text is auto-generated and must be reviewed and adapted to your ",
    "specific analysis, data, and assumptions before use in any publication.\n"
  )
}

# ============================================================
# HELPER: DESCRIPTIVE AGE-PERIOD-COHORT (APC) VISUALISATION
# ------------------------------------------------------------
# NOTE: This function computes DESCRIPTIVE age, period, and
# cohort profiles — NOT a formal identifiable APC model
# (e.g., Holford's intrinsic estimator or Carstensen's framework).
# Because of the exact linear dependence period = age + cohort,
# a fully identified APC decomposition requires constraint-based
# estimation; we report only net drift (period slope) as an
# identified quantity. Age and cohort "effects" shown here are
# marginal ratio-to-mean values useful for descriptive
# visualization, and should be interpreted accordingly.
# ============================================================
calc_apc_model <- function(df, loc = NULL) {
  if (!is.null(loc) && "location_name" %in% names(df))
    df <- dplyr::filter(df, location_name == loc)

  # Prefer Both sexes
  if ("sex_name" %in% names(df) &&
      any(grepl("^both", df$sex_name, ignore.case = TRUE)))
    df <- dplyr::filter(df, grepl("^both", sex_name, ignore.case = TRUE))

  # Prefer Rate metric
  if ("metric_name" %in% names(df) &&
      any(grepl("^rate$", df$metric_name, ignore.case = TRUE)))
    df <- dplyr::filter(df, grepl("^rate$", metric_name, ignore.case = TRUE))

  # Keep only age-specific rows (requires age_name column)
  if (!"age_name" %in% names(df)) return(NULL)

  pdat <- df %>%
    dplyr::filter(!is.na(val), val > 0,
                  !grepl("standardized|All ages", age_name, ignore.case = TRUE)) %>%
    dplyr::mutate(
      age_mid = suppressWarnings(as.numeric(gsub("[^0-9].*", "", age_name))),
      period  = as.integer(year)
    ) %>%
    dplyr::filter(!is.na(age_mid), age_mid >= 0, age_mid <= 100)

  if (nrow(pdat) < 12 ||
      length(unique(pdat$age_mid)) < 3 ||
      length(unique(pdat$period))  < 3)
    return(NULL)

  tryCatch({
    # ── Age effects ─────────────────────────────────────────────────
    age_df <- pdat %>%
      dplyr::group_by(age_mid) %>%
      dplyr::summarise(mean_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(age_mid) %>%
      dplyr::mutate(irr = mean_rate / pmax(mean(mean_rate, na.rm = TRUE), 1e-9))

    # ── Period effects (net drift) ──────────────────────────────────
    period_df <- pdat %>%
      dplyr::group_by(period) %>%
      dplyr::summarise(mean_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(period)

    pm <- tryCatch(lm(log(mean_rate) ~ period, data = period_df), error = function(e) NULL)
    net_drift <- if (!is.null(pm))
      round(100 * (exp(coef(pm)["period"]) - 1), 3) else NA_real_

    ref_p      <- mean(period_df$mean_rate, na.rm = TRUE)
    period_df  <- period_df %>%
      dplyr::mutate(irr = mean_rate / pmax(ref_p, 1e-9))

    # ── Cohort effects ──────────────────────────────────────────────
    cohort_df <- pdat %>%
      dplyr::mutate(cohort = period - age_mid) %>%
      dplyr::group_by(cohort) %>%
      dplyr::summarise(
        mean_rate = mean(val, na.rm = TRUE),
        n         = dplyr::n(),
        .groups   = "drop"
      ) %>%
      dplyr::filter(n >= 2) %>%
      dplyr::arrange(cohort)

    ref_c     <- mean(cohort_df$mean_rate, na.rm = TRUE)
    cohort_df <- cohort_df %>%
      dplyr::mutate(irr = mean_rate / pmax(ref_c, 1e-9))

    list(
      age_df    = age_df,
      period_df = period_df,
      cohort_df = cohort_df,
      net_drift = net_drift,
      n_obs     = nrow(pdat),
      location  = loc %||% "All"
    )
  }, error = function(e) NULL)
}

# ============================================================
# HELPER: HEALTH INEQUALITY (quintile-gap analysis)
# ============================================================
calc_health_inequality <- function(df, sdi_df = NULL) {
  ts_df <- df %>%
    dplyr::filter(!is.na(val), val > 0) %>%
    dplyr::group_by(location_name, year) %>%
    dplyr::summarise(val = mean(val, na.rm = TRUE), .groups = "drop")

  if (nrow(ts_df) < 10 || length(unique(ts_df$location_name)) < 4)
    return(NULL)

  if (!is.null(sdi_df) && nrow(sdi_df) >= 4 &&
      all(c("location_name", "sdi") %in% names(sdi_df))) {

    sdi_avg <- sdi_df %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(sdi = mean(sdi, na.rm = TRUE), .groups = "drop")

    merged <- dplyr::left_join(ts_df, sdi_avg, by = "location_name") %>%
      dplyr::filter(!is.na(sdi))

    if (nrow(merged) < 8) return(NULL)
    q_breaks <- quantile(unique(sdi_avg$sdi), probs = seq(0, 1, 0.2), na.rm = TRUE)
    merged <- merged %>%
      dplyr::mutate(
        quintile    = as.character(
          cut(sdi, breaks = q_breaks, labels = 1:5, include.lowest = TRUE)),
        group_label = dplyr::case_when(
          quintile == "1" ~ "Low SDI",
          quintile == "2" ~ "Low-Mid SDI",
          quintile == "3" ~ "Middle SDI",
          quintile == "4" ~ "Mid-High SDI",
          quintile == "5" ~ "High SDI",
          TRUE            ~ paste0("Q", quintile)
        )
      )
    type <- "sdi"

  } else {
    start_yr  <- min(ts_df$year, na.rm = TRUE)
    rank_ref  <- ts_df %>%
      dplyr::filter(year == start_yr) %>%
      dplyr::mutate(quintile = as.character(dplyr::ntile(val, 5))) %>%
      dplyr::select(location_name, quintile)

    merged <- dplyr::left_join(ts_df, rank_ref, by = "location_name") %>%
      dplyr::filter(!is.na(quintile)) %>%
      dplyr::mutate(
        group_label = dplyr::case_when(
          quintile == "1" ~ "Low burden",
          quintile == "2" ~ "Low-Mid burden",
          quintile == "3" ~ "Middle burden",
          quintile == "4" ~ "Mid-High burden",
          quintile == "5" ~ "High burden",
          TRUE            ~ paste0("Group ", quintile)
        )
      )
    type <- "rank"
  }

  quintile_ts <- merged %>%
    dplyr::group_by(year, group_label) %>%
    dplyr::summarise(
      mean_rate = mean(val, na.rm = TRUE),
      n_locs    = dplyr::n(),
      .groups   = "drop"
    ) %>%
    dplyr::arrange(year, group_label)

  gap_df <- quintile_ts %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      high_q = max(mean_rate, na.rm = TRUE),
      low_q  = min(mean_rate, na.rm = TRUE),
      gap    = high_q - low_q,
      ratio  = high_q / pmax(low_q, 1e-9),
      .groups = "drop"
    )

  list(
    quintile_ts = quintile_ts,
    gap_df      = gap_df,
    raw_data    = merged,
    type        = type
  )
}

# ============================================================
# SLOPE INDEX OF INEQUALITY (SII / RII)
# ============================================================
calc_sii <- function(df, sdi_df = NULL, weighted = FALSE, pop_df = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  rate_df <- df %>%
    dplyr::filter(metric_name == "Rate")
  if ("age_name" %in% names(rate_df)) {
    asr <- rate_df %>% dplyr::filter(grepl("age-standard", age_name, ignore.case = TRUE))
    if (nrow(asr) > 0) rate_df <- asr
  }
  if (nrow(rate_df) == 0) return(NULL)

  locs <- unique(rate_df$location_name)
  n_locs <- length(locs)
  if (n_locs < 3) return(NULL)

  # --- optional population weights (mean population per location) ---
  # Population is taken from pop_df (location_name, population[, year]) when
  # supplied; otherwise derived from Number/Rate metrics in df if both exist.
  pop_lookup <- NULL
  if (!is.null(pop_df) && all(c("location_name","population") %in% names(pop_df))) {
    pop_lookup <- pop_df %>% dplyr::group_by(location_name) %>%
      dplyr::summarise(population = mean(population, na.rm = TRUE), .groups = "drop")
  } else if (all(c("metric_name","measure_name") %in% names(df)) || "metric_name" %in% names(df)) {
    der <- tryCatch({
      num <- df %>% dplyr::filter(grepl("^number$", metric_name, ignore.case=TRUE))
      rat <- df %>% dplyr::filter(grepl("^rate$",   metric_name, ignore.case=TRUE))
      if (nrow(num) > 0 && nrow(rat) > 0) {
        keys <- intersect(c("location_name","year","measure_name","sex_name","age_name","cause_name"), names(df))
        j <- dplyr::inner_join(
               num %>% dplyr::select(dplyr::all_of(keys), num_val = val),
               rat %>% dplyr::select(dplyr::all_of(keys), rate_val = val),
               by = keys) %>%
             dplyr::filter(rate_val > 0) %>%
             dplyr::mutate(population = num_val / rate_val * 1e5)
        j %>% dplyr::group_by(location_name) %>%
          dplyr::summarise(population = mean(population, na.rm = TRUE), .groups = "drop")
      } else NULL
    }, error = function(e) NULL)
    pop_lookup <- der
  }
  use_wls <- isTRUE(weighted) && !is.null(pop_lookup) && nrow(pop_lookup) >= 3

  # Build fractional rank vector (same for all years)
  rank_base <- if (!is.null(sdi_df) && all(c("location_name", "sdi") %in% names(sdi_df))) {
    rank_type <- "SDI"
    sdi_df %>% dplyr::group_by(location_name) %>%
      dplyr::summarise(rank_val = mean(sdi, na.rm = TRUE), .groups = "drop") %>%
      dplyr::filter(location_name %in% locs)
  } else {
    rank_type <- "Burden"
    baseline_yr <- min(rate_df$year, na.rm = TRUE)
    rate_df %>% dplyr::filter(year == baseline_yr) %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(rank_val = mean(val, na.rm = TRUE), .groups = "drop")
  }
  rank_base <- dplyr::arrange(rank_base, rank_val)

  if (use_wls) {
    # population-weighted (Kunst-Mackenbach) cumulative-midpoint relative rank
    rank_base <- rank_base %>%
      dplyr::left_join(pop_lookup, by = "location_name") %>%
      dplyr::mutate(population = ifelse(is.na(population) | population <= 0, 1, population),
                    cum = cumsum(population),
                    frac_rank = (cum - population/2) / sum(population)) %>%
      dplyr::select(location_name, frac_rank, population)
  } else {
    rank_base <- rank_base %>%
      dplyr::mutate(frac_rank = (dplyr::row_number() - 0.5) / dplyr::n()) %>%
      dplyr::select(location_name, frac_rank)
  }
  loc_rank <- rank_base

  years <- sort(unique(rate_df$year))
  sii_res <- purrr::map_dfr(years, function(yr) {
    yr_data <- rate_df %>%
      dplyr::filter(year == yr) %>%
      dplyr::group_by(location_name) %>%
      dplyr::summarise(mean_rate = mean(val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::inner_join(loc_rank, by = "location_name")
    if (nrow(yr_data) < 3) return(NULL)
    tryCatch({
      if (use_wls) {
        m        <- lm(mean_rate ~ frac_rank, data = yr_data, weights = yr_data$population)
        mean_all <- stats::weighted.mean(yr_data$mean_rate, yr_data$population, na.rm = TRUE)
      } else {
        m        <- lm(mean_rate ~ frac_rank, data = yr_data)
        mean_all <- mean(yr_data$mean_rate, na.rm = TRUE)
      }
      sii <- unname(coef(m)["frac_rank"])
      # Relative Index of Inequality (Kunst-Mackenbach): ratio of the model-
      # predicted rate at the most disadvantaged (lowest-SDI, frac_rank = 0)
      # rank to that at the most advantaged (highest-SDI, frac_rank = 1) rank.
      # Strictly positive; RII > 1 indicates a greater burden among lower-SDI
      # populations. (The previous SII / mean_rate form went negative whenever
      # the SDI gradient was negative.) SII is retained as the absolute OLS
      # slope and may legitimately be negative.
      #
      # When the unweighted linear fit is steep relative to the mean rate
      # (|SII| > ~2 x mean, e.g. meningitis incidence), linear extrapolation
      # predicts a NON-POSITIVE rate at the advantaged end, which would make
      # the simple ratio negative/undefined. In that case we fall back to the
      # equivalent multiplicative Kunst-Mackenbach form via a log-linear
      # (quasi-Poisson) fit of the SAME unweighted rank regression, giving
      # RII = predicted_rate(rank 0) / predicted_rate(rank 1) = exp(-beta),
      # which is guaranteed strictly positive and carries the identical
      # ">1 = higher burden among lower-SDI" interpretation.
      cf        <- coef(m)
      pred_low  <- unname(cf[[1]] + cf[[2]] * 0)   # lowest-SDI rank  (most disadvantaged)
      pred_high <- unname(cf[[1]] + cf[[2]] * 1)   # highest-SDI rank (most advantaged)
      if (is.finite(pred_low) && is.finite(pred_high) &&
          pred_low > 0 && pred_high > 0) {
        rii <- pred_low / pred_high                # linear-OLS predicted ratio
      } else {
        rii <- tryCatch({
          gm <- if (use_wls)
            stats::glm(mean_rate ~ frac_rank, data = yr_data,
                       family = stats::quasipoisson(link = "log"),
                       weights = yr_data$population)
          else
            stats::glm(mean_rate ~ frac_rank, data = yr_data,
                       family = stats::quasipoisson(link = "log"))
          unname(exp(-coef(gm)[["frac_rank"]]))   # = pred_rate(0) / pred_rate(1)
        }, error = function(e) NA_real_)
      }
      tibble::tibble(year = yr, sii = sii, rii = rii, mean_rate = mean_all)
    }, error = function(e) NULL)
  })

  if (is.null(sii_res) || nrow(sii_res) == 0) return(NULL)
  list(sii_df = sii_res, rank_type = rank_type, n_locs = n_locs,
       method = if (use_wls) "WLS (population-weighted)" else "OLS (unweighted)")
}

# Prevent MASS / other forecast dependencies from masking dplyr verbs
select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename
mutate <- dplyr::mutate

message("GBD Analysis Suite — global.R loaded ✓")
