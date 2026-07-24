# GBD Comprehensive Analysis Suite

A full-featured R Shiny dashboard for Global Burden of Disease (GBD) paper-style analyses.  
Upload any GHDx CSV/Excel file and run every standard analysis in one place — no coding required.

---

## Features

| # | Tab | Analysis | Methods |
|---|-----|----------|---------|
| 1 | 📁 Data Import | Upload GHDx data + SDI file, preview, validate | Auto column detection, data.table fast load |
| 2 | 🗺️ Distribution Maps | Choropleth world maps | ASR by year, EAPC map, % change map |
| 3 | 📈 EAPC Analysis | Estimated Annual Percentage Change | GLM: ln(ASR) = α + β×year; forest plot, SDI scatter |
| 4 | 🔗 Joinpoint Regression | Trend breakpoint detection | `segmented` package; slider = exact N joinpoints; Log / Linear / Sqrt models |
| 5 | 🔬 Decomposition | Change attribution | Kitagawa 2-component (population growth + epidemiological change); Das Gupta 3-component when age-stratified data provided |
| 6 | 🔮 Forecasting | Time-series projection | ARIMA, ETS, TBATS, Theta, Holt, Neural Net, Ensemble; log-transform guarantees non-negative rates |
| 7 | 📊 Descriptive Stats | Heatmap, boxplot, sex comparison, trend lines, butterfly chart, 3-D scatter, treemap | GBD publication style |
| 8 | 🌐 Frontier Analysis | Health frontier & efficiency scores + country ranking bump chart | SDI-based / peer-group frontier; country ranking bump chart |
| 9 | 🔁 APC Model | Descriptive age-period-cohort visualisation (net drift identified) | Marginal IRR ratios; period net drift = only formally identified parameter |
| 10 | ⚖️ Health Inequality | SII, RII, quintile gap analysis | SDI-based or burden-quintile grouping |
| 11 | 💾 Export & Methods | Excel workbook, figures (PNG/PDF), methods text | `openxlsx`, ggplot2 static export, Word-ready methods |

---

## Quick Start

### Step 1 — Install packages (run once)
```r
source("install_packages.R")
```

### Step 2 — Launch the app
```r
shiny::runApp(".")
```

### Step 3 — Load data
- Click **"Load data directly"** (uses `final data.xlsx` or `GBD_Global_204_Data.csv` if present in the app folder)
- Or click **Browse** to upload any GHDx CSV / Excel file
- Optionally upload an SDI CSV to enable EAPC×SDI scatter and SDI-based frontier

---

## Data Format

### Main data file (GHDx CSV)
Download from: https://vizhub.healthdata.org/gbd-results/

| Column | Description |
|--------|-------------|
| `location_name` | Country / region name |
| `year` | Year (integer) |
| `val` | Point estimate (rate / count) |
| `upper` | Upper 95% UI |
| `lower` | Lower 95% UI |
| `measure_name` | Deaths / DALYs / Incidence / Prevalence |
| `metric_name` | Number / Rate / Percent |
| `sex_name` | Both / Male / Female |
| `age_name` | Age-standardized / All Ages / specific groups |
| `cause_name` | Disease / condition name |

Columns are auto-detected (case-insensitive). Only `location_name`, `year`, and `val` are required.

### SDI file (optional)
```
location_name, year, sdi
Global, 2021, 0.66
Egypt, 2021, 0.58
```
A downloadable template is available on the Data Import tab.

---

## Statistical Methods

### EAPC
```
ln(ASR) = α + β × year
EAPC = 100 × (eᵝ − 1)
95% CI = 100 × (e^(β ± 1.96 × SE_β) − 1)
```
Trend classification: Increasing (EAPC > 0, lower CI > 0) / Decreasing (EAPC < 0, upper CI < 0) / Stable.

### Joinpoint Regression
- R `segmented` package
- Slider always applies the **exact number of joinpoints** selected (0–5); no automatic selection
- Three model types: Log/Multiplicative (NCI default), Linear/Additive, Square Root
- Reports APC per segment, 95% CI, significance, AAPC

### Forecasting
- **ARIMA** — `auto.arima()` (AICc minimisation)
- **ETS** — `ets()` (automatic state-space selection)
- **TBATS** — `tbats()` (trigonometric seasonality)
- **Theta** — `thetaf()` (Theta method)
- **Holt** — `holt()` (double exponential smoothing)
- **Neural Net** — `nnetar()` (feed-forward NN on lagged values)
- **Ensemble** — equal-weight average of all above models
- All models use **Box-Cox λ = 0 (log transform)** with bias adjustment; back-transformation via `exp()` guarantees strictly positive point estimates and prediction intervals
- Additional `pmax(0, ...)` floor applied as a secondary safeguard
- 80% and 95% prediction intervals

### Decomposition
The app implements two decomposition approaches, selected automatically based on available data:

**(a) Kitagawa 2-component decomposition** (runs on any GHDx dataset containing both Number and Rate metrics):
- Population-growth effect = mean_rate × (pop₂ − pop₁) , using mean_rate = (rate₁ + rate₂) / 2
- Epidemiological effect = (rate₂ − rate₁) × mean_pop , using mean_pop = (pop₁ + pop₂) / 2
- Symmetric mid-point form guarantees both components sum exactly to the observed change in case counts (no residual)

**(b) Das Gupta 3-component decomposition** (runs additionally when age-stratified rates are provided):
- Adds a third **Population aging** component
- Requires age-specific rows (e.g., "<5", "5–14", "15–49", "50–69", "70+") with both Number and Rate metrics
- Uses symmetric-average factor-effect formulation with proportional rescaling to ensure exact additivity

The UI displays whichever method ran; the subtitle names the method used.

### Health Inequality (SII / RII)
Slope Index of Inequality and Relative Index of Inequality computed across SDI quintiles or burden quintiles over time.

### Frontier Analysis
SDI-based frontier (loess regression, 10th percentile of residuals as the empirical lower envelope) or peer-group frontier. Uses most-recent available SDI year per country. Efficiency scores and excess burden per country.

### APC Model
**Descriptive age-period-cohort visualisation.** Computes marginal ratio-to-mean values for age, period, and cohort groups as incidence rate ratios (IRR = exp(β)). Only the **period net drift** (linear period slope) is a formally identified quantity; age and cohort effects are descriptive marginal ratios useful for visualisation but not for formal causal inference. Users requiring formal APC inference should use dedicated packages (e.g., `Epi::apc.fit`, `bamp`).

---

## File Structure
```
gbd_shiny_app/
├── app.R                    <- entry point
├── global.R                 <- packages + helper functions + theme
├── ui.R                     <- user interface (11 tabs)
├── server.R                 <- server logic
├── install_packages.R       <- run once to install dependencies
├── GBD_Global_204_Data.csv  <- bundled demo dataset (see naming note below)
├── GBD_Global_204_SDI.csv   <- bundled SDI metadata
└── README.md
```

> **Naming note:** the `_204_` in the bundled filenames is a legacy label from the
> original 204-location GBD extract template. The files shipped here contain
> **196 unique countries/territories** (meningitis, incidence + deaths, 1990–2023,
> 599,760 rows). The filenames are retained only so the default quick-load path
> keeps working; the app reports the true location count (196) in the Data Quality panel.

---

## Required R Packages

**Core (required)**
```r
shiny, shinydashboard, shinyWidgets, shinycssloaders,
DT, plotly, dplyr, tidyr, ggplot2, readr, readxl,
stringr, purrr, tibble, forecast, segmented,
maps, RColorBrewer, viridis, broom, openxlsx,
scales, data.table, patchwork, officer, flextable
```

**Optional (enhances certain features)**
```r
ggrepel           # Non-overlapping labels on scatter plots
rnaturalearth     # Higher-quality world map polygons
rnaturalearthdata # Data for rnaturalearth
sf                # Spatial features for maps
leaflet           # Interactive Leaflet maps
leaflet.extras    # Leaflet plugins
quantreg          # Quantile regression (frontier analysis)
zip               # ZIP bundle export (all figures)
```

---

## Deploying to shinyapps.io

```r
# Install rsconnect if needed
install.packages("rsconnect")

# Authenticate (one-time — get token from shinyapps.io dashboard)
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# Deploy
rsconnect::deployApp(
  appDir      = "path/to/gbd_shiny_app",
  appName     = "GBD-Analysis-Suite",
  forceUpdate = TRUE,
  lint        = FALSE
)
```

> **Note on file size**: The bundled `GBD_Global_204_Data.csv` is ~100 MB.  
> shinyapps.io free tier allows up to 1 GB total — this is within limits.  
> If you hit size limits, remove the bundled CSV; users can upload their own data.

---

## Citation

If you use this app for a published GBD analysis, please cite the underlying methods:

- **EAPC**: Ferlay J, et al. (2019). Cancer incidence and mortality patterns in Europe. *Eur J Cancer.*
- **Joinpoint**: Kim H-J, et al. (2000). Permutation tests for joinpoint regression. *Stat Med.*
- **ARIMA/ETS**: Hyndman RJ, Khandakar Y. (2008). Automatic time series forecasting. *J Stat Softw.*
- **Kitagawa decomposition**: Kitagawa E. (1955). Components of a difference between two rates. *J Am Stat Assoc.*
- **Das Gupta decomposition**: Das Gupta P. (1993). *Standardization and Decomposition of Rates.* US Bureau of the Census.
- **SII/RII**: Wagstaff A, et al. (1991). On the measurement of inequalities in health. *Soc Sci Med.*

---

## Notes

1. **EAPC** requires age-standardised rates — use `metric = Rate`, `age = Age-standardized` in GHDx
2. **Kitagawa 2-component decomposition** runs on any dataset with both Number + Rate metrics. **Das Gupta 3-component** additionally requires age-stratified rows (multiple age groups) — not available in standard age-standardized-only GHDx downloads
3. **APC model** requires age-disaggregated rows; with age-standardized data only, period net drift is still computed
4. All charts are interactive (plotly) and exportable as PNG / PDF
5. Methods text is auto-generated and ready to paste into a manuscript
6. **BAPC forecasting** (Bayesian APC) requires INLA — see install script comments

---

*Built for GBD burden-of-disease research. Covers all standard analysis components.*
