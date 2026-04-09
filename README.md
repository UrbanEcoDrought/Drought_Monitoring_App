# Urban Ecological Drought Monitoring — Chicago Metro

A near real-time vegetation drought monitoring system for the Chicago 7-county metropolitan region.
Landsat satellite imagery is processed through a statistical modeling pipeline to detect and visualize
how vegetation greenness compares to long-term norms — updated weekly as new imagery becomes available.

**Live app:** https://mortonarb-forestecology.shinyapps.io/urban_drought_app/

---

## What This Project Does

Vegetation responds to drought stress by becoming less green. This project tracks that signal using
NDVI (Normalized Difference Vegetation Index) derived from Landsat 8 and 9 satellite imagery across
seven land cover types in the Chicago region: forest, cropland, grassland, and four urban density
classes (high, medium, low, and open).

New satellite imagery is pulled from Google Earth Engine, compared against a long-term statistical
baseline built from Landsat 5, 7, 8, and 9 data, and used to update an interactive web dashboard.
The dashboard shows current vegetation conditions, historical trends, and whether conditions are
significantly greener, browner, or normal relative to historical baselines.

This work is part of a broader effort to understand how ecological drought plays out differently
across land cover types in urban and urbanizing landscapes — relevant to land managers, conservation
planners, and climate adaptation efforts in the Chicago region.

---

## Using the App

The app is publicly accessible at https://mortonarb-forestecology.shinyapps.io/urban_drought_app/

Key features:

- **Current conditions panel** — color-coded status for each land cover type (significantly greener,
  slightly greener, normal, slightly browner, significantly browner than normal)
- **Time series plots** — NDVI trends by year, month, and week for each land cover type
- **Anomaly heat maps** — year-by-year deviation from the long-term normal
- **Distribution plots** — current NDVI distribution overlaid on the historical normal distribution

Status categories are based on whether the current year's modeled NDVI confidence interval overlaps
with the long-term normal confidence interval: non-overlapping = "significantly" different;
overlapping = "slightly" or "normal."

**Full documentation:** [Urban Drought Portal Documentation](https://docs.google.com/document/d/1I8WkmUjuPLf0SS_IF0F6P97xyH3aQhth8m9iYUQM4hs/edit?usp=sharing)

---

## Repository Structure

```
Drought_Monitoring_App/
├── 1_Get_New_Data.R                    # Main entry point — runs the weekly update pipeline
├── Deploy_Drought_App.R                # Deploys the Shiny app to shinyapps.io
│
├── NDVI_Automation_Workflow/
│   ├── Baseline_Data_Models_Norms/     # One-time historical baseline setup (scripts 01–03)
│   ├── New_Data_Models_Norms/          # Weekly update pipeline (scripts 03–06)
│   ├── data_all/                       # Processed CSVs consumed by the Shiny app
│   ├── gam_models/                     # Saved GAM model objects (.RDS)
│   └── figs/                           # QA visualizations generated after each update
│
└── Urban Drought App/                  # Shiny web application
    ├── server.R                        # Backend data processing and plot logic
    ├── ui.R                            # Dashboard layout and interface
    ├── data/                           # CSVs served to the app (copied from data_all/)
    └── www/                            # Static assets (logos, county shapefiles)
```

---

## Data Pipeline

### A. Baseline setup (one-time)

Scripts in `NDVI_Automation_Workflow/Baseline_Data_Models_Norms/`:

1. **`01_All_Landsat_Data.R`** — Queries Google Earth Engine for all available Landsat 5, 7, 8, and 9
   imagery over the Chicago 7-county region. Calculates NDVI for each image, masks by land cover type,
   and exports per-mission CSVs to Google Drive.

2. **`02_MergeLandsatData.R`** — Reads the exported CSVs from Google Drive and consolidates them into
   a single file: `NDVIall_baseline.csv`.

3. **`03_All_Models_Norms.R`** — Fits the statistical backbone of the project:
   - **Mission harmonization GAMs**: Corrects for sensor differences between Landsat missions so all
     data is on a common L8-equivalent scale.
   - **Long-term normal GAMs**: Cyclic spline models capturing the seasonal NDVI cycle across the full
     historical record — these define what "normal" looks like for each day of year.
   - **Year-specific GAMs**: A spline model for each calendar year, used to compare against the normal.
   - **Drought flagging**: Labels each year/period as significantly or slightly greener/browner than
     normal based on whether confidence intervals overlap.

   Outputs: `NDVIall_baseline_modeled.csv`, `NDVIall_years_modeled.csv`, `NDVIall_normals_modeled.csv`,
   and model objects in `gam_models/`.

### B. Weekly update pipeline

Triggered by sourcing `1_Get_New_Data.R` from the project root:

1. **`03_Pulling_New_Landsat_Data.R`** — Queries GEE for Landsat 8/9 scenes acquired since the last
   update. Exports results to Google Drive; `1_Get_New_Data.R` polls that folder until files appear
   (up to 30 minutes timeout).

2. **`04_Processing_New_Data.R`** — Reads new data from Google Drive, identifies observations not yet
   in the time series, and appends them to `NDVIall_latest.csv`.

3. **`05_Year_Specific_Adjustments.R`** — Applies mission harmonization to the new observations,
   re-fits the current year's GAM with the updated data, and recalculates drought flags.

4. **`06_Optional_CheckFigs.R`** — Generates PNG plots for visual quality-assurance review.

5. **`Deploy_Drought_App.R`** (optional) — Copies updated CSVs into the app's `data/` folder and
   redeploys to shinyapps.io. Triggered only when `pushData <- TRUE` in `1_Get_New_Data.R`.

All steps are logged to `workflow_log.txt` in the project root.

---

## Prerequisites and Setup

### R packages

```r
# Remote sensing / spatial
install.packages(c("rgee", "raster", "terra", "sf", "leaflet", "leaflet.extras"))

# Statistical modeling
install.packages(c("mgcv", "MASS"))

# Data wrangling
install.packages(c("tidyverse", "lubridate", "car"))

# Visualization
install.packages(c("ggplot2", "plotly", "scales"))

# Shiny app
install.packages(c("shiny", "shinydashboard", "bs4Dash", "shinyBS",
                   "shinyalert", "shinyjs", "shinycssloaders", "DT",
                   "forcats", "shinyGovstyle"))
```

### External accounts

| Service | Purpose |
|---|---|
| Google Earth Engine | Satellite data extraction |
| Google Drive | Intermediate file storage (GEE → local) |
| shinyapps.io | App hosting |

### Configuration

Before first use, update these variables in the scripts:

**`1_Get_New_Data.R` and `NDVI_Automation_Workflow/New_Data_Models_Norms/03_Pulling_New_Landsat_Data.R`:**
```r
user.ee <- "your-email@institution.edu"   # Your GEE-registered email
path.google <- "/path/to/your/Google Drive/My Drive/"
```

**GEE authentication (first run only):**
```r
library(rgee)
ee_install()          # one-time Python environment setup
ee_Initialize()       # opens browser for OAuth login
```

**shinyapps.io authentication (before deploying):**
```r
library(rsconnect)
rsconnect::setAccountInfo(name = "your-account", token = "...", secret = "...")
```

---

## Running the Weekly Update

Open `1_Get_New_Data.R` in RStudio and source it, or run from the command line:

```bash
Rscript 1_Get_New_Data.R
```

The script will:
1. Initialize GEE and check for new Landsat imagery
2. Wait up to 30 minutes for GEE to export files to Google Drive
3. Process and model the new data
4. Optionally deploy the updated app (set `pushData <- TRUE` at the top of the script)

Progress and errors are written to `workflow_log.txt`.

> **Note:** GEE exports run asynchronously. If the 30-minute timeout is reached with no files,
> check the [GEE Tasks panel](https://code.earthengine.google.com/) to confirm the export is running.

---

## Contributors

- Christine R. Rollinson, The Morton Arboretum (crollinson@mortonarb.org)
- Jocelyn Garcia, The Morton Arboretum (jgarcia@mortonarb.org, 2024-2025)
- Juliana Harr, The Morton Arboretum (jharr@mortonarb.org, 2024-2025)
- Lindsay Darling, The Morton Arboretum (ldarling@mortonarb.org)
- M. Ross Alexander, Consortium for Advanced Science and Engineering, University of Chicago
- Trent Ford, Illinois State Water Survey, University of Illinois, Urbana-Champaign (twford@illinois.edu)
- Ayo Andra J. Deas, The City University of New York (adeas@gc.cuny.edu, 2025)

## Funding

This research was supported by NIDIS through the FY 2022 Coping with Drought Competition —
Ecological Drought (Award NA22OAR4310233).

## Related Repositories

- [UrbanEcoDrought GitHub Organization](https://github.com/UrbanEcoDrought)
- [NDVI_Drought_Monitoring](https://github.com/UrbanEcoDrought/NDVI_drought_monitoring)
- [UrbanDrought_SpatialAnalysis_Chicago](https://github.com/UrbanEcoDrought/UrbanDrought_SpatialAnalysis_Chicago)
