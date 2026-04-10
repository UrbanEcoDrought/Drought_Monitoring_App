####################################################################################################################
#Purpose: Be a visualization portal for the Urban Drought Project, meant to be a real time portal that is updated regularly
#Pulls from NDVI_drought_monitoring workflow & UrbanDrought_SpatialAnalysis_Chicago workflow
####################################################################################################################
# Packages ----
library(shiny); library(shinydashboard)
library(leaflet); library(sf)
library(tidyverse); library(lubridate); library(scales)
library(shinyalert)

####################################################################################################################
#Palettes -----
paletteLC <- c("crop"="#ab6c28", "forest"="#68ab5f", "grassland"="#dfdfc2",
               "urban-high"="#ab0000", "urban-medium"="#eb0000", "urban-low"="#d99282", "urban-open"="#dec5c5")

graphing_colors <- c("Significantly Browner than Normal" = "#D01C8B",
                     "Slightly Browner than Normal"      = "#F1B6DA",
                     "Normal"                            = "#7C7779",
                     "Slightly Greener than Normal"      = "#B8E186",
                     "Significantly Greener than Normal" = "#4DAC26")

# Text color for each status background — light backgrounds need dark text
graphing_text_colors <- c("Significantly Browner than Normal" = "white",
                           "Slightly Browner than Normal"      = "#333333",
                           "Normal"                            = "white",
                           "Slightly Greener than Normal"      = "#333333",
                           "Significantly Greener than Normal" = "white")

# Trend direction (YrDerivTrend) — 3 levels
trend_colors <- c(
  "Getting Browner" = "#A0522D",
  "No Change"       = "#CCCCCC",
  "Getting Greener" = "#2E7D32"
)

# Trend anomaly (FlagTrend) — 7 levels; middle 5 match graphing_colors for visual consistency
trend_flag_colors <- c(
  "Abnormal Browning"           = "#8C0046",
  "Browning Faster than Normal" = "#D01C8B",
  "Browning Slower than Normal" = "#F1B6DA",
  "Normal"                      = "#7C7779",
  "Greening Slower than Normal" = "#B8E186",
  "Greening Faster than Normal" = "#4DAC26",
  "Abnormal Greening"           = "#1B6B00"
)

yrNow <- lubridate::year(Sys.Date())
day.labels <- data.frame(Date = seq.Date(as.Date(paste0(yrNow, "-01-01")),
                                         as.Date(paste0(yrNow, "-12-01")), by = "month"))
day.labels$yday <- lubridate::yday(day.labels$Date)
day.labels$Text <- paste(lubridate::month(day.labels$Date, label = T), lubridate::day(day.labels$Date))

####################################################################################################################
# Read in data -----
NDVIall_normals_modeled <- read_csv("data/NDVIall_normals_modeled.csv")
NDVIall_years_modeled   <- read_csv("data/NDVIall_years_modeled.csv")

####################################################################################################################
# Subset / derived globals ----
NDVIall_years_modeled$year <- as.numeric(NDVIall_years_modeled$year)
latest_year  <- max(NDVIall_years_modeled$year, na.rm = TRUE)
latest_yday  <- max(NDVIall_years_modeled$yday[NDVIall_years_modeled$year == latest_year], na.rm = TRUE)
most_recent_data <- filter(NDVIall_years_modeled, year == latest_year & yday == latest_yday)

NDVIall_years_modeled <- NDVIall_years_modeled %>%
  mutate(date = as.Date(yday - 1, origin = paste0(year, "-01-01")))

date_needed <- NDVIall_years_modeled %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  pull(date)

lcnames <- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

counties <- sf::read_sf("cb_2023_us_county_500k", layer = "cb_2023_us_county_500k") %>%
  st_transform(crs = 4326)
il_counties <- subset(counties,
                      counties$NAME %in% c("Cook", "DuPage", "Kane", "McHenry", "Lake", "Will", "Kendall") &
                        STATE_NAME == "Illinois")

####################################################################################################################
# Central LC metadata — all per-LC render loops derive from this single table ----
# Fields: lc (data key), label (display name), box/pct/density/ts/chg/hm (output IDs), icon (FA icon name)
lc_meta <- list(
  list(lc = "crop",         label = "Crop",         box = "cropBox",  pct = "percentile_crop",
       density = "crop_density_plot",      ts = "crop_currentTS_plot",   chg = "crop_daily",
       hm = "ndvi_heatmap_crop",   icon = "tractor"),
  list(lc = "forest",       label = "Forest",       box = "forBox",   pct = "percentile_for",
       density = "forest_density_plot",    ts = "forest_currentTS_plot", chg = "for_daily",
       hm = "ndvi_heatmap_forest", icon = "tree"),
  list(lc = "grassland",    label = "Grassland",    box = "grassBox", pct = "percentile_grass",
       density = "grassland_density_plot", ts = "grass_currentTS_plot",  chg = "grass_daily",
       hm = "ndvi_heatmap_grass",  icon = "seedling"),
  list(lc = "urban-high",   label = "Urban-High",   box = "uhBox",    pct = "percentile_uh",
       density = "uh_density_plot",        ts = "uh_currentTS_plot",     chg = "uh_daily",
       hm = "ndvi_heatmap_uh",     icon = "city"),
  list(lc = "urban-medium", label = "Urban-Medium", box = "umBox",    pct = "percentile_um",
       density = "um_density_plot",        ts = "um_currentTS_plot",     chg = "um_daily",
       hm = "ndvi_heatmap_um",     icon = "building-columns"),
  list(lc = "urban-low",    label = "Urban-Low",    box = "ulBox",    pct = "percentile_ul",
       density = "ul_density_plot",        ts = "ul_currentTS_plot",     chg = "ul_daily",
       hm = "ndvi_heatmap_ul",     icon = "house"),
  list(lc = "urban-open",   label = "Urban-Open",   box = "uoBox",    pct = "percentile_uo",
       density = "uo_density_plot",        ts = "uo_currentTS_plot",     chg = "uo_daily",
       hm = "ndvi_heatmap_uo",     icon = "shop")
)

####################################################################################################################
#Functions ------
####################################################################################################################

# By-landcover faceted plot ----
plot_by_landcover <- function(selected_lcs, selected_years, start_yday, end_yday) {
  yearly  <- NDVIall_years_modeled %>%
    filter(type %in% selected_lcs, year %in% selected_years,
           yday >= start_yday, yday <= end_yday)
  normals <- NDVIall_normals_modeled %>%
    filter(type %in% selected_lcs, yday >= start_yday, yday <= end_yday)

  in_window <- day.labels$yday >= start_yday & day.labels$yday <= end_yday
  if (any(in_window)) {
    yday_breaks <- day.labels$yday[in_window]
    yday_labels <- day.labels$Text[in_window]
  } else {
    yday_breaks <- c(start_yday, end_yday)
    yday_labels <- format(as.Date(c(start_yday, end_yday) - 1,
                                  origin = paste0(yrNow, "-01-01")), "%b %d")
  }

  ggplot() +
    geom_ribbon(data = normals,
                aes(x = yday, ymin = NormLwr, ymax = NormUpr),
                fill = "gray70", alpha = 0.4) +
    geom_line(data = normals,
              aes(x = yday, y = NormMean),
              color = "black", linetype = "dashed") +
    geom_ribbon(data = yearly,
                aes(x = yday, ymin = YrLwr, ymax = YrUpr, fill = factor(year)), alpha = 0.2) +
    geom_line(data = yearly,
              aes(x = yday, y = YrMean, color = factor(year))) +
    facet_wrap(~type, ncol = 4) +
    scale_color_viridis_d(name = "Year", direction=1, option="turbo") +
    scale_fill_viridis_d(name = "Year", direction=1, option="turbo") +
    scale_x_continuous(name = "Date", breaks = yday_breaks, labels = yday_labels) +
    scale_y_continuous(name = "NDVI") +
    labs(title = "NDVI by Land Cover Type") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text      = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# By-year faceted plot ----
plot_by_year <- function(selected_years, selected_lcs, start_yday, end_yday) {
  yearly  <- NDVIall_years_modeled %>%
    filter(type %in% selected_lcs, year %in% selected_years,
           yday >= start_yday, yday <= end_yday)
  normals <- NDVIall_normals_modeled %>%
    filter(type %in% selected_lcs, yday >= start_yday, yday <= end_yday)

  # Replicate normal ribbon for each year facet
  normals_faceted <- do.call(rbind, lapply(selected_years, function(yr) {
    cbind(normals, year = yr)
  }))

  in_window <- day.labels$yday >= start_yday & day.labels$yday <= end_yday
  if (any(in_window)) {
    yday_breaks <- day.labels$yday[in_window]
    yday_labels <- day.labels$Text[in_window]
  } else {
    yday_breaks <- c(start_yday, end_yday)
    yday_labels <- format(as.Date(c(start_yday, end_yday) - 1,
                                  origin = paste0(yrNow, "-01-01")), "%b %d")
  }

  ggplot() +
    geom_ribbon(data = normals_faceted,
                aes(x = yday, ymin = NormLwr, ymax = NormUpr),
                fill = "gray70", alpha = 0.4) +
    geom_line(data = normals_faceted,
              aes(x = yday, y = NormMean),
              color = "black", linetype = "dashed") +
    geom_ribbon(data = yearly,
                aes(x = yday, ymin = YrLwr, ymax = YrUpr, fill = type), alpha = 0.2) +
    geom_line(data = yearly,
              aes(x = yday, y = YrMean, color = type)) +
    facet_wrap(~year, ncol = 4) +
    scale_color_manual(values = paletteLC, name = "Land Cover") +
    scale_fill_manual(values = paletteLC, name = "Land Cover") +
    scale_x_continuous(name = "Date", breaks = yday_breaks, labels = yday_labels) +
    scale_y_continuous(name = "NDVI") +
    labs(title = "NDVI by Year") +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x     = element_text(angle = 90, vjust = 0.5, hjust = 1),
      strip.text      = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Window summary stats table ----
compute_window_stats <- function(selected_lcs, selected_years, start_yday, end_yday,
                                 data_yearly, data_normals) {
  selected_years <- as.numeric(selected_years)

  yearly_window <- data_yearly %>%
    filter(type %in% selected_lcs, year %in% selected_years,
           yday >= start_yday, yday <= end_yday)
  normal_window <- data_normals %>%
    filter(type %in% selected_lcs, yday >= start_yday, yday <= end_yday)

  rows <- list()
  for (lc in selected_lcs) {
    for (yr in selected_years) {
      sub <- yearly_window[yearly_window$type == lc & yearly_window$year == yr, ]
      if (nrow(sub) == 0) next
      start_row <- sub[which.min(abs(sub$yday - start_yday)), ]
      end_row   <- sub[which.min(abs(sub$yday - end_yday)),   ]
      rows[[length(rows) + 1]] <- data.frame(
        "Land Cover" = lc,
        Year         = as.character(yr),
        Start        = round(start_row$YrMean,                   3),
        End          = round(end_row$YrMean,                     3),
        Min          = round(min(sub$YrMean),                    3),
        Max          = round(max(sub$YrMean),                    3),
        Change       = round(end_row$YrMean - start_row$YrMean, 3),
        check.names  = FALSE
      )
    }
    norm_sub <- normal_window[normal_window$type == lc, ]
    if (nrow(norm_sub) > 0) {
      norm_start <- norm_sub[which.min(abs(norm_sub$yday - start_yday)), ]
      norm_end   <- norm_sub[which.min(abs(norm_sub$yday - end_yday)),   ]
      rows[[length(rows) + 1]] <- data.frame(
        "Land Cover" = lc,
        Year         = "Normal",
        Start        = round(norm_start$NormMean,                        3),
        End          = round(norm_end$NormMean,                          3),
        Min          = round(min(norm_sub$NormMean),                     3),
        Max          = round(max(norm_sub$NormMean),                     3),
        Change       = round(norm_end$NormMean - norm_start$NormMean,   3),
        check.names  = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

####################################################################################################################
# Density plot ----
density_plot <- function(LCtype, naming, NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data) {
  norm_subset        <- filter(NDVIall_normals_modeled, type == LCtype)
  ndvi_subset        <- filter(NDVIall_years_modeled, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)

  most_recent_subset$YrMean <- as.numeric(most_recent_subset$YrMean)
  recent_yday       <- most_recent_subset$yday[1]
  norm_final_subset  <- filter(norm_subset, yday == recent_yday)
  ndvi_final_subset  <- filter(ndvi_subset, yday == recent_yday, year == latest_year)

  ggplot(norm_subset) +
    geom_rect(data = norm_final_subset,
              aes(xmin = NormLwr, xmax = NormUpr, ymin = 0, ymax = 1, fill = "Normal NDVI 95% CI"),
              alpha = 0.2) +
    geom_rect(data = ndvi_final_subset,
              aes(xmin = YrLwr, xmax = YrUpr, ymin = 0, ymax = 1, fill = "Current NDVI 95% CI"),
              alpha = 0.2) +
    geom_density(aes(x = NormMean, y = after_stat(density) / max(after_stat(density))),
                 fill = "grey", alpha = 0.5) +
    geom_point(data = norm_final_subset,
               aes(x = NormMean, y = 0, shape = "Normal NDVI",  color = "Normal NDVI"),  size = 4) +
    geom_point(data = ndvi_final_subset,
               aes(x = YrMean,   y = 0, shape = "Current NDVI", color = "Current NDVI"), size = 4) +
    labs(x = paste0(naming, " Density Plot"), y = "Density") +
    scale_color_manual(name = "",
                       values = c("Normal NDVI 95% CI" = "#f46d43", "Current NDVI 95% CI" = "#74add1",
                                  "Normal NDVI"        = "#f46d43", "Current NDVI"        = "#74add1")) +
    scale_fill_manual(name = "",
                      values = c("Normal NDVI 95% CI" = "#f46d43", "Current NDVI 95% CI" = "#74add1",
                                 "Normal NDVI"        = "#f46d43", "Current NDVI"        = "#74add1")) +
    scale_shape_manual(name = "", values = c("Normal NDVI" = 16, "Current NDVI" = 18)) +
    theme_minimal()
}

####################################################################################################################
# Time series vs. normal ----
plot_timeseries_now <- function(LCtype, naming, NDVIall_normals_modeled, NDVIall_years_modeled, ...) {
  norm_subset <- filter(NDVIall_normals_modeled, type == LCtype)
  ndvi_subset <- filter(NDVIall_years_modeled, type == LCtype, year == latest_year)

  ggplot(norm_subset, aes(x = yday)) +
    geom_ribbon(aes(ymin = NormLwr, ymax = NormUpr, fill = "Normal NDVI"), alpha = 0.2) +
    geom_line(aes(y = NormMean, color = "Normal NDVI")) +
    geom_ribbon(data = ndvi_subset, aes(ymin = YrLwr, ymax = YrUpr, fill = "Current NDVI"), alpha = 0.2) +
    geom_line(data = ndvi_subset, aes(y = YrMean, color = "Current NDVI")) +
    scale_color_manual(name = "", values = c("Normal NDVI" = "black", "Current NDVI" = "#74add1")) +
    scale_fill_manual(name = "",  values = c("Normal NDVI" = "black", "Current NDVI" = "#74add1")) +
    scale_x_continuous(name = "Date", breaks = day.labels$yday, labels = day.labels$Text) +
    scale_y_continuous(name = "NDVI", limits = c(0, max(NDVIall_years_modeled$YrUpr)), expand = c(0, 0)) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
      axis.text.y       = element_text(size = 11),
      axis.title.x      = element_text(face = "bold", size = 13),
      axis.title.y      = element_text(face = "bold", size = 13),
      plot.title        = element_text(face = "bold", size = 14),
      legend.key.height = unit(1, "cm"),
      legend.position   = "bottom",
      legend.text       = element_text(size = 11),
      legend.title      = element_text(size = 12),
      panel.background  = element_rect(fill = "gray99"),
      plot.background   = element_rect(fill = "gray99")
    )
}

####################################################################################################################
# Status box helper ----
LC_status <- function(LC_type, NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data) {
  most_recent_subset <- filter(most_recent_data, type == LC_type)
  if (nrow(most_recent_subset) == 0) return(NULL)

  CI_final_subset <- filter(NDVIall_normals_modeled, type == LC_type, yday == latest_yday)
  if (nrow(CI_final_subset) == 0) return(NULL)

  most_recent_subset$YrMean    <- as.numeric(most_recent_subset$YrMean)
  CI_final_subset$NormUpr <- as.numeric(CI_final_subset$NormUpr)
  CI_final_subset$NormLwr <- as.numeric(CI_final_subset$NormLwr)

  norm_mean   <- CI_final_subset$NormMean[1]
  status      <- round(most_recent_subset$YrMean - norm_mean, digits = 2)
  pct_anomaly <- round((most_recent_subset$YrMean - norm_mean) / norm_mean * 100, 1)
  flag        <- most_recent_subset$FlagNDVI
  color       <- unname(graphing_colors[flag])
  text_color  <- unname(graphing_text_colors[flag])

  list(status = status, pct_anomaly = pct_anomaly, color = color, text_color = text_color, flag = flag)
}

####################################################################################################################
# NDVI percentile ----
ndvi_percentile <- function(LCtype, NDVIall_years_modeled, most_recent_data, latest_yday) {
  YrMean_distr_yday  <- filter(NDVIall_years_modeled, yday == latest_yday & type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  ecdf(YrMean_distr_yday$YrMean)(most_recent_subset$YrMean[1]) * 100
}

####################################################################################################################
# Change stat functions ----
daily_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  prev_day    <- date_needed - 1
  NDVI_subset <- filter(NDVIall_years_modeled, type == LC_type)
  daily_range <- filter(NDVI_subset, date == date_needed | date == prev_day)
  if (nrow(daily_range) < 2) return("Insufficient data")
  paste("Daily Change:", round(
    daily_range$YrMean[daily_range$date == date_needed] -
      daily_range$YrMean[daily_range$date == prev_day], 3))
}

weekly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  date_needed <- as.Date(date_needed)
  NDVI_subset <- NDVIall_years_modeled %>% filter(type == LC_type) %>% mutate(date = as.Date(date))
  most_recent <- NDVI_subset %>% filter(date <= date_needed)      %>% arrange(desc(date)) %>% slice(1)
  prev        <- NDVI_subset %>% filter(date <= date_needed - 7)  %>% arrange(desc(date)) %>% slice(1)
  if (nrow(most_recent) == 0 | nrow(prev) == 0) return("Insufficient data")
  paste("Weekly change:", round(most_recent$YrMean - prev$YrMean, 3))
}

monthly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  date_needed <- as.Date(date_needed)
  NDVI_subset <- NDVIall_years_modeled %>% filter(type == LC_type) %>% mutate(date = as.Date(date))
  most_recent <- NDVI_subset %>% filter(date <= date_needed)      %>% arrange(desc(date)) %>% slice(1)
  prev        <- NDVI_subset %>% filter(date <= date_needed - 30) %>% arrange(desc(date)) %>% slice(1)
  if (nrow(most_recent) == 0 | nrow(prev) == 0) return("Insufficient data")
  paste("Monthly change:", round(most_recent$YrMean - prev$YrMean, 3))
}

yearly_change <- function(LC_type, date_needed, NDVIall_years_modeled) {
  date_needed <- as.Date(date_needed)
  NDVI_subset <- NDVIall_years_modeled %>% filter(type == LC_type) %>% mutate(date = as.Date(date))
  most_recent <- NDVI_subset %>% filter(date <= date_needed)       %>% arrange(desc(date)) %>% slice(1)
  prev        <- NDVI_subset %>% filter(date <= date_needed - 365) %>% arrange(desc(date)) %>% slice(1)
  if (nrow(most_recent) == 0 | nrow(prev) == 0) return("Insufficient data")
  paste("Yearly Change:", round(most_recent$YrMean - prev$YrMean, 3))
}

####################################################################################################################
# Heatmap functions ----

heatmap_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11),
      axis.text.y       = element_text(size = 11),
      axis.title.x      = element_text(face = "bold", size = 13),
      axis.title.y      = element_text(face = "bold", size = 13),
      plot.title        = element_text(face = "bold", size = 14),
      strip.text        = element_text(face = "bold", size = 12),
      legend.key.height = unit(1, "cm"),
      legend.position   = "bottom",
      legend.text       = element_text(size = 11),
      legend.title      = element_text(size = 12),
      panel.background  = element_rect(fill = "gray99"),
      plot.background   = element_rect(fill = "gray99")
    )
}

plot_ndvi_heatmap <- function(NDVIall_years_modeled, selected_years) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))
  filtered_data <- NDVIall_years_modeled %>%
    filter(year %in% as.numeric(selected_years))
  filtered_data$FlagNDVI <- factor(filtered_data$FlagNDVI, levels = names(graphing_colors))

  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = FlagNDVI), width = 1, height = 1) +
    scale_fill_manual(values = graphing_colors, name = "NDVI Status", drop = FALSE) +
    scale_x_continuous(name = "Date", breaks = day.labels$yday, labels = day.labels$Text) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_wrap(~type, ncol = 4) +
    labs(x = "Date", y = "Year", title = "NDVI Anomaly") +
    heatmap_theme()
}

plot_trend_heatmap <- function(NDVIall_years_modeled, selected_years) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))
  filtered_data <- NDVIall_years_modeled %>%
    filter(year %in% as.numeric(selected_years))
  filtered_data$YrDerivTrend <- factor(filtered_data$YrDerivTrend, levels = names(trend_colors))

  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = YrDerivTrend), width = 1, height = 1) +
    scale_fill_manual(values = trend_colors, name = "Trend Direction", drop = FALSE) +
    scale_x_continuous(name = "Date", breaks = day.labels$yday, labels = day.labels$Text) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_wrap(~type, ncol = 4) +
    labs(x = "Date", y = "Year", title = "Trend Direction") +
    heatmap_theme()
}

plot_trend_anomaly_heatmap <- function(NDVIall_years_modeled, selected_years) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))
  filtered_data <- NDVIall_years_modeled %>%
    filter(year %in% as.numeric(selected_years))
  filtered_data$FlagTrend <- factor(filtered_data$FlagTrend, levels = names(trend_flag_colors))

  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = FlagTrend), width = 1, height = 1) +
    scale_fill_manual(values = trend_flag_colors, name = "Trend Anomaly", drop = FALSE) +
    scale_x_continuous(name = "Date", breaks = day.labels$yday, labels = day.labels$Text) +
    scale_y_discrete(expand = c(0, 0)) +
    facet_wrap(~type, ncol = 4) +
    labs(x = "Date", y = "Year", title = "Trend Anomaly") +
    heatmap_theme()
}

####################################################################################################################
#END OF FUNCTIONS ----
####################################################################################################################

# Define server logic ----
server <- function(input, output, session) {

  # Render the map ----
  output$il_county_map <- renderLeaflet({
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = -88, lat = 41.8, zoom = 8)

    for (county_name in unique(il_counties$NAME)) {
      county_data <- il_counties[il_counties$NAME == county_name, ]
      map <- map %>%
        addPolygons(
          data        = county_data,
          color       = "#444444",
          weight      = 1,
          opacity     = 1,
          fillOpacity = 0.6,
          fillColor   = "#FFEDA0",
          label       = ~NAME,
          group       = county_name
        )
    }
    map %>%
      addLayersControl(
        overlayGroups = unique(il_counties$NAME),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  ####################################################################################################################
  # Per-LC renders: status boxes, percentiles, density plots, time series, change stats, heatmaps ----
  # All seven land cover types are driven by lc_meta defined at the top of the file.
  lapply(lc_meta, function(m) {

    output[[m$box]] <- renderUI({
      result <- LC_status(m$lc, NDVIall_years_modeled, NDVIall_normals_modeled, most_recent_data)
      if (is.null(result)) {
        return(div(class = "small-box",
                   style = "background-color: #888888; color: white;",
                   div(class = "inner",
                       tags$h3(paste("No data:", m$label),
                               style = "font-size: 14px; white-space: normal;"),
                       tags$p("Data unavailable")),
                   div(class = "icon", icon("circle-exclamation"))))
      }
      div(class = "small-box",
          style = paste0("background-color: ", result$color, "; color: ", result$text_color, ";"),
          div(class = "inner",
              tags$h3(m$label, style = "font-size: 15px; white-space: normal;"),
              tags$p(HTML(paste0(
                result$flag, "<br>",
                "<small>",
                ifelse(result$pct_anomaly >= 0,
                       paste0("+", result$pct_anomaly, "% from average"),
                       paste0(result$pct_anomaly, "% from average")),
                "</small>"
              )))),
          div(class = "icon", icon(m$icon)))
    })

    output[[m$pct]] <- renderText({
      pct <- ndvi_percentile(m$lc, NDVIall_years_modeled, most_recent_data, latest_yday)
      if (is.null(pct) || is.na(pct)) {
        paste0(m$label, " NDVI Percentile: Data Unavailable")
      } else {
        paste0(m$label, " NDVI Percentile: ", round(pct, 1), "%")
      }
    })

    output[[m$density]] <- renderPlot({
      density_plot(m$lc, m$label, NDVIall_normals_modeled, NDVIall_years_modeled, most_recent_data)
    })

    output[[m$ts]] <- renderPlot({
      plot_timeseries_now(m$lc, m$label, NDVIall_normals_modeled, NDVIall_years_modeled)
    })

    output[[m$chg]] <- renderText({
      paste(
        daily_change(m$lc,   date_needed, NDVIall_years_modeled), " | ",
        weekly_change(m$lc,  date_needed, NDVIall_years_modeled), " | ",
        monthly_change(m$lc, date_needed, NDVIall_years_modeled), " | ",
        yearly_change(m$lc,  date_needed, NDVIall_years_modeled)
      )
    })

  })

  ####################################################################################################################
  # Heatmap graphs ----
  output$ndvi_heatmap_all <- renderPlot({
    req(input$selected_years)
    plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years)
  })

  output$trend_heatmap_all <- renderPlot({
    req(input$selected_years)
    plot_trend_heatmap(NDVIall_years_modeled, input$selected_years)
  })

  output$trend_anomaly_heatmap_all <- renderPlot({
    req(input$selected_years)
    plot_trend_anomaly_heatmap(NDVIall_years_modeled, input$selected_years)
  })

  ####################################################################################################################
  # NDVI Data Review graphs ----
  output$plot_by_lc <- renderPlot({
    req(input$lc_years, input$lc_types)
    start_yday <- lubridate::yday(input$lc_date_window[1])
    end_yday   <- lubridate::yday(input$lc_date_window[2])
    validate(need(start_yday <= end_yday,
      "Date window cannot cross a year boundary (Jan 1). Please choose a start and end date within the same calendar year."))
    plot_by_landcover(input$lc_types, as.numeric(input$lc_years), start_yday, end_yday)
  })

  output$stats_by_lc <- renderTable({
    req(input$lc_show_stats, input$lc_years, input$lc_types)
    start_yday <- lubridate::yday(input$lc_date_window[1])
    end_yday   <- lubridate::yday(input$lc_date_window[2])
    compute_window_stats(input$lc_types, as.numeric(input$lc_years),
                         start_yday, end_yday,
                         NDVIall_years_modeled, NDVIall_normals_modeled)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  output$plot_by_yr <- renderPlot({
    req(input$yr_years, input$yr_types)
    start_yday <- lubridate::yday(input$yr_date_window[1])
    end_yday   <- lubridate::yday(input$yr_date_window[2])
    validate(need(start_yday <= end_yday,
      "Date window cannot cross a year boundary (Jan 1). Please choose a start and end date within the same calendar year."))
    plot_by_year(as.numeric(input$yr_years), input$yr_types, start_yday, end_yday)
  })

  output$stats_by_yr <- renderTable({
    req(input$yr_show_stats, input$yr_years, input$yr_types)
    start_yday <- lubridate::yday(input$yr_date_window[1])
    end_yday   <- lubridate::yday(input$yr_date_window[2])
    compute_window_stats(input$yr_types, as.numeric(input$yr_years),
                         start_yday, end_yday,
                         NDVIall_years_modeled, NDVIall_normals_modeled)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  ####################################################################################################################
  # Welcome popup ----
  shinyalert(
    title = "Welcome to the Urban Drought Dashboard!",
    text  = "<b>A near real-time portal offering a comprehensive view of current conditions across seven land cover types in the Chicago Region, with additional tabs for deeper analysis and research.</b><br><br>
             <b>LC Types</b> = NLCD Landcover types (crop, forest, grass/grassland, urban-high, urban-medium, urban-low, urban-open)<br><br>
             <b>NDVI</b> = Normalized Difference Vegetation Index (greenness)<br><br>
             If you need to view this information again, check the <b>About Tab</b> under <b>Preliminary Information</b>.",
    type              = "info",
    html              = TRUE,
    showConfirmButton = TRUE,
    confirmButtonText = "Close"
  )

  ####################################################################################################################
}
