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

graphing_colors <- c("Significantly Browner than Normal"="#D01C8B", "Slightly Browner than Normal"="#F1B6DA",
                     "Normal"="gray",
                     "Slightly Greener than Normal"="#B8E186", "Significantly Greener than Normal"="#4DAC26")

heatmap_colors <- c("Significantly Browner than Normal"="maroon", "Slightly Browner than Normal"="pink",
                    "Normal"="gray",
                    "Slightly Greener than Normal"="olive", "Significantly Greener than Normal"="success")

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

# All data overview graph ----
all_data_graph <- function() {
  ggplot(NDVIall_years_modeled, aes(x = date, y = YrMean)) +
    geom_ribbon(aes(ymin = YrLwr, ymax = YrUpr, fill = type), alpha = 0.2) +
    geom_line(aes(color = type), size = 1) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
    labs(x = "Date", y = "NDVI Value", title = "NDVI Trends Over Time for All Land Cover Types") +
    scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
}

# 12-month overview graph ----
twelve_month_graph <- function(start_year, end_year) {
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)

  yearly_filtered_data <- NDVIall_years_modeled %>%
    filter(lubridate::year(date) >= start_year & lubridate::year(date) <= end_year)

  jan_1_dates <- unique(yearly_filtered_data$date[
    lubridate::month(yearly_filtered_data$date) == 1 &
      lubridate::day(yearly_filtered_data$date) == 1
  ])

  ggplot(yearly_filtered_data, aes(x = date, y = YrMean)) +
    geom_ribbon(aes(ymin = YrLwr, ymax = YrUpr, fill = type), alpha = 0.2) +
    geom_line(aes(color = type)) +
    geom_vline(xintercept = jan_1_dates, linetype = "dashed") +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
    labs(y = "NDVI Value", title = paste("NDVI Trends for Year", start_year)) +
    scale_x_date(date_labels = "%b %Y")
}

# Monthly overview graph ----
monthly_graph <- function(mstart_date) {
  mstart_date <- as.Date(mstart_date)
  mend_date   <- mstart_date %m+% months(1)

  month_data <- NDVIall_years_modeled %>%
    filter(date >= mstart_date & date <= mend_date)

  ggplot(month_data, aes(x = date, y = YrMean)) +
    geom_ribbon(aes(ymin = YrLwr, ymax = YrUpr, fill = type), alpha = 0.2) +
    geom_line(aes(color = type)) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
    labs(x = "Date", y = "NDVI Value", title = "NDVI Trends for Month Following Selected Start Date") +
    scale_x_date(breaks = seq(mstart_date, mend_date, by = "7 days"),
                 labels = scales::date_format("%B %d"))
}

# Weekly overview graph ----
weekly_graph <- function(wstart_date) {
  wstart_date <- as.Date(wstart_date)
  wend_date   <- wstart_date + 7

  week_data <- NDVIall_years_modeled %>%
    filter(date >= wstart_date & date <= wend_date)

  ggplot(week_data, aes(x = date, y = YrMean)) +
    geom_ribbon(aes(ymin = YrLwr, ymax = YrUpr, fill = type), alpha = 0.2) +
    geom_line(aes(color = type)) +
    scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
    labs(x = "Date", y = "NDVI Value", title = "NDVI Trends for Week Following Selected Start Date") +
    scale_x_date(breaks = seq(wstart_date, wend_date, by = "1 day"),
                 labels = scales::date_format("%B %d"))
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
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      axis.text.y       = element_text(size = 8),
      axis.title.x      = element_text(face = "bold", size = 10),
      axis.title.y      = element_text(face = "bold", size = 10),
      plot.title        = element_text(face = "bold", size = 12),
      legend.key.height = unit(1, "cm"),
      legend.position   = "bottom",
      legend.text       = element_text(size = 8),
      legend.title      = element_text(size = 10),
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

  status <- round(most_recent_subset$YrMean - CI_final_subset$NormMean[1], digits = 2)
  color  <- heatmap_colors[most_recent_subset$FlagNDVI]

  list(status = status, color = color)
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
# NDVI anomaly heatmap ----
plot_ndvi_heatmap <- function(NDVIall_years_modeled, selected_years, LC_type, naming) {
  if (length(selected_years) == 0) return(ggplot() + ggtitle("No years selected"))

  filtered_data <- NDVIall_years_modeled %>%
    filter(year %in% as.numeric(selected_years), type == LC_type)
  filtered_data$FlagNDVI <- factor(filtered_data$FlagNDVI, levels = names(graphing_colors))

  ggplot(filtered_data, aes(x = yday, y = factor(year))) +
    geom_tile(aes(fill = FlagNDVI), width = 1, height = 1) +
    scale_fill_manual(values = graphing_colors, name = "NDVI Category", drop = FALSE) +
    scale_x_continuous(name = "Date", breaks = day.labels$yday, labels = day.labels$Text) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "Date", y = "Year", title = paste0(naming, " Heat Map")) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      axis.text.y       = element_text(size = 8),
      axis.title.x      = element_text(face = "bold", size = 10),
      axis.title.y      = element_text(face = "bold", size = 10),
      plot.title        = element_text(face = "bold", size = 12),
      legend.key.height = unit(1, "cm"),
      legend.position   = "bottom",
      legend.text       = element_text(size = 8),
      legend.title      = element_text(size = 10),
      panel.background  = element_rect(fill = "gray99"),
      plot.background   = element_rect(fill = "gray99")
    )
}

####################################################################################################################
#END OF FUNCTIONS ----
####################################################################################################################

# Define server logic ----
server <- function(input, output, session) {

  yearly_filtered_data <- reactive({
    NDVIall_years_modeled %>%
      filter(year >= input$yearRange[1] & year <= input$yearRange[2])
  })

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
        return(valueBox(paste("No recent", m$label, "data available"),
                        subtitle = "No Data", icon = icon("exclamation-circle"),
                        color = "gray", width = 11))
      }
      valueBox(m$label,
               subtitle = paste("is", result$status, "from normal"),
               icon = icon(m$icon), color = result$color, width = 11)
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

    output[[m$hm]] <- renderPlot({
      req(input$selected_years)
      plot_ndvi_heatmap(NDVIall_years_modeled, input$selected_years, m$lc, m$label)
    })
  })

  ####################################################################################################################
  # NDVI Data Review graphs ----
  output$all_data_graph <- renderPlot({ all_data_graph() })

  output$yearly_graph <- renderPlot({
    req(input$yearRange)
    twelve_month_graph(input$yearRange[1], input$yearRange[2])
  })

  output$monthly_graph <- renderPlot({
    req(input$mstart_date)
    monthly_graph(input$mstart_date)
  })

  output$weekly_graph <- renderPlot({
    req(input$wstart_date)
    weekly_graph(input$wstart_date)
  })

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
