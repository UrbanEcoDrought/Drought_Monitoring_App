# Checking patterns with some static versions of figures that will be on the app
library(ggplot2)
if(!"NDVI_Automation_Workflow" %in% dir()) setwd("../..")

path.google <- "~/Google Drive/"
pathShare <- file.path(path.google, "Shared drives/Urban Ecological Drought/Monitoring App")
pathDat <- "NDVI_Automation_Workflow/data_all"
pathFigs <- "NDVI_Automation_Workflow/figs"

if(!dir.exists(pathFigs)) dir.create(pathFigs)

yrDrought <- c(2005, 2012, 2023)

LClevels <- c("crop"="#ab6c28", "forest"="#68ab5f", "grassland"="#dfdfc2", "urban-open"="#dec5c5", "urban-low"="#d99282", "urban-medium"="#eb0000", "urban-high"="#ab0000")
paletteLC <- LClevels
trendLevels <- c("Getting Browner"="tan3", 
                 "No Change"="gray", 
                 "Getting Greener"="green4")
ndviFlagLevels <- c("Significantly Browner than Normal"="#d01c8b", 
                    "Slightly Browner than Normal"="#f1b6da", 
                    "Normal"="gray", 
                    "Slightly Greener than Normal"="#b8e186", 
                    "Significantly Greener than Normal"="#28a745")
trendFlagLevels <- c("Abnormal Browning"="tan3", 
                     "Browning Faster than Normal"="#d01c8b", 
                     "Browning Slower than Normal"="#f1b6da", 
                     "Normal"="gray", 
                     "Greening Slower than Normal"="#b8e186", 
                     "Greening Faster than Normal"="#4dac26", 
                     "Abnormal Greening"="turquoise3")

# Read in the data
datYrs <- read.csv(file.path(pathDat, "NDVIall_years_modeled.csv"))
datYrs$type <- factor(datYrs$type, levels=names(LClevels))
datYrs$YrDerivTrend <- factor(datYrs$YrDerivTrend, levels=names(trendLevels))
datYrs$FlagNDVI <- factor(datYrs$FlagNDVI, levels=names(ndviFlagLevels))
datYrs$FlagTrend <- factor(datYrs$FlagTrend, levels=names(trendFlagLevels))
summary(datYrs)

datNorms <- read.csv(file.path(pathDat, "NDVIall_normals_modeled.csv"))
datNorms$type <- factor(datNorms$type, levels=names(LClevels))
summary(datNorms)

yrNow <- max(datYrs$year)
datRaw <- read.csv(file.path(pathDat, "NDVIall_baseline_modeled.csv"))
datRaw$date <- as.Date(datRaw$date)
summary(datRaw)

# Doing a test time series graph
LC="urban-medium"
yrColors <- c("2025"="dodgerblue2", "2024"="cadetblue3", "2023"="orange2", "2012"="red3", "2005"="goldenrod2")

png(file.path(pathShare, "TimeSeries.png"), height=8, width=12, units = "in", res=320)
ggplot(datYrs[datYrs$year %in% c(yrNow, yrNow-1, yrDrought), ], ) +
  facet_wrap(~type) +
  geom_ribbon(data=datNorms[, ], aes(x=yday, ymin=NormLwr, ymax=NormUpr, fill="normal"), alpha=0.2) +
  geom_line(data=datNorms[, ], aes(x=yday, y=NormMean, color="normal")) +
  geom_ribbon(data=datYrs[datYrs$year %in% c(yrDrought), ], aes(x=yday, ymin=YrLwr, ymax=YrUpr, fill=as.factor(year)), alpha=0.2) +
  geom_line(data=datYrs[datYrs$year %in% c(yrDrought), ], aes(x=yday, y=YrMean, color=as.factor(year)), linewidth=0.2) +
  geom_ribbon(data=datYrs[datYrs$year %in% c(yrNow, yrNow-1), ], aes(x=yday, ymin=YrLwr, ymax=YrUpr, fill=as.factor(year)), alpha=0.2) +
  geom_line(data=datYrs[datYrs$year %in% c(yrNow, yrNow-1), ], aes(x=yday, y=YrMean, color=as.factor(year))) +
  scale_fill_manual(values=c("normal"="black", yrColors)) +
  scale_color_manual(values=c("normal"="black", yrColors)) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 366, by = 31),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  labs(x=NULL, y="NDVI") +
  # scale_y_discrete(name="NDVI") +
  theme_minimal()
dev.off()
  
# Doing a test heatmap

png(file.path(pathShare, "HeatMap-NDVI.png"), height=8, width=12, units = "in", res=320)
ggplot(datYrs[datYrs$year %in% c(yrNow, yrNow-1, yrDrought), ], aes(x = yday, y = factor(year))) +
  facet_wrap(~type) +
  geom_tile(aes(fill = YrMean), width = 1, height = 1) +  
  scale_fill_gradientn(
    colors = c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837"), 
    name = "NDVI" 
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 366, by = 31),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Month of Year", y = "Year", title = paste0("NDVI Heat Map")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.background = element_rect(fill = "gray99"),
    plot.background = element_rect(fill = "gray99")
  )
dev.off()


png(file.path(pathShare, "HeatMap-NDVI-Anom.png"), height=8, width=12, units = "in", res=320)
ggplot(datYrs[datYrs$year %in% c(yrNow, yrNow-1, yrDrought), ], aes(x = yday, y = factor(year))) +
  facet_wrap(~type) +
  geom_tile(aes(fill = FlagNDVI), width = 1, height = 1) +  
  scale_fill_manual(
    values = ndviFlagLevels, 
    name = "NDVI Category",
    drop = FALSE  
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 366, by = 31),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Month of Year", y = "Year", title = paste0("NDVI Anomaly Heat Map")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.background = element_rect(fill = "gray99"),
    plot.background = element_rect(fill = "gray99")
  )
dev.off()

png(file.path(pathShare, "HeatMap-Derivs-Anom.png"), height=8, width=12, units = "in", res=320)
ggplot(datYrs[datYrs$year %in% c(yrNow, yrNow-1, yrDrought), ], aes(x = yday, y = factor(year))) +
  facet_wrap(~type) +
  geom_tile(aes(fill = FlagTrend), width = 1, height = 1) +  
  scale_fill_manual(
    values = trendFlagLevels, 
    name = "NDVI Trend Category",
    drop = FALSE  
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 366, by = 31),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Month of Year", y = "Year", title = paste0("Trend Anomaly Heat Map")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.background = element_rect(fill = "gray99"),
    plot.background = element_rect(fill = "gray99")
  )
dev.off()

png(file.path(pathShare, "HeatMap-Derivs.png"), height=8, width=12, units = "in", res=320)
ggplot(datYrs[datYrs$year %in% c(yrNow, yrNow-1, yrDrought), ], aes(x = yday, y = factor(year))) +
  facet_wrap(~type) +
  geom_tile(aes(fill = YrDerivTrend), width = 1, height = 1) +  
  scale_fill_manual(
    values = trendLevels, 
    name = "NDVI Trend Category",
    drop = FALSE  
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = seq(1, 366, by = 31),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Month of Year", y = "Year", title = paste0("Trend Heat Map")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 12),
    legend.key.height = unit(1, "cm"),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    panel.background = element_rect(fill = "gray99"),
    plot.background = element_rect(fill = "gray99")
  )
dev.off()


# Doing a distribution
ydayToday <- max(datYrs$yday[datYrs$year==max(datYrs$year)])


ggplot(data=datNorms, aes(x = NormMean)) + 
  facet_wrap(~type) +
  geom_density(aes(y = after_stat(density) / max(after_stat(density))), fill = "#c2a5cf", alpha = 0.5)+
  
  # Add the bounds as dashed lines with legend
  geom_vline(data=datNorms[datNorms$yday==ydayToday,], aes(xintercept = NormLwr, color="normal"), linetype = "dashed", size = 1) +
  geom_vline(data=datNorms[datNorms$yday==ydayToday,], aes(xintercept = NormUpr, color="normal"), linetype = "dashed", size = 1) +
  
  # Mean point
  geom_point(data=datNorms[datNorms$yday==ydayToday,], aes(x = NormMean, y = 0, shape = "normal", color="normal"), size = 4) +
  
  # Current NDVI point (diamond)
  # Add the bounds as dashed lines with legend
  # geom_rect(data=datYrs[datYrs$year==max(datYrs$year) & datYrs$yday==ydayToday,], aes(xmin=YrLwr, xmax=YrUpr, ymin=-Inf, ymax=Inf, fill="current"), alpha=0.2) +
  geom_vline(data=datYrs[datYrs$year==max(datYrs$year) & datYrs$yday==ydayToday,], aes(xintercept = YrLwr, color="current"), linetype = "dashed", size = 1) +
  geom_vline(data=datYrs[datYrs$year==max(datYrs$year) & datYrs$yday==ydayToday,], aes(xintercept = YrUpr, color="current"), linetype = "dashed", size = 1) +
  
  # Mean point
  geom_point(data=datYrs[datYrs$year==max(datYrs$year) & datYrs$yday==ydayToday,], aes(x = YrMean, y = 0, shape = "current", color="current"), size = 4) +
  
  # Labels
  labs(
    x = paste0(" Density Plot"),  # Dynamic x-axis label using the 'naming' parameter
    y = "Density",
    linetype = "Bound Type",  # Legend title for the lines
    shape = "Point Type"      # Legend title for the points
  ) +
  
  # Manual legend adjustments
  # scale_linetype_manual(values = c("Lower Bound" = "dashed", "Upper Bound" = "dashed")) +
  scale_shape_manual(values = c("normal" = 16, "current" = 18)) +
  scale_color_manual(values = c("normal"="#40004b", "current" = "#1b7837")) +
  scale_fill_manual(values = c("normal"="#40004b", "current" = "#1b7837")) +
  
  theme_minimal()


datYrs$date <- as.Date(strptime(paste(datYrs$year, datYrs$yday, sep="-"), format=c("%Y-%j")))
summary(datYrs)

ggplot(datYrs, aes(x = date, y = YrMean, ymin=YrLwr, ymax=YrUpr)) +
    # geom_point(size = 1) +
    # geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12*25)) +
  geom_ribbon(aes(fill=type), alpha=0.2) +
  geom_line(aes(color=type)) +
  scale_color_manual(values = paletteLC) +
    scale_fill_manual(values = paletteLC) +
    labs(
      x = "Date",
      y = "NDVI Value",
      title = "NDVI Trends Over Time for Selected Land Cover Types"
    ) +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b %Y"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    )


ggplot(datYrs[datYrs$date > max(datYrs$date)-365,]) +
  geom_point(data=datRaw[datRaw$date > max(datYrs$date)-365 & !is.na(datRaw$NDVIReprojected),], aes(x=date, y=NDVIReprojected, color=type), size = 0.5) +
  # geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12*25)) +
  geom_ribbon(aes(x=date, ymin = YrLwr, ymax=YrUpr, fill=type), alpha=0.2) +
  geom_line(aes(x=date, y = YrMean, color=type)) +
  geom_vline(xintercept=as.Date("2025-01-01")) +
  scale_color_manual(values = paletteLC) +
  scale_fill_manual(values = paletteLC) +
  labs(
    x = "Date",
    y = "NDVI Value",
    title = "NDVI Trends Over Time for Selected Land Cover Types"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )


ggplot(datYrs[datYrs$date > max(datYrs$date)-30,]) +
  geom_point(data=datRaw[datRaw$date > max(datYrs$date)-30 & !is.na(datRaw$NDVIReprojected),], aes(x=date, y=NDVIReprojected, color=type), size = 0.5) +
  # geom_smooth(method="gam", formula=y~s(x, bs="cs", k=12*25)) +
  geom_ribbon(aes(x=date, ymin = YrLwr, ymax=YrUpr, fill=type), alpha=0.2) +
  geom_line(aes(x=date, y = YrMean, color=type)) +
  geom_vline(xintercept=as.Date("2025-01-01")) +
  scale_color_manual(values = paletteLC) +
  scale_fill_manual(values = paletteLC) +
  labs(
    x = "Date",
    y = "NDVI Value",
    title = "NDVI Trends Over Time for Selected Land Cover Types"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

