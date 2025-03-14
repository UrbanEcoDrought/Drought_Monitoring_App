# Checking patterns with some static versions of figures that will be on the app
library(ggplot2)

if(!dir.exists("../figs")) dir.create("../figs")

yrDrought <- c(2005, 2012, 2023)

LClevels <- c("crop"="#ab6c28", "forest"="#68ab5f", "grassland"="#68ab5f", "urban-open"="#dec5c5", "urban-low"="#d99282", "urban-medium"="#eb0000", "urban-high"="#ab0000")

trendLevels <- c("Getting Browner"="tan3", 
                 "Normal"="gray", 
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
                     "Abnormal Greening"="green3")

# Read in the data
datYrs <- read.csv("../data_all/NDVIall_years_modeled.csv")
datYrs$type <- factor(datYrs$type, levels=names(LClevels))
datYrs$YrDerivTrend <- factor(datYrs$YrDerivTrend, levels=names(trendLevels))
datYrs$FlagNDVI <- factor(datYrs$FlagNDVI, levels=names(ndviFlagLevels))
datYrs$FlagTrend <- factor(datYrs$FlagTrend, levels=names(trendFlagLevels))
summary(datYrs)

datNorms <- read.csv("../data_all/NDVIall_normals_modeled.csv")
datNorms$type <- factor(datNorms$type, levels=names(LClevels))
summary(datNorms)

yrNow <- max(datYrs$year)


# Doing a test time series graph
LC="urban-medium"
yrColors <- c("2025"="dodgerblue2", "2024"="cadetblue3", "2023"="orange2", "2012"="red3", "2005"="goldenrod2")

png("../figs/TimeSeries-Test.png", height=8, width=12, units = "in", res=320)
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
png("../figs/HeatMap-Test.png", height=8, width=12, units = "in", res=320)
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
  labs(x = "Month of Year", y = "Year", title = paste0("Heat Map")) +
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
