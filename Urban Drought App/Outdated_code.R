#Housing for any outdated code incase we decide to use it 

####################################################################################################################
#Code for "Percentiles (Broader Context) Tab
#Reason: A bit confusing at this time (03/2025) & could be better developed later down the line 
#UI CODE
menuSubItem("Percentiles (Broader Context)",
            tabName = "percentile_broad")

tabItem(tabName = "percentile_broad", 
        tabBox(
          height = "3000px",
          width = 12,
          tabPanel("Crop Percentile & Boxplot",#CROP
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Crop Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   # The checkboxGroupInput with custom CSS
                   checkboxGroupInput("selected_years_crop", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_crop_broad")),  
                   plotOutput("crop_boxplot")
          ),
          tabPanel("Forest Percentile & Boxplot",#FOREST
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Forest Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   # The checkboxGroupInput with custom CSS
                   checkboxGroupInput("selected_years_for", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_for_broad")),  
                   plotOutput("for_boxplot")
          ),
          tabPanel("Grass Percentile & Boxplot",#GRASS
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Grass Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   # The checkboxGroupInput with custom CSS
                   checkboxGroupInput("selected_years_grass", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_grass_broad")),  
                   plotOutput("grass_boxplot")
          ),
          tabPanel("Urban-High Percentile & Boxplot",#URBAN-HIGH
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Urban-high Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   # The checkboxGroupInput with custom CSS
                   checkboxGroupInput("selected_years_uh", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_uh_broad")),  
                   plotOutput("uh_boxplot")
          ),
          tabPanel("Urban-Medium Percentile & Boxplot",#URBAN-MEDIUM
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Urban-medium Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   # The checkboxGroupInput with custom CSS
                   checkboxGroupInput("selected_years_um", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_um_broad")),  
                   plotOutput("um_boxplot")
          ),
          tabPanel("Urban-Low Percentile & Boxplot",#URBAN-LOW
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Urban-low Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   checkboxGroupInput("selected_years_ul", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  
                   h6(htmlOutput("percentile_ul_broad")),  
                   plotOutput("ul_boxplot")
          ),
          tabPanel("Urban-Open Percentile & Boxplot",#URBAN-OPEN
                   h6(tags$b(paste("Most Recent Data is from", format(date_needed, "%B %d, %Y")))),
                   br(),
                   h6(HTML("<u>Urban-open Percentile Current NDVI data compared to past observed data (selected years) & boxplot of data distribution:</u>")),
                   
                   checkboxGroupInput("selected_years_uo", "Select Years:", 
                                      choices = unique(NDVI_data$year), 
                                      selected = unique(NDVI_data$year)[1:5],
                                      inline = TRUE),  #
                   h6(htmlOutput("percentile_uo_broad")),  
                   plotOutput("uo_boxplot")
          )
        )
) 



#SERVER CODE
#Function Code 
ndvi_percentile_broad <- function(LCtype, NDVI_data, CI_csv, most_recent_data) {
  # Filter data based on land cover type
  NDVI_subset <- filter(NDVI_data, type == LCtype)
  CI_subset <- filter(CI_csv, type == LCtype)
  most_recent_subset <- filter(most_recent_data, type == LCtype)
  
  # Ensure NDVI_subset is a data frame and contains 'ReprojPred' column
  if (!is.data.frame(NDVI_subset) || !"ReprojPred" %in% colnames(NDVI_subset)) {
    stop("NDVI_subset is not a data frame or does not have 'ReprojPred' column!")
  }
  
  # Group by year and calculate percentiles
  percentiles_by_year <- NDVI_subset %>%
    group_by(year) %>%
    summarise(percentile = ecdf(ReprojPred)(mean(ReprojPred)) * 100) %>%
    arrange(desc(year))  # Sorting by most recent year
  
  return(percentiles_by_year)
}


#Server Code for Percentiles display
output$percentile_crop_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_crop)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("crop", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Crop NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Crop Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})


output$percentile_for_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_for)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("forest", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Forest NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Forest Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})
output$percentile_grass_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_grass)
  print(head(NDVI_data_yearly))
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("grassland", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Grass NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Grass Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})
output$percentile_uh_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_uh)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("urban-high", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Urban-High NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Urban-High Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})
output$percentile_um_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_um)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("urban-medium", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Urban-Medium NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Urban-Medium Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})
output$percentile_ul_broad <- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_ul)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("urban-low", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Urban-Low NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Urban-Low Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})
output$percentile_uo_broad<- renderText({
  req(NDVI_data, CI_csv, most_recent_data, input$selected_years)
  
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_uo)
  
  # Compute percentiles per year
  percentiles <- ndvi_percentile_broad("urban-open", NDVI_data_yearly, CI_csv, most_recent_data)
  
  if (is.null(percentiles) || nrow(percentiles) == 0 || all(is.na(percentiles$percentile))) {
    return("Urban-Open NDVI Percentiles: Data Unavailable")
  } else {
    percentiles <- percentiles %>% arrange(desc(year))
    # Format output to display on new lines with HTML <br> tags
    percentile_text <- paste("Urban-Open Current NDVI Percentiles by Year:",
                             paste(percentiles$year, round(percentiles$percentile, 1), "%", sep = " ", collapse = "<br>"), 
                             sep = "<br>")
    return(HTML(percentile_text))  # Ensure that HTML tags are interpreted
  }
})

#BOXPLOTS
output$crop_boxplot <- renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_crop)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Crop NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})

output$for_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_for)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Forest NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})
output$grass_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_grass)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Grass NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})
output$uh_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_uh)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Urban-High NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})
output$um_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_um)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Urban-Medium NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})
output$ul_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_ul)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Urban-Low NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})
output$uo_boxplot <-renderPlot({
  req(NDVI_data)  # Ensure NDVI_data is available
  NDVI_data_yearly <- NDVI_data %>% filter(year %in% input$selected_years_uo)
  # Reorder the 'year' variable to have the most recent year on the left
  NDVI_data_yearly$year <- factor(NDVI_data_yearly$year, levels = sort(unique(NDVI_data_yearly$year), decreasing = TRUE))
  
  boxplot(ReprojPred ~ year, data = NDVI_data_yearly, 
          main = "Urban-Open NDVI Distribution by Year",
          xlab = "Year", 
          ylab = "NDVI (ReprojPred)",
          col = "mistyrose2")
})

####################################################################################################################
#CI graphs from Shiny App code 
#Reason it's here: replaced with density plots, might be more insightful than CI's at the moment
#UI
tabBox(
  tabPanel(
    "CI for All LC Types",
    plotOutput("all_LC_CI_graph")
  ),
  tabPanel(
    "CI for Selected LC Types",
    plotOutput("selected_LC_CI_graph"),
    checkboxGroupInput(
      inputId = "LC_type",
      label = "Select Landcover Types",
      choices = c(
        "Crop" = "crop",
        "Forest" = "forest",
        "Grassland" = "grassland",
        "Urban High" = "urban-high",
        "Urban Medium" = "urban-medium",
        "Urban Low" = "urban-low",
        "Urban Open" = "urban-open"
      ),
      selected = c("crop", "forest", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open"),
      inline = TRUE
    )
  )
)
#server code 
output$selected_LC_CI_graph <- renderPlot({
  req(input$LC_type)  # Ensure at least one LC type is selected
  selected_LC_CI_graph(input$LC_type)  # Pass selected LC types to the function
})

#CI function code
#All 7 LC types 95% CI graph
all_LC_CI_graph <-function(){
  
  ggplot(CI_csv, aes(x = yday, y = mean, color = type)) + 
    geom_line(size = 1) +  
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = type), alpha = 0.2) + 
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695",
      "grassland" = "#f46d43"
    )) +
    scale_fill_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(title = "95% Confidence Intervals for All LC Type over 365 Days", x = "Day of Year", y = "Mean Value") +
    theme_minimal()
  
}

#95% CI for selected LC Types
selected_LC_CI_graph <- function(LC_types){
  LC_CI <- CI_csv %>%
    filter(type %in% LC_types)  # Filter multiple selected types
  
  ggplot(LC_CI, aes(x = yday, y = mean, color = type, fill = type)) + 
    geom_line(size = 1) +  
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +  
    scale_color_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    scale_fill_manual(values = c(
      "crop" = "#a50026",
      "forest" = "#d73027",
      "grassland" = "#f46d43",
      "urban-high" = "#fee090",
      "urban-medium" = "#74add1",
      "urban-low" = "#4575b4",
      "urban-open" = "#313695"
    )) +
    labs(title = "95% Confidence Intervals for Selected LC Type(s) Over 365 Days", 
         x = "Day of Year", 
         y = "Mean Value") +
    theme_minimal() +
    theme(legend.title = element_blank())  # Removes the legend title for better appearance
}

####################################################################################################################
#Below does the same as the density_plot function, keeping code here in case we need to troubleshoot updates are working correctly 

LC_naming<- c("forest", "crop", "grassland", "uh", "um", "ul", "uo")
LC_types<- c("forest", "crop", "grassland", "urban-high", "urban-medium", "urban-low", "urban-open")

#Seperating NDVI data based on LC type
crop_NDVI <- filter(NDVI_data, type == "crop")
forest_NDVI <- filter(NDVI_data, type == "forest")
grassland_NDVI <- filter(NDVI_data, type == "grassland")
uh_NDVI <- filter(NDVI_data, type == "urban-high")
um_NDVI <- filter(NDVI_data, type == "urban-medium")
ul_NDVI <- filter(NDVI_data, type == "urban-low")
uo_NDVI <- filter(NDVI_data, type == "urban-open")

#putting NDVI_data in order by date
NDVI_data <-NDVI_data[order(as.Date(NDVI_data$date, format="%Y-%m-%d"), decreasing = TRUE), ]

head(NDVI_data)

#Pulling yday of latest data (most recent day) 

#finding latest day & pulling date
latest_day<-head(NDVI_data, 1)
date_needed <-latest_day$date

#pulling any rows with matching date 
most_recent_data<- filter(NDVI_data, date == date_needed)

crop_most_recent <- filter(most_recent_data, type == "crop")
forest_most_recent <-filter(most_recent_data, type == "forest")
grassland_most_recent <-filter(most_recent_data, type == "grassland")
uh_most_recent <-filter(most_recent_data, type == "urban-high")
um_most_recent <-filter(most_recent_data, type == "urban-medium")
ul_most_recent <-filter(most_recent_data, type == "urban-low")
uo_most_recent <-filter(most_recent_data, type == "urban-open")


#Pulling corresponding upper/lower bound and mean for LC types
crop_CI_info <- filter(CI_csv, type == "crop")
forest_CI_info <- filter(CI_csv, type == "forest")
grassland_CI_info <- filter(CI_csv, type == "grassland")
uh_CI_info <- filter(CI_csv, type == "urban-high")
um_CI_info <- filter(CI_csv, type == "urban-medium")
ul_CI_info <- filter(CI_csv, type == "urban-low")
uo_CI_info <- filter(CI_csv, type == "urban-open")


#Pulling only data from matching yday
c <-filter(crop_CI_info, yday == most_recent_data$yday)
f<-filter(forest_CI_info, yday == most_recent_data$yday)
g<-filter(grassland_CI_info, yday == most_recent_data$yday)
uh<-filter(uh_CI_info, yday == most_recent_data$yday)
um<-filter(um_CI_info, yday == most_recent_data$yday)
ul<-filter(ul_CI_info, yday == most_recent_data$yday)
uo<-filter(uo_CI_info, yday == most_recent_data$yday)


#Density plot for each LC type

#Need this to make the legend
legend_data <- data.frame(
  Type = c("Lower Bound", "Upper Bound", "Mean", "Current NDVI"),
  x = c(NA, NA, NA, NA),  
  y = c(NA, NA, NA, NA)  
)

crop_density <- ggplot(crop_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) +  
  
  # Add the bounds as dashed lines with legend
  geom_vline(aes(xintercept = c$lwr, linetype = "Lower Bound"), color = "#40004b", size = 1) +
  geom_vline(aes(xintercept = c$upr, linetype = "Upper Bound"), color = "#40004b", size = 1) +
  
  # Mean point
  geom_point(aes(x = c$mean, y = 0, shape = "Mean"), color = "#40004b", size = 4) +
  
  # Current NDVI point (diamond)
  geom_point(aes(x = crop_most_recent$NDVI, y = 0, shape = "Current NDVI"), fill = "#1b7837", color = "#1b7837", size = 4) +
  
  # Labels
  labs(
    x = "Crop Density Plot",
    y = "Density",
    linetype = "Bound Type",  # Legend title for the lines
    shape = "Point Type"      # Legend title for the points
  ) +
  
  # Manual legend adjustments
  scale_linetype_manual(values = c("Lower Bound" = "dashed", "Upper Bound" = "dashed")) +
  scale_shape_manual(values = c("Mean" = 16, "Current NDVI" = 23)) +
  
  theme_minimal()

forest_density <- ggplot(forest_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = f$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = f$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = f$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = forest_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Forest Density Plot",
    y= "Density"
  )+
  theme_minimal()

grassland_density <-ggplot(grassland_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = g$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = g$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = g$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = grassland_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Grassland Density Plot",
    y= "Density"
  )+
  theme_minimal()

uh_density <-ggplot(uh_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = uh$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = uh$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = uh$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = uh_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-High Density Plot",
    y= "Density"
  )+
  theme_minimal()

um_density <-ggplot(um_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = um$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = um$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = um$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23, fill = "#1b7837", aes( x = um_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Medium Density Plot",
    y= "Density"
  )+
  theme_minimal()

ul_density <-ggplot(ul_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = ul$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = ul$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = ul$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = ul_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Low Density Plot",
    y= "Density"
  )+
  theme_minimal()

uo_density <-ggplot(uo_NDVI, aes(x = NDVI)) + 
  geom_density(fill = "#c2a5cf", alpha = 0.5) + 
  geom_vline(xintercept = uo$lwr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_vline(xintercept = uo$upr, linetype = "dotted", color = "#40004b", size = 1.5) +
  geom_point(aes(x = uo$mean, y = 0), color = "#40004b", size = 4) +
  geom_point(shape = 23,fill = "#1b7837", aes( x = uo_most_recent$NDVI, y = 0), color = "#1b7837", size = 4) +
  labs(
    x= "Urban-Open Density",
    y= "Density"
  )+
  theme_minimal()

#Testing the plots
crop_density 
forest_density 
grassland_density
uh_density 
um_density 
ul_density 
uo_density

#Getting difference between mean and current status (current value - mean), if current value is lower than mean it'll be negative
c_status <- round((crop_most_recent$NDVI - c$mean), digits = 5)
f_status <- round((forest_most_recent$NDVI - f$mean), digits = 5)
g_status <- round((grassland_most_recent$NDVI - g$mean), digits = 5)
uh_status <- round((uh_most_recent$NDVI - uh$mean), digits = 5)
um_status <- round((um_most_recent$NDVI - um$mean), digits = 5)
ul_status <- round((ul_most_recent$NDVI- ul$mean), digits = 5)
uo_status <- round((uo_most_recent$NDVI - uo$mean), digits = 5)

########################################################################################################################
#Code for gallery - just not sure if its essential

#UI code
tabItem(tabName = "for_graphics",
        fluidRow(
          uiOutput("for_gallery") 
        ))
tabItem(tabName = "crop_graphics",
        fluidRow(
          uiOutput("crop_gallery") 
        ))
tabItem(tabName = "grass_graphics",
        fluidRow(
          uiOutput("grass_gallery") 
        ))
tabItem(tabName = "uh_graphics",
        fluidRow(
          uiOutput("uh_gallery") 
        ))
tabItem(tabName = "um_graphics",
        fluidRow(
          uiOutput("um_gallery") 
        ))
tabItem(tabName = "ul_graphics",
        fluidRow(
          uiOutput("ul_gallery") 
        ))
tabItem(tabName = "uo_graphics",
        fluidRow(
          uiOutput("uo_gallery") 
        ))
##############################

#server code 
###########forest###############
output$for_gallery <- renderUI({
  graphic_return(for_files, "for")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "for", "forest", output)

###########crop###############

output$crop_gallery <- renderUI({
  graphic_return(crop_files, "crop")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "crop", "crop", output)
###########grass###############
output$grass_gallery <- renderUI({
  graphic_return(grass_files, "grass")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "grass", "grass", output)
###########urban###############
output$uh_gallery <- renderUI({
  graphic_return(uh_files, "uh")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "uh", "urban-high", output)

output$um_gallery <- renderUI({
  graphic_return(um_files, "um")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "um", "urban-medium", output)


output$ul_gallery <- renderUI({
  graphic_return(ul_files, "ul")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "ul", "urban-low", output)

output$uo_gallery <- renderUI({
  graphic_return(uo_files, "uo")
  do.call(fluidRow, img_list)
})

graphic_formatting(for_files, "uo", "urban-open", output)

sliderValues <- reactive({
  
  data.frame(
    Name = c("Integer",
             "Decimal",
             "Range",
             "Custom Format",
             "Animation"),
    Value = as.character(c(input$integer,
                           input$decimal,
                           paste(input$range, collapse = " "),
                           input$format,
                           input$animation)),
    stringsAsFactors = FALSE)
  
})

#function code
#Gallery functions (not working at the moment 2/17 - but not sure if graphics are needed at all)
LC_part <- c("for", "crop", "grass", "uh", "um", "ul", "uo")

graphic_return <- function(LC_file, LC_part) {
  if (length(LC_file) == 0) {
    return(tags$p("No images found."))
  }
  
  img_list <- lapply(seq_along(LC_file), function(i) {
    column(
      width = 7,  # Adjust column width as needed (12 columns per row)
      imageOutput(paste0(LC_part[i], "_image", i))
    )
  })
  
  return(img_list)
}

graphic_formatting <- function(for_files, type, category, output) {
  output$plot <- renderPlot({
    ggplot(for_files, aes(x = year, y = NDVI)) +
      geom_line() +
      ggtitle(paste("NDVI Trends for", type, category))
  })
}
########################################################################################################################