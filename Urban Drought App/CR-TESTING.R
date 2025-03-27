library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      
      box(width = 6, title = "Forest", status = "primary", solidHeader = TRUE,
          "Box content"
      ),
      
      uiOutput("cropBox"), uiOutput("cropBox2"),
      box(width = 6, title = span(icon("tree"), "Forest2"), status = "primary", solidHeader = TRUE,
          "Box content"
      )
      
    )
  )
)

# tags$style(HTML("
#                         .box.box-solid.box-primary>.box-header {
#                         # color:#fff;
#                         background:#4DAC26
#                         }
# 
#                         .box.box-solid.box-primary {
#                         background:#4DAC26;
#                         border-bottom-color:#4DAC26;
#                         border-left-color:#4DAC26;
#                         border-right-color:#4DAC26;
#                         border-top-color:#4DAC26;
#                         }
# 
#                         .box.box-solid.box-info>.box-header {
#                         color:#fff;
#                         background:#B8E186
#                         }
# 
#                         .box.box-solid.box-info {
#                         background:#B8E186;
#                         border-bottom-color:#B8E186;
#                         border-left-color:#B8E186;
#                         border-right-color:#B8E186;
#                         border-top-color:#B8E186;
#                         }
# 
#                         "))     
statTry <- c("primary", "info")
# Overwriting the box defaults with colors for the urban drought portal
graphing_colors<-c("Significantly Browner than Normal"= "#D01C8B" , "Slightly Browner than Normal"= "#F1B6DA", "Normal"="#BEBEBE", "Slightly Greener than Normal"= "#B8E186","Significantly Greener than Normal"="#4DAC26")

# Overwriting the default formatting for status boxes
# Sig. Browner than normal == "danger"
# Slight Browner than normal = "warning"
# Normal = "info"
# Slight Green = "primary"
# Sig. Green = "success"
tagsDrought <-     tags$style(HTML("
                      .box.box-solid.box-success>.box-header {
                        # color:#fff;
                        background:#4DAC26
                        }

                      .box.box-solid.box-success {
                        background:#4DAC26;
                        border-bottom-color:#4DAC26;
                        border-left-color:#4DAC26;
                        border-right-color:#4DAC26;
                        border-top-color:#4DAC26;
                        }

                      .box.box-solid.box-primary>.box-header {
                        # color:#fff;
                        background:#B8E186
                        }

                      .box.box-solid.box-primary {
                        background:#B8E186;
                        border-bottom-color:#B8E186;
                        border-left-color:#B8E186;
                        border-right-color:#B8E186;
                        border-top-color:#B8E186;
                        }
                        
                      .box.box-solid.box-info>.box-header {
                        # color:#fff;
                        background:#BEBEBE
                        }
                      .box.box-solid.box-info {
                          background:#BEBEBE;
                          border-bottom-color:#BEBEBE;
                          border-left-color:#BEBEBE;
                          border-right-color:#BEBEBE;
                          border-top-color:#BEBEBE;
                      }
                      
                      .box.box-solid.box-warning>.box-header {
                        # color:#fff;
                        background:#F1B6DA
                        }
                      .box.box-solid.box-warning {
                          background:#F1B6DA;
                          border-bottom-color:#F1B6DA;
                          border-left-color:#F1B6DA;
                          border-right-color:#F1B6DA;
                          border-top-color:#F1B6DA;
                      }
                      
                      .box.box-solid.box-danger>.box-header {
                        # color:#fff;
                        background:#D01C8B
                        }
                      .box.box-solid.box-danger {
                          background:#D01C8B;
                          border-bottom-color:#D01C8B;
                          border-left-color:#D01C8B;
                          border-right-color:#D01C8B;
                          border-top-color:#D01C8B;
                      }
                      
                                                

                        "))


server <- function(input, output) {
  
  output$cropBox <- renderUI({
    tags$div(class = "another-box", id = "cropBox",
             box(width = 6, title = "Crop", status = statTry[1], solidHeader = TRUE,
                 "Box content"
             ), tagsDrought)
  })
  output$cropBox2 <- renderUI({
    tags$div(class = "another-box", id = "cropBox2",
             box(width = 11, title = "Crop-Test", status = statTry[2], solidHeader = TRUE,
                 "Box content"
             )
             )

  })
  
}
# shinyApp(ui, server)
# shinyApp(ui, server)
shinyApp(ui = ui, server = server)

