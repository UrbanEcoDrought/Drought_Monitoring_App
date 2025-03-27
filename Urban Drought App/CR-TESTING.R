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
      box(width = 6, title = "Forest2", status = "primary", solidHeader = TRUE,
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

server <- function(input, output) {
  
  output$cropBox <- renderUI({
    tags$div(class = "another-box", id = "cropBox",
             box(width = 6, title = "Crop", status = statTry[1], solidHeader = TRUE,
                 "Box content"
             ),
    tags$style(HTML("
                        .box.box-solid.box-primary>.box-header {
                        # color:#fff;
                        background:#4DAC26
                        }

                        .box.box-solid.box-primary {
                        background:#4DAC26;
                        border-bottom-color:#4DAC26;
                        border-left-color:#4DAC26;
                        border-right-color:#4DAC26;
                        border-top-color:#4DAC26;
                        }
                                                

                        ")),
    tags$style(HTML("
      .box.box-solid.box-info>.box-header {
                        color:#fff;
                        background:#F1B6DA
                        }
      .box.box-solid.box-info {
          background:#F1B6DA;
          border-bottom-color:#F1B6DA;
          border-left-color:#F1B6DA;
          border-right-color:#F1B6DA;
          border-top-color:#F1B6DA;
      }"))

    )
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

