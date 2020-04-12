library(shiny)
library(shinythemes)
ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVIDiSURVEY",
             tabPanel("Sample Description",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Gender"),
                            tabPanel("Age"),
                            tabPanel("Edu")
                          )
                        )
                      )
             ),
             tabPanel("Results",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Isolation"),
                            tabPanel("Stress"),
                            tabPanel("Corona Concern")
                          )
                        )
                      )
             )
  )
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)