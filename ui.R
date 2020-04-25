#### Libraries  ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tools)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(maps)

#### UI ----

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             collapsible = TRUE,
             windowTitle = "COVIDiSTRESS Global Survey",
             title = div( img( src = "Covidistress2.jpg", width = 50,
                               style = "margin:-10px 5px" ), "COVIDiSTRESS Global Survey"),
             tabPanel("Sample Description",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(
                            'CountryChoice', h3("Choose Countries"), choices =  NULL,
                            options = list(create = TRUE, placeholder = 'Type country names'), multiple = TRUE
                          ),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Gender Distribution", plotlyOutput("PlotlyGender100")),
                            tabPanel("Age  Distribution", plotlyOutput("PlotlyAge")),
                            tabPanel("Distribution of Education Level", plotlyOutput("PlotlyEdu"))
                          )
                        )
                      )
             ),
             tabPanel("Results",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('MapRegionChoice', h3("Select Region"),
                                      choices = list("World" = 1, "Europe" = 2), selected = 1)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Isolation Score", plotlyOutput("PlotlyIsolationMap")),
                            tabPanel("Stress"),
                            tabPanel("Corona Concern")
                          )
                        )
                      )
             ),
             tabPanel("About")
  )
)