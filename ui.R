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
library(countrycode)
library(wpp2019)
library(markdown)
data(pop)
data(e0F)
data(e0M)
source("get_world_maps.R")

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
                                      choices = list("World" = 1, "Africa" = 2, "Asia" = 3, "Europe" = 4, 
                                                     "North America" = 5, "South America" = 6, "Oceania" = 7), 
                                      selected = 1)
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Isolation Score", plotlyOutput("PlotlyIsolationMap")),
                            tabPanel("Stress Score", plotlyOutput("PlotlyStressMap")),
                            tabPanel("Trust Score", plotlyOutput("PlotlyTrustMap")),
                            tabPanel("Corona Concern Score", plotlyOutput("PlotlyCoronaConcernMap"))
                          )
                        )
                      )
             ),
             tabPanel("About", includeMarkdown("about.Rmd"))
  )
)
