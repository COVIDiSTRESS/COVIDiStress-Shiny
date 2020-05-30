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
                                      choices = list("World" = 1, "Africa" = 2, "America" = 3,
                                          "Asia" = 4, "Europe" = 5, "Oceania" = 6), 
                                      selected = 1),
                          selectInput('ConcernChoice', h3("Concern for who?"),
                                      choices = list("Themself" = 1, "My family" = 2, "My friends" = 3,
                                                     "My country" = 4, "Other countries" = 5), 
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
             tabPanel("About")
  )
)