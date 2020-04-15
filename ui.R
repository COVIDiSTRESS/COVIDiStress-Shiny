#### Libraries  ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tools)
library(ggplot2)
library(plotly)
library(dplyr)

#### UI ----

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVIDiSURVEY",
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
                            #tabPanel("Gender", plotOutput("PlotGender100")),
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