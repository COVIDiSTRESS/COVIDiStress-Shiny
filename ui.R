#### Libraries  ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(tools)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)

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
                            tabPanel("Age  Distribution", plotlyOutput("PlotlyAge")),
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