#### Libraries  ----
library(shiny)
library(shinyWidgets)

#### UI ----

ui <- fluidPage(theme = shinytheme("darkly"),
  
  
  ## title panel ----
  titlePanel(paste0("Visualisation of Covid-19 data up to ", format(Sys.Date()-2, "%Y-%m-%d"))),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        'CountryChoice', h3("Choose Countries"), choices =  NULL,
        options = list(create = TRUE, placeholder = 'Type country names'), multiple = TRUE
      ),
      dateInput("Start_Date", "Start Date", value = "2020-03-01"),
      hidden(
        checkboxInput("CB_log", "1st panel - Log scale?", FALSE),
        checkboxInput("CB_ratio", "1st panel - ratio for 100.000 inhabitants?", FALSE)),
      checkboxInput("CheckBox_percent", "2nd panel - Increase in percent? (default: absolute)", FALSE),
      hidden(
        sliderInput("slider_lissage", h4("Smoothing strength"), min = 1, max = 5, value = 1),
        numericInput("num_Relative_case", h4("Starting at case n ? (0: no shift)"), value=0),
        numericInput("num_Relative_death", h4("Starting at death ? (0: no shift)"), value=0)),
      numericInput("prediction_data", h4("Predection forward: length of data"), value=10),
      numericInput("prediction_forward", h4("Predection forward: how far forward?"), value=5),
      helpText(tags$a(href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide", "Data from ECDC"))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                #tabPanel("resultat logo2", uiOutput("myGrid2")),
                tabPanel("Total cases and deaths", plotOutput("PlotCasesCum"),  plotOutput("PlotDeathsCum")),
                tabPanel("Increase in cases and deaths",plotOutput("PlotCasesRel"),  plotOutput("PlotDeathsRel")),#,
                tabPanel("Data", tableOutput("tableDf")))
    )
  )
)