# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Fish Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stock Assessment Inputs", tabName = "inputs", icon = icon("bar-chart")),
      menuItem("Stock Assessment Results", tabName = "results", icon = icon("file-alt")),
      menuItem("Explore Fishery Dependent Data", tabName = "fisheryDependent", icon = icon("chart-line")),
      menuItem("Explore Fishery Independent Data", tabName = "fisheryIndependent", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "home",
              h2("American Plaice (Hippoglossoides platessoides)", class = "section-title"),
              fluidRow(
                column(width = 8,
                       div(class = "info-container",
                           div(class = "info-box",
                               h3("Appearance"),
                               p("American plaice has a diamond-shaped body with a pale, light brown color, often with darker spots. The eyes are located on the right side of the body, and the fish has both dorsal and anal fins that extend along the length of the body.")
                           ),
                           div(class = "info-box",
                               h3("Biology"),
                               p("American plaice is a demersal fish that lives on the seafloor. It feeds on smaller fish and invertebrates. The species has a lifespan of up to 20 years and reaches sexual maturity at around 3-4 years of age.")
                           ),
                           div(class = "info-box",
                               h3("Range"),
                               p("American plaice is found in the North Atlantic Ocean, from the eastern coast of North America to the waters off Greenland and Iceland. It is commonly found in shallow, coastal waters and deeper offshore areas.")
                           ),
                           div(class = "info-box",
                               h3("Management"),
                               p("Management of American plaice involves setting catch limits, monitoring stock assessments, and enforcing regulations to ensure sustainable fishing practices. Measures include quota systems, closed areas, and size limits to protect juvenile fish.")
                           ),
                           div(class = "info-box",
                               h3("Stock Status"),
                               p("American plaice is classified as a species of concern due to historical overfishing and habitat degradation. Management measures have been implemented to protect and rebuild the stock, including catch limits and seasonal closures.")
                           )
                       )
                ),
                column(width = 4,
                       tags$img(src = "american_plaice.jpg", height = "300px", width = "auto", alt = "American Plaice", class = "fish-image")
                )
              )
      ),
      tabItem(tabName = "inputs",
              h2("Stock Assessment Inputs", class = "section-title"),
              fluidRow(
                column(width = 6,
                       div(class = "info-container",
                           div(class = "info-box",
                               h3("Select Input Type"),
                               selectInput("inputType", "Choose Input Type:",
                                           choices = c("Index Data" = "index",
                                                       "Catch Data" = "catch",
                                                       "Natural Mortality Data" = "mortality",
                                                       "Maturity Data" = "maturity",
                                                       "Weight-at-Age Data" = "weight",
                                                       "Catchability Data" = "catchability",
                                                       "Selectivity Data" = "selectivity"))
                           ),
                           div(class = "info-box",
                               h3("Input Data Description"),
                               uiOutput("inputDescription")
                           ),
                           div(class = "info-box",
                               h3("Input Data Plot"),
                               plotOutput("inputPlot")
                           )
                       )
                ),
                column(width = 6,
                       div(class = "info-box",
                           h3("Input Data Table"),
                           tableOutput("dataTable")
                       )
                )
              )
      ),
      tabItem(tabName = "results",
              h2("Stock Assessment Results", class = "section-title"),
              tabsetPanel(
                id = "resultsTabs",
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Estimates"),
                                          plotOutput("biomassPlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Biomass Estimates: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock. Higher biomass typically suggests a healthier stock.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Rates"),
                                          plotOutput("fishingMortalityPlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality Rates: Definitions and Interpretation"),
                                          p("Fishing mortality rates show the proportion of the stock removed by fishing. Lower rates are usually more desirable to ensure stock sustainability.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          p("Recruitment measures the addition of new fish to the stock. Successful recruitment is critical for maintaining and growing the fish population.")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          p("Biomass reference points are used to gauge the health of the stock. These include target and limit values.")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points help to determine sustainable fishing levels. These points are crucial for effective fishery management.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Reference Points"),
                                          plotOutput("fishingMortalityReferencePlot")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Diagnostics",
                         fluidRow(
                           column(width = 8,
                                  div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      p("Diagnostics help to assess the performance and fit of the stock assessment model. This includes checking residuals, fit statistics, and validation results.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Interpretation"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis and comparison with observed data.")
                                  )
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "fisheryDependent",
              h2("Explore Fishery Dependent Data", class = "section-title"),
              tabsetPanel(
                id = "Scenario1Tabs",
                tabPanel("Input Change",
                         fluidRow(
                           column(width = 8,
                                  div(class = "info-box",
                                      h3("Fishery Dependent Data Input Change"),
                                      p("Decrease catch by 50%")
                                  ),
                                  div(class = "info-box",
                                      h3("Scenario Comparison Plot"),
                                      plotOutput("comparisonPlot")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Scenario Comparison Table"),
                                      tableOutput("dataTable2")
                                      
                                  )
                           )
                         )
                ),
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Estimates"),
                                          plotOutput("biomassPlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Biomass Estimates: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock. Higher biomass typically suggests a healthier stock.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Rates"),
                                          plotOutput("fishingMortalityPlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality Rates: Definitions and Interpretation"),
                                          p("Fishing mortality rates show the proportion of the stock removed by fishing. Lower rates are usually more desirable to ensure stock sustainability.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          p("Recruitment measures the addition of new fish to the stock. Successful recruitment is critical for maintaining and growing the fish population.")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          p("Biomass reference points are used to gauge the health of the stock. These include target and limit values.")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points help to determine sustainable fishing levels. These points are crucial for effective fishery management.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Reference Points"),
                                          plotOutput("fishingMortalityReferencePlot2")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Diagnostics",
                         fluidRow(
                           column(width = 8,
                                  div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      p("Diagnostics help to assess the performance and fit of the stock assessment model. This includes checking residuals, fit statistics, and validation results.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot2")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Interpretation"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis and comparison with observed data.")
                                  )
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "fisheryIndependent",
              h2("Explore Fishery Independent Data", class = "section-title"),
              tabsetPanel(
                id = "Scenario2Tabs",
                tabPanel("Input Change",
                         fluidRow(
                           column(width = 8,
                                  div(class = "info-box",
                                      h3("Fishery Independent Data Input Change"),
                                      p("Decrease index by 50%")
                                  ),
                                  div(class = "info-box",
                                      h3("Scenario Comparison Plot"),
                                      plotOutput("comparisonPlot2")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Scenario Comparison Table"),
                                      tableOutput("dataTable3")
                                      
                                  )
                           )
                         )
                ),
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Estimates"),
                                          plotOutput("biomassPlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Biomass Estimates: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock. Higher biomass typically suggests a healthier stock.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Rates"),
                                          plotOutput("fishingMortalityPlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality Rates: Definitions and Interpretation"),
                                          p("Fishing mortality rates show the proportion of the stock removed by fishing. Lower rates are usually more desirable to ensure stock sustainability.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          p("Recruitment measures the addition of new fish to the stock. Successful recruitment is critical for maintaining and growing the fish population.")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          p("Biomass reference points are used to gauge the health of the stock. These include target and limit values.")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points help to determine sustainable fishing levels. These points are crucial for effective fishery management.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality Reference Points"),
                                          plotOutput("fishingMortalityReferencePlot3")
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Diagnostics",
                         fluidRow(
                           column(width = 8,
                                  div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      p("Diagnostics help to assess the performance and fit of the stock assessment model. This includes checking residuals, fit statistics, and validation results.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot3")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Intrepretation"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis and comparison with observed data.")
                                  )
                           )
                         )
                )
              )
      )
    )
  )
)



