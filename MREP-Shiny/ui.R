# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Stock Assessment Dashboard"),
  dashboardSidebar(width=325,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stock Assessment Inputs", tabName = "inputs", icon = icon("bar-chart")),
      menuItem("Explore Current Assessment", tabName = "results", icon = icon("file-alt")),
      menuItem("Explore Impacts of Changes in Catch", tabName = "fisheryDependent", icon = icon("chart-line")),
      menuItem("Explore Impacts of Changes in Indices", tabName = "fisheryIndependent", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "home",
                tabsetPanel(
                  id = "infoTabs",
                  tabPanel(
                    "About",
                    fluidRow(
                      column(
                        width = 12,
                        div(
                          class = "info-container",
                          div(
                            class = "info-box",
                            style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;",
                            h3("Interactive Stock Assessment Dashboard", style = "color: #00608a; font-weight: bold;"),
                            p("This interactive stock assessment modeling tool helps users understand how different data elements and uncertainties impact stock assessment models.",
                              style = "font-size: 16px; line-height: 1.5;"),
                            hr(),
                            h4("Key Questions Addressed", style = "color: #343a40; margin-top: 20px;"),
                            p(tags$ul(
                              tags$li("What are the inputs needed for a stock assessment? How are they used?"),
                              tags$li("What do assessment results look like? How do we test the performance of a stock assessment?"),
                              tags$li("How does changing inputs alter the results and performance of the assessments?")
                            )),
                            p("The tool uses American Plaice as an illustrative example.",
                              style = "font-style: italic; color: #6c757d; margin-top: 20px;"),
                            hr(),
                            p("Explore the tool to learn more about the dynamics of stock assessments!",
                              style = "text-align: center; font-weight: bold; margin-top: 20px;")
                          )
                        )
                      )
                  )
                  )
                  ,
                  tabPanel("American Plaice",
                           fluidRow(
                             column(width = 8,
                       div(class = "info-container",
                           div(class = "info-box",
                               h3("Appearance"),
                               p("American plaice is a right-eyed flounder. They have a large mouth, rounded tail, and straight lateral line. They are reddish-brown on their eyed side and white on their underside. "),
                                br(),br(),
                               h3("Biology"),
                               p("American plaice can live for more than 20 years. They feed on smaller fish and invertebrates."),
                               br(),br(),
                               h3("Range"),
                               p("American plaice are found in the North Atlantic Ocean, ranging from southern Labrador to Rhode Island. They are distributed throughout the Gulf of Maine and Georges Bank."),
                               br(),br(),
                               h3("Habitat"),
                               p("American plaice are found in waters between 130 and 980 feet deep and live on the ocean bottom in areas covered by sand, mud, or gravel."),
                               br(),br(),
                               h3("Fishery"),
                               p("American Plaice commercial landings totaled 1.5 million pounds in 2022, valued at $2.5 million. They are typically harvested using trawl nets and not commonly encounterd in the recreational fishery."),
                               br(),br(),
                               h3("Management"),
                               p("American Plaice is managed by NOAA Fisheries and the New England Fishery Management Council. Management of American plaice involves setting catch limits, minimum sizes, and time/area closures. Regular stock assessment are conducted to monitor the stock."),
                               br(),br(),
                               h3("Stock Status"),
                               p("American plaice is current not overfished and not experiencing overfishing. A rebuilding plan was put in place in 2004 and the stock was rebuilt in 2019.")
                           )
                       )
              
                ),
                column(width = 4,
                       tags$img(src = "american_plaice.jpg", height = "300px", width = "auto", alt = "American Plaice", class = "fish-image")
                )
                           )
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
                                           choices = c("Index" = "index",
                                                       "Catch" = "catch",
                                                       "Natural Mortality" = "mortality",
                                                       "Maturity" = "maturity",
                                                       "Weight-at-Age" = "weight",
                                                       "Catchability" = "catchability",
                                                       "Selectivity" = "selectivity"))
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
                                          h3("Biomass"),
                                          plotOutput("biomassPlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Biomass: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock."), 
                                          p("Spawning stock biomass (SSB) is the biomass of reproductively mature fish."), 
                                          p("For American Plaice estimates show a large decrease in the 1980s followed by a gradual increase through the last few decades.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality Definitions and Interpretation"),
                                          p("Fishing mortality (F) is the rate at which fish die due to fishing activity."), 
                                          p("For American Plaice estimates show F coming up to a peak in the mid-1980s then gradual decreasing over the next few decades.")
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
                                          p("Recruitment measures the addition of new fish to the stock."),
                                          p("For American Plaice recrtuiment has been mostly stable throughout the time series.")
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
                                          p("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished)."),
                                          p("These include target (solid grey line) and threshold (dashed grey line) values.") ,
                                          p("For American Plaice biomass has stayed above the threshold and above the target in more recent decades.")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).") ,
                                          p("For American Plaice F was over the threshold (overfishing) for the beginning of the time series then below the threshold (not overfishing) after the mid-1990s and for the remainder of the time series.")
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
                                      p("Diagnostics help to assess the performance and fit of the stock assessment model.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Interpretation"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis.")
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
                                      p("This scenario compares the regular catch (green) to catch decreased by 50% (blue) to show the impact of catch on stock assessment results.")
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
                                          h3("Biomass: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock."),
                                          p("When catch is decreased by 50% the biomass estimate is also decreased by 50%")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality: Definitions and Interpretation"),
                                          p("Fishing mortality (F) is the rate at which fish die due to fishing activity."),
                                          p("When catch is decreased by 50% the fishing mortality rate does not change."),
                                          p("This occurs becasue the proportion of fish being removed is still the same since biomass also decreased by 50%.")
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
                                          p("Recruitment measures the addition of new fish to the stock."),
                                          p("When catch is decreased by 50% the recruitment also decreases by 50%.")
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
                                          p("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished)."),
                                          p("The biomass thresholds are depicted for regular catch (solid grey line) and decreased catch (dashed grey line).") ,
                                          p("When catch is decreased by 50% the reference point also decreases and the stock status remains the same.")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).") ,
                                          p("The fishing mortaliy thresholds are depicted for regular catch (solid grey line) and decreased catch (dashed grey line)."),
                                          p("When catch is decreased by 50% the reference point remains the same.")
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
                                      p("Diagnostics help to assess the performance and fit of the stock assessment model.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot2")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Interpretation"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis.")
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
                                      p("This scenario compares the regular indices (solid lines) to indices decreased by 50% (dashed lines) to show the impact of indices on stock assessment results.")
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
                                          h3("Biomass"),
                                          plotOutput("biomassPlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Biomass: Definitions and Interpretation"),
                                          p("Biomass estimates provide an indication of the total amount of fish in the stock."),
                                          p("When indices are decreased by 50% the biomass estimate remains the same.")
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Fishing Mortality: Definitions and Interpretation"),
                                          p("Fishing mortality (F) is the rate at which fish die due to fishing activity."),
                                          p("When indices are decreased by 50% the fishing mortality rate remains the same.")
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
                                          p("Recruitment measures the addition of new fish to the stock."),
                                          p("When indices are decreased by 50% the recruitment remains the same.")                                      )
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
                                          p("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished)."),
                                          p("The biomass thresholds are depicted for regular indices (solid grey line) and decreased indices (dashed grey line).") ,
                                          p("When indices are decreased by 50% the reference point and stock status remains the same.")                                      )
                                  )
                           ),
                           column(width = 6,
                                  div(class = "info-container",
                                      div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          p("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).") ,
                                          p("The fishing mortaliy thresholds are depicted for regular indices (solid grey line) and decreased indices (dashed grey line)."),
                                          p("When indices are decreased by 50% the reference point and stock status remains the same")
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
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis")
                                  )
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "about", 
              h2("About", class = "section-title"),
              fluidRow(
                column(width = 6,
                       div(class = "info-container",
                           div(class = "info-box",
                               h3("Project Goal"),
                               p("Develop an intutitive and interactive stock assessment modeling tool to improve understanding of how different data elements and uncertainties impact an analytical model.")
                           ),
                           div(class = "info-box",
                               h3("Project Team"),
                               p("PI: Dr. Lisa Kerr"),
                               p("Analyst and web application developer: Jerelle Jesse")
                           ),
                           div(class = "info-box",
                               h3("Funding Support"),
                               p("New England Fishery Management Council")
                           ),
                           div(class = "info-box",
                               h3("Acknowledgements"),
                               p("MREP Northeast Steering Committee"),
                               p("Gulf of Maine Research Institute")
                           )
                       )
                ),
                column(width = 4,
                       br(),
                       tags$img(src = "umaine.jpg", height = "200px", width = "auto", alt = "", class = ""),
                       br(),br(),br(),
                       tags$img(src = "nefmc.jpg", height = "71px", width = "auto", alt = "", class = ""),
                       br(),br(),br(),
                       tags$img(src = "gmri.jpg", height = "117px", width = "auto", alt = "", class = ""),
                       
              )
         )
       )
    )
  )
)


