# Load required libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(ggplot2)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "I-FSH "),
  dashboardSidebar(width=300,
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
    #shinyjs::useShinyjs(),
    tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    # tags$style(HTML(" 
    #  #full-page-spinner {
    #     position: fixed;
    #     top: 0;
    #     left: 0;
    #     width: 100%;
    #     height: 100%;
    #     background-color: rgba(255, 255, 255, 0.8);
    #     z-index: 1000;
    #     display: none; /* Ensure it's hidden initially */
    #     display: flex; /* Added display flex for debugging */
    #     flex-direction: column;
    #     justify-content: center;
    #     align-items: center;
    #     text-align: center;
    #   }
    #   
    #   .spinner-icon {
    #     border: 16px solid #f3f3f3;
    #     border-top: 16px solid #00608a;
    #     border-radius: 50%;
    #     width: 120px;
    #     height: 120px;
    #     animation: spin 2s linear infinite;
    #   }
    #   
    #   .spinner-text {
    #     margin-top: 20px;
    #     font-size: 24px;
    #     font-weight: bold;
    #     color: #00608a;
    #   }
    #   
    #   @keyframes spin {
    #     0% { transform: rotate(0deg); }
    #     100% { transform: rotate(360deg); }
    #   }
    #   "))
     ),
    tabItems(
      tabItem(tabName = "home",
            tabsetPanel(
              id = "infoTabs",
              tabPanel(
                "About",
                fluidRow(
                  column(
                    width = 12,
                    tags$div(class = "info-container",
                      tags$div(class = "info-box",
                        style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px;",
                        h3("Interactive Fisheries Stock Assessment Hub", style = "color: #00608a; font-weight: bold;"),
                        tags$p("This interactive stock assessment modeling tool helps users understand how different data elements and uncertainties impact stock assessment models.",
                          style = "font-size: 18px; line-height: 1.5!important;"),
                        hr(),
                        h4("Key Questions Addressed", style = "color: #343a40; margin-top: 20px;"),
                        tags$p(tags$ul(
                          tags$li("What are the inputs needed for a stock assessment? How are they used?", style= "font-size: 18px"),
                          tags$li("What do assessment results look like? How do we test the performance of a stock assessment?",  style= "font-size: 18px"),
                          tags$li("How does changing inputs alter the results and performance of the assessments?",  style= "font-size: 18px")
                        )),
                        tags$p("The tool uses American Plaice as an illustrative example.",
                          style = "font-style: italic; color: #6c757d; margin-top: 20px;"),
                        hr(),
                        tags$p("Explore the tool to learn more about the dynamics of stock assessments!",
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
                         h3("American Plaice (Hippoglossoides platessoides)", style = "color: #00608a; font-weight: bold;"),
                       tags$div(class = "info-container",
                           tags$div(class = "info-box",
                               h3("Appearance"),
                               tags$p("American plaice is a right-eyed flounder. They have a large mouth, rounded tail, and straight lateral line. They are reddish-brown on their eyed side and white on their underside. "),
                                br(),br(),
                               h3("Biology"),
                               tags$p("American plaice can live for more than 20 years. They feed on smaller fish and invertebrates."),
                              br(),br(),
                               h3("Range"),
                               tags$p("American plaice are found in the North Atlantic Ocean, ranging from southern Labrador to Rhode Island. They are distributed throughout the Gulf of Maine and Georges Bank."),
                              br(),br(),
                               h3("Habitat"),
                               tags$p("American plaice are found in waters between 130 and 980 feet deep and live on the ocean bottom in areas covered by sand, mud, or gravel."),
                              br(),br(),
                               h3("Fishery"),
                               tags$p("American Plaice commercial landings totaled 1.5 million pounds in 2022, valued at $2.5 million. They are typically harvested using trawl nets and not commonly encounterd in the recreational fishery."),
                              br(),br(),
                               h3("Management"),
                               tags$p("American Plaice is managed by NOAA Fisheries and the New England Fishery Management Council. Management of American plaice involves setting catch limits, minimum sizes, and time/area closures. Regular stock assessment are conducted to monitor the stock."),
                              br(),br(),
                               h3("Stock Status"),
                               tags$p("American plaice is current not overfished and not experiencing overfishing. A rebuilding plan was put in place in 2004 and the stock was rebuilt in 2019.")
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
                column(width = 8,
                       tags$div(class = "info-container",
                                tags$div(class = "info-box",
                               h3("Select Input Type"),
                               selectInput("inputType", "",
                                           choices = c("Stock Index" = "index",
                                                       "Catch" = "catch",
                                                       "Natural Mortality" = "mortality",
                                                       "Maturity" = "maturity",
                                                       "Weight-at-Age" = "weight",
                                                       "Catchability" = "catchability",
                                                       "Selectivity" = "selectivity"))
                           ),
                           tags$div(class = "info-box",
                               uiOutput("inputDescription")
                           ),
                           tags$div(class = "info-box",
                               uiOutput("inputHeaders"),
                               plotOutput("inputPlot")
                           )
                       )
                ),
                column(width = 4,
                       tags$div(class = "info-box",
                           uiOutput("inputHeaders2"),
                           tableOutput("dataTable")
                       )
                )
              )
      ),
      tabItem(tabName = "results",
              h2("Stock Assessment Results", class = "section-title"),
              # div(id = "full-page-spinner",
              #     div(class="spinner-icon"),
              #     div(class = "spinner-text", "Running Assessment!"),
              # ),
              tabsetPanel(
                id = "resultsTabs",
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                          column(width = 8,
                                 tags$div(class = "info-container",
                                          tags$div(class = "info-box plot-container",
                                          h3("Spawning Stock Biomass"),
                                          plotOutput("biomassPlot")
                                    )
                                )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Spawning Stock Biomass: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass estimates provide an indication of the total amount of fish in the stock.", style= "font-size: 16px; color: #7f8c8d")), 
                                          tags$p(tags$li("Spawning stock biomass (SSB) is the biomass of reproductively mature fish.", style= "font-size: 16px; color: #7f8c8d")), 
                                          tags$p(tags$li("For American Plaice estimates show a large decrease in the 1980s followed by a gradual increase through the last few decades.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Fishing Mortality Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality (F) is the rate at which fish die due to fishing activity.", style= "font-size: 16px; color: #7f8c8d")), 
                                          tags$p(tags$li("For American Plaice estimates show F coming up to a peak in the mid-1980s then gradual decreasing over the next few decades.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-container",
                                      div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          tags$p(tags$li("Recruitment measures the addition of new fish to the stock.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("For American Plaice recrtuiment has been variable without a trend throughout the time series.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished).", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("These include target (solid grey line) and threshold (dashed grey line) values.", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("For American Plaice biomass has stayed above the threshold (not overfished) and above the target in more recent decades.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("For American Plaice F was over the threshold (overfishing) for the beginning of the time series then below the threshold (not overfishing) after the mid-1990s and for the remainder of the time series.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
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
                                  tags$div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      tags$p("Diagnostics help to assess the performance and fit of the stock assessment model.")
                                  ),
                                  tags$div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot")
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-box",
                                      h3("Interpretation"),
                                      tags$p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis.")
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
                                  tags$div(class = "info-box",
                                      h3("Fishery Dependent Data Input Change"),
                                      tags$p("This scenario compares the reported catch (green) to catch decreased by 50% (blue) to show the impact of catch on stock assessment results."),
                                      br(),
                                      selectInput("dataSelection", 
                                                  "Select Input Change:",
                                                  choices = list("Lower Catch"="low",
                                                                 "Higher Catch"="high"))
                                      ),
                                  tags$div(class = "info-box",
                                      h3("Scenario Comparison Plot"),
                                      plotOutput("comparisonPlot")
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-box",
                                      h3("Scenario Comparison Table"),
                                      tableOutput("dataTable2")
                                      
                                  )
                           )
                         )
                ),
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Spawning Stock Biomass"),
                                          plotOutput("biomassPlot2")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Spawning Stock Biomass: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass estimates provide an indication of the total amount of fish in the stock.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("Spawning stock biomass (SSB) is the biomass of reproductively mature fish.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When catch is decreased by 50% the biomass estimate is also decreased by 50%", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot2")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Fishing Mortality: Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality (F) is the rate at which fish die due to fishing activity.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When catch is decreased by 50% the fishing mortality rate does not change.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("This occurs becasue the proportion of fish being removed is still the same since biomass also decreased by 50%.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot2")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          tags$p(tags$li("Recruitment measures the addition of new fish to the stock.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When catch is decreased by 50% the recruitment also decreases by 50%.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished).", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("The biomass thresholds are depicted for regular catch (solid grey line) and decreased catch (dashed grey line).", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("When catch is decreased by 50% the reference point also decreases and the stock status remains the same.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("The fishing mortaliy thresholds are depicted for regular catch (solid grey line) and decreased catch (dashed grey line).", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When catch is decreased by 50% the reference point remains the same.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot2")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
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
                                  tags$div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      tags$p("Diagnostics help to assess the performance and fit of the stock assessment model.")
                                  ),
                                  div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot2")
                                  )
                           ),
                           column(width = 4,
                                  div(class = "info-box",
                                      h3("Interpretation"),
                                      tags$p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis.")
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
                                  tags$div(class = "info-box",
                                      h3("Fishery Independent Data Input Change"),
                                      tags$p("This scenario compares the observed indices (blue and green) to indices decreased by 50% (orange and yellow) to show the impact of indices on stock assessment results."),
                                      br(),
                                      
                                      checkboxGroupInput("indexSelection",
                                                         "Select Input Change:",
                                                         c("Observed Indices"= "base",
                                                           "Lower Indices" = "bias"),
                                                         inline=TRUE)
                                  ),
                                  tags$div(class = "info-box",
                                      h3("Scenario Comparison Plot"),
                                      plotOutput("comparisonPlot3") # change to 3 and uncomment above for checkbox group
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-box",
                                      h3("Scenario Comparison Table"),
                                      tableOutput("dataTable3")
                                      
                                  
                           )
                         )
                      )
                ),
                tabPanel("Stock Assessment Estimates",
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Spawning Stock Biomass"),
                                          plotOutput("biomassPlot3")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Spanwing Stock Biomass: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass estimates provide an indication of the total amount of fish in the stock.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("Spawning stock biomass (SSB) is the biomass of reproductively mature fish.", style= "font-size: 16px; color: #7f8c8d")), 
                                          tags$p(tags$li("When indices are decreased by 50% the biomass estimate remains similar.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                      div(class = "info-box plot-container",
                                          h3("Fishing Mortality"),
                                          plotOutput("fishingMortalityPlot3")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Fishing Mortality: Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality (F) is the rate at which fish die due to fishing activity.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When indices are decreased by 50% the fishing mortality rate remains similar.", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 8,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Recruitment"),
                                          plotOutput("recruitmentPlot3")
                                      )
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box definition-box",
                                          h3("Recruitment: Definitions and Interpretation"),
                                          tags$p(tags$li("Recruitment measures the addition of new fish to the stock.", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When indices are decreased by 50% the recruitment remains similar.", style= "font-size: 16px; color: #7f8c8d"))                                      )
                                  )
                           )
                         )
                ),
                tabPanel("Reference Points",
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Biomass Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Biomass reference points are metrics used to determine the status of the stock (overfished/ not overfished).", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("The biomass thresholds are depicted for regular indices (solid grey line) and decreased indices (dashed grey line).", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("When indices are decreased by 50% the reference point and stock status are similar.", style= "font-size: 16px; color: #7f8c8d"))                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box",
                                          h3("Fishing Mortality Reference Points: Definitions and Interpretation"),
                                          tags$p(tags$li("Fishing mortality reference points are metrics used to determine sustainable fishing levels (overfishing/ not overfishing).", style= "font-size: 16px; color: #7f8c8d")) ,
                                          tags$p(tags$li("The fishing mortaliy thresholds are depicted for regular indices (solid grey line) and decreased indices (dashed grey line).", style= "font-size: 16px; color: #7f8c8d")),
                                          tags$p(tags$li("When indices are decreased by 50% the reference point and stock status are similar", style= "font-size: 16px; color: #7f8c8d"))
                                      )
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
                                          h3("Biomass Reference Points"),
                                          plotOutput("biomassReferencePlot3")
                                      )
                                  )
                           ),
                           column(width = 6,
                                  tags$div(class = "info-container",
                                           tags$div(class = "info-box plot-container",
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
                                  tags$div(class = "info-box",
                                      h3("Model Diagnostics"),
                                      tags$p("Diagnostics help to assess the performance and fit of the stock assessment model.")
                                  ),
                                  tags$div(class = "info-box",
                                      h3("Diagnostics Plot"),
                                      plotOutput("diagnosticsPlot3")
                                  )
                           ),
                           column(width = 4,
                                  tags$div(class = "info-box",
                                      h3("Intrepretation"),
                                      tags$p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis, fit statistics, and retrospective analysis")
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
                       tags$div(class = "info-container",
                                tags$div(class = "info-box",
                               h3("Project Goal"),
                               tags$p("Develop an intutitive and interactive stock assessment modeling tool to improve understanding of how different data elements and uncertainties impact an analytical model.")
                           ),
                           tags$div(class = "info-box",
                               h3("Project Team"),
                               tags$p("PI: Dr. Lisa Kerr"),
                               tags$p("Analyst and web application developer: Jerelle Jesse")
                           ),
                           tags$div(class = "info-box",
                               h3("Funding Support"),
                               tags$p("New England Fishery Management Council")
                           ),
                           tags$div(class = "info-box",
                               h3("Acknowledgements"),
                               tags$p("MREP Greater Atlantic Steering Committee"),
                               tags$p("Gulf of Maine Research Institute")
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


