#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Introduction", tabName = "Introduction"),
    menuItem("Explore Assessment", tabName = "ExploreAssessment"),
    menuItem("Explore Scenarios", tabName = "ExploreScenarios"),
    menuItem("Compare Scenarios", tabName = "CompareScenarios")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "Introduction",
      h2(
        "An Interactive Tool for Stock Assessment",
        fluidRow(tabBox(
          title = "", width = 10, id = "tabset1",
          tabPanel("About"),
          tabPanel("Stock Info"),
          tabPanel("Glossary")
        ))
      )
    ), # close introduction
    tabItem(
      tabName = "ExploreAssessment",
      h2(
        "Assessment Input/Output Exploration",
        fluidRow(
          box(
            width = 3,
            radioButtons(
              "Inputs", "Assessment Inputs",
              c(
                "Input 1" = "1",
                "Input 2" = "2",
                "Input 3" = "3",
                "Input 4" = "4"
              )
            )
          ),
          box(
            width = 5,
            "plot input here"
          ),
          box(
            width = 3,
            "explanation of inputs"
          )
        ),
        fluidRow(
          box(
            width = 3,
            radioButtons(
              "Outputs", "Assessment Results",
              c(
                "Output 1" = "1",
                "Output 2" = "2",
                "Output 3" = "3",
                "Output 4" = "4"
              )
            )
          ),
          box(
            width = 5,
            "plot results here"
          ),
          box(
            width = 3,
            "explanation of results"
          )
        )
      )
    ), # close explore assessment
    tabItem(
      tabName = "ExploreScenarios",
      h2(
        "See what changes under different scenarios",
        fluidRow(
          box(
            width = 3,
            radioButtons(
              "scenario", "Scenarios",
              c(
                "Scenario 1" = "1",
                "Scenario 2" = "2",
                "Sceanrio 3" = "3",
                "Scenario 4" = "4"
              )
            )
          ),
          box(
            width = 5,
            "plot changes here"
          ),
          box(
            width = 3, align = "center",
            "explanation of change",
            br(),
            actionButton("go", "Run Assessment!")
          ),
        ),
        fluidRow(
          box(
            width = 3,
            sliderInput("Input", "", min = 1, max = 100, value = 50)
          )
        ),
        br(),
        fluidRow(
          box(
            width = 3,
            radioButtons(
              "Outputs", "Assessment Results",
              c(
                "Output 1" = "1",
                "Output 2" = "2",
                "Output 3" = "3",
                "Output 4" = "4"
              )
            )
          ),
          box(
            width = 5,
            "plot results here compared to base case"
          ),
          box(
            width = 3,
            "explanation of results"
          )
        ),
        br(),
        fluidRow(
          box(
            width = 11,
            "Is there a way to make a table of scenarios that are run to choose for comparison like CCIL MSE experience?"
          )
        )
      )
    ), # close explore scenarios
    tabItem(
      tabName = "CompareScenarios",
      h2(
        "Choose scenario and compare",
        fluidRow(
          box(
            width = 3,
            checkboxGroupInput(
              "scenario", "Scenarios saved from last tab",
              c(
                "Scenario 1" = "1",
                "Scenario 2" = "2",
                "Scenario 3" = "3",
                "Scenario 4" = "4"
              )
            )
          ),
          box(
            width = 5,
            "plot comparison here"
          ),
          box(
            width = 3,
            "explanation of comparison"
          )
        )
      )
    ) # close compare scenarios
  ) # close tabItems
) # close body


# Define UI for application
dashboardPage(
  dashboardHeader(title = "MREP Tool"),
  sidebar,
  body
)
