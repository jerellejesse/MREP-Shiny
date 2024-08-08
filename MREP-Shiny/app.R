library(shiny)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Fish Stock Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stock Assessment Inputs", tabName = "inputs", icon = icon("bar-chart")),
      menuItem("Stock Assessment Results", tabName = "results", icon = icon("file-alt")),
      menuItem("Compare Scenarios", tabName = "scenarios", icon = icon("exchange-alt"))
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
                column(width = 8,
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
                column(width = 4,
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
                                      h3("Definitions"),
                                      p("Diagnostics involve analyzing the model's accuracy and reliability. Key aspects include residual analysis and comparison with observed data.")
                                  )
                           )
                         )
                )
              )
      ),
      tabItem(tabName = "scenarios",
              h2("Compare Scenarios", class = "section-title"),
              fluidRow(
                column(width = 12,
                       div(class = "info-box",
                           h3("Base Case vs Fishery Independent Data"),
                           plotOutput("scenarioComparisonPlot")
                       )
                )
              )
      )
    )
  )
)


server <- function(input, output) {
  # Reactive expression to determine the input type
  selectedInput <- reactive({
    input$inputType
  })
  
  # Define a reactive expression for the data name and description
  dataName <- reactive({
    switch(selectedInput(),
           "index" = "Index Data",
           "catch" = "Catch Data",
           "mortality" = "Natural Mortality Data",
           "maturity" = "Maturity Data",
           "weight" = "Weight-at-Age Data",
           "catchability" = "Catchability Data",
           "selectivity" = "Selectivity Data"
    )
  })
  
  dataDescription <- reactive({
    switch(selectedInput(),
           "index" = p("Index data refers to estimates of fish abundance from surveys, which can be used to infer the size of a fish stock. Commonly collected by trawl or acoustic surveys."),
           "catch" = p("Catch data represents the amount of fish harvested by a fishery over a specific time period. This data is critical for understanding fishing pressure and stock exploitation."),
           "mortality" = p("Natural mortality data provides estimates of the death rate of fish in the absence of fishing. It includes factors like predation, disease, and old age."),
           "maturity" = p("Maturity data indicates the age or size at which fish become capable of reproduction. This information is crucial for understanding the reproductive capacity of a fish stock."),
           "weight" = p("Weight-at-Age data tracks the average weight of fish at various ages. This helps in assessing growth patterns and determining the health of the stock."),
           "catchability" = p("Catchability refers to the proportion of the fish stock that is captured by a fishing gear. This metric helps in adjusting catch data to reflect true stock size."),
           "selectivity" = p("Selectivity data describes how different fishing gears or methods capture fish of various sizes or ages. This affects the composition of the catch and can influence stock assessments.")
    )
  })
  
  # Update description based on selected input
  output$inputDescription <- renderUI({
    dataDescription()
  })
  
  # Update plot based on selected input
  output$inputPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))  # Example data
    plot(years, data, type = "o", xlab = "Year", ylab = dataName(),
         main = paste(dataName(), "Plot"),
         col = "blue", pch = 16)
  })
  
  # Update data table based on selected input
  output$dataTable <- renderTable({
    years <- 2000:2009
    data <- runif(length(years))  # Example data
    data_frame <- data.frame(Year = years, Value = data)
    
    # Rename 'Value' column to match the selected input type
    colnames(data_frame)[2] <- dataName()
    
    data_frame
  })
  
  # Other server logic for the remaining tabs
  
  output$biomassPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Biomass",
         main = "Biomass Estimates", col = "green", pch = 16)
  })
  
  output$fishingMortalityPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Fishing Mortality",
         main = "Fishing Mortality Rates", col = "red", pch = 16)
  })
  
  output$recruitmentPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Recruitment",
         main = "Recruitment", col = "purple", pch = 16)
  })
  
  output$biomassReferencePlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Biomass Reference",
         main = "Biomass Reference Points", col = "orange", pch = 16)
  })
  
  output$fishingMortalityReferencePlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Fishing Mortality Reference",
         main = "Fishing Mortality Reference Points", col = "blue", pch = 16)
  })
  
  output$diagnosticsPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Diagnostics",
         main = "Model Diagnostics", col = "cyan", pch = 16)
  })
  
  output$scenarioComparisonPlot <- renderPlot({
    years <- 2000:2009
    data <- runif(length(years))
    plot(years, data, type = "o", xlab = "Year", ylab = "Value",
         main = "Scenario Comparison", col = "magenta", pch = 16)
  })
}


# Run the application 
shinyApp(ui = ui, server = server)




shinyApp(ui, server)