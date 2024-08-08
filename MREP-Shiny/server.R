# Server Logic
server <- function(input, output, session) {
  # Sample data
  stock_data <- data.frame(
    Year = rep(2000:2020, each = 4),
    Age = rep(1:4, times = 21),
    Biomass = runif(84, 500, 2000),
    FishingMortality = runif(84, 0.1, 0.5),
    Recruitment = runif(84, 1000, 5000)
  )
  
  input_data <- reactive({
    switch(input$inputType,
           "index" = data.frame(Year = 2000:2020, Index = runif(21, 50, 150)),
           "catch" = data.frame(Year = 2000:2020, Catch = runif(21, 100, 500)),
           "mortality" = data.frame(Year = 2000:2020, Mortality = runif(21, 0.1, 0.5)),
           "maturity" = data.frame(Year = 2000:2020, Maturity = runif(21, 0.2, 0.8)),
           "weight" = data.frame(Year = 2000:2020, Weight = runif(21, 0.5, 2)),
           "catchability" = data.frame(Year = 2000:2020, Catchability = runif(21, 0.1, 0.4)),
           "selectivity" = data.frame(Year = 2000:2020, Selectivity = runif(21, 0.2, 1))
    )
  })
  
  output$inputDescription <- renderUI({
    inputType <- input$inputType
    descriptions <- c(
      index = "Index data provides information about the relative abundance of the fish stock.",
      catch = "Catch data shows the amount of fish removed from the stock by fishing activities.",
      mortality = "Natural mortality data indicates the rate at which fish die due to natural causes.",
      maturity = "Maturity data describes the proportion of the fish population that has reached reproductive age.",
      weight = "Weight-at-age data provides information on the average weight of fish at different ages.",
      catchability = "Catchability data reflects the probability of capturing a fish in a survey or fishery.",
      selectivity = "Selectivity data shows how the fishing gear affects different age groups of the fish stock."
    )
    p(descriptions[[inputType]])
  })
  
  output$inputPlot <- renderPlot({
    data <- input_data()
    ggplot(data, aes(x = Year, y = get(names(data)[2]))) +
      geom_line() +
      labs(title = paste(input$inputType, "Data Over Time"),
           x = "Year", y = names(data)[2])
  })
  
  output$dataTable <- renderTable({
    input_data()
  })
  
  output$biomassPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = Biomass, color = as.factor(Age))) +
      geom_line() +
      labs(title = "Biomass Estimates Over Time", x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$fishingMortalityPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = FishingMortality, color = as.factor(Age))) +
      geom_line() +
      labs(title = "Fishing Mortality Rates Over Time", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$recruitmentPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = Recruitment)) +
      geom_line() +
      labs(title = "Recruitment Over Time", x = "Year", y = "Recruitment") +
      theme_minimal()
  })
  
  output$biomassReferencePlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = Biomass)) +
      geom_line() +
      labs(title = "Biomass Reference Points", x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$fishingMortalityReferencePlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = FishingMortality)) +
      geom_line() +
      labs(title = "Fishing Mortality Reference Points", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$diagnosticsPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = Biomass)) +
      geom_point() +
      labs(title = "Model Diagnostics Plot", x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$inputChangePlot1 <- renderPlot({
    ggplot(data.frame(Year = 2000:2020, Value = runif(21, 50, 150)), aes(x = Year, y = Value)) +
      geom_line() +
      labs(title = "Fishery Dependent Data Time Series", x = "Year", y = "Value") +
      theme_minimal()
  })
  
  output$inputChangePlot2 <- renderPlot({
    ggplot(data.frame(Scenario = c("Base", "Scenario 1", "Scenario 2"), Value = runif(3, 100, 500)),
           aes(x = Scenario, y = Value)) +
      geom_bar(stat = "identity") +
      labs(title = "Scenario Comparison", x = "Scenario", y = "Value") +
      theme_minimal()
  })
  
  output$assessmentBiomassPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = Biomass)) +
      geom_line() +
      labs(title = "Biomass Estimates from Assessment Results", x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$assessmentFishingMortalityPlot <- renderPlot({
    ggplot(stock_data, aes(x = Year, y = FishingMortality)) +
      geom_line() +
      labs(title = "Fishing Mortality Rates from Assessment Results", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$surveyDataTable <- renderTable({
    data.frame(Year = 2000:2020, SurveyData = runif(21, 50, 150))
  })
  
  output$surveyDataPlot <- renderPlot({
    ggplot(data.frame(Year = 2000:2020, SurveyData = runif(21, 50, 150)),
           aes(x = Year, y = SurveyData)) +
      geom_line() +
      labs(title = "Fishery Independent Survey Data", x = "Year", y = "Survey Data") +
      theme_minimal()
  })
}