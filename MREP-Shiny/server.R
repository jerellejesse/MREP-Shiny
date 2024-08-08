# Server Logic
library(here)
library(gmRi)
library(ggplot2)
server <- function(input, output, session) {
  # data
  inputs_year <- read.csv(here::here("MREP-Shiny/data/yearly_data.csv"))
  
  input_data <- reactive({
    switch(input$inputType,
           "index" = data.frame(Year=inputs_year$year,Index1=inputs_year$V1, Index2=inputs_year$V2, Index3=inputs_year$V3, Index4=inputs_year$V4),
           "catch" = data.frame(Year= inputs_year$year, Catch=inputs_year$catch),
           "mortality" = data.frame(),
           "maturity" = "Maturity",
           "weight" = "Weight-at-age",
           "catchability" = "Catchability",
           "selectivity" = "Selectivity"
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
      
      p <- switch(input$inputType,
    "index" = ggplot(data)+ geom_line(aes(x=Year, y=Index1), color=gmri_cols("green"), linewidth=1)+
      geom_line(aes(x=Year, y=Index2), color=gmri_cols("gmri blue"), linewidth=1)+
      geom_line(aes(x=Year, y=Index3), color=gmri_cols("orange"), linewidth=1)+
      geom_line(aes(x=Year, y=Index4), color=gmri_cols("gmri green"), linewidth=1)+
      ylab("Indices"),
      
      "catch" = ggplot(data)+ geom_col(aes(x=Year, y= Catch), width= 0.8, color=gmri_cols("green"),fill=gmri_cols("green"))#+
      #geom_ribbon(aes(x=year, ymin= catch_lower, ymax=catch_upper),fill=gmri_cols("green") , alpha=0.5)
      )
      print(p)
    })

  

  output$dataTable <- renderTable({
    input_data()
  })
  
  output$biomassPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)+
    geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$fishingMortalityPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon( aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$recruitmentPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Recruitment") +
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
  
  
  #### comparisons
  # read in scenario data
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
  

  
}