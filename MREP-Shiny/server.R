# Server Logic
library(here)
library(gmRi)
library(ggplot2)
server <- function(input, output, session) {
  # data
  inputs_year <- read.csv(here::here("MREP-Shiny/data/yearly_data.csv"))
  inputs_age <- read.csv(here::here("MREP-Shiny/data/input_age.csv"))
  weight_data <- read.csv(here::here("MREP-Shiny/data/weight_data.csv"))
  catch_bias <- read.csv(here::here("MREP-Shiny/data/catch_bias_data.csv"))
  index_bias <- read.csv(here::here("MREP-Shiny/data/index_bias_data.csv"))
  refs <- read.csv(here::here("MREP-Shiny/data/Ref_data.csv"))
    
  input_data <- reactive({
    switch(input$inputType,
           "index" = data.frame(Year=inputs_year$year,Index1=inputs_year$V1, Index2=inputs_year$V2, Index3=inputs_year$V3, Index4=inputs_year$V4),
           "catch" = data.frame(Year= inputs_year$year, Catch=inputs_year$catch),
           "mortality" = data.frame(Age=inputs_age$Age, M=inputs_age$M),
           "maturity" = data.frame(Age=inputs_age$Age, Maturity=inputs_age$maturity),
           "weight" = data.frame(Year=weight_data$Year, Age=weight_data$Age, Weight=weight_data$Weight),
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
      expand_limits(y = 0)+
      ylab("Indices (kg/tow)") +
      theme_minimal(),
      
      "catch" = ggplot(data)+ geom_col(aes(x=Year, y= Catch), width= 0.8, color=gmri_cols("green"),fill=gmri_cols("green"))+
      #geom_ribbon(aes(x=year, ymin= catch_lower, ymax=catch_upper),fill=gmri_cols("green") , alpha=0.5)
      ylab("Catch (mt)") +
      theme_minimal(),
    
      "mortality" = ggplot(data)+geom_line(aes(x=Age, y=M), linewidth=1.5,color=gmri_cols("green")),
      
      "maturity" = ggplot(data)+geom_line(aes(x=Age, y=Maturity),color=gmri_cols("green"), linewidth=1.5)+
      labs(y= "Maturity") +
      theme_minimal(),
        
      "weight" = ggplot(data)+geom_line(aes(x=Year, y=Weight, color=factor(Age)), linewidth=1.5)+
      labs(y= "Weight-at-age for SSB")+
      scale_color_gmri(palette = "main", guide="none") +
      theme_minimal()
      )
      print(p)
    })

  

  output$dataTable <- renderTable({
    input_data()
  })
  
  output$biomassPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)+
    geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      expand_limits(y = 0)+
      labs(title = "", x = "Year", y = "Biomass (mt)") +
      theme_minimal()
  })
  
  output$fishingMortalityPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon( aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      expand_limits(y = 0)+
      theme_minimal()
  })
  
  output$recruitmentPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Recruitment (000s)") +
      expand_limits(y = 0)+
      theme_minimal()
  })
  
  output$biomassReferencePlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y=SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept=refs$base[2], color="grey", linewidth=1)+
      geom_hline(yintercept = refs$base[2]/2, color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0)+
      labs(x = "Year", y = "Biomass (mt)") +
      theme_minimal()
  })
  
  output$fishingMortalityReferencePlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x=year, y=F), color=gmri_cols("green"), linewidth=1) +
      geom_ribbon(aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept = refs$base[1], color="grey", linewidth=1)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$diagnosticsPlot <- renderPlot({

  })
  
  
  #### comparisons
  
  output$comparisonPlot <- renderPlot({
    ggplot() +
      geom_line(data = inputs_year, aes(x = year, y = catch), color = gmri_cols("green"), linewidth=1) +
      geom_ribbon(data = inputs_year, aes(x = year, ymin = catch_lower, ymax = catch_upper), fill = gmri_cols("green"), alpha = 0.5) +
      geom_line(data = catch_bias, aes(x = year, y = catch), color = gmri_cols("gmri blue"), linewidth=1, linetype="dashed") +
      geom_ribbon(data = catch_bias, aes(x = year, ymin = catch_lower, ymax = catch_upper), fill = gmri_cols("gmri blue"), alpha = 0.5)+
      expand_limits(y = 0)+
      labs(x = "Year", y = "Catch (mt)") +
      theme_minimal()
  })
  
  output$comparisonPlot2 <- renderPlot({
    ggplot() +
      geom_line(data = inputs_year, aes(x = year, y = V1), color = gmri_cols("green"), linewidth = 1) +
      geom_line(data = inputs_year, aes(x = year, y = V2), color = gmri_cols("gmri blue"), linewidth = 1) +
      geom_line(data = inputs_year, aes(x = year, y = V3), color = gmri_cols("orange"), linewidth = 1) +
      geom_line(data = inputs_year, aes(x = year, y = V4), color = gmri_cols("gmri green"), linewidth = 1) +
      geom_line(data = index_bias, aes(x = year, y = V1), color = gmri_cols("green"), linewidth = 1, linetype = "dashed") +
      geom_line(data = index_bias, aes(x = year, y = V2), color = gmri_cols("gmri blue"), linewidth = 1, linetype = "dashed") +
      geom_line(data = index_bias, aes(x = year, y = V3), color = gmri_cols("orange"), linewidth = 1, linetype = "dashed") +
      geom_line(data = index_bias, aes(x = year, y = V4), color = gmri_cols("gmri green"), linewidth = 1, linetype = "dashed") +
      expand_limits(y = 0) +
      labs( y = "Indices (kg/tow)", x = "Year") +
      theme_minimal()
  })
  
  output$dataTable2 <- renderTable({
    df <- data.frame(Year=inputs_year$year, "High Catch"=inputs_year$catch, "Low Catch"=catch_bias$catch)
  })
  
  output$dataTable3 <- renderTable({
    df <- data.frame(Year=inputs_year$year, "High Index"=inputs_year$V1, "Low Index"=index_bias$V1)
  })
  
  output$biomassPlot2 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=catch_bias, aes(x=year, y= SSB), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=catch_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Biomass (mt)") +
      theme_minimal()
  })
  
  output$fishingMortalityPlot2 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=catch_bias, aes(x=year, y= F), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=catch_bias, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$recruitmentPlot2 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year,aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=catch_bias, aes(x=year, y= R), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=catch_bias, aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Recruitment (000s)") +
      theme_minimal()
  })
  
  output$biomassPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= SSB), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=index_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Biomass (mt)") +
      theme_minimal()
  })
  output$fishingMortalityPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= F), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=index_bias, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
  output$recruitmentPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year,aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= R), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=index_bias, aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Recruitment") +
      theme_minimal()
  })
  
  output$biomassReferencePlot2 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y=SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept=refs$base[2], color="grey", linewidth=1)+
      geom_line(data=catch_bias, aes(x=year, y=SSB), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=catch_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      geom_hline(yintercept=refs$catch[2], color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0) +
      labs(x = "Year", y = "Biomass") +
      theme_minimal()
  })
  
  output$fishingMortalityReferencePlot2 <- renderPlot({
    ggplot() + geom_line(data=inputs_year,aes(x=year, y=F), color=gmri_cols("green"), linewidth=1) +
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept = refs$base[1], color="grey", linewidth=1)+
      geom_line(data=catch_bias, aes(x=year, y=F), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed") +
      geom_ribbon(data=catch_bias, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      geom_hline(yintercept = refs$catch[1], color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  output$biomassReferencePlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y=SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept=refs$base[2], color="grey", linewidth=1)+
      geom_line(data=index_bias, aes(x=year, y=SSB), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed")+
      geom_ribbon(data=index_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      geom_hline(yintercept=refs$index[2], color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0) +
      labs(x = "Year", y = "Biomass (mt)") +
      theme_minimal()
  })
  
  output$fishingMortalityReferencePlot3 <- renderPlot({
    ggplot() + geom_line(data=inputs_year,aes(x=year, y=F), color=gmri_cols("green"), linewidth=1) +
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept = refs$base[1], color="grey", linewidth=1)+
      geom_line(data=index_bias, aes(x=year, y=F), color=gmri_cols("gmri blue"), linewidth=1, linetype="dashed") +
      geom_ribbon(data=index_bias, aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("gmri blue") , alpha=0.5)+
      geom_hline(yintercept = refs$index[1], color="grey", linewidth=1, linetype="dashed")+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()
  })
  
}