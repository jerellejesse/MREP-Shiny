# Server Logic
library(here)
library(gmRi)
library(ggplot2)
library(tidyverse)
library(DT)
library(shinyjs)
library(shinycssloaders)
server <- function(input, output, session) {
  # data
  inputs_year <- read.csv(here::here("data/yearly_data.csv"))
  inputs_age <- read.csv(here::here("data/input_age.csv"))
  weight_data <- read.csv(here::here("data/weight_data.csv"))
  catch_bias <- read.csv(here::here("data/catch_bias_data.csv"))
  high_catch <- read.csv(here::here("data/high_catch.csv"))
  index_bias <- read.csv(here::here("data/index_bias_data.csv"))
  refs <- read.csv(here::here("data/Ref_data.csv"))
  catchability <- read.csv(here::here("data/catchability.csv"))

    
  input_data <- reactive({
    switch(input$inputType,
           "index" = data.frame(Year=inputs_year$year,Index1=inputs_year$V1, Index2=inputs_year$V2, Index3=inputs_year$V3, Index4=inputs_year$V4),
           "catch" = data.frame(Year= inputs_year$year, Catch=inputs_year$catch),
           "mortality" = data.frame(Age=inputs_age$Age, M=inputs_age$M),
           "maturity" = data.frame(Age=inputs_age$Age, Maturity=inputs_age$maturity),
           "weight" = data.frame(Year=weight_data$Year, Age=weight_data$Age, Weight=weight_data$Weight),
           "selectivity" = data.frame(Age= inputs_age$Age, Selectivity= inputs_age$selectivity),
           "catchability" = data.frame(Survey= catchability$survey, Year=catchability$Year, Catchability= catchability$catchability)
    )
  })

  output$inputDescription <- renderUI({
    inputType <- input$inputType
    descriptions <- c(
      index = tagList(
        tags$ul(
          tags$li("An index of abundance or biomass provides information about the relative abundance or biomass of the fish stock.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("This data comes from fishery independent surveys and monitoring.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("For American Plaice there are four surveys based on the Northeast Fishery Science Center trawl survey spilt bewteen different research vessels.",
                  style = "font-size: 16px; color: #7f8c8d",
                   tags$ul(
             tags$li("1. Spring Albatross (solid green)", style= "font-size: 16px; color: #407331"),
             tags$li("2. Spring Bigelow (dashed green)", style= "font-size: 16px; color: #407331"),
             tags$li("3. Fall Albatross (solid blue)", style= "font-size: 16px; color: #00608a"),
             tags$li("4. Fall Bigelow (dashed blue)", style= "font-size: 16px; color: #00608a")
          )
          )
        )
      ),
      
      catch = tagList(
        tags$ul(
          tags$li("Total catch for a stock includes commercial and recreational landings and discards.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("Dealer reports provide a census of commercial landings and Vessel Trip Reports provide a census of fishing effort by stock area and the two are linked to derive landings by stock area.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("At sea monitoring (e.g. Fisheries Observer Program) is primarily ised to estimate discarded commercial catch.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("Recreational data is collected through the MRIP program which uses a combination of angler intercept surveys and mail-based surveys to characterize catch.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("For American Plaice, catch inludes only commercial landings and discards becasue they are not encountered in the recreational fishery.", style= "font-size: 16px; color: #7f8c8d")
          )
        ),
      
      mortality = tagList(
        tags$ul(
        tags$li("Natural mortality (M) data indicates the rate at which fish die due to natural causes.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("Natural mortality is usually a fixed number but can vary over ages or time. This data can come from tagging, relationships with life-history traits, or sometimes be estimated in the stock assessment model.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("For American Plaice, natural mortality is assumed as 0.3 for all ages.", style= "font-size: 16px; color: #7f8c8d")
        )
        ),
      maturity = tagList(
        tags$ul(
        tags$li("Maturity data describes the proportion of the fish population that has reached reproductive age.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("This data comes from fishery independent surveys that take biological information.", style= "font-size: 16px; color: #7f8c8d")
        )
        ),
      weight = tagList(
        tags$ul(
        tags$li("Weight-at-age data provides information on the average weight of fish at different ages.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("There can be different weight-at-age for fishery indpendent data and fishery dependent data", style= "font-size: 16px; color: #7f8c8d")
        )
        ),
      catchability = tagList(
        tags$ul(
          tags$li("Catchability (q) data reflects the probability of capturing a fish in a survey.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("This can be estimated in the stock assessment model. Each index has an associated catchability.", style= "font-size: 16px; color: #7f8c8d"),
          tags$li("For American Plaice:", style= "font-size: 16px; color: #7f8c8d" ,
               tags$ul(
                tags$li("1. Spring Albatross (solid green)", style= "font-size: 16px; color: #407331"),
                tags$li("2. Spring Bigelow (dashed green)", style= "font-size: 16px; color: #407331"),
                tags$li("3. Fall Albatross (solid blue)", style= "font-size: 16px; color: #00608a"),
                tags$li("4. Fall Bigelow (dashed blue)", style= "font-size: 16px; color: #00608a")
               )
              )
        )),
      selectivity = tagList(
        tags$ul(
        tags$li("Selectivity data shows how the fishing gear affects different age groups of the fish stock.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("This can be estimated in the stock assessment model and gear selectivity studies.", style= "font-size: 16px; color: #7f8c8d"),
        tags$li("Selectivity can be different for different time periods.", style= "font-size: 16px; color: #7f8c8d")
        
        ))
    )
    headers <- c(
      index = "Stock Index",
      catch = "Catch",
      mortality = "Natural Mortality",
      maturity = "Maturity",
      weight = "Weight-at-age",
      catchability = "Catchability",
      selectivity = "Selectivity"
    )
  tags$div(
    h3(headers[[inputType]]),
    tags$p(descriptions[[inputType]])
    )
  })
  
  output$inputHeaders <- renderUI({
    inputType <- input$inputType
    headers <- c(
      index = "Stock Index Input Data",
      catch = "Catch Input Data",
      mortality = "Natural Mortality Input Data",
      maturity = "Maturity Input Data",
      weight = "Weight-at-age Input Data",
      catchability = "Catchability Input Data",
      selectivity = "Selectivity Input Data"
    )
    h3(headers[[inputType]])
    
  })
  
  output$inputHeaders2 <- renderUI({
    inputType <- input$inputType
    headers <- c(
      index = "Stock Index Input Data",
      catch = "Catch Input Data",
      mortality = "Natural Mortality Input Data",
      maturity = "Maturity Input Data",
      weight = "Weight-at-age Input Data",
      catchability = "Catchability Input Data",
      selectivity = "Selectivity Input Data"
    )
    h3(headers[[inputType]])
    
  })
  custom_colors <- c("Index1" = "#407331", 
                     "Index2" = "#407331",  
                     "Index3" = "#00608a",
                     "Index4"= "#00608a")  
  
  custom_linetypes <- c("Index1" = "solid",
                        "Index2" = "dashed",
                        "Index3" = "solid",
                        "Index4"= "dashed")
  
    output$inputPlot <- renderPlot({
      data <- input_data()
      
      p <- switch(input$inputType,
    "index" = ggplot(data) +
      geom_line(aes(x = Year, y = Index1, color = "Index1", linetype = "Index1"), 
                linewidth = 1) +
      geom_line(aes(x = Year, y = Index2, color = "Index2", linetype = "Index2"), 
                linewidth = 1) +
      geom_line(aes(x = Year, y = Index3, color = "Index3", linetype = "Index3"), 
                linewidth = 1) +
      geom_line(aes(x = Year, y = Index4, color = "Index4", linetype = "Index4"), 
                linewidth = 1) +
      scale_color_manual(values = c("Index1" = "#407331",
                                    "Index2" = "#407331",
                                    "Index3" = "#00608a",
                                    "Index4" = "#00608a")) +
      scale_linetype_manual(values = c("Index1" = "solid",
                                       "Index2" = "dashed",
                                       "Index3" = "solid",
                                       "Index4" = "dashed"))+
      guides(color = guide_legend("Stock Indices"), 
             linetype = guide_legend("Stock Indices")) +
      expand_limits(y = 0) +
      ylab("Stock Indices (kg/tow)") +
      theme_minimal() +
      theme(text = element_text(size = 16)),
    
      "catch" = ggplot(data)+ geom_col(aes(x=Year, y= Catch), width= 0.8, color=gmri_cols("green"),fill=gmri_cols("green"))+
      ylab("Catch (mt)") +
      theme_minimal()+
      theme(text = element_text(size = 16)),
    
      "mortality" = ggplot(data)+geom_line(aes(x=Age, y=M), linewidth=1.5,color=gmri_cols("green"))+
       theme_minimal()+
      theme(text = element_text(size = 16)),
      
      "maturity" = ggplot(data)+geom_line(aes(x=Age, y=Maturity),color=gmri_cols("green"), linewidth=1.5)+
      labs(y= "Maturity") +
       theme_minimal()+
      theme(text = element_text(size = 16)),
    
     "selectivity" = ggplot(data)+geom_line(aes(x=Age, y=Selectivity),color=gmri_cols("green"), linewidth=1.5)+
      labs(y= "Fishery selectivity") +
      theme_minimal()+
      theme(text = element_text(size = 16)),
    
      "weight" = ggplot(data)+geom_line(aes(x=Year, y=Weight, color=factor(Age)), linewidth=1.5)+
      labs(y= "Weight-at-age for SSB")+
      scale_color_gmri(palette = "main", guide="none") +
      theme_minimal()+
      theme(text = element_text(size = 16)),
    
      "catchability" = ggplot(data)+geom_line(aes(x=Year, y=Catchability, color=factor(Survey), linetype=factor(Survey)), linewidth=1.5)+
      scale_color_manual(values = custom_colors, guide="none") +
      scale_linetype_manual(values = custom_linetypes, guide="none") +
      theme_minimal() +
      theme(text = element_text(size = 16))+
      labs(color = "Survey", linetype = "Survey")
      )
      print(p)
    })

  

  # output$dataTable <- renderDT({ # have to change ui to DToutput
  #   inputType <- input$inputType
  # 
  #   if(inputType =="catchability"){
  #      data <- input_data()%>%
  #       pivot_wider(names_from = Survey, values_from = Catchability)
  #   } else {
  #    data<- input_data()
  #   }
  #   datatable(data, options=list(pageLength = 42))
  # 
  # })
  
output$dataTable <- renderTable({
     inputType <- input$inputType
  
     if(inputType =="catchability"){
         input_data()%>%
         pivot_wider(names_from = Survey, values_from = Catchability)
     } else {
       input_data()
   }
})

observeEvent(input$sidebar, {
  if (input$sidebar == "results") {
    shinyjs::show(id = "full-page-spinner")
    shinyjs::delay(3000, shinyjs::hide(id = "full-page-spinner", anim = TRUE, animType = "fade"))
     } else {
    shinyjs::hide(id = "full-page-spinner")
  }
})


selected_catch <- reactive({
  if (input$dataSelection == "low") {
    catch_bias
  } else {
    high_catch
  }
})

selected_refs <- reactive({
  if (input$dataSelection == "low"){
    #refs[c(1:3,5)]
    #colnames(refs)[colnames(refs)== "catch_low"] <- "catch"
    refs <-dplyr::rename(refs,catch=catch_low)
  }else {
    #refs[c(1:2,4:5)]
    #colnames(refs)[colnames(refs)== "catch_high"] <- "catch"
    refs <-dplyr::rename(refs, catch=catch_high)
  }
})

  output$biomassPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= SSB), color=gmri_cols("green"), linewidth=1)+
    geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      expand_limits(y = 0)+
      labs(title = "", x = "Year", y = "Spawning Stock Biomass (mt)") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$fishingMortalityPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= F), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon( aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      expand_limits(y = 0)+
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$recruitmentPlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y= R), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(aes(x=year, ymin= R_lower, ymax=R_upper),fill=gmri_cols("green") , alpha=0.5)+
      labs(title = "", x = "Year", y = "Recruitment (000s)") +
      expand_limits(y = 0)+
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$biomassReferencePlot <- renderPlot({
    ggplot(inputs_year)+ geom_line(aes(x=year, y=SSB), color=gmri_cols("green"), linewidth=1)+
      geom_ribbon(aes(x=year, ymin= SSB_lower, ymax=SSB_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept=refs$base[2], color="grey", linewidth=1)+
      geom_hline(yintercept = refs$base[2]/2, color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0)+
      labs(x = "Year", y = "Biomass (mt)") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$fishingMortalityReferencePlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x=year, y=F), color=gmri_cols("green"), linewidth=1) +
      geom_ribbon(aes(x=year, ymin= F_lower, ymax=F_upper),fill=gmri_cols("green") , alpha=0.5)+
      geom_hline(yintercept = refs$base[1], color="grey", linewidth=1)+
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$diagnosticsPlot <- renderPlot({

  })
  
  
  #### comparisons
  
  output$comparisonPlot <- renderPlot({
    selected_data <- selected_catch()
    ggplot() +
      geom_line(data = inputs_year, aes(x = year, y = catch, color = "Reported Catch", linetype="Reported Catch"), linewidth = 1) +
      geom_ribbon(data = inputs_year, aes(x = year, ymin = catch_lower, ymax = catch_upper, fill = "Reported Catch"), alpha = 0.5) +
      geom_line(data = selected_data, aes(x = year, y = catch, color = "Bias Catch", linetype = "Bias Catch"), linewidth = 1) +
      geom_ribbon(data = selected_data, aes(x = year, ymin = catch_lower, ymax = catch_upper, fill = "Bias Catch"), alpha = 0.5) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Catch (mt)", color = "", fill = "", linetype = "") + # fills out the legend
      scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      theme_minimal() +
      theme(text = element_text(size = 16)) +
      ylim(0, 40000)
    
  })
  output$comparisonPlot2 <- renderPlot({
    ggplot() +
      geom_line(data = inputs_year, aes(x = year, y = V1), color = gmri_cols("green"), linewidth = 1) +
      geom_line(data = inputs_year, aes(x = year, y = V2), color = gmri_cols("green"), linewidth = 1, linetype="dashed") +
      geom_line(data = inputs_year, aes(x = year, y = V3), color = gmri_cols("gmri blue"), linewidth = 1) +
      geom_line(data = inputs_year, aes(x = year, y = V4), color = gmri_cols("gmri blue"), linewidth = 1, linetype="dashed") +
      geom_line(data = index_bias, aes(x = year, y = V1), color = gmri_cols("orange"), linewidth = 1) +
      geom_line(data = index_bias, aes(x = year, y = V2), color = gmri_cols("orange"), linewidth = 1, linetype = "dashed") +
      geom_line(data = index_bias, aes(x = year, y = V3), color = gmri_cols("gmri green"), linewidth = 1) +
      geom_line(data = index_bias, aes(x = year, y = V4), color = gmri_cols("gmri green"), linewidth = 1, linetype = "dashed") +
      expand_limits(y = 0) +
      labs( y = "Indices (kg/tow)", x = "Year") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  output$comparisonPlot3 <- renderPlot({
   
    base_colors <- c("#407331", 
                     "#407331",  
                     "#00608a",
                     "#00608a")  
    
    base_linetypes <- c("solid",
                        "dashed",
                        "solid",
                        "dashed")
    
    bias_colors <- c("#ABB400", 
                     "#ABB400",  
                     "#00736D",
                     "#00736D")  
    
    bias_linetypes <- c("solid",
                        "dashed",
                        "solid",
                        "dashed")
    
    # Create an empty ggplot object
    p <- ggplot() +
      expand_limits(y = 0) +
      labs(y = "Indices (kg/tow)", x = "Year") +
      theme_minimal() +
      theme(text = element_text(size = 16))
    
    if ("base" %in% input$indexSelection){
        p <- p +
          geom_line(data = inputs_year, aes(x = year, y = V1), color = gmri_cols("green"), linewidth = 1) +
          geom_line(data = inputs_year, aes(x = year, y = V2), color = gmri_cols("green"), linewidth = 1, linetype="dashed") +
          geom_line(data = inputs_year, aes(x = year, y = V3), color = gmri_cols("gmri blue"), linewidth = 1) +
          geom_line(data = inputs_year, aes(x = year, y = V4), color = gmri_cols("gmri blue"), linewidth = 1, linetype="dashed")
      }
    
    if ("bias" %in% input$indexSelection) {
      p<-p+
        geom_line(data = index_bias, aes(x = year, y = V1), color = gmri_cols("gmri green"), linewidth = 1) +
        geom_line(data = index_bias, aes(x = year, y = V2), color = gmri_cols("gmri green"), linewidth = 1, linetype = "dashed") +
        geom_line(data = index_bias, aes(x = year, y = V3), color = gmri_cols("orange"), linewidth = 1) +
        geom_line(data = index_bias, aes(x = year, y = V4), color = gmri_cols("orange"), linewidth = 1, linetype = "dashed")
      }
    # ggplot() +
    #   geom_line(data = selected_index, aes(x = year, y = V1), linewidth = 1) +
    #   geom_line(data = bias_index, aes(x = year, y = V1), linewidth = 1) +
    #   expand_limits(y = 0) +
    #   labs( y = "Indices (kg/tow)", x = "Year") +
    #   theme_minimal()+
    #   theme(text = element_text(size = 16))
 p
     })
  
  output$dataTable2 <- renderTable({
    selected_data <- selected_catch()
    df <- data.frame(Year=inputs_year$year, "Reported Catch"=inputs_year$catch, "Bias Catch"=selected_data$catch)
  })
  
  output$dataTable3 <- renderTable({
    selected_data <- selected_catch()
    df <- data.frame(Year=inputs_year$year, "Observed Index1"=inputs_year$V1, "Low Index1"=index_bias$V1)
  })
  
  output$biomassPlot2 <- renderPlot({
    selected_data <- selected_catch()
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= SSB, color="Reported Catch", linetype="Reported Catch"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Reported Catch") , alpha=0.5)+
      geom_line(data=selected_data, aes(x=year, y= SSB, color="Bias Catch", linetype="Bias Catch"), linewidth=1)+
      geom_ribbon(data=selected_data, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Bias Catch") , alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Spawning Stock Biomass (mt)", color = "", fill = "", linetype = "") +
      scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$fishingMortalityPlot2 <- renderPlot({
    selected_data <- selected_catch()
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= F, color="Reported Catch", linetype="Reported Catch"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper, fill="Reported Catch"),alpha=0.5)+
      geom_line(data=selected_data, aes(x=year, y= F, color="Bias Catch", linetype="Bias Catch"), linewidth=1)+
      geom_ribbon(data=selected_data, aes(x=year, ymin= F_lower, ymax=F_upper, fill="Bias Catch"), alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Fishing Mortality", color = "", fill = "", linetype = "") +
    scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
    scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
    scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$recruitmentPlot2 <- renderPlot({
    selected_data <- selected_catch()
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= R, color="Reported Catch", linetype="Reported Catch"), linewidth=1)+
      geom_ribbon(data=inputs_year,aes(x=year, ymin= R_lower, ymax=R_upper, fill= "Reported Catch"), alpha=0.5)+
      geom_line(data=selected_data, aes(x=year, y= R, color="Bias Catch", linetype="Bias Catch"),linewidth=1)+
      geom_ribbon(data=selected_data, aes(x=year, ymin= R_lower, ymax=R_upper, fill="Bias Catch"),alpha=0.5)+
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Recruitment (000s)", color = "", fill = "", linetype = "") +
      scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$biomassPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= SSB, color="Reported Index",linetype="Reported Index"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Reported Index") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= SSB, color="Bias Index", linetype="Bias Index"), linewidth=1)+
      geom_ribbon(data=index_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Bias Index") , alpha=0.5)+
      expand_limits(y = 0) +
      scale_color_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_fill_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Index" = "solid", "Bias Index" = "dashed")) +
      labs(title = "", x = "Year", y = "Spawning Stock Biomass (mt)", color = "", fill = "", linetype = "")+
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  output$fishingMortalityPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year, aes(x=year, y= F, color="Reported Index", linetype="Reported Index"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Reported Index") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= F, color="Bias Index", linetype="Bias Index"), linewidth=1)+
      geom_ribbon(data=index_bias, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Bias Index") , alpha=0.5)+
      scale_color_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_fill_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Index" = "solid", "Bias Index" = "dashed")) +
      labs(title = "", x = "Year", y = "Fishing Mortality", color = "", fill = "", linetype = "") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$recruitmentPlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y= R, color="Reported Index",linetype="Reported Index"), linewidth=1)+
      geom_ribbon(data=inputs_year,aes(x=year, ymin= R_lower, ymax=R_upper,fill="Reported Index") , alpha=0.5)+
      geom_line(data=index_bias, aes(x=year, y= R, color="Bias Index", linetype="Bias Index"), linewidth=1)+
      geom_ribbon(data=index_bias, aes(x=year, ymin= R_lower, ymax=R_upper,fill="Bias Index") , alpha=0.5)+
      expand_limits(y = 0) +
      scale_color_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_fill_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Index" = "solid", "Bias Index" = "dashed")) +
      labs(title = "", x = "Year", y = "Recruitment", color = "", fill = "", linetype = "") +
      theme_minimal()+
      theme(text = element_text(size = 16))
  })
  
  output$biomassReferencePlot2 <- renderPlot({
    selected_data <- selected_catch()
    selected_rps <- selected_refs()
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y=SSB, color="Reported Catch",linetype="Reported Catch"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Reported Catch") , alpha=0.5)+
      geom_hline(yintercept=selected_rps$base[2], color="grey", linewidth=1)+
      geom_line(data=selected_data, aes(x=year, y=SSB, color="Bias Catch", linetype="Bias Catch"), linewidth=1)+
      geom_ribbon(data=selected_data, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Bias Catch") , alpha=0.5)+
      geom_hline(yintercept=selected_rps$catch[2], color="grey", linewidth=1, linetype="dashed")+
      scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      labs(title = "", x = "Year", y = "Biomass", color = "", fill = "", linetype = "") +
      expand_limits(y = 0) +
      theme_minimal()+
      theme(text = element_text(size = 16))+
      theme(legend.position="bottom")
  })
  
  output$fishingMortalityReferencePlot2 <- renderPlot({
    selected_data <- selected_catch()
    selected_rps <- selected_refs()
    ggplot() + geom_line(data=inputs_year,aes(x=year, y=F, color="Reported Catch",linetype="Reported Catch"), linewidth=1) +
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Reported Catch") , alpha=0.5)+
      geom_hline(yintercept = selected_rps$base[1], color="grey", linewidth=1)+
      geom_line(data=selected_data, aes(x=year, y=F, color="Bias Catch", linetype="Bias Catch"), linewidth=1 ) +
      geom_ribbon(data=selected_data, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Bias Catch") , alpha=0.5)+
      geom_hline(yintercept = selected_rps$catch[1], color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0) +
      scale_color_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_fill_manual(values = c("Reported Catch" = "#407331", "Bias Catch" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Catch" = "solid", "Bias Catch" = "dashed")) +
      labs(title = "", x = "Year", y = "Fishing Mortality", color = "", fill = "", linetype = "") +
      theme(text = element_text(size = 16))+
      theme_minimal()+
      theme(legend.position="bottom")
  })
  output$biomassReferencePlot3 <- renderPlot({
    ggplot()+ geom_line(data=inputs_year,aes(x=year, y=SSB, color="Reported Index",linetype="Reported Index"), linewidth=1)+
      geom_ribbon(data=inputs_year, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Reported Index") , alpha=0.5)+
      geom_hline(yintercept=refs$base[2], color="grey", linewidth=1)+
      geom_line(data=index_bias, aes(x=year, y=SSB, color="Bias Index", linetype="Bias Index"), linewidth=1)+
      geom_ribbon(data=index_bias, aes(x=year, ymin= SSB_lower, ymax=SSB_upper,fill="Bias Index") , alpha=0.5)+
      geom_hline(yintercept=refs$index[2], color="grey", linewidth=1, linetype="dashed")+
      expand_limits(y = 0) +
      scale_color_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_fill_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Index" = "solid", "Bias Index" = "dashed")) +
      labs(title = "", x = "Year", y = "Biomass (mt)", color = "", fill = "", linetype = "") +
      theme_minimal()+
      theme(text = element_text(size = 16))+
      theme(legend.position="bottom")
    
  })
  
  output$fishingMortalityReferencePlot3 <- renderPlot({
    ggplot() + geom_line(data=inputs_year,aes(x=year, y=F, color="Reported Index",linetype="Reported Index"), linewidth=1) +
      geom_ribbon(data=inputs_year, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Reported Index") , alpha=0.5)+
      geom_hline(yintercept = refs$base[1], color="grey", linewidth=1)+
      geom_line(data=index_bias, aes(x=year, y=F, color="Bias Index", linetype="Bias Index"), linewidth=1) +
      geom_ribbon(data=index_bias, aes(x=year, ymin= F_lower, ymax=F_upper,fill="Bias Index") , alpha=0.5)+
      geom_hline(yintercept = refs$index[1], color="grey", linewidth=1, linetype="dashed")+
      scale_color_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_fill_manual(values = c("Reported Index" = "#407331", "Bias Index" = "#00608a")) +
      scale_linetype_manual(values = c("Reported Index" = "solid", "Bias Index" = "dashed")) +
      labs(title = "", x = "Year", y = "Biomass (mt)", color = "", fill = "", linetype = "") +
      theme_minimal()+
      theme(text = element_text(size = 16))+
      theme(legend.position="bottom")
  })
  
}