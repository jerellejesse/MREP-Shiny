# Server Logic
library(here)
library(gmRi)
library(ggplot2)
library(tidyverse)
library(DT)
library(shinyjs)
library(shinycssloaders)
SHEET_ID <- "19f3SOqC12goVIdomD-AR3R2as0icae3JQKTD0-_QjdE"
# At the top of your server function
gs4_auth(path = "mrep-shiny-c8ab17132080.json")

# Define a custom theme with darker grid lines
custom_theme <-  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray40", size = .5),
    # Darker major grid lines
    panel.grid.minor = element_line(color = "gray50", size = 0.25),
    # Darker minor grid lines
    text = element_text(size = 20) # Consistent text size
  )

# Apply the custom theme globally
theme_set(custom_theme)


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
    switch(
      input$inputType,
      "index" = data.frame(
        Year = inputs_year$year,
        Index1 = round(inputs_year$V1,2),
        Index2 = round(inputs_year$V2,2),
        Index3 = round(inputs_year$V3,2),
        Index4 = round(inputs_year$V4,2)
      ),
      "catch" = data.frame(Year = inputs_year$year, Catch = round(inputs_year$catch,2)),
      "mortality" = data.frame(Age = inputs_age$Age, M = inputs_age$M),
      "maturity" = data.frame(Age = inputs_age$Age, Maturity = inputs_age$maturity),
      "weight" = data.frame(
        Year = weight_data$Year,
        Age = weight_data$Age,
        Weight = weight_data$Weight
      ),
      "selectivity" = data.frame(Age = inputs_age$Age, Selectivity = round(inputs_age$selectivity,2)),
      "catchability" = data.frame(
        Survey = catchability$survey,
        Year = catchability$Year,
        Catchability = signif(catchability$catchability, digits=3)
      )
    )
  })
  
  output$inputDescription <- renderUI({
    inputType <- input$inputType
    descriptions <- c(
      index = tagList(tags$ul(
        tags$li(
          "An index of abundance or biomass provides information about the relative abundance or biomass of the fish stock.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "This data comes from fishery independent surveys and monitoring.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "For American Plaice there are four surveys based on the Northeast Fishery Science Center trawl survey spilt bewteen different research vessels.",
          style = "font-size: 20px; color: #7f8c8d",
          tags$ul(
            tags$li("1. Spring Albatross (solid green)", style = "font-size: 20px; color: #407331"),
            tags$li("2. Spring Bigelow (dashed green)", style = "font-size: 20px; color: #407331"),
            tags$li("3. Fall Albatross (solid blue)", style = "font-size: 20px; color: #00608a"),
            tags$li("4. Fall Bigelow (dashed blue)", style = "font-size: 20px; color: #00608a")
          )
        )
      )),
      
      catch = tagList(tags$ul(
        tags$li(
          "Total catch for a stock includes commercial and recreational landings and discards.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "Dealer reports provide a census of commercial landings and Vessel Trip Reports provide a census of fishing effort by stock area and the two are linked to derive landings by stock area.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "At sea monitoring (e.g. Fisheries Observer Program) is primarily ised to estimate discarded commercial catch.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "Recreational data is collected through the MRIP program which uses a combination of angler intercept surveys and mail-based surveys to characterize catch.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "For American Plaice, catch inludes only commercial landings and discards becasue they are not encountered in the recreational fishery.",
          style = "font-size: 20px; color: #7f8c8d"
        )
      )),
      
      mortality = tagList(tags$ul(
        tags$li(
          "Natural mortality (M) data indicates the rate at which fish die due to natural causes.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "Natural mortality is usually a fixed number but can vary over ages or time. This data can come from tagging, relationships with life-history traits, or sometimes be estimated in the stock assessment model.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "For American Plaice, natural mortality is assumed as 0.3 for all ages.",
          style = "font-size: 20px; color: #7f8c8d"
        )
      )),
      maturity = tagList(tags$ul(
        tags$li(
          "Maturity data describes the proportion of the fish population that has reached reproductive age.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "This data comes from fishery independent surveys that take biological information.",
          style = "font-size: 20px; color: #7f8c8d"
        )
      )),
      weight = tagList(tags$ul(
        tags$li(
          "Weight-at-age data provides information on the average weight of fish at different ages.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "There can be different weight-at-age for fishery indpendent data and fishery dependent data",
          style = "font-size: 20px; color: #7f8c8d"
        )
      )),
      catchability = tagList(tags$ul(
        tags$li(
          "Catchability (q) data reflects the probability of capturing a fish in a survey.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "This can be estimated in the stock assessment model. Each index has an associated catchability.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "For American Plaice:",
          style = "font-size: 20px; color: #7f8c8d" ,
          tags$ul(
            tags$li("1. Spring Albatross (solid green)", style = "font-size: 20px; color: #407331"),
            tags$li("2. Spring Bigelow (dashed green)", style = "font-size: 20px; color: #407331"),
            tags$li("3. Fall Albatross (solid blue)", style = "font-size: 20px; color: #00608a"),
            tags$li("4. Fall Bigelow (dashed blue)", style = "font-size: 20px; color: #00608a")
          )
        )
      )),
      selectivity = tagList(tags$ul(
        tags$li(
          "Selectivity data shows how the fishing gear affects different age groups of the fish stock.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "This can be estimated in the stock assessment model and gear selectivity studies.",
          style = "font-size: 20px; color: #7f8c8d"
        ),
        tags$li(
          "Selectivity can be different for different time periods.",
          style = "font-size: 20px; color: #7f8c8d"
        )
        
      ))
    )
    headers <- c(
      index = "Abundance Index",
      catch = "Catch",
      mortality = "Natural Mortality",
      maturity = "Maturity",
      weight = "Weight-at-age",
      catchability = "Catchability",
      selectivity = "Selectivity"
    )
    tags$div(h3(headers[[inputType]]), tags$p(descriptions[[inputType]]))
  })
  
  output$inputHeaders <- renderUI({
    inputType <- input$inputType
    headers <- c(
      index = "Abundance Index Input Data",
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
      index = "Abundance Index Input Data",
      catch = "Catch Input Data",
      mortality = "Natural Mortality Input Data",
      maturity = "Maturity Input Data",
      weight = "Weight-at-age Input Data",
      catchability = "Catchability Input Data",
      selectivity = "Selectivity Input Data"
    )
    h3(headers[[inputType]])
    
  })
  custom_colors <- c(
    "Index1" = "#407331",
    "Index2" = "#407331",
    "Index3" = "#00608a",
    "Index4" = "#00608a"
  )
  
  custom_linetypes <- c(
    "Index1" = "solid",
    "Index2" = "dashed",
    "Index3" = "solid",
    "Index4" = "dashed"
  )
  
  output$inputPlot <- renderPlot({
    data <- input_data()
    
    p <- switch(
      input$inputType,
      "index" = ggplot(data) +
        geom_line(
          aes(
            x = Year,
            y = Index1,
            color = "Index1",
            linetype = "Index1"
          ),
          linewidth = 1
        ) +
        geom_line(
          aes(
            x = Year,
            y = Index2,
            color = "Index2",
            linetype = "Index2"
          ),
          linewidth = 1
        ) +
        geom_line(
          aes(
            x = Year,
            y = Index3,
            color = "Index3",
            linetype = "Index3"
          ),
          linewidth = 1
        ) +
        geom_line(
          aes(
            x = Year,
            y = Index4,
            color = "Index4",
            linetype = "Index4"
          ),
          linewidth = 1
        ) +
        scale_color_manual(
          values = c(
            "Index1" = "#407331",
            "Index2" = "#407331",
            "Index3" = "#00608a",
            "Index4" = "#00608a"
          )
        ) +
        scale_linetype_manual(
          values = c(
            "Index1" = "solid",
            "Index2" = "dashed",
            "Index3" = "solid",
            "Index4" = "dashed"
          )
        ) +
        guides(
          color = guide_legend("Abundance Indices"),
          linetype = guide_legend("Abundance Indices")
        ) +
        expand_limits(y = 0) +
        ylab("Abundance Indices (kg/tow)"),
      
      "catch" = ggplot(data) + geom_col(
        aes(x = Year, y = Catch),
        width = 0.8,
        color = "#00608a",
        fill = "#00608a"
      ) +
        ylab("Catch (mt)") ,
      
      "mortality" = ggplot(data) + geom_line(
        aes(x = Age, y = M),
        linewidth = 1.5,
        color = "#00608a"
      ),
      
      "maturity" = ggplot(data) + geom_line(
        aes(x = Age, y = Maturity),
        color = "#00608a",
        linewidth = 1.5
      ) +
        labs(y = "Maturity") ,
      
      "selectivity" = ggplot(data) + geom_line(
        aes(x = Age, y = Selectivity),
        color = "#00608a",
        linewidth = 1.5
      ) +
        labs(y = "Fishery selectivity") ,
      
      "weight" = ggplot(data) + geom_line(aes(
        x = Year,
        y = Weight,
        color = factor(Age)
      ), linewidth = 1.5) +
        labs(y = "Weight-at-age for SSB", color = "Age") +
        scale_color_gmri(palette = "main") ,
      
      "catchability" = ggplot(data) + geom_line(
        aes(
          x = Year,
          y = Catchability,
          color = factor(Survey),
          linetype = factor(Survey)
        ),
        linewidth = 1.5
      ) +
        scale_color_manual(values = custom_colors, guide = "none") +
        scale_linetype_manual(values = custom_linetypes, guide = "none") +
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
  
  # output$dataTable <- renderTable({
  #   inputType <- input$inputType
  #   
  #   if (inputType == "catchability") {
  #     input_data() %>%
  #       pivot_wider(names_from = Survey, values_from = Catchability)
  #   } else {
  #     input_data()
  #   }
  # })
  
  
  output$dataTable <- renderDataTable({
    inputType <- input$inputType
    
    if (inputType == "catchability") {
      input_data() %>%
        pivot_wider(names_from = Survey, values_from = Catchability)
    } else {
      input_data()
    }
  }, options = list(
    pageLength = 42,
    initComplete = JS(
      "function(settings, json) {",
      "$('td').css({'font-size': '18px'});",  # Change font size here
      "}"),
    columnDefs = list(list(targets = 0, visible = FALSE))
    ))
  
  
  observeEvent(input$sidebar, {
    if (input$sidebar == "results") {
      shinyjs::show(id = "full-page-spinner")
      shinyjs::delay(3000,
                     shinyjs::hide(
                       id = "full-page-spinner",
                       anim = TRUE,
                       animType = "fade"
                     ))
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
    if (input$dataSelection == "low") {
      #refs[c(1:3,5)]
      #colnames(refs)[colnames(refs)== "catch_low"] <- "catch"
      refs <- dplyr::rename(refs, catch = catch_low)
    } else {
      #refs[c(1:2,4:5)]
      #colnames(refs)[colnames(refs)== "catch_high"] <- "catch"
      refs <- dplyr::rename(refs, catch = catch_high)
    }
  })
  
  output$biomassPlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x = year, y = SSB),
                                    color = "#00608a",
                                    linewidth = 1) +
      geom_ribbon(
        aes(x = year, ymin = SSB_lower, ymax = SSB_upper),
        fill = "#00608a" ,
        alpha = 0.5
      ) +
      expand_limits(y = 0) +
      labs(title = "", x = "Year", y = "Spawning Stock Biomass (mt)")
  })
  
  output$fishingMortalityPlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x = year, y = F),
                                    color = "#00608a",
                                    linewidth = 1) +
      geom_ribbon(
        aes(x = year, ymin = F_lower, ymax = F_upper),
        fill = "#00608a" ,
        alpha = 0.5
      ) +
      labs(title = "", x = "Year", y = "Fishing Mortality") +
      expand_limits(y = 0)
  })
  
  output$recruitmentPlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x = year, y = R),
                                    color = "#00608a",
                                    linewidth = 1) +
      geom_ribbon(
        aes(x = year, ymin = R_lower, ymax = R_upper),
        fill = "#00608a" ,
        alpha = 0.5
      ) +
      labs(title = "", x = "Year", y = "Recruitment (000s)") +
      expand_limits(y = 0)
  })
  
  output$biomassReferencePlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x = year, y = SSB),
                                    color = "#00608a",
                                    linewidth = 1) +
      geom_ribbon(
        aes(x = year, ymin = SSB_lower, ymax = SSB_upper),
        fill = "#00608a" ,
        alpha = 0.5
      ) +
      geom_hline(
        yintercept = refs$base[2],
        color = "black",
        linewidth = 1
      ) +
      geom_hline(
        yintercept = refs$base[2] / 2,
        color = "black",
        linewidth = 1,
        linetype = "dashed"
      ) +
      expand_limits(y = 0) +
      labs(x = "Year", y = "Biomass (mt)")
  })
  
  output$fishingMortalityReferencePlot <- renderPlot({
    ggplot(inputs_year) + geom_line(aes(x = year, y = F),
                                    color = "#00608a",
                                    linewidth = 1) +
      geom_ribbon(
        aes(x = year, ymin = F_lower, ymax = F_upper),
        fill = "#00608a" ,
        alpha = 0.5
      ) +
      geom_hline(
        yintercept = refs$base[1],
        color = "black",
        linewidth = 1
      ) +
      labs(title = "", x = "Year", y = "Fishing Mortality")
  })
  
  output$diagnosticsPlot <- renderPlot({
    ggplot(base_retro, aes(x = year, y = SSB, color = factor(peel), group = peel)) +
      geom_line(linewidth = 1) +
      labs(x = "Year",
           y = "SSB",
           color = "Peel")+
      scale_color_gmri(palette = "main")
  })
  output$diagnosticsPlotb <- renderPlot({
    ggplot(base_retro, aes(x = year, y = F, color = factor(peel), group = peel)) +
      geom_line(linewidth = 1) +
      labs(x = "Year",
           y = "F",
           color = "Peel")+
      scale_color_gmri(palette = "main")
  }) 
  
  #### comparisons
  dynamicCatchText <- reactive({
    if (input$dataSelection == "low") {
      "This scenario compares the base catch (blue) to catch decreased by 50% (green) to show the impact of catch on stock assessment results."
    } else if (input$dataSelection == "high") {
      "This scenario compares the base catch (blue) to catch increased by 50% (orange) to show the impact of catch on stock assessment results."
    }
  })
  
  # Render the text dynamically
  output$dynamicCatchText <- renderUI({
    tags$p(dynamicCatchText(), style = "font-size: 20px;")
  })
  
  
  output$comparisonPlot <- renderPlot({
    selected_data <- selected_catch()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    
    ggplot() +
      geom_line(
        data = inputs_year,
        aes(
          x = year,
          y = catch,
          color = "Base Catch",
          linetype = "Base Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = catch_lower,
          ymax = catch_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = catch,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = catch_lower,
          ymax = catch_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      labs(
        x = "Year",
        y = "Catch (mt)",
        color = "",
        fill = "",
        linetype = ""
      ) + # fills out the legend
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed"))  +
      ylim(0, 40000)
    
  })
  output$comparisonPlot2 <- renderPlot({
    ggplot() +
      geom_line(
        data = inputs_year,
        aes(x = year, y = V1),
        color = "#407331",
        linewidth = 1
      ) +
      geom_line(
        data = inputs_year,
        aes(x = year, y = V2),
        color = "#407331",
        linewidth = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = inputs_year,
        aes(x = year, y = V3),
        color = "#00608a",
        linewidth = 1
      ) +
      geom_line(
        data = inputs_year,
        aes(x = year, y = V4),
        color = "#00608a",
        linewidth = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = index_bias,
        aes(x = year, y = V1),
        color = "#EA4F12",
        linewidth = 1
      ) +
      geom_line(
        data = index_bias,
        aes(x = year, y = V2),
        color = "#EA4F12",
        linewidth = 1,
        linetype = "dashed"
      ) +
      geom_line(
        data = index_bias,
        aes(x = year, y = V3),
        color = "#ABB400",
        linewidth = 1
      ) +
      geom_line(
        data = index_bias,
        aes(x = year, y = V4),
        color = "#ABB400",
        linewidth = 1,
        linetype = "dashed"
      ) +
      expand_limits(y = 0) +
      labs(y = "Indices (kg/tow)", x = "Year")
  })
  output$comparisonPlot3 <- renderPlot({
    base_colors <- c("#407331", "#407331", "#00608a", "#00608a")
    
    base_linetypes <- c("solid", "dashed", "solid", "dashed")
    
    bias_colors <- c("#ABB400", "#ABB400", "#00736D", "#00736D")
    
    bias_linetypes <- c("solid", "dashed", "solid", "dashed")
    
    # Create an empty ggplot object
    p <- ggplot() +
      expand_limits(y = 0) +
      labs(y = "Indices (kg/tow)", x = "Year") +
      theme_minimal() +
      theme(text = element_text(size = 20))
    
    if ("base" %in% input$indexSelection) {
      p <- p +
        geom_line(
          data = inputs_year,
          aes(x = year, y = V1),
          color = "#407331",
          linewidth = 1
        ) +
        geom_line(
          data = inputs_year,
          aes(x = year, y = V2),
          color = "#407331",
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_line(
          data = inputs_year,
          aes(x = year, y = V3),
          color = "#00608a",
          linewidth = 1
        ) +
        geom_line(
          data = inputs_year,
          aes(x = year, y = V4),
          color = "#00608a",
          linewidth = 1,
          linetype = "dashed"
        )
    }
    
    if ("bias" %in% input$indexSelection) {
      p <- p +
        geom_line(
          data = index_bias,
          aes(x = year, y = V1),
          color = "#ABB400",
          linewidth = 1
        ) +
        geom_line(
          data = index_bias,
          aes(x = year, y = V2),
          color = "#ABB400",
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_line(
          data = index_bias,
          aes(x = year, y = V3),
          color = "#EA4F12",
          linewidth = 1
        ) +
        geom_line(
          data = index_bias,
          aes(x = year, y = V4),
          color = "#EA4F12",
          linewidth = 1,
          linetype = "dashed"
        )
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
  
  output$dataTable2 <- renderDataTable({
    selected_data <- selected_catch()
    df <- data.frame(
      Year = inputs_year$year,
      "Base Catch" = round(inputs_year$catch,2),
      "Bias Catch" = round(selected_data$catch,2)
    )
  }, options = list(
    pageLength = 42,
    initComplete = JS(
      "function(settings, json) {",
      "$('td').css({'font-size': '18px'});",  # Change font size here
      "}"),
    columnDefs = list(list(targets = 0, visible = FALSE))
  ))
  
  output$dataTable3 <- renderDataTable({
    selected_data <- selected_catch()
    df <- data.frame(
      Year = inputs_year$year,
      "Base Index1" = round(inputs_year$V1,2),
      "Low Index1" = round(index_bias$V1,2),
      "Base Index2"= round(inputs_year$V2,2),
      "Low Index2" = round(index_bias$V2,2),
      "Base Index3"= round(inputs_year$V3,2),
      "Low Index3" = round(index_bias$V3,2),
      "Base Index4"= round(inputs_year$V4,2),
      "Low Index4" = round(index_bias$V4,2)
    )
  }, options = list(
    pageLength = 42,
    initComplete = JS(
      "function(settings, json) {",
      "$('td').css({'font-size': '18px'});",  # Change font size here
      "}"),
    columnDefs = list(list(targets = 0, visible = FALSE)),
    scrollY=TRUE,
    scrollX=TRUE
  ))
  
  output$biomassPlot2 <- renderPlot({
    selected_data <- selected_catch()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = SSB,
        color = "Base Catch",
        linetype = "Base Catch"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = SSB,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      labs(
        title = "",
        x = "Year",
        y = "Spawning Stock Biomass (mt)",
        color = "",
        fill = "",
        linetype = ""
      ) +
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed"))
  })
  
  output$fishingMortalityPlot2 <- renderPlot({
    selected_data <- selected_catch()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = F,
        color = "Base Catch",
        linetype = "Base Catch"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = F,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      labs(
        title = "",
        x = "Year",
        y = "Fishing Mortality",
        color = "",
        fill = "",
        linetype = ""
      ) +
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed"))
  })
  
  output$recruitmentPlot2 <- renderPlot({
    selected_data <- selected_catch()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = R,
        color = "Base Catch",
        linetype = "Base Catch"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = R_lower,
          ymax = R_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = R,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = R_lower,
          ymax = R_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      labs(
        title = "",
        x = "Year",
        y = "Recruitment (000s)",
        color = "",
        fill = "",
        linetype = ""
      ) +
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed"))
  })
  
  output$biomassPlot3 <- renderPlot({
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = SSB,
        color = "Base Index",
        linetype = "Base Index"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Base Index",
          color = "Base Index",
          linetype = "Base Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = index_bias,
        aes(
          x = year,
          y = SSB,
          color = "Bias Index",
          linetype = "Bias Index"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = index_bias,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Bias Index",
          color = "Bias Index",
          linetype = "Bias Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      scale_color_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_fill_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_linetype_manual(values = c("Base Index" = "solid", "Bias Index" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Spawning Stock Biomass (mt)",
        color = "",
        fill = "",
        linetype = ""
      )
  })
  output$fishingMortalityPlot3 <- renderPlot({
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = F,
        color = "Base Index",
        linetype = "Base Index"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Base Index",
          color = "Base Index",
          linetype = "Base Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = index_bias,
        aes(
          x = year,
          y = F,
          color = "Bias Index",
          linetype = "Bias Index"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = index_bias,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Bias Index",
          color = "Bias Index",
          linetype = "Bias Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      scale_color_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_fill_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_linetype_manual(values = c("Base Index" = "solid", "Bias Index" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Fishing Mortality",
        color = "",
        fill = "",
        linetype = ""
      )
  })
  
  output$recruitmentPlot3 <- renderPlot({
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = R,
        color = "Base Index",
        linetype = "Base Index"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = R_lower,
          ymax = R_upper,
          fill = "Base Index",
          color = "Base Index",
          linetype = "Base Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_line(
        data = index_bias,
        aes(
          x = year,
          y = R,
          color = "Bias Index",
          linetype = "Bias Index"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = index_bias,
        aes(
          x = year,
          ymin = R_lower,
          ymax = R_upper,
          fill = "Bias Index",
          color = "Bias Index",
          linetype = "Bias Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      expand_limits(y = 0) +
      scale_color_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_fill_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_linetype_manual(values = c("Base Index" = "solid", "Bias Index" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Recruitment",
        color = "",
        fill = "",
        linetype = ""
      )
  })
  
  output$biomassReferencePlot2 <- renderPlot({
    selected_data <- selected_catch()
    selected_rps <- selected_refs()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = SSB,
        color = "Base Catch",
        linetype = "Base Catch"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = selected_rps$base[2],
        color = "black",
        linewidth = 1
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = SSB,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = selected_rps$catch[2],
        color = "black",
        linewidth = 1,
        linetype = "dashed"
      ) +
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Biomass",
        color = "",
        fill = "",
        linetype = ""
      ) +
      expand_limits(y = 0) +
      theme(legend.position = "bottom")
  })
  
  output$fishingMortalityReferencePlot2 <- renderPlot({
    selected_data <- selected_catch()
    selected_rps <- selected_refs()
    # Dynamic colors
    bias_color <- if (input$dataSelection == "low") {
      "#407331"  # Blue for lower catch
    } else {
      "#EA4F12"  # orange for higher catch
    }
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = F,
        color = "Base Catch",
        linetype = "Base Catch"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Base Catch",
          color = "Base Catch",
          linetype = "Base Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = selected_rps$base[1],
        color = "black",
        linewidth = 1
      ) +
      geom_line(
        data = selected_data,
        aes(
          x = year,
          y = F,
          color = "Bias Catch",
          linetype = "Bias Catch"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = selected_data,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Bias Catch",
          color = "Bias Catch",
          linetype = "Bias Catch"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = selected_rps$catch[1],
        color = "black",
        linewidth = 1,
        linetype = "dashed"
      ) +
      expand_limits(y = 0) +
      scale_color_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_fill_manual(values = c("Base Catch" = "#00608a", "Bias Catch" = bias_color)) +
      scale_linetype_manual(values = c("Base Catch" = "solid", "Bias Catch" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Fishing Mortality",
        color = "",
        fill = "",
        linetype = ""
      ) +
      theme(legend.position = "bottom")
  })
  output$biomassReferencePlot3 <- renderPlot({
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = SSB,
        color = "Base Index",
        linetype = "Base Index"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Base Index",
          color = "Base Index",
          linetype = "Base Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = refs$base[2],
        color = "black",
        linewidth = 1
      ) +
      geom_line(
        data = index_bias,
        aes(
          x = year,
          y = SSB,
          color = "Bias Index",
          linetype = "Bias Index"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = index_bias,
        aes(
          x = year,
          ymin = SSB_lower,
          ymax = SSB_upper,
          fill = "Bias Index",
          color = "Bias Index",
          linetype = "Bias Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = refs$index[2],
        color = "black",
        linewidth = 1,
        linetype = "dashed"
      ) +
      expand_limits(y = 0) +
      scale_color_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_fill_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_linetype_manual(values = c("Base Index" = "solid", "Bias Index" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Biomass (mt)",
        color = "",
        fill = "",
        linetype = ""
      ) +
      theme(legend.position = "bottom")
    
  })
  
  output$fishingMortalityReferencePlot3 <- renderPlot({
    ggplot() + geom_line(
      data = inputs_year,
      aes(
        x = year,
        y = F,
        color = "Base Index",
        linetype = "Base Index"
      ),
      linewidth = 1
    ) +
      geom_ribbon(
        data = inputs_year,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Base Index",
          color = "Base Index",
          linetype = "Base Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = refs$base[1],
        color = "black",
        linewidth = 1
      ) +
      geom_line(
        data = index_bias,
        aes(
          x = year,
          y = F,
          color = "Bias Index",
          linetype = "Bias Index"
        ),
        linewidth = 1
      ) +
      geom_ribbon(
        data = index_bias,
        aes(
          x = year,
          ymin = F_lower,
          ymax = F_upper,
          fill = "Bias Index",
          color = "Bias Index",
          linetype = "Bias Index"
        ) ,
        alpha = 0.5,
        linewidth = 0.75
      ) +
      geom_hline(
        yintercept = refs$index[1],
        color = "black",
        linewidth = 1,
        linetype = "dashed"
      ) +
      scale_color_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_fill_manual(values = c(
        "Base Index" = "#00608a",
        "Bias Index" = "#407331"
      )) +
      scale_linetype_manual(values = c("Base Index" = "solid", "Bias Index" = "dashed")) +
      labs(
        title = "",
        x = "Year",
        y = "Biomass (mt)",
        color = "",
        fill = "",
        linetype = ""
      ) +
      theme(legend.position = "bottom")
  })
  
  # Check if Google Sheets is properly authenticated
  sheets_authorized <- reactive({
    tryCatch({
      token <- gs4_has_token()
      if (!token) {
        message("No valid Google Sheets token found")
        return(FALSE)
      }
      return(TRUE)
    }, error = function(e) {
      message("Authorization error: ", e$message)
      FALSE
    })
  })
  
  # Handle form submission
  observeEvent(input$submit, {
    # Validate required fields
    if (is.null(input$name) || input$name == "") {
      showNotification("Please enter your name", type = "error")
      return()
    }
    
    # Check if Google Sheets is authorized
    if (!sheets_authorized()) {
      showNotification("Google Sheets authentication failed. Please check your credentials.",
                       type = "error")
      return()
    }
    
    # Create new feedback entry
    new_feedback <- data.frame(
      timestamp = as.character(Sys.time()),
      name = input$name,
      email = input$email,
      organization = input$organization,
      role = input$role,
      easy = input$easy,
      relevant = input$relevant,
      clear = input$clear,
      improvements = input$improvements,
      comments = input$comments,
      stringsAsFactors = FALSE
    )
    
    # Save to Google Sheets with proper error handling
    tryCatch({
      sheet_append(SHEET_ID, new_feedback)
      
      # Move success notification inside tryCatch
      showNotification("Feedback saved successfully!", type = "message")  # Changed from "success" to "message")
                       
                       # Show thank you message
                       output$thankyou <- renderUI({
                         div(
                           class = "info-box",
                           h3("Thank You!"),
                           p(
                             "We appreciate your feedback. Your response has been recorded."
                           )
                         )
                       })
                       
                       # Reset form
                       updateTextInput(session, "name", value = "")
                       updateTextInput(session, "email", value = "")
                       updateTextInput(session, "organization", value = "")
                       updateTextInput(session, "role", value = "")
                       updateTextInput(session, "easy", value = "")
                       updateTextInput(session, "relevant", value = "")
                       updateTextInput(session, "clear", value = "")
                       updateTextInput(session, "improvements", value = "")
                       updateTextAreaInput(session, "comments", value = "")
                       
    }, error = function(e) {
      showNotification("Failed to save feedback. Please try again.", type = "error")
    })
  })
}
