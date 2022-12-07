library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
library(DT)
library(caret)
library(leaflet)

#source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")
source("DataHelper.R")


shinyServer(function(session, input, output) {
  
  
  ##-----------------------------------------Data TAB-------------------------------------------
  getDataTabData <- reactive({
    if (input$dataBorough == "All"){
      if (input$dataArrangeTable){
        if (input$dataArrange ==  "Descending"){
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% arrange(desc(Price))
        }else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% arrange(Price)
        }
      }
      else{
        newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])
      }
    } else{
      if (input$dataArrangeTable){
        if (input$dataArrange ==  "Descending"){
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough)
        }else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% arrange(Price) %>% filter(Borough == input$dataBorough)
        }
      }
      else{
        newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Borough == input$dataBorough)
      }
      #newData <- readData() %>% filter(Borough == input$dataBorough)
    }
  })
  
  
  output$table <- renderDataTable({
    
    tab <- getDataTabData()
    
    output$download <- downloadHandler(
      filename = function(){"Airbnb_Data.csv"}, 
      content = function(fname){
        write.csv(tab, fname)
      }
    )
    
    
    datatable(tab, options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '200px', targets = c(1, 3)))
    ))
    
  })
  
  
  output$geoPlot <- renderLeaflet({
    if (input$dataArrange == "Descending"){
      geoData <- getDataTabData() %>% arrange(desc(Price)) %>% select(Lat, Long, Price, Type, Rating)
    }else{
    geoData <- getDataTabData() %>% arrange(Price) %>% select(Lat, Long, Price, Type, Rating)
    }
    geoData <- geoData[1:input$plotnum,]
    
    l <- leaflet() %>% addTiles()
    
    for (i in seq(1:input$plotnum)){
      l <- l %>% addMarkers(lng=geoData[i, 2]$Long, lat=geoData[i,1]$Lat, label = paste0("Type: ", geoData[i,4]$Type," ($",geoData[i,3]$Price, ", Rating: ",geoData[i,5]$Rating , ")"))
    }
    
    l
    
  })
  
  
  
  output$dataTabInfo <- renderText({
    #get filtered data
    newData <- getDataTabData()
    paste0("The average Airbnb price for ", input$dataBorough, " Borough(s) is: $", round(mean(newData$Price, na.rm = TRUE), 2), " (See Below for More Filtering)")
  })
  
  
  ## ------------------------------------Visualizations TAB--------------------------------------
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  

  
  
  ##--------------------------------------Modelling TAB-------------------------------------------------
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", "Shyam", icon = icon("list"),
      color = "lime", fill = TRUE
    )
  })
  

  
  })
