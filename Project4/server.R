library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(caret)
library(leaflet)
library(corrplot)


source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")

#For Github render
#source("DataHelper.R")


shinyServer(function(session, input, output) {
  
  output$theme <- renderUI({
    if (input$theme[1] == TRUE){
      shinyDashboardThemes(
        theme = "grey_dark"
      )
    }else{
      shinyDashboardThemes(
        theme = "grey_light"
      )
    }
    })
  
  ##-----------------------------------------Data TAB-------------------------------------------
  getDataTabData <- reactive({
    if(input$moreOpts){
      if (input$dataBorough == "All"){
        if (input$dataArrangeTable){
          if (input$dataArrange ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2]) %>% arrange(desc(Price)) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2]) %>% arrange(Price) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
          }
        }
        else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2]) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
        }
      } else{
        if (input$dataArrangeTable){
          if (input$dataArrange ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% arrange(Price) %>% filter(Borough == input$dataBorough) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
          }
        }
        else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% filter(Borough == input$dataBorough) %>% filter(Type == input$typeListData) %>% filter(Make_Year >= input$minYearData)
        }
      }
    }else{
      if (input$dataBorough == "All"){
        if (input$dataArrangeTable){
          if (input$dataArrange ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% 
              filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2]) %>% arrange(desc(Price))
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2])%>% 
              filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% arrange(Price)
          }
        }
        else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])
        }
      } else{
        if (input$dataArrangeTable){
          if (input$dataArrange ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% arrange(Price) %>% filter(Borough == input$dataBorough)
          }
        }
        else{
          newData <- readData() %>% filter(Rating>=input$ratingSlider[1]) %>% filter(Rating<=input$ratingSlider[2]) %>% filter(Price >= input$priceSlider[1]) %>% filter(Price <= input$priceSlider[2])%>% filter(Borough == input$dataBorough)
        }
      }
    }
    
  })
  
  output$table <- renderDataTable({
    
    tab <- getDataTabData()
    
    #print(input$allCols)
    #df <- NULL
    
    
    #for (i in input$allCols){
    #  df <- cbind(df, data.frame(tab)[i])
    #}
    #tab <- data.frame(getDataTabData())[, input$allCols]

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
  
  output$observs <- renderInfoBox({
    newData <- getDataTabData()
    infoBox(
      paste0("Total Listings"), paste0("Total Airbnb Listings based on above filters are ", nrow(newData)), icon = icon("globe"),
      color = "red", fill = TRUE
    )
  })
  
  
  ## ------------------------------------Visualizations TAB--------------------------------------
  
  getFilteredVizData <- reactive({
    if(input$moreOpts1){
      if (input$dataBorough1 == "All"){
        #if (input$dataArrangeTable1){
          if (input$dataArrange1 ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price)) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(Price) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
          }
        #}
        #else{
        #  newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
        #}
      } else{
        #if (input$dataArrangeTable1){
          if (input$dataArrange1 ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough1) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% arrange(Price) %>% filter(Borough == input$dataBorough1) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
          }
        #}
        #else{
        #  newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% filter(Borough == input$dataBorough1) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1)
        #}
      }
    }else{
      if (input$dataBorough1 == "All"){
        #if (input$dataArrangeTable1){
          if (input$dataArrange1 ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% 
              filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price))
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% 
              filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% arrange(Price)
          }
        #}
        #else{
        #  newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])
        #}
      } else{
        #if (input$dataArrangeTable1){
          if (input$dataArrange1 ==  "Descending"){
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough1)
          }else{
            newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% arrange(Price) %>% filter(Borough == input$dataBorough1)
          }
        #}
        #else{
        #  newData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2]) %>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2])%>% filter(Borough == input$dataBorough1)
        #}
      }
    }
    
  })
  
  
  getDataViz <- readData()
  
  output$plot1 <- renderPlot({
    

    if (input$varX == "Number of Listings"){
      df <- getDataViz %>% group_by_(input$varY, input$grpBy) %>% summarize(count = n())
    }
    
    
    g <- ggplot(df, aes_string(x = input$varY, y = "count")) + 
      geom_bar(aes_string(fill = input$varY), stat = "identity") +
      facet_grid(~get(input$grpBy)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90), 
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"),
            strip.text.x = element_text(size = 14))
      
    g
    
  })
  
  output$geoPlot <- renderLeaflet({
    
    if(input$moreOpts1){
      if (input$dataBorough1 == "All"){
        if (input$dataArrange1 ==  "Descending"){
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price)) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1) %>% select(Lat, Long, Price, Type, Rating)
        }else{
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(Price) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1) %>% select(Lat, Long, Price, Type, Rating)
        }
      } else{
        if (input$dataArrange1 ==  "Descending"){
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough1) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1) %>% select(Lat, Long, Price, Type, Rating)
        }else{
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(Price) %>% filter(Borough == input$dataBorough1) %>% filter(Type == input$typeListData1) %>% filter(Make_Year >= input$minYearData1) %>% select(Lat, Long, Price, Type, Rating)
        }
      }
    }else{
      if (input$dataBorough1 == "All"){
        if (input$dataArrange1 ==  "Descending"){
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price)) %>% select(Lat, Long, Price, Type, Rating)
        }else{
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(Price) %>% select(Lat, Long, Price, Type, Rating)
        }
      } else{
        if (input$dataArrange1 ==  "Descending"){
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(desc(Price)) %>% filter(Borough == input$dataBorough1) %>% select(Lat, Long, Price, Type, Rating)
        }else{
          geoData <- readData() %>% filter(Rating>=input$ratingSlider1[1]) %>% filter(Rating<=input$ratingSlider1[2])%>% filter(Price >= input$priceSlider1[1]) %>% filter(Price <= input$priceSlider1[2]) %>% arrange(Price) %>% filter(Borough == input$dataBorough1) %>% select(Lat, Long, Price, Type, Rating)
        }
      }
    }
    
    geoData <- geoData[1:input$plotnum1,]
    
    l <- leaflet() %>% addTiles()
    
    for (i in seq(1:input$plotnum1)){
      l <- l %>% addMarkers(lng=geoData[i, 2]$Long, lat=geoData[i,1]$Lat, label = paste0("Type: ", geoData[i,4]$Type," ($",geoData[i,3]$Price, ", Rating: ",geoData[i,5]$Rating , ")"))
    }
    
    
    l
    
  })
  
  output$priceBox <- renderInfoBox({
    newData <- getFilteredVizData()
    infoBox(
      paste0("Price"), paste0("Average Price of Airbnb Listings (Based on above Filters) is: $", round(mean(newData$Price, na.rm = TRUE), 2)), icon = icon("dollar-sign"),
      color = "red", fill = TRUE
    )
  })
  
  output$ratingBox <- renderInfoBox({
    newData <- getFilteredVizData()
    infoBox(
      paste0("Rating"), paste0("Average Rating of Airbnb Listings (Based on above Filters) is: ", round(mean(newData$Rating, na.rm = TRUE), 2)), icon = icon("star"),
      color = "red", fill = TRUE
    )
  })
  
  output$availBox <- renderInfoBox({
    newData <- getFilteredVizData()
    infoBox(
      paste0("Availability"), paste0("Average Availability of Airbnb Listings (Based on above Filters) is for ", round(mean(newData$Availability, na.rm = TRUE), 2), " Days"), icon = icon("calendar-days"),
      color = "red", fill = TRUE
    )
  })
  
  output$viztotalObs <- renderInfoBox({
    newData <- getFilteredVizData()
    infoBox(
      paste0("Listings"), paste0("Total Airbnb Listings based on above filters are ", nrow(newData)), icon = icon("globe"),
      color = "red", fill = TRUE
    )
  })

  
  
  ##--------------------------------------Modelling TAB-------------------------------------------------

  

  
  })
