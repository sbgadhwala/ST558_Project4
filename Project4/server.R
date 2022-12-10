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
library(shinycssloaders)


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
    
    
    
    columns = names(readData())
    
    columns = input$allCols
   
    
    tab <- data.frame(getDataTabData())
    
    tt <- tab[, columns, drop=FALSE]

    output$download <- downloadHandler(
      filename = function(){"Airbnb_Data.csv"}, 
      content = function(fname){
        write.csv(tt, fname)
      }
    )
    
    
    datatable(tt, options = list(
      autoWidth = TRUE
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
      df <- getDataViz %>% group_by_(input$varY, input$grpBy) %>% summarize(Listings = n())
    
    
    if (input$plotType == "Bar Plot"){
    g <- ggplot(df, aes_string(x = input$varY, y = "Listings")) + 
      geom_bar(aes_string(fill = input$varY), stat = "identity") +
      facet_grid(~get(input$grpBy)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90), 
            axis.text=element_text(size=14),
            axis.title=element_text(size=16,face="bold"),
            strip.text.x = element_text(size = 14))
    }else if(input$plotType == "Line Plot"){
      g <- ggplot(df, aes_string(x = input$varY, y = "Listings", group = 1, color = input$grpBy)) + 
        geom_point() +
        geom_line(method = "lm") +
        facet_grid(~get(input$grpBy)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90), 
              axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"),
              strip.text.x = element_text(size = 14))
    }else if(input$plotType == "Box Plot"){
      g <- ggplot(df, aes_string(x = input$varY, y = "Listings")) + 
        geom_boxplot(size=1, aes_string(color = input$grpBy)) + 
        geom_jitter(size=3, aes_string(color = input$grpBy)) +
        facet_grid(~get(input$grpBy)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90), 
              axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"),
              strip.text.x = element_text(size = 14))
    }else{
      g <- ggplot(df, aes_string(x = input$varY, y = "Listings", group = 1, color = input$grpBy)) + 
        geom_point() +
        geom_smooth(method = "lm") +
        facet_grid(~get(input$grpBy)) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90), 
              axis.text=element_text(size=14),
              axis.title=element_text(size=16,face="bold"),
              strip.text.x = element_text(size = 14))
    }
    }
    
    if (input$varX == "Average Price"){
      df <- getDataViz %>% group_by_(input$varY, input$grpBy) %>% summarize(Average_Price = mean(Price))
      
      
      if (input$plotType == "Bar Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Price")) + 
          geom_bar(aes_string(fill = input$varY), stat = "identity") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Line Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Price", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_line(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Box Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Price")) + 
          geom_boxplot(size=1, aes_string(color = input$grpBy)) + 
          geom_jitter(size=3, aes_string(color = input$grpBy)) +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else{
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Price", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_smooth(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }
    }
    
    if (input$varX == "Average Availability"){
      df <- getDataViz %>% group_by_(input$varY, input$grpBy) %>% summarize(Average_Availability = mean(Availability))
      
      
      if (input$plotType == "Bar Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Availability")) + 
          geom_bar(aes_string(fill = input$varY), stat = "identity") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Line Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Availability", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_line(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Box Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Availability")) + 
          geom_boxplot(size=1, aes_string(color = input$grpBy)) + 
          geom_jitter(size=3, aes_string(color = input$grpBy)) +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else{
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Availability", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_smooth(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }
    }
    
    if (input$varX == "Average Ratings"){
      df <- getDataViz %>% group_by_(input$varY, input$grpBy) %>% summarize(Average_Rating = mean(Rating))
      
      
      if (input$plotType == "Bar Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Rating")) + 
          geom_bar(aes_string(fill = input$varY), stat = "identity") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Line Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Rating", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_line(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else if(input$plotType == "Box Plot"){
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Rating")) + 
          geom_boxplot(size=1, aes_string(color = input$grpBy)) + 
          geom_jitter(size=3, aes_string(color = input$grpBy)) +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }else{
        g <- ggplot(df, aes_string(x = input$varY, y = "Average_Rating", group = 1, color = input$grpBy)) + 
          geom_point() +
          geom_smooth(method = "lm") +
          facet_grid(~get(input$grpBy)) +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90), 
                axis.text=element_text(size=14),
                axis.title=element_text(size=16,face="bold"),
                strip.text.x = element_text(size = 14))
      }
    }
    
    g
    
  })
  
  output$plot2 <- renderPlot({
    
    cor = c("Price", "Make_Year", "Service_Fee", "Lat", "Long", "Min_Stay", "Host_Listings", "Rating")
    
    cor = input$corrCols
    
    
    df <- data.frame(readData())
    
    corDf <- df[, cor, drop=FALSE]
    
    cor <- cor(corDf, method = "spearman")
    cor
    
    g <- corrplot(cor, hc.order = FALSE, 
             addCoef.col = "black", type = "full"
    )
    
    g
  })
  
  output$plot3 <- renderPlot({
    
    g <- ggplot(readData(), aes_string(x = input$corRating2, y = input$corRating)) + 
      geom_jitter(color = "red") + 
      geom_smooth(method = "lm")
    
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
      paste0("Listings"), paste0("Total Airbnb Listings (Based on above filters) are: ", nrow(newData)), icon = icon("globe"),
      color = "red", fill = TRUE
    )
  })

  
  
  ##--------------------------------------Modelling TAB-------------------------------------------------
  

  output$GLMDetails <- renderPrint({
    paste0("From the 'Fit Model' tab, click on Build Models to get summary and stats of GLM Model")
  })
  
  output$GLMTestMetrics <- renderPrint({
    paste0("Once the model is built, fit statistics will appear here")
  })
  
  output$CTDetails <- renderPrint({
    paste0("From the 'Fit Model' tab, click on Build Models to get summary of Classification Tree Model")
  })
  
  output$CTTestMetrics <- renderPrint({
    paste0("Once the model is built, fit statistics will appear here")
  })
  
  output$RFDetails <- renderPrint({
    paste0("From the 'Fit Model' tab, click on Build Models to get summary of Random Forest Model")
  })
  
  output$RFTestMetrics <- renderPrint({
    paste0("Once the model is built, fit statistics will appear here")
  })
  
  
  
  observeEvent(input$buildGLMModel, {
    
    output$GLMDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$GLMTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    regData <- readData()
    
    regData$Host_Identity <- as_factor(regData$Host_Identity)
    regData$Borough <- as_factor(regData$Borough)
    regData$Neighbourhood <- as_factor(regData$Neighbourhood)
    regData$Available_Now <-as_factor(regData$Available_Now)
    regData$Cancellation <- as_factor(regData$Cancellation)
    regData$Type <- as_factor(regData$Type)
    regData$Rating <- as_factor(regData$Rating)
    
    
    rating <- regData$Rating
    
    selectedCols = input$varsForGLM
    
    modelDf <- regData[, selectedCols, drop=FALSE]
    
    modelDf$Rating <- rating
    modelDf$Rating <- as_factor(modelDf$Rating)
    
    
    
    set.seed(1)
    trainIndex <- createDataPartition(modelDf$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData <- modelDf[trainIndex, ]
    testData <- modelDf[-trainIndex, ]
    
    glm_model <- train(
      Rating ~.,
      data = trainData,
      trControl = trainControl(method = "cv", number = input$cvGLM),
      preprocess = c("center", "scale"),
      method = "glmnet"
    )
    
    predGLM <- predict(glm_model, newdata = testData)
    
    stats <- postResample(predGLM, obs = testData$Rating)
    
    
    output$GLMDetails <- renderPrint({
      glm_model
    })
    
    output$GLMTestMetrics <- renderPrint({
      stats
    })
    
  })
  
  observeEvent(input$buildCTModel, {
    
    output$CTDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$CTTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    regData <- readData()
    
    regData$Host_Identity <- as_factor(regData$Host_Identity)
    regData$Borough <- as_factor(regData$Borough)
    regData$Neighbourhood <- as_factor(regData$Neighbourhood)
    regData$Available_Now <-as_factor(regData$Available_Now)
    regData$Cancellation <- as_factor(regData$Cancellation)
    regData$Type <- as_factor(regData$Type)
    regData$Rating <- as_factor(regData$Rating)
    
    
    rating <- regData$Rating
    
    selectedCols = input$varsForCT
    
    modelDf <- regData[, selectedCols, drop=FALSE]
    
    modelDf$Rating <- rating
    modelDf$Rating <- as_factor(modelDf$Rating)
    
    
    
    set.seed(1)
    trainIndex <- createDataPartition(modelDf$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData <- modelDf[trainIndex, ]
    testData <- modelDf[-trainIndex, ]
    
    ctModel <- train(Rating ~ .,
                     data = trainData,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = input$cvCT),
                     tuneLength = input$tuneLengthCT,
                     tuneGrid = expand.grid(cp = seq(input$cpCT[1], input$cpCT[2], by=input$cpSkipCT))
    )
    
    cm_CT <- confusionMatrix(data = predict(ctModel, newdata = testData),
                             reference = testData$Rating)
    
    ps_CT <- postResample(predict(ctModel, newdata = testData), testData$Rating)
    
    output$CTDetails <- renderPrint({
      ctModel
    })
    
    
    output$CTTestMetrics <- renderPrint({
      #cm_CT
      ps_CT
    })
    
    output$CTVarImpGraph <- renderPlot({
      plot(varImp(ctModel))
    })
    
  })
  
  observeEvent(input$buildRFModel, {
    
    output$RFDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$RFTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    regData <- readData()
    
    regData$Host_Identity <- as_factor(regData$Host_Identity)
    regData$Borough <- as_factor(regData$Borough)
    regData$Neighbourhood <- as_factor(regData$Neighbourhood)
    regData$Available_Now <-as_factor(regData$Available_Now)
    regData$Cancellation <- as_factor(regData$Cancellation)
    regData$Type <- as_factor(regData$Type)
    regData$Rating <- as_factor(regData$Rating)
    
    
    rating <- regData$Rating
    
    selectedCols = input$varsForRF
    
    modelDf <- regData[, selectedCols, drop=FALSE]
    
    modelDf$Rating <- rating
    modelDf$Rating <- as_factor(modelDf$Rating)
    
    
    
    set.seed(1)
    trainIndex <- createDataPartition(modelDf$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData <- modelDf[trainIndex, ]
    testData <- modelDf[-trainIndex, ]
    
    rfModel <- train(Rating ~ .,
                     data = trainData,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = input$cvRF),
                     tuneLength = input$tuneLengthRF,
                     tuneGrid = expand.grid(mtry = input$rfmTry[1]:input$rfmTry[2])
    )
    
    cm_RF <- confusionMatrix(data = predict(rfModel, newdata = testData),
                             reference = testData$Rating)
    
    ps_RF <- postResample(predict(rfModel, newdata = testData), testData$Rating)
    
    output$RFDetails <- renderPrint({
      rfModel
    })
    
    
    output$RFTestMetrics <- renderPrint({
      ps_RF
    })
    
    output$RFVarImpGraph <- renderPlot({
      g <- plot(varImp(rfModel))
      g
    })
    
  })
  
  observeEvent(input$buildModels, {
    
    observe({
      updateSelectizeInput(
        session, "selectModel", choices = c()
      )
    })
    
    regData <- readData()
    
    regData$Host_Identity <- as_factor(regData$Host_Identity)
    regData$Borough <- as_factor(regData$Borough)
    regData$Neighbourhood <- as_factor(regData$Neighbourhood)
    regData$Available_Now <-as_factor(regData$Available_Now)
    regData$Cancellation <- as_factor(regData$Cancellation)
    regData$Type <- as_factor(regData$Type)
    regData$Rating <- as_factor(regData$Rating)

    
    rating <- regData$Rating
    
    
    #---------GLM---------------------------

    output$GLMDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$GLMTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    selectedCols1 = input$varsForGLM
    
    modelDf1 <- regData[, selectedCols1, drop=FALSE]
    
    modelDf1$Rating <- rating
    modelDf1$Rating <- as_factor(modelDf1$Rating)
    
    
    
    set.seed(1)
    trainIndex <- createDataPartition(modelDf1$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData1 <- modelDf1[trainIndex, ]
    testData1 <- modelDf1[-trainIndex, ]
    
    glm_model <- train(
      Rating ~.,
      data = trainData1,
      trControl = trainControl(method = "cv", number = input$cvGLM),
      preprocess = c("center", "scale"),
      method = "glmnet"
    )
    
    predGLM <- predict(glm_model, newdata = testData1)
    
    glm_stats <- postResample(predGLM, obs = testData1$Rating)
    
    
    output$GLMDetails <- renderPrint({
      glm_model
      })
    
    output$GLMTestMetrics <- renderPrint({
      glm_stats
    })

    
    
    #-------------CT-------------------
    
    output$CTDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$CTTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    selectedCols2 = input$varsForCT
    
    modelDf2 <- regData[, selectedCols2, drop=FALSE]
    
    modelDf2$Rating <- rating
    modelDf2$Rating <- as_factor(modelDf2$Rating)
    
    
    
    set.seed(1)
    ##trainIndex <- createDataPartition(modelDf$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData2 <- modelDf2[trainIndex, ]
    testData2 <- modelDf2[-trainIndex, ]
    
    ctModel <- train(Rating ~ .,
                     data = trainData2,
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = input$cvCT),
                     tuneLength = input$tuneLengthCT,
                     tuneGrid = expand.grid(cp = seq(input$cpCT[1], input$cpCT[2], by=input$cpSkipCT))
    )
    
    cm_CT <- confusionMatrix(data = predict(ctModel, newdata = testData2),
                             reference = testData2$Rating)
    
    ps_CT <- postResample(predict(ctModel, newdata = testData2), testData2$Rating)
    
    output$CTDetails <- renderPrint({
      ctModel
    })
    
    
    output$CTTestMetrics <- renderPrint({
      ps_CT
    })
    
    output$CTVarImpGraph <- renderPlot({
      plot(varImp(ctModel))
    })
    
    
    #-----------RF------------------
    
    output$RFDetails <- renderPrint({
      "Model Build in Progress"
    })
    
    output$RFTestMetrics <- renderPrint({
      "Model Build in Progress"
    })
    
    selectedCols3 = input$varsForRF
    
    modelDf3 <- regData[, selectedCols3, drop=FALSE]
    
    modelDf3$Rating <- rating
    modelDf3$Rating <- as_factor(modelDf3$Rating)
    
    
    
    set.seed(1)
    #trainIndex <- createDataPartition(modelDf$Rating, p=input$testTrainPartition, list = FALSE)
    
    trainData3 <- modelDf3[trainIndex, ]
    testData3 <- modelDf3[-trainIndex, ]
    
    rfModel <- train(Rating ~ .,
                     data = trainData3,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = input$cvRF),
                     tuneLength = input$tuneLengthRF,
                     tuneGrid = expand.grid(mtry = input$rfmTry[1]:input$rfmTry[2])
    )
    
    cm_RF <- confusionMatrix(data = predict(rfModel, newdata = testData3),
                             reference = testData3$Rating)
    
    ps_RF <- postResample(predict(rfModel, newdata = testData3), testData3$Rating)
    
    output$RFDetails <- renderPrint({
      rfModel
    })
    
    
    output$RFTestMetrics <- renderPrint({
      ps_RF
    })
    
    output$RFVarImpGraph <- renderPlot({
      g <- plot(varImp(rfModel))
      g
    })
    
    
    
    
    
    observe({
      updateSelectizeInput(
        session, "selectModel", choices = c("Generalized Linear Regression", "Classification Tree", "Random Forest")
      )
    })
  })

  

  #---------------Prediction---------------
  
  output$GLMPred <- renderPrint({
    paste0("Prediction Rating will appear here once you enter the values for variables and click on 'Predict'")
  })
  
  output$CTPred <- renderPrint({
    paste0("Prediction Rating will appear here once you enter the values for variables and click on 'Predict'")
  })
  
  output$RFPred <- renderPrint({
    paste0("Prediction Rating will appear here once you enter the values for variables and click on 'Predict'")
  })
  
  
  })
