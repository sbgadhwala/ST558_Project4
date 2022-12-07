library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
library(DT)
library(caret)
source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  
  
  #Data TAB
  
  getDataTabData <- reactive({
    if (input$dataBorough == "All"){
      newData <- readData()
    } else{
      newData <- readData() %>% filter(Borough == input$dataBorough)
    }
  })

  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #tab <- tibble()
  
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
  
  
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", "Shyam", icon = icon("list"),
      color = "lime", fill = TRUE
    )
  })
  

  
  })
