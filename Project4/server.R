library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
library(DT)
library(caret)


# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {

  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$table <- renderDataTable({
    
    tab <- read_csv("C:\\Users\\sbgad\\Desktop\\airbnb\\cleanData_Airbnb.csv")
    
    datatable(tab, options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = '200px', targets = c(1, 3)))
    ))
    
  })
  
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      tab <- read_csv("C:\\Users\\sbgad\\Desktop\\airbnb\\cleanData_Airbnb.csv")
      write.csv(tab, fname)
    }
  )
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", "Shyam", icon = icon("list"),
      color = "lime", fill = TRUE
    )
  })
  

  
  })
