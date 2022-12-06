library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
data("GermanCredit")
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
    
    tab <- GermanCredit %>%
      select("Class", "InstallmentRatePercentage") %>%
      group_by(Class, InstallmentRatePercentage)
    
    tibble(tab)
    
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", "Shyam", icon = icon("list"),
      color = "lime", fill = TRUE
    )
  })
  

  
  })
