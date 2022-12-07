library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
library(DT)
library(caret)
source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")



sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("About", tabName = "about", icon = icon("circle-info"), badgeLabel = "i", badgeColor = "blue"),
    br(),
    menuItem("Data", icon = icon("table"), tabName = "data", badgeLabel = "Options", badgeColor = "orange"),
    selectizeInput("dataBorough", h5("Borough", style = "color:orange;"), selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
    br(),
    menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations", badgeLabel = "Options", badgeColor = "red"),
    selectizeInput("vizBorough", h5("Borough", style = "color:red;"), selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
    br(),
    menuItem("Modelling", icon = icon("chart-line"), tabName = "modelling",
             badgeLabel = " ", badgeColor = "lime")
  )
)

body <- dashboardBody(
  
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  
  skin = "black",
  
  ## PLOTS AND SLIDERS
  tabItems(
    tabItem(tabName = "about",
            fluidPage(
              fluidRow(
                theme = shinytheme("cyborg"),
                titlePanel(HTML("<h1><center><font size=14> About </font></center></h1>")),
                HTML("<h6><center><font size=4> Hello Hello Hello Hello Hello <br> 
                     https://www.kaggle.com/datasets/arianazmoudeh/airbnbopendata <br>
                     BBBB Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello  </font></center></h6>")
              )
            )
    ),
    
    
    
    tabItem(tabName = "vizualizations",
            fluidRow(
              box(title = "Histogram",plotOutput("plot1", height = 400),
                  collapsible = TRUE,
                  background = "purple",
                  solidHeader = TRUE),
              
              box(
                title = "Inputs",
                collapsible = TRUE,
                background = "black",
                solidHeader = TRUE,
                "Box content here", 
                br(), 
                "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
              )
            )
    ),
    
    
    tabItem(tabName = "data",
            fluidPage(
              
              fluidRow(
                downloadButton('download',"Download Table")
              ),
              br(),
              fluidRow(
                dataTableOutput("table")
              ),
              br(),
              fluidRow(
                checkboxInput("queryData", "Query through the data?")
                ),
              conditionalPanel(condition = "input.queryData == 1", 
                fluidRow(
                  selectInput("borough", "Select Borough", choices = c("Bronx", "Brookyln", "Manhattan", "Queens", "Staten Island"),
                              multiple = TRUE),
                  selectInput("borough", "Select Borough", choices = c("Bronx", "Brookyln", "Manhattan", "Queens", "Staten Island"),
                              multiple = TRUE)
                )
              )
            )
    ),
    
    tabItem(tabName = "modelling",
            
            ## TABS
            fluidRow(
              tabBox(
                title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset1",
                tabPanel("Tab1", "First tab content"),
                tabPanel("Tab2", "Tab content 2")
              ),
              tabBox(
                side = "right", height = "250px",
                selected = "Tab3",
                tabPanel("Tab1", "Tab content 1"),
                tabPanel("Tab2", "Tab content 2"),
                tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
              )
            ),
            
            fluidRow(
              infoBoxOutput("progressBox")
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  skin = "red",
  dashboardHeader(title = "NYC Airbnb Data"),
  sidebar,
  body
)


