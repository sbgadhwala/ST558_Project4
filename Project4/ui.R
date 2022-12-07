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




sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("About", tabName = "about", icon = icon("circle-info"), badgeLabel = "i", badgeColor = "blue"),
    
    br(),
    
    menuItem("Data", icon = icon("table"), tabName = "data"),
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
  
  ##-----------------------------------------About TAB---------------------------------------------
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
    
    
    ##-----------------------------------------Data TAB---------------------------------------------
    tabItem(tabName = "data",
            fluidPage(
              fluidRow(
                downloadButton('download',"Export to CSV")
              ),
              
              br(),
              
              fluidRow(
                box(title = h4("Data for Airbnb Listings Details in New York City", style = "color:black;"), dataTableOutput("table"),
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    width = 900)
              ),
              
              h4("You can see ", strong("summaries"), " for a few variables below:"),
              textOutput("dataTabInfo"),
              
              
              br(),
              br(),
              
              fluidRow(
                column(4, 
                       box(title = h4("Filters to apply on data (Changes apply in table as well)",style = "color:black;"),
                         selectizeInput("dataBorough", h5("Select Borough", style = "color:black;"), selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                         sliderInput("ratingSlider", h5("Filter by range for Ratings of the Listings", style = "color:black;"), min = 1, max = 5, value = c(1, 5)),
                         selectizeInput("dataArrange", h5("Geographically see the Listing with Ascending or Descending Prices", style = "color:black;"), selected = "Descending", choices = c("Ascending", "Descending")),
                         checkboxInput("dataArrangeTable", "Arrange the data by Price in Table as well?"),
                         br(),
                         sliderInput("plotnum", h5("Chnage the number of Listings from filtered data Geographically", style = "color:black;"), min = 1, max = 100, value = 10, step = 1),
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 800
                       )
                       ),
                column(8, box(title = h4("Geospatially represented Filtered and Arranged Airbnb Listings",style = "color:black;"),leafletOutput("geoPlot", height = 500, width = 1000),
                              collapsible = TRUE,
                              solidHeader = TRUE,
                              width = 800))
              )
            )
    ),
    
    ##-----------------------------------------Visualizations TAB---------------------------------------------
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
    
    
    
    ##-----------------------------------------Modelling TAB---------------------------------------------
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


