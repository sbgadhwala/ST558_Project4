library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(caret)
library(leaflet)

source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")

#For Github render
#source("DataHelper.R")




sidebar <- dashboardSidebar(
  sidebarMenu(
    
    br(),
    menuItem("About", tabName = "about", icon = icon("circle-info"), badgeLabel = "i", badgeColor = "green"),
    
    br(),
    
    menuItem("Data & Summaries", icon = icon("table"), tabName = "data", badgeLabel = "Map", badgeColor = "green"),
    br(),
    
    menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations"),
    #selectizeInput("vizBorough", h5("Borough", style = "color:red;"), selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
    
    br(),
    
    menuItem("Modelling", icon = icon("chart-line"), tabName = "modelling")
  )
)

body <- dashboardBody(
  
  uiOutput("theme"),

  
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
                box(title = h4("Data for Airbnb Listings Details in New York City"), dataTableOutput("table"),
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    #background = "red",
                    width = 900)
              ),
              
              fluidRow(
                downloadButton('download',"Export as CSV")
              ),
              
              br(),
              
              h4("You can see ", strong("summaries"), " for important variables at the end of the page after applying filters:"),

              
              br(),
              br(),
              
              fluidRow(
                column(4, 
                       box(title = h4("Filters to apply on data ", strong("(Changes apply in table as well)", style = "color:red;")),
                         selectizeInput("dataBorough", "Select Borough", selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                         sliderInput("priceSlider", "Filter by Price range of the Listings", step = 5, min = min(readData()$Price), max = max(readData()$Price), value = c(min(readData()$Price), max(readData()$Price))),
                         sliderInput("ratingSlider", "Filter by Rating range of the Listings", min = 1, max = 5, value = c(1, 5)),
                         selectizeInput("dataArrange", "Geographically see the Listings with Ascending or Descending Prices", selected = "Descending", choices = c("Ascending", "Descending")),
                         checkboxInput("dataArrangeTable", "Arrange the data by Price in Table as well"),
                         br(),
                         sliderInput("plotnum", "Chnage the number of Listings from filtered data Geographically", min = 1, max = 100, value = 10, step = 1),
                         br(),
                         checkboxInput("moreOpts", h5("Need to apply", strong("more Filters", style = "color:red;"), "on data?")),
                         br(),
                         conditionalPanel(condition = "input.moreOpts == 1", 
                                          selectizeInput("typeListData", "Type of Listing", choices = levels(as_factor(readData()$Type))),
                                          numericInput("minYearData", "Construction of listing should be at least after (or in) the year:", value = 2010, min=2003, max=2022, step=1)),
                         
                         
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 800
                       )
                       ),
                column(8, box(title = h4("Geospatially represented Filtered and Arranged Airbnb Listings (May include multiple listings at a single location)"),leafletOutput("geoPlot", height = 500, width = 1000),
                              collapsible = TRUE,
                              solidHeader = TRUE,
                              width = 800))
              ),
              
              fluidRow(
                infoBoxOutput("priceBox"),
                infoBoxOutput("ratingBox"),
                infoBoxOutput("availBox")
              )
            )
    ),
    
    ##-----------------------------------------Visualizations TAB---------------------------------------------
    tabItem(tabName = "vizualizations",
            fluidPage(
            
            fluidRow(
              box(title = "Count of Listings by Boroughs",
                  plotOutput("samplePlot", height = 400),
                  br(),
                  selectizeInput("plotBoroughs", "Select Borough", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"), selected = "All"),
                  sliderInput("plotBoroughRating", "Rating Range:", min = 1, max = 5, value = c(1, 5), step = 1),
                  sliderInput("plotBoroughsPrice", "Select Range of Price of Listing", step = 5, min = min(readData()$Price), max = max(readData()$Price), value = c(min(readData()$Price), max(readData()$Price))),
                  
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE),
              
              box(
                title = "Inputs",
                collapsible = TRUE,
                #background = "black",
                solidHeader = TRUE,
                "Box content here", 
                br(), 
                "More box content",
                sliderInput("slider", "Slider input:", 1, 100, 50),
                textInput("text", "Text input:")
              )
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
    )
  )
)

    ##-----------------------------------------General TAB---------------------------------------------
# Put them together into a dashboardPage
dashboardPage(
  skin = "red",
  dashboardHeader(title = "NYC Airbnb Listings",
                  tags$li(
                      materialSwitch(inputId = "theme", label = strong("Dark Mode", style = "color:white;"), status = "default"),
                      title = "Dark Mode",
                      style = "margin-top: 15px;",
                    class = "dropdown")
                  ),
  sidebar,
  body
)


