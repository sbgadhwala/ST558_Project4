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


sidebar <- dashboardSidebar(

  sidebarMenu(id = "sMenu",
              
              br(),
              menuItem("About", tabName = "about", icon = icon("circle-info"), badgeLabel = " ", badgeColor = "red"),
              
              br(),
              
              menuItem("Data", icon = icon("table"), tabName = "data", badgeLabel = " ", badgeColor = "red"),
    
              br(),
              
              menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations", badgeLabel = "Maps", badgeColor = "red"),
              
              br(),
              
              menuItem("Modelling", icon = icon("chart-line"), tabName = "modelling", badgeLabel = " ", badgeColor = "red")
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
                box(title = h4("Data for Airbnb Listings Details in New York City"), 
                    dataTableOutput("table"),
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    #background = "red",
                    width = 900)
              ),
              
              fluidRow(
                downloadButton('download',"Export as CSV")
              ),
              
              br(),
              
              h4("If you want a filtered table, you can apply filters for ", strong("Rows and Columns"), " below:"),

              
              br(),
              br(),
              
              fluidRow(
                column(4, 
                       box(title = h4(strong("Filter the Row data from the table", style = "color:red;")),
                         selectizeInput("dataBorough", "Select Borough", selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                         sliderInput("priceSlider", "Filter by Price range of the Listings", step = 5, min = min(readData()$Price), max = max(readData()$Price), value = c(min(readData()$Price), max(readData()$Price))),
                         sliderInput("ratingSlider", "Filter by Rating range of the Listings", min = 1, max = 5, value = c(1, 5)),
                         selectizeInput("dataArrange", "Select Sorting by Ascending or Descending Price of Listings", selected = "Descending", choices = c("Ascending", "Descending")),
                         checkboxInput("dataArrangeTable", "Arrange the data table by above selection of sorting"),
                         br(),
                         checkboxInput("moreOpts", h5("Need to apply", strong("more Filters", style = "color:red;"), "on data?")),
                         br(),
                         conditionalPanel(condition = "input.moreOpts == 1", 
                                          selectizeInput("typeListData", "Type of Listing", choices = levels(as_factor(readData()$Type))),
                                          numericInput("minYearData", "Construction of listing should be at least after (or in) the year:", value = 2010, min=2003, max=2022, step=1)),
                         
                         
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 800
                       )),
                       
                column(4,
                       box(title = h4(strong("Filter Columns from the table", style = "color:red;")),
                         pickerInput(
                           inputId = "allCols", 
                           label = "Select Columns", 
                           choices = c("Host_Identity", "Borough", "Neighbourhood",
                                       "Lat", "Long", "Available_Now", "Cancellation",
                                       "Type", "Make_Year", "Price", "Service_Fee",
                                       "Min_Stay", "Rating", "Host_Listings", "Availability"), 
                           options = list(
                             `actions-box` = TRUE, 
                             size = 10,
                             `selected-text-format` = "count > 0"
                           ), 
                           multiple = TRUE,
                           selected = names(readData())
                         ),
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 800
                       )),
              ),
              fluidRow(
                infoBoxOutput("observs"),
              )
              
            )
    ),
    
    ##-----------------------------------------Visualizations TAB---------------------------------------------
    tabItem(tabName = "vizualizations",
            fluidPage(
            
            h2(strong("Build your own Visualization:")),
            
            fluidRow(
              
              column(3, selectizeInput("varX", "Plot", choices = c("Number of Listings", "Average Price", "Average Availability", "Average Ratings"), selected = "Number of Listings")),
              column(3, selectizeInput("varY", "By", choices = c("Borough", "Type", "Make_Year", "Host_Identity", "Available_Now", "Cancellation"), selected = "Borough")),
              column(3, selectizeInput("grpBy", "Further Classified By", choices = c("Borough", "Type", "Make_Year", "Host_Identity", "Available_Now", "Cancellation"), selected = "Type")),
              column(3, selectizeInput("plotType", "Plot Type", choices = c("Bar Plot", "Box Plot", "Line Plot", "Scatter Plot"), selected = "Bar Plot"))
              

            ),
              
            fluidRow(
              box(title = "Your Graph",width = 1200,
                  h5("Hint: "),
                  
                  plotOutput("plot1", width = 1200),
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE
                  )
            ),
            br(),
            h2(strong("Correaltion PLot:")),
            fluidRow(
              column(6,
              pickerInput(
                inputId = "corrCols", 
                label = "Select Variables to see Corrplot between them:", 
                choices = names(readData()), 
                options = list(
                  `actions-box` = TRUE, 
                  size = 10,
                  `selected-text-format` = "count > 3"
                ), 
                multiple = TRUE,
                selected = c("Price", "Rating")
              ))
            ),
            fluidRow(
              box(title = "Plot2",
                  #plotOutput("plot1", height = 400),
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE
              )
            ),
            
            h2(strong("Geospatial Visualization:")),
            
            fluidRow(
              
              column(4, 
                     box(title = h4(strong("Apply Filters to see Listings on Map", style = "color:red;")),
                         selectizeInput("dataBorough1", "Select Borough", selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                         sliderInput("priceSlider1", "Filter by Price range of the Listings", step = 5, min = min(readData()$Price), max = max(readData()$Price), value = c(min(readData()$Price), max(readData()$Price))),
                         sliderInput("ratingSlider1", "Filter by Rating range of the Listings", min = 1, max = 5, value = c(1, 5)),
                         selectizeInput("dataArrange1", "Show Listings on Map sorted by Ascending or Descending Prices", selected = "Descending", choices = c("Ascending", "Descending")),
                         br(),
                         sliderInput("plotnum1", "Chnage the number of Listings on Map", min = 1, max = 100, value = 10, step = 1),
                         br(),
                         checkboxInput("moreOpts1", h5("Apply", strong("more Filters", style = "color:red;"))),
                         br(),
                         conditionalPanel(condition = "input.moreOpts1 == 1", 
                                          selectizeInput("typeListData1", "Type of Listing", choices = levels(as_factor(readData()$Type))),
                                          numericInput("minYearData1", "Construction of listing should be at least after (or in) the year:", value = 2010, min=2003, max=2022, step=1)),
                         
                         
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 800
                     )
              ),
              column(8, box(title = h4("Geospatially represented Filtered and Arranged Airbnb Listings (May include multiple listings at a single location)"),leafletOutput("geoPlot", height = 650, width = 1000),
                            collapsible = TRUE,
                            solidHeader = TRUE,
                            width = 800))
            ),
            
            fluidRow(
              infoBoxOutput("viztotalObs"),
            ),fluidRow(
              infoBoxOutput("priceBox"),
              infoBoxOutput("ratingBox"),
              infoBoxOutput("availBox")
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
  dashboardHeader(
    title = span("NYC", tags$img(src = "air.png", width = '50%')),
    #title = "NYC Airbnb Listings",
                  tags$li(
                      materialSwitch(inputId = "theme", label = strong("Switch Theme", style = "color:white;"), status = "default"),
                      style = "margin-top: 15px;",
                    class = "dropdown")
                  ),
  sidebar,
  body
)


