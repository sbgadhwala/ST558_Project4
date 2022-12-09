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
              
              menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations", badgeLabel = "Map", badgeColor = "green"),
              
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
              box(title = "Your Generated Graph with above selected Variables:",width = 1200,
                  h6("Tip: Bar Plot works best with Borough as the first classification, Box Plot works best with the Make_Year as the first classification, Line and Scatter plots work best with Make_Year as first calssification, and Borough as the second one."),
                  
                  plotOutput("plot1", width = 1200),
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE
                  )
            ),
            br(),
            h2(strong("Correaltion Plots:")),
            fluidRow(
              column(6,
              pickerInput(
                inputId = "corrCols", 
                label = "Select Variables to see Corrplot between them:", 
                choices = c("Price", "Make_Year", "Service_Fee", "Lat", "Long", "Min_Stay", "Host_Listings", "Rating", "Availability"), 
                options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                multiple = TRUE,
                selected = c("Price", "Make_Year", "Service_Fee", "Rating")
              )
              ),
              column(2,
                     selectizeInput("corRating", "Select first Variable to see its Plot with second Variable:", names(readData()), selected = "Availability")
                     ),
              column(2,
                     selectizeInput("corRating2", "Select second Variable to see its Plot with first Variable:", names(readData()), selected = "Host_Listings")
              )
            ),
            fluidRow(
              box(title = "Corr Plot Between Selected Variables",
                  plotOutput("plot2", height = 400),
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE
              ),
              box(title = "Trend Graph for selected Variables",
                  plotOutput("plot3", height = 400),
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
            fluidPage(
            fluidRow(
              tabBox(title = "Set Model Details",
                id = "tabset1",
                tabPanel("Modeling Info", "First tab content"),
                
                tabPanel("Model Fitting", 
                         fluidRow(
                           column(12,
                         box(
                           h4("Response Variable: ", strong("Rating", style = "color:red;")),
                             h5("Model Type: Classification"),
                             sliderInput("testTrainPartition", h5(strong("Select the proportion of "), strong("Train/Test", style = "color:red;"), strong(" data ratio for each models below")), min = 0, max = 1, step = 0.05, value = 0.7),
                             collapsible = TRUE,
                             solidHeader = TRUE
                           )
                         )
                         ),
                         
                         fluidRow(
                           
                           column(4,
                           box(title = h5("Set tuning parameters to build", strong("GLM Regression", style = "color:red;"), " model"), 
                               
                               pickerInput(
                                 inputId = "varsForGLM", 
                                 label = "Select All predictor variables you want to use to Train the GLM Regression Model:", 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability")
                               ),
                               br(),
                               sliderInput("cvGLM", "Set the Cross Validation K-Folds", min = 2, max = 15, step = 1, value = 10),
                               
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               #background = "red",
                               width = 2000
                               )
                               ),
                           
                           column(4,
                           box(title = h5("Set tuning parameters to build the ", strong("Classification Tree", style = "color:red;"), " Model"),
                               pickerInput(
                                 inputId = "varsForCT", 
                                 label = "Select All predictor variables you want to use to Train the Classification Tree Model:", 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability")
                               ),
                               br(),
                               sliderInput("cvCT", "Set the Repeated Cross Validation K-Folds", min = 2, max = 15, step = 1, value = 5),
                               br(),
                               sliderInput("tuneLengthCT", "Set the Tune Length", min = 1, max = 15, step = 1, value = 10),
                               br(),
                               sliderInput("cpCT", "Set the Range of 'cp' parameter value", min = 0, max = 0.5, step = 0.01, value = c(0.1,0.2)),
                               br(),
                               numericInput("cpSkipCT", "Set the increment value of cp", min = 0.005, max = 0.05, value = 0.01),
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               #background = "red",
                               width = 2000
                           )
                           ),
                           
                           column(4,
                           box(title = h5("Set tuning parameters to build the ", strong("Random Forest", style = "color:red;"), " Model"), 
                               pickerInput(
                                 inputId = "varsForRF", 
                                 label = "Select All predictor variables you want to use to Train the Random Forest Model:", 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability")
                               ),
                               br(),
                               sliderInput("cvRF", "Set the Repeated Cross Validation K-Folds", min = 2, max = 15, step = 1, value = 7),
                               br(),
                               sliderInput("cvRF", "Set the Range of 'mtry' parameter value", min = 1, max = 20, step = 1, value = c(3, 10)),
                               br(),
                               sliderInput("tunelengthRF", "Set the 'tunelength' parameter value ", min = 1, max = 15, step = 1, value = 10),
                               br(),
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               #background = "red",
                               width = 2000
                           )
                           )
                           
                         ),
                         actionButton("buildModels", strong("Click here to build all Models", style = "color:red;"), width = 770)
                         
                         ),
                
                tabPanel("Prediction", "Tab content 3")
              ),
              
              tabBox(title = "See Model Statistics",
                id = "tabset2",
                tabPanel("Generalized Linear Regression Model", textOutput("successruntext")),
                
                tabPanel("Classification Tree", "Hello"),
                
                tabPanel("Random Forest", "Tab content 3")
              )
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


