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
                  h6("Tip: Bar Plot works best with Borough as the first classification, Box Plot works best with the Make_Year as the first classification, Line and Scatter plots work best with Make_Year as first classification, and Borough as the second one."),
                  
                  plotOutput("plot1", width = 1200),
                  collapsible = TRUE,
                  #background = "black",
                  solidHeader = TRUE
                  )
            ),
            br(),
            h2(strong("Correlation Plots:")),
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
              column(8, box(title = h4(strong("Geospatially represented Filtered and Arranged Airbnb Listings (May include multiple listings at a single location)")),leafletOutput("geoPlot", height = 650, width = 1000),
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
                
                tabPanel("Fit Model", 
                         fluidRow(
                           column(12,
                         box(
                           h4("Response Variable: ", strong("Rating", style = "color:red;")),
                             h5("Model Type: Classification"),
                             sliderInput("testTrainPartition", h5(strong("Select the proportion of "), strong("Train/Test", style = "color:red;"), strong(" data ratio for each models below")), min = 0, max = 1, step = 0.01, value = 0.7),
                             collapsible = TRUE,
                             solidHeader = TRUE,
                           width = 12
                           )
                         )
                         ),
                         
                         fluidRow(
                           
                           column(4, align="left",
                                  br(),
                           box(style = "background-color:#858585;",
                             title = h5("Set tuning parameters to build", strong("GLM Regression", style = "color:red;"), " model", style = "color:white;"), 
                               
                               pickerInput(
                                 inputId = "varsForGLM", 
                                 label = h5(strong("Select All predictor variables you want to use to Train the GLM Regression Model:", style = "color:black;")), 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Borough", "Cancellation", "Price", "Type")
                               ),
                               h6(strong("Note: As the number of selected variables increase, the time taken to build the model would also increase", style = "color:black;")),
                               br(),
                               sliderInput("cvGLM", h5(strong("Set the Cross Validation K-Folds", style = "color:black;")), min = 2, max = 15, step = 1, value = 3),
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               background = "black",
                               width = 2000,
                               conditionalPanel("input.selectModel.length != 0", 
                                              br(),
                                              actionButton("buildGLMModel", strong("Rebuild GLM Model", style = "color:red;"), width = 215)
                               )
                               #br(),
                               #actionButton("buildGLMModel", strong("Run only GLM Model", style = "color:red;"), width = 215)
                               )
                               ),
                           
                           column(4, align="left",
                                  br(),
                           box(style = "background-color:#858585;",
                             title = h5("Set tuning parameters to build the ", strong("Classification Tree", style = "color:red;"), " Model", style = "color:white;"),
                               pickerInput(
                                 inputId = "varsForCT", 
                                 label = h5(strong("Select All predictor variables you want to use to Train the Classification Tree Model:", style = "color:black;")), 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Borough", "Cancellation", "Price", "Type")
                               ),
                               h6(strong("Note: As the number of selected variables increase, the time taken to build the model would also increase", style = "color:black;")),
                               br(),
                               sliderInput("cvCT", h5(strong("Set the Cross Validation K-Folds", style = "color:black;")), min = 2, max = 15, step = 1, value = 5),
                               br(),
                               sliderInput("tuneLengthCT", h5(strong("Set the 'tunelength' value", style = "color:black;")), min = 1, max = 15, step = 1, value = 10),
                               br(),
                               sliderInput("cpCT", h5(strong("Set the Range of 'cp' parameter value", style = "color:black;")), min = 0, max = 0.5, step = 0.01, value = c(0.1,0.2)),
                               br(),
                               numericInput("cpSkipCT", h5(strong("Set the increment value of cp", style = "color:black;")), min = 0.005, max = 0.05, value = 0.01),
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               background = "black",
                               width = 2000,
                             conditionalPanel("input.selectModel.length != 0", 
                                              br(),
                                              actionButton("buildCTModel", strong("Rebuild CT Model", style = "color:red;"), width = 215)
                             )
                             #br(),
                             #actionButton("buildCTModel", strong("Run only CT Model", style = "color:red;"), width = 215)
                           )
                           ),
                           
                           column(4, align="left",
                                  br(),
                           box(style = "background-color:#858585;",
                             title = h5("Set tuning parameters to build the ", strong("Random Forest", style = "color:red;"), " Model", style = "color:white;"), 
                               pickerInput(
                                 inputId = "varsForRF", 
                                 label = h5(strong("Select All predictor variables you want to use to Train the Random Forest Model:", style = "color:black;")), 
                                 choices = c("Host_Identity", "Borough", "Neighbourhood", "Lat", "Long", "Available_Now", "Cancellation", "Type", "Make_Year", "Price", "Service_Fee",  "Min_Stay", "Host_Listings", "Availability"), 
                                 options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                 multiple = TRUE,
                                 selected = c("Borough", "Cancellation", "Price", "Type")
                               ),
                               h6(strong("Note: As the number of selected variables increase, the time taken to build the model would also increase", style = "color:black;")),
                               br(),
                               sliderInput("cvRF", h5(strong("Set the Cross Validation K-Folds", style = "color:black;")), min = 2, max = 15, step = 1, value = 2),
                               br(),
                               sliderInput("tunelengthRF", h5(strong("Set the 'tunelength' value", style = "color:black;")), min = 1, max = 15, step = 1, value = 2),
                             
                               br(),
                               sliderInput("rfmTry", h5(strong("Set the Range of 'mtry' parameter value", style = "color:black;")), min = 1, max = 20, step = 1, value = c(1, 2)),
                               br(),
                               collapsible = TRUE,
                               solidHeader = TRUE,
                               background = "black",
                               width = 2000,
                             
                             conditionalPanel("input.selectModel.length != 0", 
                                              br(),
                                              actionButton("buildRFModel", strong("Rebuild RF Model", style = "color:red;"), width = 215)
                             )
                           )
                           )
                           
                         ),
                         br(),
                         
                         actionButton("buildModels", strong("RUN ALL MODELS", style = "color:red;"), width = 770)
                         
                         ),
                
                tabPanel("Prediction", 
                         
                         h4(strong("Make a Prediction:")),
                         selectizeInput("selectModel", "Select a Model for Prediction", choices = c()),
                         conditionalPanel("input.selectModel.length==0",
                                          h5("Please go to 'Fit Model' tab and click on", strong('Run all Models', style = "color:red;"), "to select a model to make a prediction.")
                         ),
                         conditionalPanel("input.selectModel == 'Classification Tree'",
                                          br(),
                                          h5(strong("You have selected ", strong("Classification Tree", style = "color:red;"), "Model for prediction")),
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction:")),
                                          br(),
                                          conditionalPanel("input.varsForCT.indexOf('Host_Identity')!=-1",
                                                           selectInput("predHostICT", "Select if host is verified:", choices = unique(readData()$Host_Identity), selected = readData()$Host_Identity[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Borough')!=-1",
                                                           selectInput("predBorCT", "Select Borough of the Listing:", choices = unique(readData()$Borough), selected = readData()$Borough[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Neighbourhood')!=-1",
                                                           selectInput("predNeighCT", "Select Neighbourhood of the Listing:", choices = unique(readData()$Neighbourhood), selected = readData()$Neighbourhood[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Lat')!=-1",
                                                           numericInput("predLatCT", "Enter Latitude:", value = -40.7, step = 0.001)),
                                          conditionalPanel("input.varsForCT.indexOf('Long')!=-1",
                                                           numericInput("predLongCT", "Enter Longitude:", value = -73.9, step = 0.001)),
                                          conditionalPanel("input.varsForCT.indexOf('Available_Now')!=-1",
                                                           selectInput("predANCT", "Select Current Availability:", choices = unique(readData()$Available_Now), selected = readData()$Available_Now[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Cancellation')!=-1",
                                                           selectInput("predCanCT", "Select Cancellation Policy:", choices = unique(readData()$Cancellation), selected = readData()$Cancellation[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Type')!=-1",
                                                           selectInput("predtypeCT", "Select Type of Listing:", choices = unique(readData()$Type), selected = readData()$Type[1])),
                                          conditionalPanel("input.varsForCT.indexOf('Make_Year')!=-1",
                                                           numericInput("predYrCT", "Enter Year in which the Listing was Constructed:", min= 2000, max = 2022, value = 2010, step = 1)),
                                          conditionalPanel("input.varsForCT.indexOf('Price')!=-1",
                                                           numericInput("predPriceCT", "Enter the Price for the Listing", value = 500, min=0, step = 1)),
                                          conditionalPanel("input.varsForCT.indexOf('Service_Fee')!=-1",
                                                           numericInput("predSerFeeCT", "Enter the Service Fee for the Listing:", value = 100, min=0, step = 1)),
                                          conditionalPanel("input.varsForCT.indexOf('Min_Stay')!=-1",
                                                           numericInput("predMinStayCT", "Enter the Minimum Stay (in Days) for the Listing:", value = 10, min=0, step = 1)),
                                          conditionalPanel("input.varsForCT.indexOf('Host_Listings')!=-1",
                                                           numericInput("predHLCT", "Enter the number of total Listings host has:", value = 2, min=0, step = 1)),
                                          conditionalPanel("input.varsForCT.indexOf('Host_Listings')!=-1",
                                                           numericInput("predAvCT", "Enter the availability (in Days) of the Listing:", value = 365, min=0, step = 1)),
                                          br(),
                                          actionButton("predictCTBtn", strong("Predict", style = "color:red;")),
                                          br(),
                                          h4(strong("Prediction using Classification Tree model:")),
                                          shinycssloaders::withSpinner(
                                            verbatimTextOutput("CTPred"),
                                            type = 6, color = "red")
                                          ),
                         
                         
                         conditionalPanel("input.selectModel == 'Generalized Linear Regression'",
                                          br(),
                                          h5(strong("You have selected ", strong("Generalized Linear Regression", style = "color:red;"), "Model for prediction")),
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction:")),
                                          br(),
                                          conditionalPanel("input.varsForGLM.indexOf('Host_Identity')!=-1",
                                                           selectInput("predHostIGLM", "Select if host is verified:", choices = unique(readData()$Host_Identity), selected = readData()$Host_Identity[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Borough')!=-1",
                                                           selectInput("predBorGLM", "Select Borough of the Listing:", choices = unique(readData()$Borough), selected = readData()$Borough[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Neighbourhood')!=-1",
                                                           selectInput("predNeighGLM", "Select Neighbourhood of the Listing:", choices = unique(readData()$Neighbourhood), selected = readData()$Neighbourhood[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Lat')!=-1",
                                                           numericInput("predLatGLM", "Enter Latitude:", value = -40.7, step = 0.001)),
                                          conditionalPanel("input.varsForGLM.indexOf('Long')!=-1",
                                                           numericInput("predLongGLM", "Enter Longitude:", value = -73.9, step = 0.001)),
                                          conditionalPanel("input.varsForGLM.indexOf('Available_Now')!=-1",
                                                           selectInput("predANGLM", "Select Current Availability:", choices = unique(readData()$Available_Now), selected = readData()$Available_Now[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Cancellation')!=-1",
                                                           selectInput("predCanGLM", "Select Cancellation Policy:", choices = unique(readData()$Cancellation), selected = readData()$Cancellation[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Type')!=-1",
                                                           selectInput("predtypeGLM", "Select Type of Listing:", choices = unique(readData()$Type), selected = readData()$Type[1])),
                                          conditionalPanel("input.varsForGLM.indexOf('Make_Year')!=-1",
                                                           numericInput("predYrGLM", "Enter Year in which the Listing was Constructed:", min= 2000, max = 2022, value = 2010, step = 1)),
                                          conditionalPanel("input.varsForGLM.indexOf('Price')!=-1",
                                                           numericInput("predPriceGLM", "Enter the Price for the Listing", value = 500, min=0, step = 1)),
                                          conditionalPanel("input.varsForGLM.indexOf('Service_Fee')!=-1",
                                                           numericInput("predSerFeeGLM", "Enter the Service Fee for the Listing:", value = 100, min=0, step = 1)),
                                          conditionalPanel("input.varsForGLM.indexOf('Min_Stay')!=-1",
                                                           numericInput("predMinStayGLM", "Enter the Minimum Stay (in Days) for the Listing:", value = 10, min=0, step = 1)),
                                          conditionalPanel("input.varsForGLM.indexOf('Host_Listings')!=-1",
                                                           numericInput("predHLGLM", "Enter the number of total Listings host has:", value = 2, min=0, step = 1)),
                                          conditionalPanel("input.varsForGLM.indexOf('Host_Listings')!=-1",
                                                           numericInput("predAvGLM", "Enter the availability (in Days) of the Listing:", value = 365, min=0, step = 1)),
                                          br(),
                                          actionButton("predictGLMBtn", strong("Predict", style = "color:red;")),
                                          br(),
                                          h4(strong("Prediction using Generalized Linear Regression model:")),
                                          shinycssloaders::withSpinner(
                                            verbatimTextOutput("GLMPred"),
                                            type = 6, color = "red")
                         ),
                         
                         
                         conditionalPanel("input.selectModel == 'Random Forest'",
                                          br(),
                                          h5(strong("You have selected ", strong("Random Forest", style = "color:red;"), "Model for prediction")),
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction:")),
                                          br(),
                                          conditionalPanel("input.varsForRF.indexOf('Host_Identity')!=-1",
                                                           selectInput("predHostIRF", "Select if host is verified:", choices = unique(readData()$Host_Identity), selected = readData()$Host_Identity[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Borough')!=-1",
                                                           selectInput("predBorRF", "Select Borough of the Listing:", choices = unique(readData()$Borough), selected = readData()$Borough[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Neighbourhood')!=-1",
                                                           selectInput("predNeighRF", "Select Neighbourhood of the Listing:", choices = unique(readData()$Neighbourhood), selected = readData()$Neighbourhood[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Lat')!=-1",
                                                           numericInput("predLatRF", "Enter Latitude:", value = -40.7, step = 0.001)),
                                          conditionalPanel("input.varsForRF.indexOf('Long')!=-1",
                                                           numericInput("predLongRF", "Enter Longitude:", value = -73.9, step = 0.001)),
                                          conditionalPanel("input.varsForRF.indexOf('Available_Now')!=-1",
                                                           selectInput("predANRF", "Select Current Availability:", choices = unique(readData()$Available_Now), selected = readData()$Available_Now[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Cancellation')!=-1",
                                                           selectInput("predCanRF", "Select Cancellation Policy:", choices = unique(readData()$Cancellation), selected = readData()$Cancellation[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Type')!=-1",
                                                           selectInput("predtypeRF", "Select Type of Listing:", choices = unique(readData()$Type), selected = readData()$Type[1])),
                                          conditionalPanel("input.varsForRF.indexOf('Make_Year')!=-1",
                                                           numericInput("predYrRF", "Enter Year in which the Listing was Constructed:", min= 2000, max = 2022, value = 2010, step = 1)),
                                          conditionalPanel("input.varsForRF.indexOf('Price')!=-1",
                                                           numericInput("predPriceRF", "Enter the Price for the Listing", value = 500, min=0, step = 1)),
                                          conditionalPanel("input.varsForRF.indexOf('Service_Fee')!=-1",
                                                           numericInput("predSerFeeRF", "Enter the Service Fee for the Listing:", value = 100, min=0, step = 1)),
                                          conditionalPanel("input.varsForRF.indexOf('Min_Stay')!=-1",
                                                           numericInput("predMinStayRF", "Enter the Minimum Stay (in Days) for the Listing:", value = 10, min=0, step = 1)),
                                          conditionalPanel("input.varsForRF.indexOf('Host_Listings')!=-1",
                                                           numericInput("predHLRF", "Enter the number of total Listings host has:", value = 2, min=0, step = 1)),
                                          conditionalPanel("input.varsForRF.indexOf('Host_Listings')!=-1",
                                                           numericInput("predAvRF", "Enter the availability (in Days) of the Listing:", value = 365, min=0, step = 1)),
                                          br(),
                                          actionButton("predictRFBtn", strong("Predict", style = "color:red;")),
                                          br(),
                                          h4(strong("Prediction using Random Forest model:")),
                                          shinycssloaders::withSpinner(
                                            verbatimTextOutput("RFPred"),
                                            type = 6, color = "red")
                         )
                         
                         )
              ),
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              tabBox(title = "See Model Statistics",
                id = "tabset2",
                tabPanel("Generalized Linear Regression Model", 
                         h4(strong("Fit Statistics and Summary of Generalized Linear Regression Model:")),
                         
                         shinycssloaders::withSpinner(
                            verbatimTextOutput("GLMDetails"),
                            type = 6, color = "red"
                            ),
                         br(),
                         h4(strong("Fit Statistics of Generalized Linear Regression Model on Testing Data set:")),
                         shinycssloaders::withSpinner(
                           verbatimTextOutput("GLMTestMetrics"),
                           type = 6, color = "red"
                         )
                         ),
                
                tabPanel("Classification Tree", 
                         h4(strong("Fit Statistics and Summary of Classification Tree Model:")),
                         
                         shinycssloaders::withSpinner(
                           verbatimTextOutput("CTDetails"),
                           type = 6, color = "red"
                         ),
                         br(),
                         h4(strong("Fit Statistics of Classification Tree Model on Testing Data set:")),
                         shinycssloaders::withSpinner(
                           verbatimTextOutput("CTTestMetrics"),
                           type = 6, color = "red"),
                         br(),
                         h4(strong("Variable Importance Plot:")),
                         plotOutput("CTVarImpGraph")
                         ),
                
                tabPanel("Random Forest", 
                         h4(strong("Fit Statistics and Summary of Random Forest Model:")),
                         
                         shinycssloaders::withSpinner(
                           verbatimTextOutput("RFDetails"),
                           type = 6, color = "red"
                         ),
                         br(),
                         h4(strong("Fit Statistics of Random Forest Model on Testing Data set:")),
                         shinycssloaders::withSpinner(
                           verbatimTextOutput("RFTestMetrics"),
                           type = 6, color = "red"),
                         br(),
                         h4(strong("Variable Importance Plot:")),
                           plotOutput("RFVarImpGraph")
                         )
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


