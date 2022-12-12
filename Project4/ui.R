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

#source("C:\\Users\\sbgad\\Desktop\\NCSU Documents\\Fall 2022\\ST 558\\Project 4\\ST558_Project4\\Project4\\DataHelper.R")

#For Github render
source("DataHelper.R")


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
                titlePanel(HTML("<h1><center><font size=14> New York City Airbnb - Estimating Ratings for New Hosts</font></center></h1>")),
                br(),
                fluidRow(
                column(12, align = "center", height = 12,
                       span(tags$img(src = "nyc.jpg", width = '30%', height = "30%"))
                ),
                #column(6, align = "center", height = 12,
                #       span(tags$img(src = "nyc2.jpg", width = '30%', height = "30%"))
                #)
                ),
                #br(),
                br(),
                box(title = strong("Application Introduction and Preface", style = "color:red;"),
                column(12, align = "flex-end",
                       
                       h4("The main prupose of this app is to let users, who are looking to put up their property on Airbnb in New York City, predict their listing's overall rating
                       based on various attributes related to their property(s).
                       This app is aimed towards people who are potentially looking to put up their property or deciding to 
                       bring a change in their property(s) on the Airbnb website listings in order to increase their listing's booking frequency. 
                       Every customer that books the listing via airbnb 
                          gives a certain rating to the place, which can depend on various factors like price of the property, type of the property, 
                          Area of the property, to name some. The ideal aim as a host would be to receive a high rating from the user. There are many 
                          successful airbnb property listings which are highly rated by the customers. They have certain characteristics attached to them; 
                          in which Borough the property is located in, what is the neighbourhood, what is the price of the booking, are there any cancellation 
                          charges, is there any service fee, and lots of other factors. If these factors align with the customers' liking, they are highly 
                          likely to give a good rating. There are also listings on Airbnb website with not-so-good ratings. All of them have these attributes attached 
                          with them as well."
                       )
                       ),
                width=12,
                collapsible = TRUE,
                solidHeader = TRUE
                
                ),
                
                box(title = strong("Data", style = "color:red;"),
                    column(12, align = "flex-end",
                           
                           h4("The data for Airbnb listings in New York City is taken from Kaggle. You can find this data set",
                              a("here.", href="https://www.kaggle.com/datasets/arianazmoudeh/airbnbopendata")),
                           
                           checkboxInput("showData", "Show Details about the Data"),
                           conditionalPanel("input.showData == 1", 
                                            h4("The data set was taken from the above given link and was then cleaned up to extract relevant features and 
                                               remove missing values. The features included in this application are:"),
                                            h5("Host_Identity (Categorical): If the host is verified on Airbnb website (verified, unconfirmed)"),
                                            h5("Borough (Categorical): What Borough is the listed property located in (Brookyln, Manhattan, Queens, Bronx, Staten Island)"),
                                            h5("Neighbourhood (Categorical): What Neighbourhood is the listed property located in (226 Neighbourhoods)"),
                                            h5("Lat (Numeric): Latitude of the listed property"),
                                            h5("Long (Numeric): Longitude of the listed property"),
                                            h5("Available_Now (Categorical): If the listed property is currently available or not (true, false)"),
                                            h5("Cancellation (Categorical): What is the cancellation rules of the listing (strict, moderate, flexible)"),
                                            h5("Type (Categorical): What is the type of property (Private Room, Entire Home/apt, Shared Room, Hotel Room)"),
                                            h5("Make_Year (Numeric): The year of construction of the listed property"),
                                            h5("Price (Numeric): The Price (in $) for the listed property for 1 night"),
                                            h5("Service_Fee (Numeric): The Service Fee (in $) for the listed property for 1 night"),
                                            h5("Min_Stay (Numeric): The minimum number of days the listing has to be booked by customer"),
                                            h5("Rating (Categorical): Rating of the listing (1, 2, 3, 4, 5)"),
                                            h5("Host_Listings (Numeric): The number of total listings that the host has"),
                                            h5("Availability (Numeric): The number of days the listing is available to be booked in a year"),
                                            )
                    ),
                    width=12,
                    collapsible = TRUE,
                    solidHeader = TRUE
                    
                ),
                
                box(title = strong("User Guide for the Application", style = "color:red;"),
                    column(12, align = "flex-end",
                           
                           h4("The application is divided into 4 tabs. You can switch between any tabs from the left side of any page."),
                           br(),
                           h4(strong("About:")),
                           h4("This page has the information about the app and applications of the app. This is the page that you are currently in."),
                           br(),
                           h4(strong("Data:")),
                           h4("In this tab, you can see the raw data that is used in this app. There you can apply filters 
                              for both rows and columns, and can see the filtered data. An option to download the dynamic data is also 
                              given in this tab. The table on this tab is reactive, and will react to any filters that you wish to apply."),
                           br(),
                           h4(strong("Visualizations:")),
                           h4("In this tab, you can plot the trends of different attributes that are relevant to the end aim. You can also classify 
                              the plots two-fold based on the above mentioned categorical variables from the data. The second part of this page includes 
                              the correlation plots. You can see a corr plot for the variables of your choice, and a correlation plot between 
                              any two variables of your choice."),
                           h4("The last part of this page is the Geospatial representation where you can visually see the geolocation on the Airbnb listings based on 
                              any filters that you wish to apply that are provided on that page. You can also further arrange and see the listings 
                              on the map by ascending or descending order of their Prices."),
                           br(),
                           h4(strong("Modelling:")),
                           h4("This is divided into three tabs. First is the 'Modelling Info' tab 
                              where you will get an overview of different models that this app offers. The models include Generalized Linear Regression Model,
                              Classification Tree Model, and Random Forest Model. The response variable is the Rating of the listing, and input parameters can change 
                              as per your discretion."),
                           h4("The next tab is 'Fit Model' tab. Here, you would be able to adjust the parameters that you want to train your models 
                              to, like adjust train/test data ratio, variables to build model, cross validation k-folds, etc, varying model to model. There is a button to run all the models. Once you have set the model train parameters of your choice, you can click on that 
                              button to run all the models. Once you click on that button, you will see the loading screen on the right pane of the page where each model's fit 
                              statistcs would be displayed. On the right pane of the page, you can switch between different models to that respective 
                              model's summary and fit statistics."),
                           h4("The last tab is the 'Prediction' tab. Here you would be able to select a model that you want to use for prediction. Note 
                              that the variables used to train a model would be presented there for you to give an input to make a prediction. This may 
                              change from one model to another. Finally, once you have given the input for all variables, you can click on the 'Predict' 
                              button. After the click, you will see the predicted Rating of the listing below. You can repeat this step by changing different 
                              input parameters, and switching between different models.")
                           
                    ),
                    width=12,
                    collapsible = TRUE,
                    solidHeader = TRUE
                    
                ),
                
                box(title = strong("Sample Scenario", style = "color:red;"),
                    column(12, align = "flex-end",
                           
                           h4("Supose you have a home that you want to list on the Airbnb webiste, but you are not sure what price you should keep or what 
                              service fee you should charge such that users would prefer your listing and give a good rating."
                           ),
                           h4("So first, you can go to data tab, and just have a glance at the data, and apply filters to see prices and ratings of listings 
                              in your borough and neighbourhood just to get an overview."),
                           h4("Then you can go to vizualizations tab and see a trend of what the prices of homes have been in your borough. There you can 
                              also see a price range of listings around the location of your home from the maps. You can also plot the corr plot to see which 
                              factors affect the most or are correlated to 'Rating' variable."),
                           h4("Finally, you can go to modelling tab, fit all 3 models according to parameters and 
                              the selection of the variables of your choice. Once the models are built, you can go to prediction tab, and enter the values of the 
                              selected variables according to your property or your discretion, and predict the Rating."),
                           h4("There you'll see a predicted Rating for your home if you were to list it on Airbnb as per those input variables on prediction tab. There 
                              you can change the inputs again to get a new prediction."),
                           h4("For example, if you got a predicted Rating as 3 for a input price of $600, you can change the price of your home to 400$ and that 
                              might increase the predicted rating from 3 to 4 or to any other value. You can similarly play around with different input values 
                              and see the change in predicted Rating, and from that, you can finalize the set of values that you can put for your home on Airbnb website.")
                    ),
                    width=12,
                    collapsible = TRUE,
                    solidHeader = TRUE
                    
                ),
                
               )
            )
    ),
    
    
    ##-----------------------------------------Data TAB---------------------------------------------
    tabItem(tabName = "data",
            fluidPage(
              fluidRow(
                box(title = h4(strong("Data for Airbnb Listings Details in New York City")), 
                    dataTableOutput("table"),
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    #background = "red",
                    width = 900)
              ),
              
              fluidRow(
                column(2,
                downloadButton('download',"Export as CSV")),
                column(10, h5("The table shown above is dynamic. It will react to the filters applied below. Click on 'Export as CSV' anytime to download the table (even with the filters applied)"))
              ),
              
              br(),
              
              h4("If you want a filtered table, you can apply filters for ", strong("Rows and Columns"), " below:"),
              br(),
              
              fluidRow(
                column(4, 
                       box(title = h4(strong("Filter the Row data from the table", style = "color:red;")),
                         #selectizeInput("dataBorough", "Select Borough", selected = "All", choices = c("All", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")),
                         
                         pickerInput(
                           inputId = "dataBorough", 
                           label = "Select Borough", 
                           choices = unique(readData()$Borough), 
                           options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                           multiple = TRUE,
                           selected = unique(readData()$Borough)
                         ),
                         
                         sliderInput("priceSlider", "Filter by Price range of the Listings", step = 5, min = min(readData()$Price), max = max(readData()$Price), value = c(min(readData()$Price), max(readData()$Price))),
                         sliderInput("ratingSlider", "Filter by Rating range of the Listings", min = 1, max = 5, value = c(1, 5)),
                         selectizeInput("dataArrange", "Select Sorting by Ascending or Descending Price of Listings", selected = "Descending", choices = c("Ascending", "Descending")),
                         checkboxInput("dataArrangeTable", "Arrange the data table by above selection of sorting"),
                         br(),
                         checkboxInput("moreOpts", "Show All Filters"),
                         br(),
                         conditionalPanel(condition = "input.moreOpts == 1", 
                                          #selectizeInput("typeListData", "Type of Listing", choices = levels(as_factor(readData()$Type))),
                                          
                                          pickerInput(
                                            inputId = "typeListData", 
                                            label = "Select Type of Listing:", 
                                            choices = levels(as_factor(readData()$Type)), 
                                            options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                            multiple = TRUE,
                                            selected = levels(as_factor(readData()$Type))
                                          ),
                                          
                                          sliderInput("minYearData", "Select Years of Construction of Listing Range:", value = c(min(readData()$Make_Year),max(readData()$Make_Year)), min=min(readData()$Make_Year), max=max(readData()$Make_Year), step=1),
                                          
                                          pickerInput(
                                            inputId = "dataHostIdentity", 
                                            label = "Host Identity:", 
                                            choices = levels(as_factor(readData()$Host_Identity)), 
                                            options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                            multiple = TRUE,
                                            selected = levels(as_factor(readData()$Host_Identity))
                                          ),
                                          
                                          pickerInput(
                                            inputId = "dataNeigh", 
                                            label = "Select Neighbourhood:", 
                                            choices = levels(as_factor(readData()$Neighbourhood)), 
                                            options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                            multiple = TRUE,
                                            selected = levels(as_factor(readData()$Neighbourhood))
                                          ),
                                          
                                          pickerInput(
                                            inputId = "dataAvailNow", 
                                            label = "Select Current Availability:", 
                                            choices = levels(as_factor(readData()$Available_Now)), 
                                            options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                            multiple = TRUE,
                                            selected = levels(as_factor(readData()$Available_Now))
                                          ),
                                          
                                          pickerInput(
                                            inputId = "dataCancel", 
                                            label = "Select Cancellation Rules:", 
                                            choices = levels(as_factor(readData()$Cancellation)), 
                                            options = list(`actions-box` = TRUE, size = 10,`selected-text-format` = "count > 3"), 
                                            multiple = TRUE,
                                            selected = levels(as_factor(readData()$Cancellation))
                                          ),
                                          
                                          sliderInput("dataSerFee", "Select Service Fee Range:", value = c(min(readData()$Service_Fee),max(readData()$Service_Fee)), min=min(readData()$Service_Fee), max=max(readData()$Service_Fee), step=1),
                                          
                                          sliderInput("dataMinStay", "Select Minimum Stay (in Days) Range:", value = c(0,max(readData()$Min_Stay)), min=min(readData()$Min_Stay), max=max(readData()$Min_Stay), step=1),
                                          
                                          sliderInput("dataHostL", "Select Range of other Listings owned by the host:", value = c(min(readData()$Host_Listings), max(readData()$Host_Listings)), min=min(readData()$Host_Listings), max=max(readData()$Host_Listings), step=1),
                                          
                                          sliderInput("dataAvail", "Select Availability (in Days) Range:", value = c(0,max(readData()$Availability)), min=min(readData()$Availability), max=max(readData()$Availability), step=1),
                                          
                                          sliderInput("dataLat", "Select Latitude Range:", value = c(min(readData()$Lat), max(readData()$Lat)), min=min(readData()$Lat), max=max(readData()$Lat), step=0.000001),
                                          
                                          sliderInput("dataLong", "Select Longitude Range:", value = c(min(readData()$Long), max(readData()$Long)), min=min(readData()$Long), max=max(readData()$Long), step=0.000001)
                                          
                                          
                                          ),
                         
                         
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
              withMathJax(),
            fluidRow(
              tabBox(title = "Set Model Details",
                id = "tabset1",
                tabPanel("Modeling Info", 
                         
                         #h4(strong("Modeling section: Use this app to train models")),
                         #br(),
                         box(title = strong("Overview", style = "color:red;"),
                         h4("In this R Shiny Application, you as a user are given the full control to tune and train all
                            3 models that the app offers. 
                            Since our target variable is 'Rating' which is a categorical varaible, this app offers classification models 
                            to train and predict 'Rating' variable based on various predictors. 
                            The 3 models are", strong("Generalized Linear Regression Model, Classification Tree and,
                            Random Forest Model.")),
                         h4("To get details about each model, expand the model box(es) shown below:"),
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         width = 12
                         ),
                         
                         box(title = strong("Generalized Linear Regression", style = "color:red;"),
                             h4("The generalized linear model (GLM) expands the general linear model so that the dependent variable is linearly 
                             related to the factors and covariates via a specified link function. 
                             The model calculates the effect that each of the predicting variable has on the target variable, and calculates the 
                             weighted coefficients accoridngly such that the response variable is a categorical value, in this case, Rating of the 
                             airbnb Listing. The model allows for the dependent variable to have a non-normal distribution. One of the good things about this 
                                model is that it works best when the predictor variables are uncorrelated (which is true till some extent for this
                                data).
                                The link used in this case is a Logit link since the target variable is categorical. The model eqution 
                                can be written out as:"),
                             withMathJax(
                               helpText('$$ {\\ln\\left(\\frac{p}{1-p}\\right)} = \\beta_0 + \\beta_1\\ Feature_1 + .... + \\beta_n\\ Feature_n + \\ e$$')
                             ),
                             h4("One of the drawbacks of this model is, unlike forward and backward stepwise models, 
                                this model does not select the best features for the model, so sometimes the accuracy of this model
                                does not get optimized."),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = TRUE,
                             width = 12
                         ),
                         
                         box(title = strong("Classifiaction Tree", style = "color:red;"),
                             h4("A classification Tree or a Decision Tree is a form of 
                                supervised machine learning technique where we continuously split the data according to a certain parameter. The 
                                decision trees are built using recursive partitioning. In this method, the model
                                splits the data into subsets, which is then split repeatedly into even smaller subsets, and so on and so forth. 
                                The process stops when the algorithm determines the data within the subsets are similar or 
                                have met another criterion. Model starts at the tree root and split the data on the feature that 
                                results in the largest information gain. In an iterative process, we can then repeat this splitting 
                                procedure at each child node. Some advantages of this method are; less processing power required to build this 
                                model, can handle missing values and it does not require normalized data"),
                             span(tags$img(src = "ct.png", width = '25%', height = "25%", align = "center")),
                             h4("The information gain is calculated on each node using the following formula:"),
                             withMathJax(
                               helpText('$$ - \\sum {P( X_i log_b P(X_i))}$$')
                             ),
                               
                             
                             h4("The drawbacks of this model are; a small change in data can lead to change in trees's nodes and classification 
                                and hence lead to change in values, it takes more time to train as compared to GLM, and generally has a lower 
                                overall prediction accuracy if the data is skewed heavily or biased."),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = TRUE,
                             width = 12
                         ),
                         
                         box(title = strong("Random Forest", style = "color:red;"),
                             h4("Random Forest models are an improvised version of classification tree models. In Random Forest, 
                                the model constructs many individual decision trees at training by taking random subsets of training data set. Predictions from all trees are pooled 
                                to make the final prediction which is the mode of the classes for classification. Feature importance is an important 
                                factor that wieghs in for this model to see how the split of any tree for randomly divided data is being affected 
                                by any given variable."),
                             h4("The importance for each feature is calculated by the following formula:"),
                             withMathJax(
                               helpText('$$ \\frac{\\sum{n i_j}}{\\sum{n i_k}} $$ where j = node splits on feature i, and k = total nodes')
                             ),
                             
                             
                             h4("Some of the advantages of this model is that it can make classifications more dynamically observing data from multiple subsets and, hence, 
                                the predictions are generally more accurate than normal Classification Tree and GLM. It can also handle large data sets quite well. One of the main 
                                disadvantages of this model is that it takes a lot more time to train since it builds a number of trees as a part of the 
                                training "),
                             collapsible = TRUE,
                             collapsed = TRUE,
                             solidHeader = TRUE,
                             width = 12
                         ),
                         
                         h5("If you got an overview of the models, you can now go to the next tab that is 'Fit Model' Tab, to train the 3 models listed above. You can change the tuning parameters 
                            that vary for each model as per your choice. You would also be able to control the size of training and testing data set. 
                            After the model is built, you should see the models' statistics on the right pane of this page. To make a prediction, you can go to 
                            'Prediction' tab.")
                         
                         ),
                
                
                
                
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
                               width = 2000
                               #conditionalPanel("input.selectModel.length != 0", 
                              #                br(),
                              #                actionButton("buildGLMModel", strong("Rebuild GLM Model", style = "color:red;"), width = 215)
                              # )
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
                               width = 2000
                             #conditionalPanel("input.selectModel.length != 0", 
                            #                  br(),
                            #                  actionButton("buildCTModel", strong("Rebuild CT Model", style = "color:red;"), width = 215)
                            #)
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
                               width = 2000
                             
                             #conditionalPanel("input.selectModel.length != 0", 
                            #                  br(),
                            #                  actionButton("buildRFModel", strong("Rebuild RF Model", style = "color:red;"), width = 215)
                            # )
                           )
                           )
                           
                         ),
                         br(),
                         
                         actionButton("buildModels", strong("Run All Models", style = "color:red;"), width = 770)
                         
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
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction.")),
                                          h5(strong("This is a dynamic page, you can change the inputs and again click on 'Predict' to make a prediction.")),
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
                                          h4(strong("Predicted Rating on a scale of 1 to 5 using Classification Tree model:")),
                                          shinycssloaders::withSpinner(
                                            verbatimTextOutput("CTPred"),
                                            type = 6, color = "red")
                                          ),
                         
                         
                         conditionalPanel("input.selectModel == 'Generalized Linear Regression'",
                                          br(),
                                          h5(strong("You have selected ", strong("Generalized Linear Regression", style = "color:red;"), "Model for prediction")),
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction:")),
                                          h5(strong("This is a dynamic page, you can change the inputs and again click on 'Predict' to make a prediction.")),
                                          
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
                                          h4(strong("Predicted Rating on a scale of 1 to 5 using Generalized Linear Regression model:")),
                                          shinycssloaders::withSpinner(
                                            verbatimTextOutput("GLMPred"),
                                            type = 6, color = "red")
                         ),
                         
                         
                         conditionalPanel("input.selectModel == 'Random Forest'",
                                          br(),
                                          h5(strong("You have selected ", strong("Random Forest", style = "color:red;"), "Model for prediction")),
                                          h5(strong("The following are the variables that were used to train the model. Enter values for those variables to make a prediction:")),
                                          h5(strong("This is a dynamic page, you can change the inputs and again click on 'Predict' to make a prediction.")),
                                          
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
                                          h4(strong("Predicted Rating on a scale of 1 to 5 using Random Forest model:")),
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
                           type = 6, color = "red")
                         #br(),
                         #h4(strong("Variable Importance Plot:")),
                         #plotOutput("CTVarImpGraph")
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
    title = span(tags$img(src = "air2.png", width = '50%')),
    #title = "NYC Airbnb Listings",
                  tags$li(
                      materialSwitch(inputId = "theme", label = strong("Switch to Dark Theme", style = "color:white;"), status = "default"),
                      style = "margin-top: 15px;",
                    class = "dropdown")
                  ),
  sidebar,
  body
)


