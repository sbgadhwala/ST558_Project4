# New York City Airbnb - Predicting Ratings for New Hosts

<img
  src="Project4/www/air.png"
  style="display: inline-block; margin: 0 auto; max-width: 50%">

The main prupose of this app is to let users, who are looking to put up their property on Airbnb in New York City, predict their listing's overall rating based on various attributes related to their property(s). This App also lets you visualize the trends of different attributes relevant to the Airbnb data, and also lets you see geospatially how are the competitors near you are doing. You can also filter data from the available data set to gain important insights.

Sample Scenario:
Supose you have a home that you want to list on the Airbnb webiste, but you are not sure what price you should keep or what service fee you should charge or other factors such that users would prefer your listing and give a good rating. For cases like this, this app would come handy as it provides just the right amount of insights and prediction of Ratings according to your property's attributes for you to make a decision.

To run this application you would need the following packages: 
  * shiny
  * shinydashboard
  * dashboardthemes
  * shinythemes
  * shinyWidgets
  * tidyverse
  * DT
  * caret
  * leaflet
  * corrplot
  * shinycssloaders
  
To install these packages and load it in your environment, you can run the following code:
```{r}
  
install.packages("shiny")
install.packages("shinydashboard")
install.packages("dashboardthemes")
install.packages("shinythemes")
install.packages("shinyWidgets")
install.packages("tidyverse")
install.packages("DT")
install.packages("caret")
install.packages("leaflet")
install.packages("corrplot")
install.packages("shinycssloaders")



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
  
```
  
To Run the Application, you can use the following chunk of code:

```{r}

library(shiny)

shiny::runGitHub("ST558_Project4", "sbgadhwala", ref="main", subdir = "Project4")

```
