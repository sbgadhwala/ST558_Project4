library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(tidyverse)
data("GermanCredit")
library(DT)
library(caret)



sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("About", tabName = "about", icon = icon("circle-info"), badgeLabel = " ", badgeColor = "blue"),
    menuItem("Data", icon = icon("table"), tabName = "data", badgeLabel = " ", badgeColor = "orange"),
    menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations", 
             menuItem("Chicago", icon = icon("c"), tabName = "opt1", badgeLabel = " ", badgeColor = "purple"),
             menuItem("Denver", icon = icon("d"), tabName = "opt2", badgeLabel = " ", badgeColor = "purple"),
             menuItem("Los Angeles", icon = icon("l"), tabName = "opt3", badgeLabel = " ", badgeColor = "purple")
             ),
    menuItem("Modelling", icon = icon("chart-line"), tabName = "modelling",
             badgeLabel = " ", badgeColor = "lime")
  )
)

body <- dashboardBody(
  
  shinyDashboardThemes(
    theme = "grey_light"
  ),
  skin = "white",
  ## PLOTS AND SLIDERS
  tabItems(
    tabItem(tabName = "about",
            fluidRow(
              theme = shinytheme("united"),
              titlePanel(HTML("<h1><center><font size=14> About </font></center></h1>")),
              HTML("<h6><center><font size=4> Hello Hello Hello Hello Hello <br> BBBB Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello Hello  </font></center></h6>")
            )
    ),
    
    
    
    tabItem(tabName = "opt1",
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
            dataTableOutput("table")
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
            )
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "USA Crime Analysis App"),
  sidebar,
  body
)


