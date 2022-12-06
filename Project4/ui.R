library(shiny)
library(shinydashboard)
library(dashboardthemes)



sidebar <- dashboardSidebar(
  sidebarMenu(

    menuItem("About", tabName = "about", icon = icon("circle-info")),
    menuItem("Data", icon = icon("table"), tabName = "data"),
    menuItem("Vizualizations", icon = icon("chart-pie"), tabName = "vizualizations",
             menuItem("Option 1", icon = icon("chart-pie"), tabName = "opt1"),
             menuItem("Option 2", icon = icon("chart-pie"), tabName = "opt2"),
             menuItem("Option 3", icon = icon("chart-pie"), tabName = "opt3")
             ),
    menuItem("Modelling", icon = icon("chart-line"), tabName = "modelling",
             badgeLabel = "Interesting", badgeColor = "yellow")
  )
)

body <- dashboardBody(
  
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  skin = "white",
  ## PLOTS AND SLIDERS
  tabItems(
    tabItem(tabName = "about",
            fluidRow(
              box(title = "Histogram",plotOutput("plot1", height = 250),
                  collapsible = TRUE,
                  background = "maroon",
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


