
# This is my shiny app
# author: Javier Vallejos 

# we loading the libraries 
library(shiny)
library(leaflet)

# Defining UI for application that draws usisng leaflet library
shinyUI(fluidPage(

    # Application title
    titlePanel("Wildfires in Bolivia"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("nro", "Number of Wildfires",
                        min = 1, max = 200,
                        value = 4),
            h4("You can choose the region to see:"),
            checkboxInput("region1","Show/Hide Region 1", value = TRUE),
            checkboxInput("region2","Show/Hide Region 2", value = FALSE),
            div("If you want report one wildfire point, please send a message
               to +591 60606060 or write us wildfires.bolivia@gmail.com. 
                Thanks for you support.",
                style = "color:blue")
           
        ),

        # Show a map with points of wildfires in Bolivia
        mainPanel(
            h2("Wildfires points",style = "color:red",align = "center"),
            h4("Here we can see the points per region."),
            leafletOutput("mymap"),
            h3("Here you can copy and paste the coordinates:"),
            h4("Latitude"),
            textOutput("nn"),
            h4("Longitude"),
            textOutput("nn1")
           
            
           
        )
    )
))
