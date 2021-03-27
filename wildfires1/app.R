#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# we loading the libraries 
library(shiny)
library(leaflet)

# Defining UI for application that draws usisng leaflet library
ui <- fluidPage(
    
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
)


library(shiny)
library(leaflet)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # Here we are using the slidebar to show the points
    n1 <- reactive({
        #  divide per region
        nro <- round(input$nro/2) 
        
        # Calculating the points of region 1 when the checkbox is TRUE    
        if(input$region1){
            
            ltrg1 <- -1*runif(nro, min = 15.1, max = 18.2)
            lnrg1 <- -1*runif(nro, min = 59.1, max = 66.2)
            
        }
        else {
            ltrg1 <- 0
            lnrg1 <- 0
        }
        
        # Calculating the points of region 2 when the checkbox is TRUE
        if(input$region2){
            ltrg2 <- -1*runif(nro, min = 17.1, max = 19.9)
            lnrg2 <- -1*runif(nro, min = 57.3, max = 59.9)
        }
        else {
            ltrg2 <- 0
            lnrg2 <- 0
        }
        
        
        # Here we joins all data of after put on the map 
        if(ltrg1 == 0)
        {
            lt <- c(ltrg2)
        }
        else {
            lt <- c(ltrg1, ltrg2) }
        
        if(lnrg1 == 0){
            ln <- c(lnrg2)
        } 
        else {
            ln <- c(lnrg1,lnrg2)
        }
        
        # create a data frame 
        d <-  data.frame(lt,ln)
        
        list(d = d)
    })
    
    
    
    output$mymap <- renderLeaflet({
        
        a <- n1()$d  # call the data frame with lat and long
        
        leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            setView(lng= "-64.481328", lat="-17.226198", zoom = 5) %>%
            addCircles(a$ln,a$lt,
                       radius = 200, color = '#ff0000')
    })
    
    # This part show the text of latitude 
    output$nn <- renderText({
        a <- n1()$d
        
        as.character(a$lt)
    })
    
    # This part show the text of longitude
    output$nn1 <- renderText({
        a <- n1()$d
        
        as.character(a$ln)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
