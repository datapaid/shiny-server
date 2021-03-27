#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
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
   
})


