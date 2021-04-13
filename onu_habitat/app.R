
library(tidyverse)
library(shiny)
library(shinydashboard)
library(highcharter)
library(reactable)
library(magrittr)


df <- rio::import("output/base_simulacion.xlsx")


## SOlo para filtrar algunos indicadores de ejemplo
# unique(df$objetivo)
# df %>% filter(objetivo %in% c("Ciudades Ordenadas")) %>% select(indicador, descripcion) %>% distinct() %>% pull(descripcion)
# df %>% filter(objetivo %in% c("Ciudades sustentables y resilientes")) %>% select(indicador, descripcion) %>% distinct() %>% pull(descripcion)
# df %>% filter(objetivo %in% c("Ciudades saludables y con calidad de vida")) %>% select(indicador, descripcion) %>% distinct() %>% pull(descripcion)
# df %>% filter(objetivo %in% c("Ciudades inclusvas")) %>% select(indicador, descripcion) %>% distinct() %>% pull(descripcion)
df %<>% filter(descripcion %in% c("Porosidad de la mancha urbana", "Densidad de población", "Diversificación de funciones urbanas", "Eficiencia en el uso del suelo",
                                 "Huella Hídrica","Huella de Carbono de la ciudad","Área verde per cápita", "Índice de Contaminación Atmosférica",
                                 "Variación anual de la tasa de delitos contra la vida","Índice de percepción de seguridad" ,"Índice de victimización de personas" ,"Tiempo promedio de viaje diario",
                                 "Índice de Pobreza Multidimensional","Índice modificado de Inequidad de Género" ,"Índice de Inequidad socioespacial","Proporción de personas que se sienten discriminadas"))




barra <- dashboardSidebar(sidebarMenu(
  id = "sidebarid",
  menuItem("Página Principal", icon = icon("home"), href = "http://srgmld.github.io/"),
  menuItem("Landing Page", tabName = "landing", icon = icon("globe")),
  menuItem("Organización", tabName = "orden", icon = icon("sort-amount-down-alt")),
  menuItem("Sostenibilidad", tabName = "sus_res", icon = icon("leaf")),
  menuItem("Salud", tabName = "salud", icon = icon("hospital")),
  menuItem("Igualdad", tabName = "inclusivo", icon = icon("users")),
  menuItem("Productividad", tabName = "productivo", icon = icon("chart-line"))
  # menuItem(uiOutput("png"))
  # menuItem("Gobernabilidad", tabName = "gobernar", icon = icon("globe"))#,
  # conditionalPanel(
  #   'input.sidebarid == "orden"',
  #   selectInput("indica_1", "Seleccionar Indicador:", c(df %>% filter(objetivo == "Ciudades Ordenadas") %>% select(descripcion) %>% distinct()))
  #   ),
  # conditionalPanel(
  #   'input.sidebarid == "sus_res"',
  #   selectInput("indica_2", "Seleccionar Indicador:", c(df %>% filter(objetivo == "Ciudades sustentables y resilientes") %>% select(descripcion) %>% distinct()))
  # )
  )
)


cuerpo <-  dashboardBody(
  tabItems(
    tabItem(tabName = "landing",
            img(src="https://rafalopezv.io/static/sm/onu_habitat/ONU-logo-horizontal-color.png", height=100, width=300, align="left"),
            img(src="https://rafalopezv.io/static/sm/onu_habitat/logo-header-2.png", height=80, width=300, align="right"),
            HTML("<hr>
                 <br>
                 <br>
                 <br>
                 <center><h1>Landing Page Producto 1: Visualizador de Indicadores de la PNDIC</h1></center>"),
            img(src="https://rafalopezv.io/static/sm/onu_habitat/landing_shiny_politica2.png", height=500, width=500, align="right"),
            HTML("<br><br>
                 <h3><b>Nota</b></h3>
                 Este visualizador fue desarrollado como una muestra referencial para el Producto 1: Visualizador de Indicadores de la PNDIC.<br>
                 Este visualizador se estructura en un Panel de Control que permita al usuario indentificar los indicadores de manera ordenada e intuitiva.<br>
                 Los vlaores y datos para este ejemplio fueron generados de manera aleatorea con el fin de simular la herramienta y mostrar la funcionalidad de la misma.<br>
                 Contar con los datos y requerimientos oficiales permitirá generar estructuras, gráficos, tablas y material en genral de manera específica y precisa.<br>
                 <br>
                 <h3><b>Funcionalidad</b></h3>
                 Este panel de control esta compuesto por un cuerpo general y un barra lateral que contiene una lista de los diferentes temas u objetivos.
                 A través de la barra lateral el usuario puede ingresar a los diferentes temas y acceder a la información que compone cada teama.<br>
                 El landing page es la pagina principal que transmite distinta información sobre el funcionamiento y estructura de la herramienta.
                 De la misma manera, esta sección contiene una tabla dinámica que permite al usuario revisar de manera resumida la información y datos existentes por cada tema.
                 <hr><br>
                 <h3><b>TABLA: RESUMEN CONTENIDO DE INDICADORES</b></h3>"),
            fluidRow(
              box(width = 12, reactableOutput("table_1"))
            )
    ),
    tabItem(tabName = "orden",
            fluidRow(    
              # box(title = "Organización en las Ciudades", width = 12, background = "light-blue")
              HTML("<center><h3><b>Organización en las Ciudades</b></h3></center><br>")
            ),
            fluidRow(
              box(selectInput("indica_1", "Seleccionar Indicador:", 
                              c(df %>% filter(objetivo == "Ciudades Ordenadas") %>% select(descripcion) %>% distinct() %>% arrange(descripcion)))),
              box(selectInput("ciudad_1", "Seleccionar Ciudad",
                              c(df %>% filter(objetivo == "Ciudades Ordenadas") %>% select(ciudades) %>% distinct() %>% arrange(ciudades))))
            ),
            fluidRow(
              box(highchartOutput("plot1_1", height = 500)),
              box(highchartOutput("polar_1", height = 500))
              ),
            downloadButton("descarga_1","Descargar datos Ciudades Ordenadas en csv", class = "butt1"),
            tags$head(tags$style(".butt1{background-color:#4CA64C;} .butt1{color: black;} .butt1{font-family: Courier New}"))
            ),
    tabItem(tabName = "sus_res",
            fluidRow(    
              # box(title = "Sostenibilidad en las Ciudades", width = 12, background = "light-blue")
              HTML("<center><h3><b>Sostenibilidad en las Ciudades</b></h3></center><br>")
            ),
            fluidRow(
              box(selectInput("indica_2", "Seleccionar Indicador:", 
                  c(df %>% filter(objetivo == "Ciudades sustentables y resilientes") %>% select(descripcion) %>% distinct() %>% arrange(descripcion)))),
              box(selectInput("ciudad_2", "Seleccionar Ciudad",
                              c(df %>% filter(objetivo == "Ciudades sustentables y resilientes") %>% select(ciudades) %>% distinct() %>% arrange(ciudades))))
            ),
            fluidRow(
              box(highchartOutput("plot1_2", height = 500)),
              box(highchartOutput("polar_2", height = 500))
            ),
            downloadButton("descarga_2","Descargar datos Ciudades Sostenibles en csv", class = "butt2"),
            tags$head(tags$style(".butt2{background-color:#4CA64C;} .butt2{color: black;} .butt2{font-family: Courier New}"))
            ),
    tabItem(tabName = "salud",
            h2("En esta sección se visualiza los indicadores del tema: Salud en las Ciudades")),
    tabItem(tabName = "inclusivo",
            h2("En esta sección se visualiza los indicadores del tema: Igualdad en las Ciudades")),
    tabItem(tabName = "productivo",
            h2("En esta sección se visualiza los indicadores del tema: Producción en las Ciudades"))
    # tabItem(tabName = "gobernar",
    #         h2("En esta sección se visualiza los indicadores del tema: Goberabilidad en las Ciudades"))
    )
)

  

ui <- dashboardPage(
  dashboardHeader(title = "Indicadores PNDIC"),
  barra,
  cuerpo
 
  )
  

server <- function(input, output, session) {
  
  # output$image_1 <- renderImage({
  #   list(src = "input/landing_shiny_politica2.png",
  #        # contentType = "image/jpg",
  #        width = "60%",
  #        height = "auto",
  #        align = "center"
  #   )
  # }, deleteFile = FALSE)
  
  # 
  # output$png <- renderUI({
  #   tags$a(img(src = "https://rafalopezv.io/static/sm/home2.png", width = "200px"), href = "https://srgmld.github.io/", target = "_blank")
  # })
  
  output$table_1 <- renderReactable({
    
    reactable(df %>% select(objetivo,descripcion,ciudades),
              groupBy = c("objetivo", "descripcion"),
              columns = list(
                objetivo = colDef(name = "Objetivo"),
                descripcion = colDef(name = "Indicador"),
                ciudades = colDef(name = "Ciudad")
              ))
    
  })
  
  output$plot1_1 <- renderHighchart({
    temp <- df %>% filter(descripcion == input$indica_1) %>% 
      mutate(valor = round(valor,2)) %>% 
      arrange(ciudades) 
    
    
    temp %>% 
      hchart("bar", name = unique(temp$descripcion), color = "#e63946",
             hcaes(ciudades, valor)) %>% 
      hc_chart(style = list(fontFamily = "Open Sans")) %>% 
      hc_tooltip(borderWidth = 0) %>% 
      # hc_plotOptions(column = list(
      #   dataLabels = list(
      #     enabled = T
      #   )
      # )
      # ) %>% 
      hc_title(text = unique(temp$descripcion)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
    
  })
  
  output$polar_1 <- renderHighchart({
    
    temp <- df %>% filter(ciudades == input$ciudad_1, objetivo == "Ciudades Ordenadas") %>% 
      mutate(valor = round(valor,2))
    
    if (nrow(temp) > 1) {
      
      highchart() %>% 
        hc_chart(polar = TRUE) %>% 
        hc_title(text = unique(temp$ciudades)) %>% 
        hc_xAxis(categories = c(temp$descripcion),
                 tickmarkPlacement = "on",
                 lineWidth = 0) %>% 
        hc_yAxis(gridLineInterpolation = "polygon",
                 lineWidth = 0,
                 min = 0) %>% 
        hc_series(
          list(
            name = "Valor",
            data = c(temp$valor),
            pointPlacement = "on",
            type = "column"
          )
        ) %>% 
        hc_legend(enabled = FALSE)
      
    } else{
      
      temp %>% 
        hchart('column', hcaes(x = descripcion, y = valor)) %>% 
        hc_title(text = unique(temp$ciudades)) %>% 
        hc_yAxis(title = list(text = "")) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_legend(enabled = FALSE)
      
    }
    
  })
  
  output$descarga_1 <- downloadHandler(
    
     
    filename = function(){
      # paste("data-", Sys.Date(), ".csv", sep="")
      paste0("ciudades_ordenadas", ".csv")
      
    },
    content = function(file){
      write.csv(df %>% filter(objetivo == "Ciudades Ordenadas"), file)
    }
  )
  
  output$plot1_2 <- renderHighchart({
    
    temp <- df %>% filter(descripcion == input$indica_2) %>% 
      mutate(valor = round(valor,2)) %>% 
      arrange(ciudades) 
    
    
    temp %>% 
      hchart("bar", name = unique(temp$descripcion), color = "#e63946",
             hcaes(ciudades, valor)) %>% 
      hc_chart(style = list(fontFamily = "Open Sans")) %>% 
      hc_tooltip(borderWidth = 0) %>% 
      # hc_plotOptions(column = list(
      #   dataLabels = list(
      #     enabled = T
      #   )
      # )
      # ) %>% 
      hc_title(text = unique(temp$descripcion)) %>% 
      hc_xAxis(title = list(text = "")) %>% 
      hc_yAxis(title = list(text = ""))
    
    
  })
  
  output$polar_2 <- renderHighchart({
    
    temp <- df %>% filter(ciudades == input$ciudad_2, objetivo == "Ciudades sustentables y resilientes") %>% 
      mutate(valor = round(valor,2))
    
    
    
    highchart() %>% 
      hc_chart(polar = TRUE) %>% 
      hc_title(text = unique(temp$ciudades)) %>% 
      hc_xAxis(categories = c(temp$descripcion),
               tickmarkPlacement = "on",
               lineWidth = 0) %>% 
      hc_yAxis(gridLineInterpolation = "polygon",
               lineWidth = 0,
               min = 0) %>% 
      hc_series(
        list(
          name = "Valor",
          data = c(temp$valor),
          pointPlacement = "on",
          type = "column"
        )
      ) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  output$descarga_2 <- downloadHandler(
    
    
    filename = function(){
      # paste("data-", Sys.Date(), ".csv", sep="")
      paste0("ciudades_sostenibles", ".csv")
      
    },
    content = function(file){
      write.csv(df %>% filter(objetivo == "Ciudades sustentables y resilientes"), file)
    }
  )
}

shinyApp(ui, server)

