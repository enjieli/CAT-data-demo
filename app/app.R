rm(list=ls())
library(leaflet)
library(sf)
library(tidyverse)
library(shiny)
library(ggmap)
library(classInt)
library(RColorBrewer)

load("stops_route_sp.rda")
raw_df<-read_csv("CTA_-_Ridership_-_Avg._Weekday_Bus_Stop_Boardings_in_October_2012.csv")
#######################################
#######################################
#######################################
#######################################

ui <- shinyUI(fluidPage(
  wellPanel(fluidRow(
    column(
      width = 12,
      align = "center",
      h3("CAT Average Weekday Bus Ridership"),
      column(width = 6, selectInput("route", label = "Choose a route:",
                                    choices = sort(unique((stops_route_sp$routes))),
                                    multiple = TRUE)),
      column(width = 6, selectInput("type", label = "Type of ridership:",
                                    choices = c('boardings','alightings'), selected = 'boardings'))
    ))),
 fluidRow(
  column(
    width = 6,
    plotOutput('scatter_plot', brush = "plot_brush", height = 600),
    dataTableOutput('table')
  ),
  column(width = 6, leafletOutput('map', height = 1200))
),
hr()))



server <- function(input, output) {
  
  filteredData <- reactive({
    stops_route_sp %>% 
      filter(type == input$type) %>%
      filter (routes %in% input$route | stop_id %in% filteredstops())
  })
  
  
  filteredstops <- reactive({
    stops<- brushedPoints(raw_df, input$plot_brush, 
                          xvar = "boardings", yvar = "alightings")
    stops$stop_id
  })
  
  
  output$scatter_plot <- renderPlot({
      raw_df %>%
        filter(boardings !=0 &alightings !=0) %>%
        ggplot(aes(x= boardings, y= alightings, color= boardings))+
        geom_point(shape = 19,alpha= 0.3, size= 2) +
        geom_abline(aes(intercept = 0 , slope= 1 ), linetype= "dashed") + 
        scale_x_continuous(trans = 'log') +
        scale_y_continuous(trans = 'log') +
        scale_size(range = c(0.5, 5))  +
        scale_color_viridis_c("boardings",option="plasma",trans = "log") +
        theme_bw() +
        xlab("Boardings") +
        ylab("Alightings") 
    })
  
  output$table <- renderDataTable({
    filteredData() %>% 
      st_set_geometry(NULL) %>%
      select(stop_id,on_street,cross_street,routes,ridership,type)
  }) 
  
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView( lng=-87.705, lat=41.85443,zoom=10) %>%
      addProviderTiles(providers$Stamen.Toner,group="Stamen Toner") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,group="Open Street Map") %>%
      addLayersControl(baseGroups = c("Stamen Toner", "Open Street Map")) 
    
  })
  
  
    observe({
      pal <- colorFactor(palette = 'Dark2', domain = filteredData()$routes)
    
    leafletProxy("map", data = filteredData()) %>% 
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers( color = ~pal(routes),
                        radius = ~ ifelse(ridership <=100, 6, ifelse((ridership <=1000),10,15)),
                        stroke = FALSE, 
                        fillOpacity = 0.8,
                        popup = ~paste("stop_id:", stop_id, 
                          "<br>", "stop_id:", routes, 
                          "<br>", "ridership:", ridership)) %>%
      addLegend("topright", pal = pal, opacity = 1,
                values = ~routes, 
                title = "Routes")
    
  })
  
  
}

shinyApp(ui, server)