library(shiny)

library(tidyverse)

library(sf)
library(tigris)
library(leaflet)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")

# file.copy(from = "data/big/similarity_index.csv",
#           to = "area_similarity/data")

#create similarity index
#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv")

base_map_data <- similarity_index %>% 
  prep_similarity_index()

#create pa shape
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

#grid_geo <- st_read("data/big/grid_shapefile/grid_shapefile.shp")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  similarity_grid_reactive <- reactive({
    
    req(input$map_shape_click$id)
    
    prep_similarity_index(similarity_index_data = similarity_index,
                          selected_grid_id_reactive()) %>% 
      mutate(highlight_area = geo_index_reference == geo_index_compare,
             highlight_area = as.factor(highlight_area))
    
  })
  
  output$map <- renderLeaflet({
    
    #req(similarity_grid_reactive())
    
    base_map_data %>% 
      st_transform(crs = "EPSG:4326") %>% 
      leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE
                       )) %>%
      addCircles(layerId = ~geo_index_compare,
                 stroke = F,
                 weight = 1,
                 radius = 10^4)
    
  })
  
  selected_grid_id_reactive <- reactive({
    
    input$map_shape_click$id
    
  })
  
  #observer for chloropleth
  observe({
    
    req(input$map_shape_click$id)
    
    similarity_grid_distance <- similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326") %>%
      mutate(grid_opacity = case_when(highlight_area == "TRUE" ~ .9,
                                      highlight_area == "FALSE" ~ .6))
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = similarity_grid_distance$distance)
    
    leafletProxy("map", data = similarity_grid_distance) %>%
      clearGroup("highlight_shape") %>%
      addCircles(group = "highlight_shape",
                 layerId = ~geo_index_compare,
                 fillColor = ~pal(distance),
                 fillOpacity = ~grid_opacity,
                 stroke = F,
                 weight = 1, 
                 radius = 10^4)
    
  })
  
  observe({
    
    req(input$map_shape_click$id)
    
    similarity_grid_distance <- similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326") %>%
      mutate(grid_opacity = case_when(highlight_area == "TRUE" ~ .9,
                                      highlight_area == "FALSE" ~ .6))
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = similarity_grid_distance$distance)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy <- leafletProxy("map", data = similarity_grid_distance)
    
    proxy %>% clearControls()
    
    proxy %>% 
      addLegend(pal = pal, 
                values = ~distance, 
                opacity = 0.7, 
                #labFormat = labelFormat(suffix = "%"),
                title = "Dissimilarity from selected area",
                position = "bottomright",
                group = "legend")
    
  })
  
  observeEvent(input$map_shape_click, {
    
    p <- input$map_shape_click$id
    print(p)
    
    output$clicked_grid_id <- renderPrint(p)
    
  })
  
})
