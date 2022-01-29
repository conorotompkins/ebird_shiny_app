library(shiny)

library(tidyverse)
library(vroom)
library(tools)
library(lubridate)

library(sf)
library(tigris)
library(leaflet)
library(hrbrthemes)
library(ggspatial)

theme_set(theme_ipsum())

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/create_venn_diagram.R")

#load abunds_table
abunds_table <- vroom("data/big/abunds_table.csv")

#create similarity index
#load similarity index data
bins <- c(-Inf, 0, seq(from = 10, to = 80, by = 10), Inf)
length(bins)

bin_labels <- c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60",
                "60-70", "70-80", "80+")
length(bin_labels)

similarity_index <- read_csv("data/big/similarity_index.csv")

# base_map_data <- similarity_index %>%
#   prep_similarity_index(54) %>%
#   select(geo_index_compare, geometry) %>%
#   distinct(geo_index_compare, geometry)
# 
# base_map_data %>% 
#   st_write("data/big/grid_shapefile/grid_shapefile.shp")

base_map_data <- st_read("data/big/grid_shapefile/grid_shapefile.shp") %>% 
  set_names(c("geo_index_compare", "geometry"))

#create region shape
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_input, NAME)) %>% 
  st_transform(crs = original_raster_crs)

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  similarity_index_reactive <- reactive({
    
    similarity_index %>%
      filter(month == input$month_input)
    
  })
  
  # selected_grid_id_reactive <- reactive({
  #   
  #   input$geo_index_compare_input
  #   
  # })
  
  similarity_grid_reactive <- reactive({
    
    req(selected_grid_id_reactive())
    
    similarity_index_reactive() %>%
      prep_similarity_index(selected_grid_id_reactive()) %>% 
      mutate(distance_bin = cut(distance, breaks = bins, labels = bin_labels)) %>% 
      complete(month, distance_bin = bin_labels) %>% 
      st_sf() 
    
  })
  
  output$mouse_interactions <- renderText({
    
    req(input$chloropleth_map_shape_click$id)
    
    str_c("Click: ", selected_grid_id_reactive(), "\n",
          "Hover: ", mouseover_grid_id_reactive(), sep = "")
    
  })
  
  transparency_reactive <- reactive({
    
    input$transparency_slider_input
    
  })
  
  output$chloropleth_map <- renderLeaflet({
    
    #print(similarity_grid_reactive())
    
    base_map_data %>%
      st_transform(crs = "EPSG:4326") %>% 
      leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE
                       )) %>%
      addPolygons(group = "tiles",
                  layerId = ~geo_index_compare,
                  stroke = T,
                  weight = 1) %>% 
      addLayersControl(overlayGroups = c("Tiles", "Legend"),
                       #autoZIndex = T,
                       options = layersControlOptions(collapsed = FALSE))
    
    
  })
  
  selected_grid_id_reactive <- reactive({
    
    input$chloropleth_map_shape_click$id
    
  })
  
  mouseover_grid_id_reactive <- reactive({

    input$chloropleth_map_shape_mouseover$id
    
  })
  
  #observer for chloropleth
  observe({
    
    req(input$chloropleth_map_shape_click$id, similarity_grid_reactive())
    
    similarity_grid_distance <- similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326") %>% 
      mutate(grid_opacity = input$transparency_slider_input)
    
    pal <- colorFactor(
      palette = "viridis",
      domain = similarity_grid_distance$distance_bin)
    
    leafletProxy("chloropleth_map", data = similarity_grid_distance) %>%
      clearGroup("Tiles") %>%
      addPolygons(group = "Tiles",
                  layerId = ~geo_index_compare,
                  color = "#444444",
                  fillColor = ~pal(distance_bin),
                  fillOpacity = ~grid_opacity,
                  stroke = T,
                  weight = 1,
                  label = ~paste0("geo_id: ", geo_index_compare, "\n", "Dissimilarity: ", round(distance,0)),
                  highlightOptions = highlightOptions(color = "white", bringToFront = T))
    
  })
  
  observe({
    
    req(input$chloropleth_map_shape_click$id)
    
    similarity_grid_distance <- similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326")
    
    pal <- colorFactor(
      palette = "viridis",
      domain = similarity_grid_distance$distance_bin)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy <- leafletProxy("chloropleth_map", data = similarity_grid_distance)
    
    proxy %>% clearControls()
    
    proxy %>% 
      addLegend(group = "Legend",
                pal = pal, 
                values = ~distance_bin, 
                opacity = 0.7, 
                #labFormat = labelFormat(suffix = "%"),
                title = "Dissimilarity from selected area",
                position = "bottomright")
    
  })
  
  output$histogram <- renderPlot({
    
    similarity_grid_reactive() %>% 
      count(month, distance_bin) %>% 
      ggplot(aes(distance_bin, n, fill = distance_bin)) +
      geom_col() +
      scale_fill_viridis_d() +
      guides(fill = "none") +
      labs(x = "Dissimilarity")
    
  })
  
  #calculate time since leaflet mouseover changed. if that time is > 3 seconds, render venn diagram
  
  output$venn_diagram <- renderPlot({

    reference <- selected_grid_id_reactive()

    compare <- mouseover_grid_id_reactive()
    
    create_venn_diagram(reference, compare, similarity_grid_reactive(), abunds_table)

  })
  
})
