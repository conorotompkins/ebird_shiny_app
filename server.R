library(shiny)

library(tidyverse)

library(sf)
library(tigris)
library(hrbrthemes)
library(ggspatial)

theme_set(theme_ipsum())

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")

#create similarity index
#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv")

# similarity_geo_tile <- st_read("data/similarity_geo_tile/similarity_geo_tile.shp") %>% 
#   set_names(c("x", "y", "geo_index_reference", "geo_index_compare", "distance", "highlight_grid", "geometry"))

base_map_data <- similarity_index %>% 
  prep_similarity_index()

# base_map_data %>% 
#   ggplot() +
#   geom_sf(aes(fill = distance))
# 
# base_map_data %>% 
#   ggplot() +
#   geom_text(aes(x, y, label = geo_index_compare))

#create region shape
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_input, NAME)) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

# region_shape_moll %>% 
#   ggplot() +
#   geom_sf()

#grid_geo <- st_read("data/big/grid_shapefile/grid_shapefile.shp")

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  similarity_index_reactive <- reactive({
    
    similarity_index %>%
      filter(month == input$month_input)
    
  })
  
  selected_grid_id_reactive <- reactive({
    
    input$geo_index_compare_input
    
  })
  
  similarity_grid_reactive <- reactive({
    
    similarity_index_reactive() %>%
      prep_similarity_index(selected_grid_id_reactive())
    
  })
  
  output$clicked_grid_id <- renderPrint({
    
    p <- selected_grid_id_reactive()
    #print(p)
    p
    
  })
  
  transparency_reactive <- reactive({
    
    input$transparency_slider_input
    
  })
  
  output$chloropleth_map <- renderPlot({
    
    print(similarity_grid_reactive())
    
    similarity_grid_reactive() %>%
      ggplot() +
      annotation_map_tile(type = "stamenbw") + 
      geom_sf(aes(fill = distance),
              alpha = transparency_reactive()) +
      geom_point(data = filter(similarity_grid_reactive(),
                               highlight_grid == T),
                 aes(x, y),
                 color = "white") +
      geom_sf(data = region_shape_moll, alpha = 0) +
      scale_fill_viridis_c()
    
  })
  
  output$histogram <- renderPlot({
    
    similarity_grid_reactive() %>% 
      ggplot(aes(distance)) +
      geom_histogram()
    
  })
  
})
