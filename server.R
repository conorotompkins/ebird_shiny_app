library(shiny)

library(tidyverse)

library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")
#source("scripts/functions/recreate_tile.R")

# file.copy(from = "data/big/similarity_index.csv",
#           to = "area_similarity/data")

#create similarity index
#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv")

# similarity_geo_tile <- st_read("data/similarity_geo_tile/similarity_geo_tile.shp") %>% 
#   set_names(c("x", "y", "geo_index_reference", "geo_index_compare", "distance", "highlight_grid", "geometry"))

base_map_data <- similarity_index %>% 
  prep_similarity_index()

base_map_data %>% 
  ggplot() +
  geom_sf(aes(fill = distance))

base_map_data %>% 
  ggplot() +
  geom_text(aes(x, y, label = geo_index_compare))

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
  
  selected_grid_id_reactive <- reactive({
    
    input$geo_index_compare_input
    
  })
  
  similarity_grid_reactive <- reactive({
    
    similarity_index %>%
      prep_similarity_index(selected_grid_id_reactive())
    
  })
  
  output$clicked_grid_id <- renderPrint({
    
    p <- selected_grid_id_reactive()
    #print(p)
    p
    
  })
  
  output$chloropleth_map <- renderPlot({
    
    print(similarity_grid_reactive())
    
    similarity_grid_reactive() %>%
      ggplot() +
      geom_sf(aes(fill = distance)) +
      geom_point(data = filter(similarity_grid_reactive(),
                               highlight_grid == T),
                 aes(x, y),
                 color = "white") +
      scale_fill_viridis_c()
    
  })
  
  #output$table_output <- renderTable(head(similarity_grid_reactive()))
  
})
