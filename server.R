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
bins <- c(-Inf, 0, seq(from = 10, to = 80, by = 10), Inf)
length(bins)

bin_labels <- c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60",
                "60-70", "70-80", "80+")
length(bin_labels)

similarity_index <- read_csv("data/big/similarity_index.csv")

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
  
  selected_grid_id_reactive <- reactive({
    
    input$geo_index_compare_input
    
  })
  
  similarity_grid_reactive <- reactive({
    
    similarity_index_reactive() %>%
      prep_similarity_index(selected_grid_id_reactive()) %>% 
      mutate(distance_bin = cut(distance, breaks = bins, labels = bin_labels)) %>% 
      complete(month, distance_bin = bin_labels) %>% 
      st_sf() 
    
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
    
    #print(similarity_grid_reactive())
    
    similarity_grid_reactive() %>%
      ggplot() +
      #annotation_map_tile(type = "stamenbw") + 
      geom_sf(aes(fill = distance_bin,
                  color = distance_bin),
              alpha = transparency_reactive(),
              color = NA) +
      geom_point(data = filter(similarity_grid_reactive(),
                               highlight_grid == T),
                 aes(x, y),
                 color = "white") +
      geom_sf(data = region_shape_moll, alpha = 0) +
      scale_fill_viridis_d() +
      scale_color_viridis_d() +
      labs(fill = "Dissimilarity")
    
    
  }, res = 96)
  
  output$histogram <- renderPlot({
    
    similarity_grid_reactive() %>% 
      count(month, distance_bin) %>% 
      ggplot(aes(distance_bin, n, fill = distance_bin)) +
      geom_col() +
      scale_fill_viridis_d() +
      guides(fill = "none") +
      labs(x = "Dissimilarity")
    
  })
  
})
