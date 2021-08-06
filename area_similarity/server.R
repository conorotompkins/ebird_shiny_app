library(shiny)

library(tidyverse)

library(sf)
library(tigris)
library(leaflet)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/get_reference_coords.R")

#create similarity index
#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
  rename(geo_id_reference = geo_id_1,
         geo_id_compare = geo_id_2)
# 
# #create IDs for each geo_id
# geo_id_index <- similarity_index %>% 
#     select(geo_id_reference, geo_id_compare) %>% 
#     pivot_longer(cols = everything(),
#                  names_to = "type", values_to = "geo_id") %>% 
#     arrange(geo_id) %>% 
#     distinct(geo_id) %>% 
#     separate(geo_id, into = c("x", "y"), sep = "_", remove = F) %>% 
#     mutate(across(.cols = c(x, y), as.numeric)) %>% 
#     arrange(x, y) %>% 
#     mutate(grid_id = row_number())

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

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  similarity_grid_reactive <- reactive({
    
    req(input$reference_coords)
    
    prep_similarity_index(similarity_index, input$reference_coords)
    
  })
  
  reference_coords_reactive <- reactive({
    
    req(similarity_grid_reactive())
    
    get_reference_coords(similarity_grid_reactive(), input$reference_coords) %>% 
      st_transform(crs = "EPSG:4326")
    
    
  })
  
  # reference_coords_reactive <- reactive({
  #     
  #     req(input$reference_coords)
  #     
  #     print(input$reference_coords)
  #     
  #     reference_coords <- geo_id_index %>% 
  #         filter(grid_id == input$reference_coords) %>% 
  #         select(geo_id)
  #     
  #     print("reference_coords")
  #     reference_coords %>% 
  #         print()
  #     return(reference_coords)
  #     
  # })
  
  # similarity_geo_reactive <- reactive({
  #     
  #     req(input$reference_coords)
  #     
  #     similarity_geo <- similarity_index %>% 
  #         #join comparison geo_ids to get their grid_id
  #         left_join(geo_id_index, by = c("geo_id_compare" = "geo_id")) %>% 
  #         rename(grid_id_compare = grid_id) %>% 
  #         #filter on the reference geo_id
  #         filter(geo_id_reference == pull(reference_coords_reactive(), geo_id)) %>% 
  #         separate(geo_id_compare, into = c("x_compare", "y_compare"), sep = "_")
  #     
  #     print("similarity_geo")
  #     similarity_geo %>% 
  #         head() %>% 
  #         print()
  #     return(similarity_geo)
  #     
  # })
  
  # reference_coords_updated_reactive <- reactive({
  #     
  #     req(input$reference_coords)
  #     
  #     reference_coords_updated <- reference_coords_reactive() %>% 
  #         separate(geo_id, into = c("x", "y"), sep = "_") %>% 
  #         mutate(across(everything(), as.numeric)) %>% 
  #         st_as_sf(coords = c("x", "y"), crs = mollweide)
  #     
  #     print("reference_coords_updated")
  #     print(reference_coords_updated)
  #     return(reference_coords_updated)
  # })
  
  
  # similarity_geo_updated_reactive <- reactive({
  #     
  #     req(input$reference_coords)
  #     
  #     similarity_geo_updated <- similarity_geo_reactive() %>% 
  #         #turn compare x,y into sf coordinates
  #         mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
  #         st_as_sf(coords = c("x_compare", "y_compare"), crs = mollweide) %>%
  #         #join to get grid_id of reference coords
  #         left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
  #         rename(grid_id_reference = grid_id) %>% 
  #         #reorder variables
  #         select(geo_id_reference, grid_id_reference, geometry, grid_id_compare, distance)
  #   
  #     print("similarity_geo_updated")
  #     print(similarity_geo_updated)
  #     return(similarity_geo_updated)
  # })
  
  
  # similarity_grid_reactive <- reactive({
  #     
  #     req(input$reference_coords)
  #     
  #     similarity_geo_updated_reactive() %>% 
  #         #create grid based on compare coords from similarity_geo
  #         st_make_grid(n = 10, crs = mollweide) %>% 
  #         st_as_sf() %>%
  #         #get grid_id from transformed geo_id_index
  #         st_join(geo_id_index %>% 
  #                     separate(geo_id, into = c("x", "y"), sep = "_") %>% 
  #                     mutate(across(.cols = everything(), as.numeric)) %>% 
  #                     st_as_sf(coords = c("x", "y"), crs = mollweide),
  #                 join = st_intersects)
  #     
  # })
  
  output$table <- renderTable({
    
    req(input$reference_coords)
    
    similarity_grid_reactive() #%>% 
    #st_join(similarity_geo_updated_reactive(), join = st_intersects)
    
  })
  
  output$map <- renderLeaflet({
    
    req(input$reference_coords)
    
    similarity_grid_distance <- similarity_grid_reactive() %>% 
      #st_join(similarity_geo_updated_reactive(), join = st_intersects) %>% 
      st_transform(crs = "EPSG:4326")
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = similarity_grid_distance$distance)
    
    similarity_grid_distance %>%
      leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE,
                                                     #minZoom = 9,
                                                     #maxZoom = 8
                       )) %>%
      addPolygons(layerId = ~grid_id_compare,
                  fillColor = ~pal(distance),
                  fillOpacity = .7,
                  stroke = F,
                  #color = "#FCCF02",
                  #weight = 1,
                  # highlightOptions = highlightOptions(
                  #     stroke = T,
                  #     color = "black",
                  #     weight = 2,
                  #     opacity = NULL,
                  #     fill = NULL,
                  #     fillColor = NULL,
                  #     fillOpacity = NULL,
                  #     dashArray = NULL,
                  #     bringToFront = T,
                  #     sendToBack = NULL
                  # )
      ) %>%
      #addPolygons(reference_coords_reactive()) %>% 
      addLegend("bottomright", pal = pal, values = ~distance,
                title = "Distance",
                opacity = 1
      )
    
  })
  
  observeEvent(input$map_shape_click, { 
    p <- input$map_shape_click  # typo was on this line
    print(p)
    
    output$clicked_grid_id <- renderText(p$id)
  })
  
})
