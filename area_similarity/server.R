library(shiny)

library(tidyverse)

library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

#create similarity index
similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
    arrange(geo_id_1, distance) %>% 
    separate(geo_id_2, into = c("x", "y"), sep = "_") %>% 
    mutate(across(.cols = c(x, y), as.numeric))

geo_id_lookup <- similarity_index %>% 
    distinct(geo_id_1) %>% 
    mutate(geo_id = row_number())

similarity_index <- similarity_index %>% 
    left_join(geo_id_lookup, by = "geo_id_1") %>% 
    select(geo_id, everything())

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
    
    selected_geo_id_reactive <- reactive({
        
        input$geo_id_selection
        
    })
    
    
    filtered_similarity_data_reactive <- reactive({
        
        similarity_index %>% 
            filter(geo_id == selected_geo_id_reactive())
        
    })
    
    selected_geo_shape_reactive <- reactive({
        
        filtered_similarity_data_reactive() %>% 
            distinct(geo_id, geo_id_1) %>% 
            separate(geo_id_1, into = c("x", "y"), sep = "_") %>% 
            mutate(across(.cols = c(x, y), as.double))
        
    })
    
    output$table <- renderTable({
        
        selected_geo_shape_reactive()
        
    })
    
    # output$map <- renderPlot({
    #     
    #     filtered_similarity_data_reactive() %>% 
    #         ggplot() +
    #         geom_tile(aes(x, y, fill = distance)) +
    #         geom_sf(data = pa_shape_moll, alpha = 0) +
    #         geom_point(data = selected_geo_shape_reactive(),
    #                   aes(x, y)) +
    #         scale_fill_viridis_c(direction = -1) +
    #         labs(fill = "Similarity")
    #     
    # })
    
    output$leaflet_map <- renderLeaflet({
        
        filtered_similarity_data_reactive() %>% 
            separate(geo_id_2, into = c("x", "y"), sep = "_") %>% 
            mutate(across(.cols = c(x, y), as.double)) %>% 
            st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
            st_make_grid(n = 10, crs = mollweide) %>% 
            leaflet() %>% 
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE,
                                                           #minZoom = 9, 
                                                           #maxZoom = 8
                             )) %>% 
            # setView(lng = -80.01181092430839, lat = 40.44170119122286, zoom = 10) %>% 
            # setMaxBounds(lng1 = -79.5, lng2 = -80.5, lat1 = 40.1, lat2 = 40.7) %>% 
            addPolygons(layerId = ~geo_id,
                        fillColor = "#000000",
                        fillOpacity = .7,
                        stroke = TRUE,
                        color = "#FCCF02",
                        weight = 1)
    })
    
})
