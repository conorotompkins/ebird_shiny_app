library(shiny)

library(tidyverse)

library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

#create similarity index
#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
    rename(geo_id_reference = geo_id_1,
           geo_id_compare = geo_id_2)

#create IDs for each geo_id
geo_id_index <- similarity_index %>% 
    select(geo_id_reference, geo_id_compare) %>% 
    pivot_longer(cols = everything(),
                 names_to = "type", values_to = "geo_id") %>% 
    arrange(geo_id) %>% 
    distinct(geo_id) %>% 
    separate(geo_id, into = c("x", "y"), sep = "_", remove = F) %>% 
    mutate(across(.cols = c(x, y), as.numeric)) %>% 
    arrange(x, y) %>% 
    mutate(grid_id = row_number())

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
    
    reference_coords_reactive <- reactive({
        
        req(input$reference_coords)
        
        print(input$reference_coords)
        
        geo_id_index %>% 
            filter(grid_id == input$reference_coords) %>% 
            select(geo_id)
        
    })
    
    similarity_geo_reactive <- reactive({
        
        req(input$reference_coords)
        
        similarity_index %>% 
            #join comparison geo_ids to get their grid_id
            left_join(geo_id_index, by = c("geo_id_compare" = "geo_id")) %>% 
            rename(grid_id_compare = grid_id) %>% 
            #filter on the reference geo_id
            filter(geo_id_reference == pull(reference_coords_reactive())) %>% 
            separate(geo_id_compare, into = c("x_compare", "y_compare"), sep = "_")
        
    })
    
    reference_coords_updated_reactive <- reactive({
        
        req(input$reference_coords)
        
        reference_coords_reactive() %>% 
            separate(geo_id, into = c("x", "y"), sep = "_") %>% 
            mutate(across(everything(), as.numeric)) %>% 
            st_as_sf(coords = c("x", "y"), crs = mollweide)
    })
    
    
    similarity_geo_updated_reactive <- reactive({
        
        req(input$reference_coords)
        
        similarity_geo_reactive() %>% 
            #turn compare x,y into sf coordinates
            mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
            st_as_sf(coords = c("x_compare", "y_compare"), crs = mollweide) %>%
            #join to get grid_id of reference coords
            left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
            rename(grid_id_reference = grid_id) %>% 
            #reorder variables
            select(geo_id_reference, grid_id_reference, geometry, grid_id_compare, distance)
        
    })
    
    
    similarity_grid_reactive <- reactive({
        
        req(input$reference_coords)
        
        similarity_geo_updated_reactive() %>% 
            #create grid based on compare coords from similarity_geo
            st_make_grid(n = 10, crs = mollweide) %>% 
            st_as_sf() %>%
            #get grid_id from transformed geo_id_index
            st_join(geo_id_index %>% 
                        separate(geo_id, into = c("x", "y"), sep = "_") %>% 
                        mutate(across(.cols = everything(), as.numeric)) %>% 
                        st_as_sf(coords = c("x", "y"), crs = mollweide),
                    join = st_intersects)
        
    })
    
    output$table <- renderTable({
        
        req(input$reference_coords)
        
        similarity_grid_reactive() %>% 
            st_join(similarity_geo_updated_reactive(), join = st_intersects)
        
    })
    
    output$map <- renderPlot({
        
        req(input$reference_coords)
        
        similarity_grid_reactive() %>% 
            st_join(similarity_geo_updated_reactive(), join = st_intersects) %>% 
            ggplot() +
            geom_sf(aes(fill = distance), lwd = 0) +
            geom_sf(data = pa_shape_moll, alpha = 0) +
            geom_sf(data = reference_coords_updated_reactive()) +
            geom_sf_label(aes(label = grid_id)) +
            scale_fill_viridis_c(direction = 1) +
            labs(fill = "Distance")
        
    })
    
})
