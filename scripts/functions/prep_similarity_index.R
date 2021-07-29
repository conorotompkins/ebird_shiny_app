#function to prepare similarity index data for graphing

prep_similarity_index <- function(similarity_index_data, select_grid_id){
  
  #create IDs for each geo_id
  geo_id_index <- similarity_index_data %>% 
    select(geo_id_reference, geo_id_compare) %>% 
    pivot_longer(cols = everything(),
                 names_to = "type", values_to = "geo_id") %>% 
    arrange(geo_id) %>% 
    distinct(geo_id) %>% 
    separate(geo_id, into = c("x", "y"), sep = "_", remove = F) %>% 
    mutate(across(.cols = c(x, y), as.numeric)) %>% 
    arrange(x, y) %>% 
    mutate(grid_id = row_number())
  
  # geo_id_index %>% 
  #   ggplot(aes(x, y, label = grid_id)) +
  #   geom_label()
  
  geo_id_index <- geo_id_index %>% 
    select(-c(x, y))
  
  #choose one grid_id to test
  reference_coords <- geo_id_index %>% 
    filter(grid_id == select_grid_id) %>% 
    select(geo_id)
  
  similarity_geo <- similarity_index %>% 
    #join comparison geo_ids to get their grid_id
    left_join(geo_id_index, by = c("geo_id_compare" = "geo_id")) %>% 
    rename(grid_id_compare = grid_id) %>% 
    #filter on the reference geo_id
    filter(geo_id_reference == pull(reference_coords)) %>% 
    separate(geo_id_compare, into = c("x_compare", "y_compare"), sep = "_")
  
  similarity_geo
  
  #turn reference x,y into sf coordinates
  reference_coords <- reference_coords %>% 
    separate(geo_id, into = c("x", "y"), sep = "_") %>% 
    mutate(across(everything(), as.numeric)) %>% 
    st_as_sf(coords = c("x", "y"), crs = mollweide)
  
  similarity_geo <- similarity_geo %>% 
    #turn compare x,y into sf coordinates
    mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
    st_as_sf(coords = c("x_compare", "y_compare"), crs = mollweide) %>%
    #join to get grid_id of reference coords
    left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
    rename(grid_id_reference = grid_id) %>% 
    #reorder variables
    select(geo_id_reference, grid_id_reference, geometry, grid_id_compare, distance)
  
  similarity_grid <- similarity_geo %>% 
    #create grid based on compare coords from similarity_geo
    st_make_grid(n = 10, crs = mollweide) %>% 
    st_as_sf() %>%
    #get grid_id from transformed geo_id_index
    st_join(geo_id_index %>% 
              separate(geo_id, into = c("x", "y"), sep = "_") %>% 
              mutate(across(.cols = everything(), as.numeric)) %>% 
              st_as_sf(coords = c("x", "y"), crs = mollweide),
            join = st_intersects)
  
  similarity_grid_distance <- similarity_grid %>% 
    st_join(similarity_geo, join = st_intersects)
  
  return(similarity_grid_distance)
  
}
  