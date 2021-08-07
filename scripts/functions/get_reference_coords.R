get_reference_coords <- function(similarity_index_data, select_grid_id){
  
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
  
  geo_id_index <- geo_id_index %>% 
    select(-c(x, y))
  
  #choose one grid_id to test
  reference_coords <- geo_id_index %>% 
    filter(grid_id == select_grid_id) %>% 
    select(geo_id)
  
  #turn reference x,y into sf coordinates
  reference_coords <- reference_coords %>% 
    separate(geo_id, into = c("x", "y"), sep = "_") %>% 
    mutate(across(everything(), as.numeric)) %>% 
    st_as_sf(coords = c("x", "y"), crs = mollweide)
  
  return(reference_coords)
  
}