library(tidyverse)
library(sf)
library(tictoc)

#function to prepare similarity index data for graphing
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

prep_similarity_index <- function(similarity_index_data, select_geo_index = 16){
  
  tic()
  #reference is the geo_id of interest
  #compare are the geo_ids we are comparing to the reference_geo_id
  similarity_index <- similarity_index_data %>% 
    rename(geo_id_reference = geo_id_1,
           geo_id_compare = geo_id_2)
  
  #create an index for each geo_id
  geo_id_index <- similarity_index %>% 
    select(geo_id_reference, geo_id_compare) %>% 
    pivot_longer(cols = everything(),
                 names_to = "type", values_to = "geo_id") %>% 
    arrange(geo_id) %>% 
    distinct(geo_id) %>% 
    separate(geo_id, into = c("x", "y"), sep = "_", remove = F) %>% 
    mutate(x_num = x,
           y_num = y) %>% 
    mutate(across(.cols = c(x_num, y_num), as.numeric)) %>% 
    arrange(x_num, y_num) %>% 
    mutate(geo_index = row_number())
  
  #drop x and y columns
  geo_id_index <- geo_id_index %>% 
    select(-c(x_num, y_num, x, y))

  reference_coords <- geo_id_index %>% 
    filter(geo_index == select_geo_index) %>% 
    select(geo_id)
  
  similarity_geo <- similarity_index %>% 
    #join comparison geo_ids to get their geo_index
    left_join(geo_id_index, by = c("geo_id_compare" = "geo_id")) %>% 
    rename(geo_index_compare = geo_index) %>% 
    #filter on the select_geo_index
    filter(geo_id_reference == pull(reference_coords)) %>% 
    #separe geo_id_compare into x and y cols
    separate(geo_id_compare, into = c("x_compare", "y_compare"), sep = "_", remove = F)

  similarity_geo <- similarity_geo %>% 
    #turn compare x,y into sf coordinates
    mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
    st_as_sf(coords = c("x_compare", "y_compare"), crs = mollweide) %>%
    #join to get geo_index of reference coords
    left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
    rename(geo_index_reference = geo_index) %>% 
    mutate(highlight_grid = geo_index_compare == select_geo_index) %>% 
    #reorder variables
    select(month, geo_id_reference, geo_index_reference, geometry, geo_id_compare, geo_index_compare, correlation, highlight_grid)
  
  similarity_geo <- similarity_geo %>% 
    #calculate x,y from coordinate
    mutate(x = map_dbl(geometry, 1),
           y = map_dbl(geometry, 2)) %>% 
    mutate(geometry = st_buffer(geometry, 
                                dist = 26700/2,
                                endCapStyle = "SQUARE"))
  
  toc()
  return(similarity_geo)
}