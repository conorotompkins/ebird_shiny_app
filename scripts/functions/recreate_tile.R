library(tidyverse)
library(sf)

recreate_tile <- function(x){
  
  mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
  
  coords_geo <- x %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = mollweide)
  
  coord_bbox <- coords_geo %>% 
    st_bbox() %>% 
    st_as_sfc() %>% 
    st_buffer(26700/2)
  
  grid_geo <- coord_bbox %>% 
    st_make_grid(n=c(22, 14))
  
  grid_geo <- grid_geo[coords_geo]
  
  grid_geo <- st_sf(grid_geo) %>% 
    rename(geometry = grid_geo)
  
  return(grid_geo)
  
}