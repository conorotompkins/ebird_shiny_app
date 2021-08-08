library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(mapdeck)

set.seed(1234)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/get_reference_coords.R")

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
  prep_similarity_grid(30)

similarity_index %>% 
  st_drop_geometry() %>% 
  count(grid_id_reference) %>% 
  distinct(n)

similarity_index %>% 
  select(grid_id_compare, geometry) %>% 
  mutate(centroid = st_point_on_surface(geometry),
         lon = map_dbl(centroid, 1),
         lat = map_dbl(centroid, 2)) %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(lon, lat))

similarity_index %>% 
  select(grid_id_compare, geometry) %>% 
  rename(grid_id = grid_id_compare) %>% 
  mutate(centroid = st_point_on_surface(geometry),
         lon = map_dbl(centroid, 1),
         lat = map_dbl(centroid, 2)) %>% 
  select(-centroid) %>% 
  st_write("data/big/grid_shapefile/grid_shapefile.shp")

test <- st_read("data/big/grid_shapefile/grid_shapefile.shp")

test %>% 
  ggplot() +
  geom_sf() +
  geom_point(aes(lon, lat))
