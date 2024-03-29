library(sf)

source("scripts/functions/prep_similarity_index.R")

similarity_index <- read_csv("data/big/similarity_index.csv")

base_map_data <- similarity_index %>%
  prep_similarity_index(54) %>%
  select(geo_index_compare, geometry) %>%
  distinct(geo_index_compare, geometry)

base_map_data %>% 
  ggplot() +
  geom_sf()

list.files("data/big/grid_shapefile", full.names = T) %>% 
  map(file.remove)

base_map_data %>%
  st_write("data/big/grid_shapefile/grid_shapefile.shp")