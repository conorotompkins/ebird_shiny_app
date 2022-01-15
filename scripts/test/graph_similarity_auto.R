library(tidyverse)
library(sf)
library(tigris)
library(leaflet)
library(mapdeck)

set.seed(1234)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_str <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_str, NAME)) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv")

similarity_geo <- similarity_index %>% 
  prep_similarity_index(182)

similarity_geo %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text()

similarity_geo %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = distance)) +
  scale_fill_viridis_c()

similarity_geo %>% 
  ggplot() +
  geom_sf(aes(fill = distance)) +
  geom_sf(data = region_shape_moll, alpha = 0) +
  geom_point(data = filter(similarity_geo, highlight_grid == T),
             aes(x, y), 
             color = "white") +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")
