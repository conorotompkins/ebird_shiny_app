library(tidyverse)
library(vroom)
library(sf)
library(tigris)
library(leaflet)
library(mapdeck)
library(ggspatial)

set.seed(1234)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_str <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_str, NAME)) %>% 
  st_transform(crs = original_raster_crs)

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

#load similarity index data
similarity_index <- vroom("data/big/similarity_index.csv")

similarity_geo <- similarity_index %>% 
  prep_similarity_index(45) %>% 
  mutate(month = fct_relevel(month, month.abb))

similarity_geo %>% 
  distinct(x, y, geo_index_compare) %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text()

bins <- c(-Inf, 0, seq(from = 10, to = 80, by = 10), Inf)
length(bins)

bin_labels <- c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60",
                "60-70", "70-80", "80+")
length(bin_labels)

similarity_geo_binned <- similarity_geo %>% 
  mutate(distance_bin = cut(distance, breaks = bins, labels = bin_labels))

levels(similarity_geo_binned$distance_bin)

similarity_geo_binned %>% 
  filter(month == "Oct") %>% 
  complete(distance_bin = bin_labels) %>% 
  st_sf() %>% 
  #View()
  ggplot() +
  annotation_map_tile(type = "stamenbw") + 
  geom_sf(aes(fill = distance_bin, color = distance_bin), alpha = .25) +
  geom_sf(data = region_shape_moll, alpha = 0) +
  geom_point(data = filter(similarity_geo_binned, highlight_grid == T),
             aes(x, y), 
             color = "white") +
  scale_fill_viridis_d(direction = 1) +
  scale_color_viridis_d(direction = 1) +
  guides(color = "none") +
  labs(fill = "Distance")

similarity_geo_binned %>% 
  complete(month, distance_bin = bin_labels) %>% 
  st_sf() %>% 
  filter(month == "Oct") %>% 
  filter(!is.na(distance)) %>% 
  count(month, distance_bin) %>% 
  ggplot(aes(distance_bin, n, fill = distance_bin)) +
  geom_col() +
  facet_wrap(~month, ncol = 2) +
  scale_fill_viridis_d()

similarity_geo_binned %>% 
  ggplot(aes(distance, fill = distance_bin)) +
  geom_histogram(binwidth = 10) +
  geom_rug() +
  facet_wrap(~month, ncol = 2, scales = "free") +
  scale_fill_viridis_d()
