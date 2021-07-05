library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)
library(widyr)

set.seed(1234)

options(tigris_use_cache = TRUE)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

pa_shape_moll %>% 
  ggplot() +
  geom_sf()

abunds_table <- 
  list.files("data/big/species_abundance", full.names = T) %>% 
  .[1:5] %>% 
  #keep(str_detect(., "American Redstart")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext,
         abundance = coalesce(abundance, 0))

abundance_summary <- abunds_table %>%
  group_by(comName, month, x, y) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>%
  ungroup() %>% 
  mutate(species_id = str_c(comName, sep = "_"),
         geo_id = str_c(x, y, sep = "_")) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide)

rm(abunds_table)

grid_geo <- abundance_summary %>% 
  st_make_grid(crs = mollweide) %>% 
  st_as_sf()

grid_geo %>% 
  ggplot() +
  geom_sf(data = pa_shape_moll) +
  geom_sf(alpha = .3)
  
bird_grid <- grid_geo %>% 
  st_join(abundance_summary, join = st_intersects) %>% 
  mutate(centroid = map(geometry, st_point_on_surface)) %>% 
  group_by(comName, month, centroid) %>% 
  summarize(n = n(),
            abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

rm(abundance_summary)

bird_grid %>%
  filter(month == "Apr") %>% 
  ggplot() +
  geom_sf(aes(fill = abundance), lwd = 0) +
  geom_sf(data = pa_shape_moll, alpha = 0, lwd = 1, color = "black") +
  facet_wrap(~comName) +
  scale_fill_viridis_c()

bird_grid <- bird_grid %>% 
  select(comName, month, centroid, abundance) %>% 
  st_drop_geometry() %>% 
  mutate(x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2))

test_similarity %>%
  count(x, y) %>% 
  ggplot() +
  geom_sf(data = pa_shape_moll) +
  geom_point(aes(x, y)) 

similarity_index <- bird_grid %>% 
  mutate(geo_id = str_c(x, y, sep = "_"),
         species_id = str_c(comName, month, sep = "_")) %>% 
  select(geo_id, species_id, abundance) %>% 
  pairwise_dist(geo_id, species_id, abundance, diag = F, upper = F) %>% 
  rename(geo_id_1 = item1,
         geo_id_2 = item2)

similarity_index %>% 
  ggplot(aes(distance)) +
  geom_histogram()

similarity_index %>% 
  write_csv("data/big/similarity_index.csv")

target_coords <- similarity_index %>% 
  select(geo_id_1) %>% 
  slice_head(n = 1)

target_coords

similarity_geo <- similarity_index %>% 
  filter(geo_id_1 == pull(target_coords)) %>% 
  separate(geo_id_2, into = c("x", "y"), sep = "_")

similarity_geo

target_coords <- target_coords %>% 
  separate(geo_id_1, into = c("x", "y"), sep = "_") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide)

similarity_geo %>% 
  mutate(across(.cols = c(x, y), as.numeric)) %>% 
  ggplot() +
  geom_tile(aes(x, y, fill = distance)) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  geom_sf(data = target_coords) +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Similarity")
 
  
