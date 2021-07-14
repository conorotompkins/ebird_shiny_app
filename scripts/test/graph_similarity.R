library(tidyverse)
library(sf)
library(tigris)

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

#load similarity index data
similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
  rename(geo_id_reference = geo_id_1,
         geo_id_compare = geo_id_2)

#check that each geo_id occurs 99 times
similarity_index %>% 
  count(geo_id_reference) %>% 
  distinct(n)

similarity_index %>% 
  count(geo_id_compare) %>% 
  distinct(n)

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

geo_id_index %>% 
  ggplot(aes(x, y, label = grid_id)) +
  geom_label()

geo_id_index <- geo_id_index %>% 
  select(-c(x, y))

#choose one grid_id to test
reference_coords <- geo_id_index %>% 
  filter(grid_id == 72) %>% 
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

similarity_grid %>% 
  st_join(similarity_geo, join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = distance), lwd = 0) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  geom_sf(data = reference_coords) +
  geom_sf_label(aes(label = grid_id)) +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")
