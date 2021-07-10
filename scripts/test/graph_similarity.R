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

similarity_index <- read_csv("data/big/similarity_index.csv")

similarity_index %>% 
  count(geo_id_1) %>% 
  distinct(n)

similarity_index %>% 
  count(geo_id_2) %>% 
  distinct(n)

#create IDs for each geo_id
geo_id_index <- similarity_index %>% 
  select(geo_id_1, geo_id_2) %>% 
  pivot_longer(cols = everything(),
               names_to = "type", values_to = "geo_id") %>% 
  arrange(geo_id) %>% 
  distinct(geo_id) %>% 
  separate(geo_id, into = c("x", "y"), sep = "_", remove = F) %>% 
  mutate(across(.cols = c(x, y), as.numeric)) %>% 
  arrange(x, y) %>% 
  mutate(id = row_number())

geo_id_index %>% 
  ggplot(aes(x, y, label = id)) +
  geom_label()

geo_id_index <- geo_id_index %>% 
  select(-c(x, y))


#need to get ids of geo_id_1 and geo_id_2 to match up
# target_coords <- similarity_index %>% 
#   distinct(geo_id_1) %>% 
#   filter(row_number() == 100) %>% 
#   select(geo_id_1)

target_coords <- geo_id_index %>% 
  filter(id == 72) %>% 
  select(geo_id)

similarity_geo <- similarity_index %>% 
  left_join(geo_id_index, by = c("geo_id_2" = "geo_id")) %>% 
  filter(geo_id_1 == pull(target_coords)) %>% 
  separate(geo_id_2, into = c("x", "y"), sep = "_")

similarity_geo

target_coords <- target_coords %>% 
  separate(geo_id, into = c("x", "y"), sep = "_") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide)

similarity_geo <- similarity_geo %>% 
  mutate(across(.cols = c(x, y), as.numeric)) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide)# %>% 
  #left_join(target_coord_index)

length(similarity_geo$id)

nrow(similarity_geo)

similarity_grid <- similarity_geo %>% 
  st_make_grid(n = 10, crs = mollweide) %>% 
  st_as_sf() %>%
  st_join(geo_id_index %>% 
            separate(geo_id, into = c("x", "y"), sep = "_") %>% 
            mutate(across(.cols = everything(), as.numeric)) %>% 
            st_as_sf(coords = c("x", "y"), crs = mollweide),
          join = st_intersects)


similarity_grid %>% 
  st_join(select(similarity_geo, -id), join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = distance), lwd = 0) +
  geom_sf(data = target_coords) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  geom_sf(data = target_coords) +
  geom_sf_label(aes(label = id)) +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")