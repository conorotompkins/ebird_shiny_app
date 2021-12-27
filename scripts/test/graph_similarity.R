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
similarity_index <- read_csv("data/big/similarity_index.csv")

select_grid_id <- 26

#check that each geo_id occurs 99 times
# similarity_grid <- prep_similarity_grid(similarity_index, 30) %>% 
#   mutate(highlight_grid = grid_id_reference == grid_id_compare,
#          highlight_grid = as.factor(highlight_grid))


similarity_grid <- similarity_index %>% 
  rename(geo_id_reference = geo_id_1,
         geo_id_compare = geo_id_2) 

#create IDs for each geo_id
geo_id_index <- similarity_grid %>% 
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
  filter(grid_id == select_grid_id) %>% 
  select(geo_id)

similarity_geo <- similarity_grid %>% 
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

similarity_geo %>% 
  #turn compare x,y into sf coordinates
  mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
  ggplot(aes(x_compare, y_compare)) +
  geom_point()

similarity_geo <- similarity_geo %>% 
  #turn compare x,y into sf coordinates
  mutate(across(.cols = c(x_compare, y_compare), as.numeric)) %>% 
  st_as_sf(coords = c("x_compare", "y_compare"), crs = mollweide) %>%
  #join to get grid_id of reference coords
  left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
  rename(grid_id_reference = grid_id) %>% 
  #reorder variables
  select(geo_id_reference, grid_id_reference, geometry, grid_id_compare, distance)

similarity_geo <- similarity_geo %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>% 
  mutate(x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2))

similarity_geo <- similarity_geo %>% 
  mutate(highlight_grid = grid_id_compare == select_grid_id)

similarity_geo %>% 
  filter(highlight_grid == T)

similarity_geo %>% 
  count(grid_id_reference) %>% 
  distinct(n)

similarity_geo %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = distance)) +
  scale_fill_viridis_c()

similarity_geo %>% 
  st_drop_geometry() %>% 
  count(grid_id_reference) %>% 
  distinct(n)

similarity_geo %>% 
  st_drop_geometry() %>% 
  count(grid_id_compare) %>% 
  distinct(n)

similarity_geo %>% 
  ggplot() +
  geom_tile(aes(x, y, fill =  distance)) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  geom_point(data = filter(similarity_geo, highlight_grid == T),
             aes(x, y), 
             color = "white") +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")

similarity_geo %>% 
  st_transform(crs = "EPSG:4326") %>% 
  mapdeck() %>% 
  add_sf(fill_colour = "distance",
         fill_opacity = .8,
         stroke_colour = "highlight_grid",
         legend = T,
         auto_highlight = T,
         tooltip = "grid_id_compare",
         radius = 10^4)

pal <- colorNumeric(
  palette = "viridis",
  domain = similarity_geo$distance)

similarity_geo %>% 
  mutate(grid_opacity = case_when(highlight_grid == "TRUE" ~ .9,
                                  highlight_grid == "FALSE" ~ .6)) %>%
  st_transform(crs = "EPSG:4326") %>% 
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addCircles(layerId = ~geometry,
             fillColor = ~pal(distance),
             fillOpacity = ~grid_opacity,
             stroke = F,
             weight = 1,
             radius = 10^4) %>%
  addLegend("bottomright", pal = pal, values = ~distance,
            title = "Distance",
            opacity = 1)