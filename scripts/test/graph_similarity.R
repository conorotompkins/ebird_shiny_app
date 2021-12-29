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

#check that each combination of geo_id_1, geo_id_2 occurs once
similarity_index %>% 
  count(geo_id_1, geo_id_2, sort = T) %>% 
  distinct(n)

#reference is the geo_id of interest
#compare are the geo_ids we are comparing to the reference_geo_id
similarity_index <- similarity_index %>% 
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

geo_id_index %>%
  ggplot(aes(x_num, y_num, label = geo_index)) +
  geom_label()

#drop x and y columns
geo_id_index <- geo_id_index %>% 
  select(-c(x_num, y_num, x, y))

#choose one geo_index to test
select_geo_index <- 26

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
  #join to get geo_index of reference coords
  left_join(geo_id_index, by = c("geo_id_reference" = "geo_id")) %>% 
  rename(geo_index_reference = geo_index) %>% 
  #reorder variables
  select(month, geo_id_reference, geo_index_reference, geometry, geo_index_compare, distance)

similarity_geo <- similarity_geo %>% 
  #calculate x,y from coordinate
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2))

similarity_geo <- similarity_geo %>% 
  mutate(highlight_grid = geo_index_compare == select_geo_index)

similarity_geo %>% 
  filter(highlight_grid == T)

similarity_geo %>% 
  count(geo_id_reference) %>% 
  distinct(n)

similarity_geo %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = distance)) +
  scale_fill_viridis_c()

similarity_geo %>% 
  st_drop_geometry() %>% 
  count(geo_id_reference) %>% 
  distinct(n)

similarity_geo %>% 
  st_drop_geometry() %>% 
  count(geo_index_compare) %>% 
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
