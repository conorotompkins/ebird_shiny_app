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

#check that each combination of geo_id_1, geo_id_2 occurs once
similarity_index %>% 
  count(geo_id_1, geo_id_2) %>% 
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
  mutate(across(.cols = c(x_num, y_num), parse_number)) %>% 
  arrange(x_num, y_num) %>% 
  mutate(geo_index = row_number())

geo_id_index %>% 
  ggplot() +
  geom_point(aes(x_num, y_num), alpha = .3) +
  geom_text(aes(x_num, y_num, label = geo_index)) +
  geom_sf(data = region_shape_moll, alpha = 0)

#drop x and y columns
geo_id_index <- geo_id_index %>% 
  select(-c(x_num, y_num, x, y))

#choose one geo_index to test
select_geo_index <- 45

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
# reference_coords <- reference_coords %>% 
#   separate(geo_id, into = c("x", "y"), sep = "_") %>% 
#   mutate(across(everything(), as.numeric)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = mollweide)

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
  select(month, geo_id_reference, geo_index_reference, geometry, geo_index_compare, correlation)

similarity_geo <- similarity_geo %>% 
  #calculate x,y from coordinate
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2))

similarity_geo %>% 
  mutate(geometry = st_buffer(geometry, 
                              dist = 26700/2,
                              endCapStyle = "SQUARE")) %>% 
  plot()

similarity_geo <- similarity_geo %>% 
  mutate(highlight_grid = geo_index_compare == select_geo_index)

similarity_geo %>% 
  filter(highlight_grid == T)

similarity_geo <- similarity_geo %>% 
  mutate(geometry = st_buffer(geometry, 
                              dist = 26700/2,
                              endCapStyle = "SQUARE"))

similarity_geo %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text() +
  facet_wrap(~month)

similarity_geo %>% 
  filter(month == "Oct") %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = correlation)) +
  scale_fill_viridis_c(direction = -1)

similarity_geo %>% 
  filter(month == "Oct") %>% 
  ggplot() +
  geom_sf(aes(fill = correlation)) +
  geom_sf(data = region_shape_moll, alpha = 0) +
  geom_point(data = filter(similarity_geo, highlight_grid == T),
             aes(x, y), 
             color = "white") +
  scale_fill_viridis_c(direction = -1) +
  labs(fill = "Distance")

#leaflet
similarity_geo %>% 
  filter(month == "Oct") %>% 
  st_transform(crs = "EPSG:4326") %>% 
  mapdeck() %>% 
  add_sf(fill_colour = "correlation",
         fill_opacity = .8,
         stroke_colour = "highlight_grid",
         legend = T,
         auto_highlight = T,
         tooltip = "grid_id_compare"
  )

pal <- colorNumeric(
  palette = "viridis",
  domain = similarity_geo$correlation)

similarity_geo %>% 
  filter(month == "Oct") %>% 
  mutate(grid_opacity = case_when(highlight_grid == T ~ .1,
                                  highlight_grid == F ~ .8)) %>%
  st_transform(crs = "EPSG:4326") %>% 
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(layerId = ~geometry,
              color = "#444444",
              fillColor = ~pal(correlation),
              fillOpacity = ~grid_opacity,
              stroke = T,
              weight = 1,
              label = ~paste0("geo_id: ", geo_index_compare, "\n", "Correlation: ", round(correlation,0)),
              highlightOptions = highlightOptions(color = "white", bringToFront = T)) %>%
  addLegend("bottomright", pal = pal, values = ~correlation,
            title = "Correlation",
            opacity = 1)

