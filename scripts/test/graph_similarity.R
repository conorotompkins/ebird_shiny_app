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
  rename(geo_id_reference = geo_id_1,
         geo_id_compare = geo_id_2)

#check that each geo_id occurs 99 times
similarity_index %>% 
  count(geo_id_reference) %>% 
  distinct(n)

similarity_index %>% 
  count(geo_id_compare) %>% 
  distinct(n)

similarity_grid <- prep_similarity_grid(similarity_index, 72) %>% 
  mutate(highlight_grid = grid_id_reference == grid_id_compare,
         highlight_grid = as.factor(highlight_grid))

similarity_grid %>% 
  ggplot() +
  geom_sf(aes(fill = distance, size = highlight_grid), color = "black") +
  scale_size_manual(values = c(0, .75)) +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")

similarity_grid %>% 
  st_transform(crs = "EPSG:4326") %>% 
  mapdeck() %>% 
  add_sf(fill_colour = "distance",
         fill_opacity = .8,
         stroke_colour = "highlight_grid",
         legend = T,
         auto_highlight = T,
         tooltip = "grid_id_compare")

pal <- colorNumeric(
  palette = "viridis",
  domain = similarity_grid$distance)

similarity_grid %>% 
  mutate(grid_opacity = case_when(highlight_grid == "TRUE" ~ 1,
                                  highlight_grid == "FALSE" ~ .8)) %>% 
  st_transform(crs = "EPSG:4326") %>% 
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE
                   )) %>%
  addPolygons(layerId = ~geometry,
              fillColor = ~pal(distance),
              fillOpacity = ~grid_opacity,
              stroke = F,
              weight = 1) %>%
  addLegend("bottomright", pal = pal, values = ~distance,
            title = "Distance",
            opacity = 1
  )
