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

similarity_grid_distance <- prep_similarity_index(similarity_index, 72)

reference_coords <- get_reference_coords(similarity_index, 72)

similarity_grid_distance %>% 
  #st_join(similarity_geo, join = st_intersects) %>% 
  ggplot() +
  geom_sf(aes(fill = distance), lwd = 0) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  geom_sf_label(aes(label = grid_id)) +
  geom_sf(data = reference_coords) +
  scale_fill_viridis_c(direction = 1) +
  labs(fill = "Distance")

pal <- colorNumeric(
  palette = "viridis",
  domain = similarity_grid_distance$distance)

similarity_grid_distance %>% 
  st_transform(crs = "EPSG:4326") %>% 
  mapdeck() %>% 
  add_sf(fill_colour = "distance",
         fill_opacity = .8,
         legend = T,
         auto_highlight = T,
         tooltip = "grid_id_compare") %>% 
  add_sf(data = st_transform(reference_coords, crs = "EPSG:4326"))

similarity_grid_distance %>% 
  st_transform(crs = "EPSG:4326") %>% 
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE,
                                                 #minZoom = 9,
                                                 #maxZoom = 8
                   )) %>%
  # setView(lng = -80.01181092430839, lat = 40.44170119122286, zoom = 10) %>%
  # setMaxBounds(lng1 = -79.5, lng2 = -80.5, lat1 = 40.1, lat2 = 40.7) %>%
  addPolygons(layerId = ~geometry,
              fillColor = ~pal(distance),
              fillOpacity = .7,
              stroke = F,
              #color = "#FCCF02",
              weight = 1) %>%
  addLegend("bottomright", pal = pal, values = ~distance,
            title = "Distance",
            opacity = 1
  )


