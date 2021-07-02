library(tidyverse)
library(vroom)
library(tools)
library(tigris)
library(sf)
library(leaflet)
library(mapdeck)
library(mapview)

options(tigris_use_cache = TRUE)

abunds_table <- 
  list.files("data/big/species_abundance", full.names = T) %>% 
  keep(str_detect(., "American Redstart")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext)

redstart_peak <- abunds_table %>% 
  group_by(comName, month) %>% 
  mutate(abundance_month = mean(abundance, na.rm = T)) %>% 
  ungroup() %>% 
  filter(abundance_month == max(abundance_month))

redstart_peak <- redstart_peak %>% 
  group_by(comName, month, x, y) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_shape %>% 
  ggplot() +
  geom_sf()

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

pa_shape_moll %>% 
  ggplot() +
  geom_sf()

redstart_peak %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = abundance)) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  scale_fill_viridis_c()

redstart_peak <- redstart_peak %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
  st_transform(crs = "EPSG:4326")

st_crs(redstart_peak)

redstart_peak %>% 
  ggplot(aes(color = abundance)) +
  geom_sf() +
  scale_color_viridis_c()

mapdeck(style = mapdeck_style('dark'), pitch = 0) %>% 
  add_screengrid(
    data = redstart_peak
    , lat = "y"
    , lon = "x"
    , weight = "abundance",
    , cell_size = 10
    , opacity = 0.8
    , colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )

mapdeck(style = mapdeck_style('dark'), pitch = 0) %>% 
  add_grid(
    data = redstart_peak,
    lat = "y",
    lon = "x",
    colour = "abundance",
    #colour_function = "sum",
    cell_size = 1000,
    #opacity = 0.8,
    colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )


grid_geo <- redstart_peak %>% 
  st_make_grid(cellsize = c(.4, .4)) %>% 
  st_as_sf()

redstart_grid <- grid_geo %>% 
  st_join(redstart_peak, join = st_intersects) %>% 
  mutate(centroid = map(geometry, st_point_on_surface)) %>% 
  group_by(centroid) %>% 
  summarize(n = n(),
            abundance = mean(abundance, na.rm = T))

redstart_grid %>%           
  ggplot() +
  geom_sf(aes(fill = abundance), lwd = 0) +
  geom_sf(data = pa_shape_moll, alpha = 0) +
  scale_fill_viridis_c()

mapdeck() %>% 
  add_sf(redstart_grid,
         fill_colour = "abundance",
         fill_opacity = .8,
         auto_highlight = T,
         tooltip = "comName")

mapview(redstart_grid,
        zcol = "abundance",
        alpha.regions = 0.8)

pal <- colorNumeric(
  palette = "viridis",
  domain = redstart_grid$abundance)

leaflet(redstart_grid) %>% 
  addTiles() %>% 
  addPolygons(color = ~pal(abundance), opacity = 0, fillOpacity = .5) %>% 
  addLegend("bottomright", pal = pal, values = ~abundance,
            title = "Abundance",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 1
  )

