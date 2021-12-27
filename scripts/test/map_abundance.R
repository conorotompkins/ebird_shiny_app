library(tidyverse)
library(vroom)
library(tools)
library(lubridate)
library(tigris)
library(sf)
library(leaflet)
library(mapdeck)
library(mapview)

options(tigris_use_cache = TRUE)

abunds_table <- list.files("data/big/species_abundance", full.names = T) %>% 
  keep(str_detect(., "Dark-eyed Junco")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  rename(abundance = value)

abunds_table %>% 
  count(month) %>% 
  distinct(n)

abunds_table %>% 
  count(x, y) %>% 
  distinct(n)

abunds_table %>% 
  count(month, x, y) %>% 
  filter(n == 5) %>% 
  distinct(month)

peak_month <- abunds_table %>% 
  group_by(comName, month) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>% 
  arrange(desc(abundance)) %>% 
  slice(1) %>% 
  pull(month)

species_peak <- abunds_table %>% 
  filter(month == peak_month) %>% 
  group_by(comName, x, y, month) %>% 
  summarize(abundance = mean(abundance, na.rm = T))

species_peak %>% 
  count(month)

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

species_peak %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = abundance)) +
  geom_sf(data = pa_shape_moll, alpha = 0, color = "black") +
  facet_wrap(~month) +
  scale_fill_viridis_c()

species_peak <- species_peak %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
  st_transform(crs = "EPSG:4326")

st_crs(species_peak)

species_peak %>% 
  ggplot(aes(color = abundance)) +
  geom_sf() +
  scale_color_viridis_c()

mapdeck(style = mapdeck_style('dark'), pitch = 0) %>% 
  add_screengrid(
    data = species_peak
    , lat = "y"
    , lon = "x"
    , weight = "abundance",
    , cell_size = 10
    , opacity = 0.8
    , colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )

mapdeck(style = mapdeck_style('dark'), pitch = 0) %>% 
  add_grid(
    data = species_peak,
    lat = "y",
    lon = "x",
    colour = "abundance",
    #colour_function = "sum",
    cell_size = 10^4,
    #opacity = 0.8,
    colour_range = colourvalues::colour_values(1:6, palette = "plasma")
  )

species_peak %>% 
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2)) %>% 
  mutate(abundance = coalesce(abundance, 0)) %>% 
  ggplot() +
  #geom_point(aes(x, y)) +
  geom_raster(aes(x, y, fill = abundance))

species_peak %>%           
  ggplot() +
  geom_sf(aes(fill = abundance), lwd = 0) +
  geom_sf(data = pa_shape_moll, alpha = 0, color = "black") +
  scale_fill_viridis_c()

mapdeck() %>% 
  add_sf(species_peak,
         fill_colour = "abundance",
         fill_opacity = .8,
         auto_highlight = T,
         tooltip = "comName")

mapview(species_peak,
        zcol = "abundance",
        alpha.regions = 0.8)

pal <- colorNumeric(
  palette = "viridis",
  domain = species_peak$abundance)

leaflet(species_peak) %>% 
  addTiles() %>% 
  addCircles(color = ~pal(abundance), radius = 10^4) %>% 
  #addPolygons(color = ~pal(abundance), opacity = 0, fillOpacity = .5) %>% 
  addLegend("bottomright", pal = pal, values = ~abundance,
            title = "Abundance",
            opacity = 1
  )

