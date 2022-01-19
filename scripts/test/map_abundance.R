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
  keep(str_detect(., "Cape May Warbler")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  mutate(x = round(x, 2),
         y = round(y, 2)) %>% 
  rename(abundance = value)

abunds_table %>% 
  count(date) %>% 
  distinct(n)

#not all months have the same number of weeks. this is fine i think
abunds_table %>% 
  count(month) %>% 
  distinct(n)

abunds_table %>% 
  group_by(month) %>% 
  filter(n() == 1490) %>% 
  ungroup() %>% 
  distinct(month)

abunds_table %>% 
  count(x, y) %>% 
  distinct(n)

abunds_table %>% 
  count(month, x, y) %>% 
  distinct(n)

abunds_table %>% 
  count(month, x, y) %>% 
  filter(n == 5) %>% 
  distinct(month)

peak_months <- abunds_table %>% 
  group_by(comName, month) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>% 
  arrange(desc(abundance))

species_peak <- abunds_table %>% 
  group_by(comName, x, y, month) %>% 
  summarize(abundance = mean(coalesce(abundance, 0), na.rm = T)) %>% 
  semi_join(peak_months, by = "month")

species_peak %>% 
  count(month)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_input, NAME)) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

buffer <- 10^4 * 4

region_shape_moll %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = st_buffer(region_shape_moll, buffer),
          color = "red", alpha = 0)

species_peak %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = abundance)) +
  geom_sf(data = region_shape_moll, alpha = 0, color = "black") +
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

