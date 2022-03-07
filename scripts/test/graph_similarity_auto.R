library(tidyverse)
library(vroom)
library(sf)
library(tigris)
library(leaflet)
library(mapdeck)
library(ggspatial)

set.seed(1234)

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- read_csv("data/big/target_region.csv")

region_shape <- states(cb = T) %>% 
  semi_join(region_input, by = c("NAME" = "state_name")) %>% 
  st_transform(crs = original_raster_crs)

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

#load similarity index data
similarity_index <- vroom("data/big/similarity_index.csv")

similarity_geo <- similarity_index %>% 
  prep_similarity_index(26) %>% 
  mutate(month = fct_relevel(month, month.abb))

similarity_geo %>% 
  distinct(x, y, geo_index_compare) %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text()

bins <- seq(from = -1, to = 1, by = .2)
bins
length(bins)

bin_labels <- c("-1 to -.8", ".8 to -.6", "6 to -.4", "-.4 to -.2", "-.2 to 0", 
                "0 to .2",".2 to .4", ".4 to .6", ".6 to .8", ".8 to 1")
length(bin_labels)

similarity_geo_binned <- similarity_geo %>% 
  filter(!is.na(correlation)) %>% 
  mutate(correlation = round(correlation, 2),
         correlation_bin = cut(correlation, breaks = bins, labels = bin_labels),
         correlation_bin = as.factor(correlation_bin))

#bin_labels <- levels(similarity_geo_binned$correlation_bin)
similarity_geo_binned %>% 
  filter(is.na(correlation)) %>% 
  distinct(geo_id_compare, geometry) %>% 
  ggplot() +
  geom_sf()


similarity_geo_binned %>% 
  filter(is.na(correlation_bin)) %>% 
  select(correlation, correlation_bin) %>% 
  view()

similarity_geo_binned %>% 
  st_drop_geometry() %>% 
  count(correlation, correlation_bin, .drop = F) %>% 
  ggplot(aes(correlation_bin, correlation, fill = n)) +
  geom_tile()

similarity_geo_binned %>% 
  filter(month == "Oct") %>% 
  st_sf() %>% 
  #View()
  ggplot() +
  annotation_map_tile(type = "stamenbw") + 
  geom_sf(aes(fill = correlation), color = NA, alpha = .75) +
  geom_sf(data = region_shape_moll, alpha = 0) +
  geom_point(data = filter(similarity_geo_binned, highlight_grid == T),
             aes(x, y), 
             color = "white") +
  scale_fill_viridis_c(direction = -1) +
  #scale_color_viridis_d(direction = -1) +
  guides(color = "none") +
  labs(fill = "Correlation")

plot <- similarity_geo_binned %>% 
  complete(month, correlation_bin) %>% 
  mutate(correlation_bin = factor(correlation_bin, levels = bin_labels)) %>% 
  count(month, correlation_bin) %>% 
  filter(month == "Jun") %>% 
  ggplot(aes(correlation_bin, n, fill = correlation_bin)) +
  geom_col() +
  facet_wrap(~month, ncol = 2) +
  scale_fill_viridis_d()

str(plot)

similarity_geo_binned %>% 
  st_drop_geometry() %>% 
  mutate(correlation_bin = factor(correlation_bin, levels = bin_labels)) %>% 
  count(month, correlation_bin, .drop = F) %>% 
  ggplot(aes(correlation_bin, n, fill = correlation_bin)) +
  geom_col() +
  scale_fill_viridis_d() +
  guides(fill = "none") +
  labs(title = "Correlation",
       #x = x_label,
       y = "Count of Areas") +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "both")),
        axis.title.x = element_text(angle = 0, size = 15),
        axis.title.y = element_text(size = 15))

similarity_geo_binned %>% 
  ggplot(aes(correlation, fill = correlation_bin)) +
  geom_histogram(bins = 100) +
  geom_rug() +
  facet_wrap(~month, ncol = 2) +
  scale_fill_viridis_d()



pal_c <- colorNumeric(
  palette = "viridis",
  domain = similarity_geo_binned$correlation)

pal_d <- colorFactor(
  palette = viridis::viridis(n = 11),
  domain = similarity_geo_binned$correlation_bin,
  levels = bin_labels)

base_map_data <- similarity_geo_binned %>% 
  distinct(geo_index_compare, geometry)

similarity_geo_binned %>% 
  filter(month == "May") %>% 
  complete(correlation_bin = bin_labels) %>% 
  mutate(correlation_bin = factor(correlation_bin, levels = bin_labels)) %>% 
  st_as_sf() %>% 
  mutate(grid_opacity = case_when(highlight_grid == T ~ .1,
                                  highlight_grid == F ~ .8)) %>%
  st_transform(crs = "EPSG:4326") %>% 
  leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite,
                   options = providerTileOptions(noWrap = TRUE)) %>%
  addPolygons(layerId = ~geometry,
              color = "#444444",
              fillColor = ~pal_d(correlation_bin),
              fillOpacity = ~grid_opacity,
              stroke = T,
              weight = 1,
              popup = ~paste0("geo_id: ", geo_index_compare, "\n", "Correlation: ", round(correlation, 2)),
              highlightOptions = highlightOptions(color = "white", bringToFront = T)) %>%
  addLegend("bottomright", pal = pal_d, values = ~correlation_bin,
            title = "Correlation with clicked tile",
            opacity = 1)

