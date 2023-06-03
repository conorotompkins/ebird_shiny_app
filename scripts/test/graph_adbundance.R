library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)

options(tigris_use_cache = TRUE)

abunds_table <- list.files("data/big/species_abundance", full.names = T, recursive = TRUE) %>% 
  keep(str_detect(., "New World Warblers")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'path') %>% 
  mutate(path = basename(path) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  rename(abundance = value) %>% 
  filter(path != "Hermit Warbler")

abunds_table %>% 
  distinct(path)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_str <- abunds_table %>% 
  distinct(region) %>%
  pull()

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_str, NAME)) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_shape %>% 
  ggplot() +
  geom_sf()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

#find month with highest mean abundance per species, map that
peak_abundance_map_data <- abunds_table %>% 
  group_by(common_name, month, x, y) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

peak_abundance_map_data |> 
  count(common_name, month, x, y) |> 
  filter(n > 1)

peak_month <- peak_abundance_map_data |> 
  group_by(common_name, month) |> 
  summarize(abundance = mean(abundance)) |> 
  ungroup() |> 
  group_by(common_name) |> 
  filter(abundance == max(abundance)) |> 
  select(common_name, month)

peak_abundance_map_data <- peak_abundance_map_data %>% 
  semi_join(peak_month) |> 
  mutate(common_name = fct_reorder(common_name, abundance, mean, .desc = T)) |> 
  group_by(common_name) |> 
  mutate(abundance_rel = abundance / max(abundance))

peak_abundance_map_data |> 
  count(common_name)

peak_abundance_map_plot <- peak_abundance_map_data %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = abundance_rel)) +
  geom_sf(data = region_shape_moll, alpha = 0, color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(common_name~str_c("Peaks in: ", month)) +
  labs(fill = "Abundance") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"))

peak_abundance_map_plot

peak_abundance_map_plot %>% 
  ggsave(filename = "output/peak_abundance_map_plot.png", 
         width = 15, 
         height = 10)

abundance_histogram <- abunds_table %>% 
  ggplot(aes(abundance, fill = common_name)) +
  geom_histogram(alpha = .8) +
  facet_wrap(~common_name) +
  guides(fill = "none") +
  theme_ipsum() +
  theme(plot.background = element_rect(fill = "white"))

abundance_histogram

abundance_histogram %>% 
  ggsave(filename = "output/abundance_histogram.png",
         width = 15,
         height = 15)

top_areas <- abunds_table %>% 
  group_by(common_name, x, y) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(common_name, desc(mean_abundance)) %>% 
  group_by(common_name) %>% 
  slice_head(n = 300) %>% 
  mutate(id = row_number() %>% as.character) %>% 
  ungroup()

location_tile_heatmap_plot <- abunds_table %>% 
  inner_join(top_areas) %>% 
  mutate(id = fct_reorder(id, mean_abundance)) %>% 
  ggplot(aes(x = date, y = id, fill = abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~common_name) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

location_tile_heatmap_plot

location_tile_heatmap_plot %>% 
  ggsave(filename = "output/location_tile_heatmap_plot.png",
         width = 15,
         height = 15)

mean_abunds_table <- abunds_table %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(common_name, month) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

polar_frequency_plot <- mean_abunds_table %>% 
  mutate(common_name = fct_reorder(common_name, mean_abundance, .fun = mean, .desc = T)) %>% 
  ggplot(aes(x = month, y = mean_abundance, fill = common_name, color = common_name, group = common_name)) +
  geom_polygon(alpha = .5) +
  coord_polar(theta = "x") +
  # guides(fill = "none",
  #        color = "none") +
  labs(title = "Species Abundance",
       subtitle = "2018 Pennsylvania",
       x = NULL,
       y = "Mean Abundance",
       fill = "Species",
       color = "Species") +
  theme_bw()

polar_frequency_plot

polar_frequency_plot %>% 
  ggsave(filename = "output/polar_frequency_plot.png",
         width = 10,
         height = 10)