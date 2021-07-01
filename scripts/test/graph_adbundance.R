library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)

options(tigris_use_cache = TRUE)

abunds_table <- 
  list.files("data/big/species_abundance", full.names = T) %>% 
  #keep(str_detect(., "American Redstart")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext)

abunds_table %>% 
  distinct(comName)

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

#find month with highest mean abundance per species, map that
peak_abundance_map_data <- abunds_table %>% 
  group_by(comName, month, x, y) %>% 
  summarize(mean_abundance_loc = mean(abundance, na.rm = T)) %>% 
  ungroup()

peak_abundance_map_data <- peak_abundance_map_data %>% 
  group_by(comName, month) %>%
  mutate(mean_abundance = mean(mean_abundance_loc, na.rm = T)) %>%
  ungroup() %>%
  group_by(comName) %>% 
  filter(mean_abundance == max(mean_abundance, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(comName = fct_reorder(comName, mean_abundance, .desc = T))

peak_abundance_map_plot <- peak_abundance_map_data %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = mean_abundance_loc)) +
  geom_sf(data = pa_shape_moll, alpha = 0, color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(comName~str_c("Peaks in: ", month)) +
  labs(fill = "Abundance") +
  theme_void()

peak_abundance_map_plot

abundance_histogram <- abunds_table %>% 
  ggplot(aes(abundance, fill = comName)) +
  geom_histogram(alpha = .8, binwidth = 1) +
  facet_wrap(~comName) +
  guides(fill = "none") +
  theme_ipsum()

top_areas <- abunds_table %>% 
  group_by(comName, x, y) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(comName, desc(mean_abundance)) %>% 
  group_by(comName) %>% 
  slice_head(n = 300) %>% 
  mutate(id = row_number() %>% as.character) %>% 
  ungroup()

location_tile_heatmap_plot <- abunds_table %>% 
  inner_join(top_areas) %>% 
  mutate(id = fct_reorder(id, mean_abundance)) %>% 
  ggplot(aes(x = date, y = id, fill = abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~comName) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

mean_abunds_table <- abunds_table %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(comName, month) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

polar_frequency_plot <- mean_abunds_table %>% 
  ggplot(aes(x = month, y = mean_abundance, fill = comName, color = comName, group = comName)) +
  geom_polygon(alpha = .5) +
  coord_polar(theta = "x") +
  guides(fill = "none",
         color = "none") +
  labs(title = "Species Abundance",
       subtitle = "2018 Pennsylvania",
       x = NULL,
       y = "Mean Abundance",
       fill = "Species",
       color = "Species") +
  theme_bw()



