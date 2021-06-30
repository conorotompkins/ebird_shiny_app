library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)

options(tigris_use_cache = TRUE)

abunds_table <- vroom("data/big/ebirdst_test_save.csv")


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

abunds_table %>% 
  distinct(species)

#find month with highest mean abundance per species, map that
abunds_table %>% 
  group_by(species, month, x, y) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(species, month) %>%
  mutate(mean_abundance = mean(abundance, na.rm = T)) %>%
  ungroup() %>%
  group_by(species) %>% 
  filter(mean_abundance == max(mean_abundance, na.rm = T)) %>% 
  ungroup() %>% 
  #st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
  ggplot() +
  #geom_sf(aes(color = abundance)) +
  geom_raster(aes(x, y, fill = abundance)) +
  geom_sf(data = pa_shape_moll, alpha = 0, color = "black") +
  scale_fill_viridis_c() +
  facet_wrap(species~month, ncol = 1) +
  labs(fill = "Abundance") +
  theme_void()

abunds_table %>% 
  ggplot(aes(abundance, fill = species)) +
  geom_histogram(alpha = .5) +
  theme_ipsum()



top_areas <- abunds_table %>% 
  group_by(species, x, y) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(species, desc(mean_abundance)) %>% 
  group_by(species) %>% 
  slice_head(n = 300) %>% 
  mutate(id = row_number() %>% as.character)

abunds_table %>% 
  inner_join(top_areas) %>% 
  mutate(id = fct_reorder(id, mean_abundance)) %>% 
  ggplot(aes(x = date, y = id, fill = abundance)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(~species)


mean_abunds_table <- abunds_table %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(species, month) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

mean_abunds_table %>% 
  ggplot(aes(x = month, y = mean_abundance, fill = species, color = species, group = species)) +
  geom_polygon(alpha = .5) +
  #facet_wrap(~species) +
  #scale_y_continuous(limits = c(0, max(mean_abunds_table$mean_abundance))) +
  coord_polar(theta = "x") +
  labs(title = "Species Abundance",
       subtitle = "2018 Pennsylvania",
       x = NULL,
       y = "Mean Abundance",
       fill = "Species",
       color = "Species") +
  theme_bw()



