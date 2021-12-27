library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)
library(widyr)

set.seed(1234)

options(tigris_use_cache = TRUE)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

pa_shape <- states(cb = T) %>% 
  filter(NAME == "Pennsylvania") %>% 
  st_transform(crs = original_raster_crs)

pa_bbox <- pa_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

pa_shape_moll <- pa_shape %>% 
  st_transform(mollweide)

pa_shape_moll %>% 
  ggplot() +
  geom_sf()

abunds_table <- list.files("data/big/species_abundance", full.names = T) %>% 
  #keep(str_detect(., "Dark-eyed Junco")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName') %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  rename(abundance = value) %>% 
  mutate(abundance = coalesce(abundance, 0))

abundance_summary <- abunds_table %>%
  group_by(comName, month, x, y) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>%
  ungroup() %>% 
  mutate(species_id = str_c(comName, sep = "_"),
         geo_id = str_c(x, y, sep = "_")) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
  st_filter(pa_shape_moll, join = st_intersects)

abundance_summary %>% 
  st_drop_geometry() %>% 
  count(comName) %>% 
  distinct(n)

abundance_summary %>% 
  filter(is.na(comName))

abundance_summary %>% 
  filter(is.na(month))

abundance_summary %>%
  filter(comName == "American Tree Sparrow",
         month == "Jan") %>% 
  ggplot() +
  geom_sf(data = pa_shape_moll, color = "red") +
  geom_sf(aes(color = abundance)) +
  scale_color_viridis_c()

rm(abunds_table)

pairwise_dist_f <- function(x, geo_id, comName, abundance){
  
  pairwise_dist(tbl = x, item = geo_id, feature = comName, value = abundance,
                diag = T, upper = T)
  
}

similarity_index <- abundance_summary %>% 
  filter(month == "Apr") %>% 
  select(geo_id, comName, abundance) %>% 
  #for future development
  #group_nest(month) %>% 
  # mutate(dist_data = pmap(list(geo_id, comName, abundance),
  #                         ~pairwise_dist(data, ..1, ..2, ..3, diag = T, upper = T)))
  pairwise_dist(geo_id, comName, abundance, diag = T, upper = T) %>% 
  rename(geo_id_1 = item1,
         geo_id_2 = item2) %>% 
  mutate(month = "Apr") %>% 
  select(month, everything())

similarity_index %>% 
  count(geo_id_1) %>% 
  distinct(n)

similarity_index %>% 
  ggplot(aes(distance)) +
  geom_histogram()

similarity_index %>% 
  write_csv("data/big/similarity_index.csv")
