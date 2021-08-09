library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)

options(tigris_use_cache = TRUE)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

grid_shape <- st_read("data/big/grid_shapefile/grid_shapefile.shp") %>% 
  st_transform(crs = mollweide)

#consider function i can map against a csv that filters it with st_filter

filter_abundance_csv <- function(file, crs, target_grid_id){
  
  filtered_file <- file %>% 
    vroom(., delim = ",") %>% 
    st_as_sf(coords = c("x", "y"),
             crs = mollweide) %>% 
    st_filter(grid_shape %>% 
                filter(grid_id == target_grid_id),
              join = st_intersects) %>% 
    st_drop_geometry()
  
  return(filtered_file)
}

abunds_table_filtered <- 
  list.files("data/big/species_abundance", full.names = T) %>% 
  keep(str_detect(., "Warbler")) %>% 
  .[1] %>% 
  set_names() %>% 
  map_dfr(filter_abundance_csv, 
          target_grid_id = 23, 
          .id = "comName") %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext)

abunds_table_filtered %>% 
  distinct(comName)

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

# abunds_table_filtered <- abunds_table %>% 
#   st_filter(grid_shape %>% 
#               slice_head(n = 72),
#             join = st_intersects) %>% 
#   st_drop_geometry()

mean_abunds_month <- abunds_table_filtered %>% 
  mutate(month = month(date, label = T)) %>% 
  group_by(comName, month) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T)) %>% 
  ungroup()

polar_frequency_plot <- mean_abunds_month %>% 
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

polar_frequency_plot

abunds_mean_max <- abunds_table_filtered %>%
  group_by(comName) %>% 
  summarize(mean_abundance = mean(abundance, na.rm = T),
            peak_abundance = max(abundance, na.rm = T)) %>% 
  ungroup()

abunds_mean_max %>% 
  ggplot(aes(mean_abundance, peak_abundance, label = comName, fill = comName)) +
  geom_label() +
  guides(fill = "none")
