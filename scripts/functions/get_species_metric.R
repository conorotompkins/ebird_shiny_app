library(tidyverse)
library(janitor)
library(ebirdst)
library(sf)
library(raster)
library(fasterize)
library(rnaturalearth)
library(lubridate)
library(tigris)
library(glue)
library(tictoc)

filter <- dplyr::filter
select <- dplyr::select

#https://cornelllabofornithology.github.io/ebirdst/

options(timeout = max(300, getOption("timeout")),
        tigris_use_cache = TRUE)

get_species_metric <- function(region_input, target_family_common_name, target_species_var, metric, target_resolution){

  # region_input <- "Pennsylvania, New Jersey, New York, Maryland, Ohio, West Virginia, Delaware"
  # 
  # target_family_common_name <- "New World Warblers"
  # 
  # target_species_var <- "Cape May Warbler"
  # 
  # metric <- "abundance"
  # 
  # target_resolution <- "lr"
  
  region_input_list <- str_split(region_input, ", ", simplify = F) %>% 
    unlist()
  
  target_species <- ebirdst_runs %>% 
    filter(common_name == target_species_var) %>% 
    distinct(species_code) %>% 
    pull(species_code)
  
  length(target_species) %>% 
    str_c("Length of target_species:", ., sep = " ") %>% 
    message()
  
  glue("Pulling", metric, "data for", target_species, "AKA", target_species_var, 
       "at resolution", target_resolution, "in", region_input, .sep = " ") %>% 
    message()
  
  tic()
  sp_path <- ebirdst_download(species = target_species, tifs_only = T, force = F)
  toc()
  
  glue("Downloaded files to", sp_path, .sep = " ") %>% 
    message()
  
  message("Loading raster")
  species_metric <- load_raster(metric, path = sp_path, resolution = target_resolution)
  
  original_raster_crs <- raster::crs(species_metric) %>% 
    as.character()
  
  str_c("Getting polygons for", region_input, "from Tigris", sep = " ") %>% 
    message()
  
  mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
  
  original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
  
  buffer <- 10^4 * 4
  
  region_shape <- states(cb = T) %>% 
    filter(NAME %in% region_input_list) %>% 
    st_transform(crs = original_raster_crs) %>% 
    summarize()
  
  region_shape_buffered <- st_buffer(region_shape, buffer)
  
  region_shape_spdf <- as(region_shape, 'Spatial')
  
  region_shape_spdf_buffered <- as(region_shape_buffered, 'Spatial')
  
  region_shape_buffered_moll <- region_shape %>% 
    st_buffer(buffer) %>% 
    st_transform(mollweide)
  
  region_bbox <- region_shape %>% 
    sf::st_bbox(crs = original_raster_crs)
  
  region_shape_moll <- region_shape %>% 
    st_transform(mollweide)
  
  # region_shape_moll %>%
  #   ggplot() +
  #   geom_sf(alpha = 0) +
  #   geom_sf(data = region_shape_buffered_moll, color = "red", alpha = 0)
  
  message("Cropping raster")
  
  tic()
  #plot(species_metric$w2019.10.05)
  
  species_metric_cropped <- species_metric %>% 
    crop(region_shape_spdf_buffered)
  
  #plot(species_metric_cropped$w2019.10.05)
  
  species_metric_cropped <- species_metric_cropped %>% 
    mask(region_shape_spdf_buffered)
  
  #plot(species_metric_cropped$w2019.10.05)
  
  species_metric_cropped <- species_metric_cropped %>% 
    projectRaster(crs = mollweide, method = "ngb")
  
  #plot(species_metric_cropped$w2019.10.05)
  toc()
  
  message("Transforming raster to tibble")
  tic()
  species_metric_table <- species_metric_cropped %>% 
    as.data.frame(xy = T) %>% 
    as_tibble() %>% 
    mutate(common_name = target_species_var,
           metric_desc = metric) %>% 
    pivot_longer(-c(common_name, x, y, metric_desc), names_to = "date", values_to = "value") %>% 
    mutate(date = str_remove(date, "^w"),
           date = str_replace_all(date, "\\.", "-"),
           date = ymd(date),
           month = month(date, label = T),
           region = region_input) %>% 
    st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
    st_filter(region_shape_buffered_moll, join = st_intersects) %>% 
    mutate(x = map_dbl(geometry, 1),
           y = map_dbl(geometry, 2)) %>% 
    st_drop_geometry() %>% 
    mutate(family_common_name = target_family_common_name) %>% 
    select(family_common_name, everything())
  toc()
  
  species_metric_table
  
  # species_metric_table %>%
  #   filter(date == "2019-10-05") %>%
  #   distinct(x, y) %>%
  #   ggplot() +
  #   geom_point(aes(x, y)) +
  #   geom_sf(data = region_shape_moll, alpha = 0)
}

get_species_metric <- possibly(get_species_metric, otherwise = NA)

#references
# https://www.jamiecmontgomery.com/post/cropping-rasters-down-to-size/
