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

get_species_metric <- function(region_input, target_species_var, metric, target_resoluion){

  target_species <- ebirdst_runs %>% 
    filter(common_name == target_species_var) %>% 
    distinct(species_code) %>% 
    pull(species_code)
  
  length(target_species) %>% 
    str_c("Length of target_species:", ., sep = " ") %>% 
    message()
  
  glue("Pulling", metric, "data for", target_species, "AKA", target_species_var, 
       "at resolution", target_resoluion, "in", region_input, .sep = " ") %>% 
    message()
  
  tic()
  sp_path <- ebirdst_download(species = target_species, tifs_only = T, force = F)
  toc()
  glue("Downloaded files to", sp_path, .sep = " ") %>% 
    message()
  
  message("Loading raster")
  species_metric <- load_raster(metric, path = sp_path, resolution = target_resoluion)
  
  original_raster_crs <- raster::crs(species_metric) %>% 
    as.character()
  
  str_c("Getting polygons for", region_input, "from Tigris", sep = " ") %>% 
    message()
  
  mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
  
  original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"
  
  region_shape <- states(cb = T) %>% 
    filter(str_detect(region_input, NAME)) %>% 
    st_transform(crs = original_raster_crs) %>% 
    summarize()
  
  region_bbox <- region_shape %>% 
    sf::st_bbox(crs = original_raster_crs)
  
  region_shape_moll <- region_shape %>% 
    st_transform(mollweide)
  
  message("Cropping raster")
  
  tic()
  species_metric_cropped <- species_metric %>% 
    crop(region_shape) %>% 
    projectRaster(crs = mollweide, method = "ngb")
  toc()
  
  message("Transforming raster to tibble")
  tic()
  species_metric_table <- species_metric_cropped %>% 
    as.data.frame(xy = T) %>% 
    as_tibble() %>% 
    mutate(target_species = target_species_var,
           metric_desc = metric) %>% 
    pivot_longer(-c(target_species, x, y, metric_desc), names_to = "date", values_to = "value") %>% 
    mutate(date = str_remove(date, "^w"),
           date = str_replace_all(date, "\\.", "-"),
           date = ymd(date),
           month = month(date, label = T),
           region = region_input) %>% 
    st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
    st_filter(region_shape_moll, join = st_intersects) %>% 
    mutate(x = map_dbl(geometry, 1),
           y = map_dbl(geometry, 2)) %>% 
    st_drop_geometry()
  toc()
  
  species_metric_table
}

get_species_metric <- possibly(get_species_metric, otherwise = NA)
