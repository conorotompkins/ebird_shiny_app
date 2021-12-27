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

get_species_metric <- function(target_species_var, metric, target_resoluion){
  
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
  
  #target_species_var <- "Northern Cardinal"
  #metric <- "count"
  
  target_species <- ebirdst_runs %>% 
    filter(common_name == target_species_var) %>% 
    pull(species_code)
  
  glue("Pulling", metric, "data for", target_species, "AKA", target_species_var, "at resolution", target_resoluion, .sep = " ") %>% 
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
  
  message("Getting state polygon from Tigris")
  pa_shape <- states(cb = T) %>% 
    filter(NAME == "Pennsylvania") %>% 
    st_transform(crs = original_raster_crs)
  
  pa_bbox <- pa_shape %>% 
    sf::st_bbox(crs = original_raster_crs)
  
  message("Cropping raster")
  mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"
  
  tic()
  species_metric_cropped <- species_metric %>% 
    #consider cropping by shape instead of bbox
    #crop(pa_shape) %>% 
    crop(pa_bbox) %>% 
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
           month = month(date, label = T))
  toc()
  
  species_metric_table
}

