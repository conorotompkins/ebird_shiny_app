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
library(hrbrthemes)

#https://cornelllabofornithology.github.io/ebirdst/

options(timeout = max(300, getOption("timeout")),
        tigris_use_cache = TRUE)

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

get_abundance_table <- function(target_species_var){
  
  target_species <- ebirdst_runs %>% 
    filter(common_name == target_species_var) %>% 
    pull(species_code)
  
  glue("Pulling data for", target_species_var, "AKA", target_species, .sep = " ") %>% 
    message()
  
  tic()
  sp_path <- ebirdst_download(species = target_species, tifs_only = T, force = F)
  toc()
  glue("Downloaded files to", sp_path, .sep = " ") %>% 
    message()
  
  message("Loading raster")
  abunds <- load_raster("abundance", path = sp_path)
  
  original_raster_crs <- raster::crs(abunds) %>% 
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
  abunds_cropped <- abunds %>% 
    crop(pa_bbox) %>% 
    projectRaster(crs = mollweide, method = "ngb")
  toc()
  
  message("Transforming raster to tibble")
  tic()
  abunds_table <- abunds_cropped %>% 
    as.data.frame(xy = T) %>% 
    as_tibble() %>% 
    pivot_longer(-c(x, y), names_to = "date", values_to = "abundance") %>% 
    mutate(date = str_remove(date, "^X"),
           date = str_replace_all(date, "\\.", "-"),
           date = ymd(date),
           month = month(date, label = T))# %>% 
  # group_by(month, x, y) %>% 
  # summarize(abundance = mean(abundance, na.rm = T))
  toc()
  
  abunds_table
}

species_table <- tibble(species = c("Common Yellowthroat", "Indigo Bunting"))

species_table

species_table <- 
  species_table %>% 
  mutate(abundance_table = map(species, ~get_abundance_table(target_species_var = .x)))

abunds_table <- 
  species_table %>% 
  unnest(abundance_table)

abunds_table %>% 
  write_csv("data/big/ebirdst_test_save.csv")