library(tidyverse)
library(ebirdst)
library(terra)
library(sf)
library(stars)
library(tigris)

#get raster
path <- ebirdst_download(species = "Cape May Warbler", tifs_only = T, force = F)

full_raster <- load_raster(path = path, product = "abundance", period = "full-year", resolution = "lr")

raster_crs <- crs(full_raster)
cat(raster_crs)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

full_raster |> 
  plot()

full_raster |> 
  class()

full_raster_transformed <- full_raster |> 
  st_as_stars() |> 
  st_as_sf() |> 
  select(-1)

full_raster_transformed |> 
  plot()

glimpse(full_raster_transformed)

#get state polygons
region_input <- "Pennsylvania"

region_input_list <- str_split(region_input, ", ", simplify = F) %>% 
  unlist()

region_shape <- states(cb = T) %>% 
  filter(NAME %in% region_input_list) %>% 
  st_transform(crs = raster_crs) %>% 
  summarize()

crs(region_shape) |> 
  cat()

region_shape |> 
  ggplot() +
  geom_sf()

#test plot of cape may warbler
full_raster |> 
  st_as_stars() |> 
  st_as_sf() |> 
  rename(abundance = 1) |> 
  filter(abundance > 0) |> 
  st_transform(mollweide) |> 
  plot()

full_raster_transformed |> 
  st_filter(region_shape, .predicate = sf::st_covered_by) |> 
  st_transform(mollweide) |> 
  mutate(center = map(geometry, st_centroid),
         lon = map_dbl(center, 1),
         lat = map_dbl(center, 2),
         buffered_square = st_buffer()) |>  
  # st_drop_geometry() |> 
  # st_set_geometry("center") |> 
  ggplot() +
  geom_sf() +
  geom_point(aes(lon, lat))

full_raster_transformed |> 
  st_write("data/big/full_raster_transformed.shp")