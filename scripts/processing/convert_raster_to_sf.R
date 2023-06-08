library(tidyverse)
library(ebirdst)
library(terra)
library(sf)
library(stars)

path <- ebirdst_download(species = "Cape May Warbler", tifs_only = T, force = F)

full_raster <- load_raster(path = path, product = "abundance", period = "full-year", resolution = "lr")

raster_crs <- crs(full_raster)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

full_raster |> 
  plot()

full_raster |> 
  class()

full_raster_transformed <- full_raster |> 
  st_as_stars() |> 
  st_as_sf() |> 
  st_transform(mollweide) |> 
  select(-1)

full_raster_transformed |> 
  plot()

glimpse(full_raster_transformed)

full_raster |> 
  st_as_stars() |> 
  st_as_sf() |> 
  rename(abundance = 1) |> 
  filter(abundance > 0) |> 
  st_transform(mollweide) |> 
  plot()

full_raster_transformed |> 
  st_write("data/big/full_raster_transformed.shp")