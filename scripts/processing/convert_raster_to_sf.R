library(tidyverse)
library(ebirdst)
library(terra)
library(sf)
library(stars)

path <- ebirdst_download(species = "Cape May Warbler", tifs_only = T, force = F)

test <- load_raster(path = path, product = "abundance", period = "full-year", resolution = "lr")

test |> 
  plot()

test |> 
  class()

test |> 
  rast() |> 
  vect()
  st_as_sf()

st_read(path)

test |> 
  st_as_stars() |> 
  st_as_sf() |> 
  rename(abundance = 1) |> 
  filter(abundance > 0) |> 
  plot()
