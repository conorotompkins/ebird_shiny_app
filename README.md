# eBird Shiny App
a repo for designing a shiny app for ebird data

Use `ebirdst` library to get location, abundance, time series data

Features:
* Seasonality map
  * raster map with squares representing square kilometers
    * squares are filled by mean adbundance of species 
    * User can click on a square to get more information about seasonality of a specific bird taxonomic unit (family, species etc)
      * User clicks, side chart creats a polar line graph with mean abundance for that month in that square kilometer
* Hotspot similarity index
  * Based on which species are in that hotspot and the mean abundance of those species, create  similarity index of hotspots.
  * User will be able to see which hotspots are similar and disimilar
  * User can click on a hotspot in a map
    * Side chart will show top 5 most and least similar hotspots
* Location guesser
  * given a set of birds, build a model that classifies where the observation was made.
  * map shaded by % positive prediction
