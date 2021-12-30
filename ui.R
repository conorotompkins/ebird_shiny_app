library(shiny)
library(shinythemes)
library(shinyWidgets)

library(tidyverse)

library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)

# similarity_index <- read_csv("area_similarity/data/similarity_index.csv") %>% 
#     separate(geo_id_2, into = c("x", "y"), sep = "_") %>% 
#     mutate(geo_id = row_number())

# Define UI for application that draws a histogram
ui <- shinyUI(
  
  fluidPage(
    
    # Application title
    titlePanel("eBird Region Similarity Index"),
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("text"),
      #tableOutput("table"),
      verbatimTextOutput("clicked_grid_id"),
      leafletOutput("map")
      
    )
  )
)
