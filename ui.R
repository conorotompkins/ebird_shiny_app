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
      
      column(
        width = 2,
        shiny::selectizeInput("month_input",
                              label = "Select Month",
                              choices = month.abb),
        textOutput("text"),
        verbatimTextOutput("clicked_grid_id"),
        numericInput(inputId = "geo_index_compare_input",
                     label = "geo_index_compare",
                     value = 16,
                     min = 1,
                     max = 199,
                     step = 1),
        sliderInput("transparency_slider_input",
                    "Tile Transparency",
                    min = 0,
                    max = 1,
                    value = 1,
                    step = .25)
      ),
      
      column(
        width = 10,
        plotOutput("chloropleth_map", click = "plot_click"),
        plotOutput("histogram")
      )
    )
  )
)
