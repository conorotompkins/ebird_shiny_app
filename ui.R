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
      
      fluidRow(
        column(
          width = 2,
          shiny::selectizeInput("month_input",
                                label = "Select Month",
                                choices = month.abb),
          sliderInput("transparency_slider_input",
                      "Tile Transparency",
                      min = 0,
                      max = 1,
                      value = 1,
                      step = .25),
          verbatimTextOutput("mouse_interactions"),
          radioButtons("toggle_venn_diagram", 
                       "Toggle Venn Diagram",
                       choices = c("On", "Off"),
                       selected = "Off")
        ),
        column(
          width = 7,
          
          leafletOutput("chloropleth_map"),
          plotOutput("histogram")
        ),
        column(
          
          width = 3,
          
          plotOutput("venn_diagram")
          
        )
      )
    )
  )
) 
