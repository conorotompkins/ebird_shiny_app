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
    h5("Click a tile in the map to get started"),
    # Show a plot of the generated distribution
    mainPanel(
      
      fluidRow(
        column(
          width = 9,
          
          column(width = 6,
                 
                 sliderTextInput(inputId = "month_input",
                                 label = "Select Month",
                                 choices = month.abb,
                                 selected = lubridate::month(Sys.Date(), label = T),
                                 animate = animationOptions(interval = 3000)
                                 )
                 
          ),
          column(width = 6,
                 
                 sliderInput("transparency_slider_input",
                             "Tile Transparency",
                             min = 0,
                             max = 1,
                             value = .5,
                             step = .1)
                 
          ),
          #verbatimTextOutput("mouse_interactions")
          
          leafletOutput("chloropleth_map"),
          plotOutput("histogram", height = 300)
          
        ),
        column(
          
          width = 3,
          
          radioButtons(inputId = "mouse_interaction_type",
                       label = "Select tiles to compare",
                       choices = c("Reference", "Compare"),
                       selected = "Reference",
                       inline = T
          ),
          
          plotOutput("venn_diagram", click = "plot_click"),
          tableOutput("venn_table")
          
        )
      )
    )
  )
) 
