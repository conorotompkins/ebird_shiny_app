library(shiny)
library(shinythemes)
library(shinyWidgets)

library(tidyverse)

library(hrbrthemes)
library(scales)
library(leaflet)
library(sf)

similarity_index <- read_csv("data/big/similarity_index.csv") %>% 
    separate(geo_id_2, into = c("x", "y"), sep = "_") %>% 
    mutate(geo_id = row_number())

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("Hotspot Similarity Index"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(
                inputId = "reference_coords",
                label = "Area",
                value = 1,
                min = 1, 
                max = 100
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            tableOutput("table"),
            leafletOutput("map")
            
        )
    )
))
