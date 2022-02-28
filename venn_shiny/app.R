library(shiny)
library(tidyverse)
library(ggVennDiagram)
library(sf)

genes <- paste("gene",1:1000,sep="")
set.seed(20210419)
x <- list(A=sample(genes,300),
          B=sample(genes,525))

venn <- Venn(x)
venn_data <- process_data(venn)@region %>% 
  mutate(centroid = st_point_on_surface(geometry),
         x = map_dbl(centroid, 1),
         y = map_dbl(centroid, 2)) %>% 
  select(x, y, name, geometry)

ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny Venn Diagram"),
  
  mainPanel(
    plotOutput("venn_diagram", click = "plot_click"),
    textOutput("print"),
    tableOutput("venn_table")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$venn_diagram <- renderPlot({
    
    venn_data %>% 
      ggplot(aes(x, y, fill = name, label = name)) +
      geom_sf() +
      geom_label()
    
  })
  
  output$venn_table <- renderTable({
    
    req(input$plot_click)
    
    nearPoints(st_drop_geometry(venn_data), 
               input$plot_click,
               threshold = 100)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

