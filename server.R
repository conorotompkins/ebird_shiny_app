library(shiny)

library(tidyverse)
library(vroom)
library(tools)
library(lubridate)

library(sf)
library(tigris)
library(leaflet)
library(hrbrthemes)
library(janitor)

theme_set(theme_ipsum())

options(tigris_use_cache = TRUE)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/create_venn_diagram.R")

#load abunds_table
abunds_table <- vroom("data/big/abunds_table.csv")

#create similarity index
#load similarity index data
bins <- c(-Inf, 0, seq(from = 10, to = 80, by = 10), Inf)
length(bins)

bin_labels <- c("0", "1-10", "10-20", "20-30", "30-40", "40-50", "50-60",
                "60-70", "70-80", "80+")
length(bin_labels)

similarity_index <- read_csv("data/big/similarity_index.csv")

base_map_data <- st_read("data/big/grid_shapefile/grid_shapefile.shp") %>% 
  set_names(c("geo_index_compare", "geometry"))

#create region shape
mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_input, NAME)) %>% 
  st_transform(crs = original_raster_crs)

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  abunds_table_reactive <- reactive({
    
    abunds_table %>% 
      filter(month == input$month_input)
    
  })
  
  similarity_index_reactive <- reactive({
    
    similarity_index %>%
      filter(month == input$month_input)
    
  })
  
  #create reactive containing a list of two variables
  #drives from toggle
  #when toggle is "geo id 1", write to first value in list
  #when toggle is "geo id 2", write to second value in list
  #extract values from list to get geo ids
  #### switch to reactiveVal containers and an observeEvent pattern ####
  mouse_reference <- reactiveVal("")
  mouse_compare <- reactiveVal("")    
  
  observeEvent(input$chloropleth_map_shape_click$id,{ 
    
    switch(input$mouse_interaction_type, 
           "Reference" = mouse_reference(input$chloropleth_map_shape_click$id),
           "Compare" = mouse_compare(input$chloropleth_map_shape_click$id)
    )
  })
  
  output$mouse_interactions <- renderText({
    
    str_c("Reference: ", mouse_reference(), "\n",
          "Compare: ", mouse_compare(), sep = "")
    
  })
  
  similarity_grid_reactive <- reactive({
    
    req(mouse_reference())
    
    bins <- seq(from = -1, to = 1, by = .2)
    
    bin_labels <- c("-1 to -.8", ".8 to -.6", "6 to -.4", "-.4 to -.2", "-.2 to 0", 
                    "0 to .2",".2 to .4", ".4 to .6", ".6 to .8", ".8 to 1")
    
    similarity_index_reactive() %>%
      prep_similarity_index(mouse_reference()) %>% 
      filter(!is.na(correlation)) %>%
      mutate(correlation = round(correlation, 2),
             correlation_bin = cut(correlation, breaks = bins, labels = bin_labels),
             correlation_bin = as.factor(correlation_bin))
    
  })
  
  output$mouse_interactions <- renderText({
    
    req(input$chloropleth_map_shape_click$id)
    
    str_c("Click: ", mouse_reference(), "\n",
          "Hover: ", mouse_compare(), sep = "")
    
  })
  
  transparency_reactive <- reactive({
    
    input$transparency_slider_input
    
  })
  
  output$chloropleth_map <- renderLeaflet({
    
    #print(similarity_grid_reactive())
    
    base_map_data %>%
      st_transform(crs = "EPSG:4326") %>% 
      leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE
                       )) %>%
      addPolygons(group = "Tiles",
                  layerId = ~geo_index_compare,
                  stroke = T,
                  weight = 1,
                  fillOpacity = 0
      )
    
  })
  
  selected_grid_id_reactive <- reactive({
    
    input$chloropleth_map_shape_click$id
    
  })
  
  mouseover_grid_id_reactive <- reactive({
    
    input$chloropleth_map_shape_mouseover$id
    
  })
  
  highlight_cell <- reactive({
    
    req(input$chloropleth_map_shape_click$id, similarity_grid_reactive())
    
    similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326") %>% 
      filter(geo_index_compare == geo_index_reference)
    
  })
  
  #observer for chloropleth
  observe({
    
    req(input$chloropleth_map_shape_click$id, similarity_grid_reactive())
    
    bin_labels <- c("-1 to -.8", ".8 to -.6", "6 to -.4", "-.4 to -.2", "-.2 to 0", 
                    "0 to .2",".2 to .4", ".4 to .6", ".6 to .8", ".8 to 1")
    
    similarity_grid_corr <- similarity_grid_reactive() %>%
      st_transform(crs = "EPSG:4326") %>% 
      mutate(grid_opacity = input$transparency_slider_input) %>% 
      complete(correlation_bin = bin_labels) %>% 
      mutate(correlation_bin = factor(correlation_bin, levels = bin_labels)) %>% 
      st_as_sf()
    
    pal_d <- colorFactor(
      palette = viridis::viridis(110),
      domain = similarity_grid_corr$correlation_bin,
      levels = bin_labels)
    
    leafletProxy("chloropleth_map", data = similarity_grid_corr) %>%
      clearGroup("Chloropleth") %>%
      addPolygons(group = "Chloropleth",
                  layerId = ~geo_index_compare,
                  color = "#444444",
                  fillColor = ~pal_d(correlation_bin),
                  fillOpacity = ~grid_opacity,
                  stroke = T,
                  weight = 1,
                  label = ~paste0("geo_id: ", geo_index_compare, "\n", "Correlation: ", round(correlation,4)),
                  highlightOptions = highlightOptions(color = "white", bringToFront = T)) %>% 
      addPolygons(data = highlight_cell(),
                  color = "white",
                  fillColor = "white",
                  fillOpacity = 1,
                  dashArray = "1")
  })
  
  output$histogram <- renderPlot({
    
    bin_labels <- c("-1 to -.8", ".8 to -.6", "6 to -.4", "-.4 to -.2", "-.2 to 0", 
                    "0 to .2",".2 to .4", ".4 to .6", ".6 to .8", ".8 to 1")
    
    ws <- str_c(rep(" ", 135), collapse = "")
    
    x_label <- str_c("Less Similar", ws, "More Similar")
    
    similarity_grid_reactive() %>%
      st_drop_geometry() %>% 
      mutate(correlation_bin = factor(correlation_bin, levels = bin_labels)) %>% 
      count(month, correlation_bin, .drop = F) %>% 
      ggplot(aes(correlation_bin, n, fill = correlation_bin)) +
      geom_col() +
      scale_fill_viridis_d() +
      guides(fill = "none") +
      labs(title = "Correlation",
           x = x_label,
           y = "Count of Regions") +
      theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                           ends = "both")),
            axis.title.x = element_text(angle = 0, size = 15),
            axis.title.y = element_text(size = 15))
    
    
  })
  
  #calculate time since leaflet mouseover changed. if that time is > 3 seconds, render venn diagram
  output$venn_diagram <- renderPlot({
    
    req(mouse_reference() != "",
        mouse_compare() != "")

    create_venn_diagram(mouse_reference(), mouse_compare(), similarity_grid_reactive(), abunds_table_reactive())
    
  })
  
  
  
  venn_segment_reactive <- reactive({
    
    req(mouse_reference() != "",
        mouse_compare() != "",
        input$plot_click)
    
    reference <- similarity_grid_reactive() %>% 
      filter(geo_index_reference == mouse_reference()) %>% 
      distinct(geo_index_reference, geo_id_reference) %>% 
      pull(geo_id_reference)
    
    compare <- similarity_grid_reactive() %>% 
      filter(geo_index_compare == mouse_compare()) %>% 
      distinct(geo_index_compare, geo_id_compare) %>% 
      pull(geo_id_compare)
    
    reference_list <- abunds_table_reactive() %>% 
      filter(geo_id == reference) %>% 
      group_by(common_name) %>% 
      summarize(appears = sum(abundance > 0)) %>% 
      filter(appears > 0) %>% 
      ungroup() %>% 
      distinct(common_name) %>% 
      pull(common_name)
    
    compare_list <- abunds_table_reactive() %>% 
      filter(geo_id == compare) %>% 
      group_by(common_name) %>% 
      summarize(appears = sum(abundance > 0)) %>% 
      filter(appears > 0) %>% 
      ungroup() %>% 
      distinct(common_name) %>% 
      pull(common_name)
    
    venn_list <- list("Reference" = reference_list, "Compare" = compare_list)
    
    venn_data <- Venn(venn_list) %>% 
      process_data() %>% 
      .@region %>% 
      mutate(centroid = st_point_on_surface(geometry),
             x = map_dbl(centroid, 1),
             y = map_dbl(centroid, 2)) %>% 
      st_drop_geometry() %>% 
      select(x, y, name)
    
    nearPoints(df = venn_data, 
               coordinfo = input$plot_click,
               xvar = "x",
               yvar = "y",
               threshold = 50) %>% 
      distinct(name) %>% 
      pull()
    
  })
  
  output$venn_segment <- renderText(venn_segment_reactive())
  
  output$venn_table <- renderTable({
    
    req(mouse_reference() != "",
        mouse_compare() != "",
        input$plot_click)
    
    reference <- similarity_grid_reactive() %>% 
      filter(geo_index_reference == mouse_reference()) %>% 
      distinct(geo_index_reference, geo_id_reference) %>% 
      pull(geo_id_reference)
    
    compare <- similarity_grid_reactive() %>% 
      filter(geo_index_compare == mouse_compare()) %>% 
      distinct(geo_index_compare, geo_id_compare) %>% 
      pull(geo_id_compare)
    
    reference_df <- abunds_table_reactive() %>% 
      filter(geo_id == reference) %>% 
      group_by(family_common_name, common_name) %>% 
      summarize(appears = sum(abundance > 0)) %>% 
      filter(appears > 0) %>% 
      ungroup() %>% 
      distinct(family_common_name, common_name) %>% 
      mutate(type = "Reference")
    
    compare_df <- abunds_table_reactive() %>% 
      filter(geo_id == compare) %>% 
      group_by(family_common_name, common_name) %>% 
      summarize(appears = sum(abundance > 0)) %>% 
      filter(appears > 0) %>% 
      ungroup() %>% 
      distinct(family_common_name, common_name) %>% 
      mutate(type = "Compare")
    
    reference_df_segment <- anti_join(reference_df, compare_df, by = c("family_common_name", "common_name"))
    
    compare_df_segment <- anti_join(compare_df, reference_df, by = c("family_common_name", "common_name"))
    
    both_df_segment <- reference_df %>% 
      semi_join(compare_df, by = c("family_common_name", "common_name")) %>% 
      mutate(type = "Reference..Compare")
    
    print(both_df_segment)
    
    venn_df <- bind_rows(reference_df_segment, compare_df_segment, both_df_segment) %>% 
      filter(type == venn_segment_reactive()) %>%
      count(type, family_common_name, sort = T) %>% 
      mutate(type = case_when(type == "Reference..Compare" ~ "Both",
                              TRUE ~ type)) %>% 
      set_names(c("Type", "Family", "Count")) %>% 
      adorn_totals(where = "row")
    
    venn_df %>% 
      distinct(Type) %>% 
      print()
    
    venn_df
    
  })
  
})
