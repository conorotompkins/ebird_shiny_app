library(tidyverse)
library(ggVennDiagram)

create_venn_diagram <- function(reference_id, compare_id, similarity_df, table){
  
  reference <- similarity_df %>% 
    filter(geo_index_reference == reference_id) %>% 
    distinct(geo_index_reference, geo_id_reference) %>% 
    pull(geo_id_reference)
  
  compare <- similarity_df %>% 
    #mutate(geo_id_compare = str_c(x, y, sep = "_")) %>% 
    filter(geo_index_compare == compare_id) %>% 
    distinct(geo_index_compare, geo_id_compare) %>% 
    pull(geo_id_compare)
  
  reference_list <- table %>% 
    filter(geo_id == reference) %>% 
    group_by(comName) %>% 
    summarize(appears = sum(abundance > 0)) %>% 
    filter(appears > 0) %>% 
    ungroup() %>% 
    distinct(comName) %>% 
    pull(comName)
  
  compare_list <- table %>% 
    filter(geo_id == compare) %>% 
    group_by(comName) %>% 
    summarize(appears = sum(abundance > 0)) %>% 
    filter(appears > 0) %>% 
    ungroup() %>% 
    distinct(comName) %>% 
    pull(comName)
  
  venn_list <- list("Reference" = reference_list, "Compare" = compare_list)
  
  ggVennDiagram(venn_list) +
    labs(fill = "Distinct Species") +
    scale_fill_gradient(low = "grey",
                        high = "black",
                        guide = guide_colorbar(direction = "horizontal",
                                               title.position = "bottom")) +
    scale_color_manual(values = c("#FFFFFF", "#FFFFFF")) +
    theme(legend.position = "bottom")
}