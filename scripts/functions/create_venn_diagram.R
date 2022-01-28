library(tidyverse)
library(ggVennDiagram)

create_venn_diagram <- function(reference_id, compare_id){
  
  reference <- similarity_geo %>% 
    filter(geo_index_reference == 45) %>% 
    distinct(geo_index_reference, geo_id_reference) %>% 
    pull(geo_id_reference)
  
  compare <- similarity_geo %>% 
    #mutate(geo_id_compare = str_c(x, y, sep = "_")) %>% 
    filter(geo_index_compare == 26) %>% 
    distinct(geo_index_compare, geo_id_compare) %>% 
    pull(geo_id_compare)
  
  reference_list <- abunds_table %>% 
    filter(geo_id == reference) %>% 
    group_by(comName) %>% 
    summarize(appears = sum(abundance > 0)) %>% 
    filter(appears > 0) %>% 
    ungroup() %>% 
    distinct(comName) %>% 
    pull(comName)
  
  compare_list <- abunds_table %>% 
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
    scale_fill_viridis_c() +
    scale_color_manual(values = c("#FFFFFF", "#FFFFFF"))
}