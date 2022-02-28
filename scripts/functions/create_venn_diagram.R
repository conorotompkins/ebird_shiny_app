library(tidyverse)
library(ggVennDiagram)
library(sf)

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
    group_by(common_name) %>% 
    summarize(appears = sum(abundance > 0)) %>% 
    filter(appears > 0) %>% 
    ungroup() %>% 
    distinct(common_name) %>% 
    pull(common_name)
  
  compare_list <- table %>% 
    filter(geo_id == compare) %>% 
    group_by(common_name) %>% 
    summarize(appears = sum(abundance > 0)) %>% 
    filter(appears > 0) %>% 
    ungroup() %>% 
    distinct(common_name) %>% 
    pull(common_name)
  
  venn_list <- list("Reference" = reference_list, "Compare" = compare_list)
  
  Venn(venn_list) %>% 
    process_data() %>% 
    .@region %>% 
    mutate(centroid = st_point_on_surface(geometry),
           x = map_dbl(centroid, 1),
           y = map_dbl(centroid, 2)) %>% 
    select(x, y, name, geometry, count) %>% 
    mutate(pct = count / sum(count),
           pct = round(pct * 100, 1),
           venn_label = str_c(count, "\n", pct, "%")) %>% 
    ggplot(aes(fill = name)) +
    geom_sf() +
    geom_label(aes(x, y, label = venn_label)) +
    guides(fill = "none") +
    theme_void()
}