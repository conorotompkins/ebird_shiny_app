library(tidyverse)
library(ebirdst)
library(rebird)

ebirdst_tax <- ebirdst_runs %>% 
  mutate(source = "ebirdst")

rebird_tax <- ebirdtaxonomy() %>% 
  mutate(source = "rebird")

ebirdst_tax %>% 
  anti_join(rebird_tax, by = c("common_name" = "comName")) %>% 
  distinct(common_name)

ebirdst_tax %>% 
  anti_join(rebird_tax, by = c("scientific_name" = "sciName")) %>% 
  distinct(common_name)

ebirdst_tax %>% 
  anti_join(rebird_tax, by = c("species_code" = "speciesCode")) %>% 
  distinct(common_name)

rebird_tax %>% 
  anti_join(ebirdst_tax, by = c("comName" = "common_name")) %>% 
  distinct(comName)

rebird_tax %>% 
  anti_join(ebirdst_tax, by = c("sciName" = "scientific_name")) %>% 
  distinct(comName)

rebird_tax %>% 
  anti_join(ebirdst_tax, by = c("speciesCode" = "species_code")) %>% 
  distinct(comName)
            

rebird_tax %>% 
  count(familyComName, sort = T) %>% 
  View()
