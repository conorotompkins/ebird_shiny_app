library(tidyverse)
library(rebird)
library(conflicted)

source("scripts/functions/fun_pull_abundance_data.R")

conflict_prefer("select", "dplyr")

ebird_tax <- ebirdtaxonomy()

pa_songbirds <- ebird_tax %>% 
  filter(order == "Passeriformes") %>% 
  semi_join(ebirdregionspecies("US-PA-003"), by = "speciesCode")

pa_songbirds %>% 
  count(familyComName, sort = T)

species_table <- pa_songbirds %>% 
  select(sciName, comName, speciesCode, order, familyComName, familySciName) %>% 
  filter(familyComName == "New World Warblers")

glimpse(species_table)

species_table <- 
  species_table %>% 
  mutate(abundance_table = map(comName, ~get_abundance_table(target_species_var = .x)))

species_table %>%
  select(comName, abundance_table) %>% 
  pwalk(~write_csv(x = .y, file = paste0("data/big/species_abundance/", .x, ".csv") ) )

# abunds_table <- 
#   species_table %>% 
#   unnest(abundance_table)

# abunds_table %>% 
#   group_split(comName) %>% 
#   walk(~write_csv(.x, paste0("data/big/species_abundance/", .x$comName[1], ".csv")))

# read_csv("data/big/species_abundance/Ovenbird.csv") %>% 
#   glimpse()
# 
# abunds_table %>% 
#   write_csv("data/big/ebirdst_test_save.csv")
