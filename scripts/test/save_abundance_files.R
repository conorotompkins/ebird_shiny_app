library(tidyverse)
library(rebird)
library(conflicted)

source("scripts/functions/pull_species_metric.R")

conflict_prefer("select", "dplyr")

ebird_tax <- ebirdtaxonomy()

pa_songbirds <- ebird_tax %>% 
  filter(order == "Passeriformes") %>% 
  semi_join(ebirdregionspecies("US-PA-003"), by = "speciesCode")

target_birds <- pa_songbirds %>% 
  count(familyComName, sort = T) %>% 
  slice(1:2)

species_table <- pa_songbirds %>% 
  select(sciName, comName, speciesCode, order, familyComName, familySciName) %>% 
  semi_join(target_birds, by = c("familyComName" = "familyComName"))

glimpse(species_table)

species_table <- 
  species_table %>% 
  mutate(resolution = "lr",
         metric = "abundance") %>% 
  mutate(abundance_table = pmap(list(comName, metric, resolution), ~get_species_metric(..1, ..2, ..3)))

species_table %>%
  select(comName, abundance_table) %>% 
  pwalk(~write_csv(x = .y, file = paste0("data/big/species_abundance/", .x, ".csv") ) )
