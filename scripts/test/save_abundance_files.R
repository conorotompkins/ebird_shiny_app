library(tidyverse)
library(rebird)
library(conflicted)

source("scripts/functions/pull_species_metric.R")

conflict_prefer("select", "dplyr")

ebird_tax <- ebirdtaxonomy() %>% 
  rename(scientific_name = sciName,
         common_name = comName,
         species_code = speciesCode,
         family_common_name = familyComName)

state_names <- tibble(state_name = state.name,
                      state_abb = state.abb)

location_birds <- tibble(region = c("US-PA", "US-NJ")) %>% 
  mutate(birds = map(region, ebirdregionspecies),
         region = str_remove(region, "^US-")) %>% 
  left_join(state_names, by = c(region = "state_abb")) %>% 
  unnest(cols = c(birds)) %>% 
  rename(state_abb = region,
         species_code = speciesCode)

location_birds <- ebirdst_runs %>% 
  select(common_name, scientific_name, species_code) %>% 
  inner_join(location_birds, by = c("species_code"))

perching_birds <- ebird_tax %>% 
  filter(order == "Passeriformes") %>% 
  select(scientific_name, common_name, species_code, family_common_name)

location_songbirds <- location_birds %>% 
  inner_join(perching_birds, by = c("species_code", "common_name", "scientific_name"))

top_families <- location_songbirds %>% 
  count(family_common_name, sort = T) %>% 
  slice(1:2)

species_table <- location_songbirds %>% 
  semi_join(top_families) %>% 
  select(state_name, state_abb, scientific_name, common_name, species_code, family_common_name)

region_str <- species_table %>% 
  distinct(state_name) %>% 
  pull() %>% 
  str_c(collapse = ", ")

glimpse(species_table)

# test <- get_species_metric("Pennsylvania, New Jersey", "Black-billed Magpie", "abundance", "lr")
# 
# test <- tibble(region = region_str,
#                comName = "Black-billed Magpie",
#                metric = "abundance",
#                resolution = "lr") %>% 
#   mutate(abundance_table = pmap(list(region, comName, metric, resolution), 
#                                 ~get_species_metric(..1, ..2, ..3, ..4)))

species_table <- species_table %>% 
  distinct(common_name) %>% 
  mutate(resolution = "lr",
         metric = "abundance",
         region = region_str) %>%  
  mutate(abundance_table = pmap(list(region, common_name, metric, resolution), 
                                ~get_species_metric(..1, ..2, ..3, ..4)))

species_table %>% 
  select(common_name, abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == T) %>% 
  unnest(abundance_table) %>% 
  # mutate(x = map_dbl(geometry, 1),
  #        y = map_dbl(geometry, 2)) %>% 
  distinct(x, y) %>% 
  ggplot(aes(x, y)) +
  geom_point()

species_table %>% 
  select(common_name, abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == F)

species_table %>% 
  select(common_name, abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == T) %>% 
  pwalk(~write_csv(x = .y, file = paste0("data/big/species_abundance/", .x, ".csv") ) )
