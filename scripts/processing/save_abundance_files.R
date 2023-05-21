library(tidyverse)
library(rebird)
library(conflicted)

source("scripts/functions/get_species_metric.R")

conflict_prefer("select", "dplyr")

ebird_tax <- ebirdtaxonomy() %>% 
  rename(scientific_name = sciName,
         common_name = comName,
         species_code = speciesCode,
         family_common_name = familyComName)

state_names <- tibble(state_name = state.name,
                      state_abb = state.abb)

target_region <- tibble(region = c("US-PA"))

location_birds <- target_region %>% 
  mutate(birds = map(region, ebirdregionspecies),
         region = str_remove(region, "^US-")) %>% 
  left_join(state_names, by = c(region = "state_abb")) %>% 
  unnest(cols = c(birds)) %>% 
  rename(state_abb = region,
         species_code = speciesCode)

location_birds <- ebirdst_runs %>% 
  select(common_name, scientific_name, species_code) %>% 
  inner_join(location_birds, by = c("species_code"))

ebird_tax <- rebird::ebirdtaxonomy() %>% 
  filter(familyComName == "New World Warblers") %>% 
  select(sciName, comName, speciesCode, order, familyComName) |> 
  rename(scientific_name = sciName,
         common_name = comName,
         species_code = speciesCode,
         order = order,
         family_common_name = familyComName)

ebird_tax

location_songbirds <- location_birds %>% 
  inner_join(ebird_tax, by = c("species_code", "common_name", "scientific_name"))

top_families <- location_songbirds %>% 
  count(family_common_name, sort = T) %>% 
  slice(1:5)

top_families

species_table <- location_songbirds %>% 
  semi_join(top_families) %>% 
  select(state_name, state_abb, scientific_name, common_name, species_code, family_common_name)

species_table %>% 
  distinct(state_name) %>% 
  write_csv("data/big/target_region.csv")

region_str <- species_table %>% 
  distinct(state_name) %>% 
  pull() %>% 
  str_c(collapse = ", ")

glimpse(species_table)

test <- get_species_metric("Pennsylvania", "New World Warblers", "Cape May Warbler", "abundance", "lr")
# 
# test <- tibble(region = region_str,
#                family_common_name = "New World Warblers",
#                common_name = "Cape May Warbler",
#                metric = "abundance",
#                resolution = "lr") %>%
#   mutate(abundance_table = pmap(list(region, family_common_name, common_name, metric, resolution),
#                                 ~get_species_metric(..1, ..2, ..3, ..4, ..5)))

species_table <- species_table %>% 
  distinct(family_common_name, common_name) %>% 
  mutate(resolution = "lr",
         metric = "abundance",
         region = region_str) %>%  
  mutate(abundance_table = pmap(list(region, family_common_name, common_name, metric, resolution), 
                                ~get_species_metric(..1, ..2, ..3, ..4, ..5)))

species_table %>% 
  select(abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == T) %>% 
  unnest(abundance_table) %>% 
  distinct(x, y) %>% 
  ggplot() +
  geom_point(aes(x, y))

species_table %>% 
  select(common_name, abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == F)

species_table_output <- species_table %>% 
  select(family_common_name, common_name, abundance_table) %>% 
  mutate(test = map_lgl(abundance_table, is.data.frame)) %>% 
  filter(test == T) %>% 
  select(-test)

species_table_output %>% 
  distinct(family_common_name) %>% 
  pull() %>% 
  str_c("data/big/species_abundance/", .) %>% 
  map(dir.create)

species_table_output %>% 
  pwalk(~write_csv(x = ..3, file = paste0("data/big/species_abundance/", ..1, "/", ..2, ".csv") ) )
