library(tidyverse)
library(lubridate)
library(vroom)
library(tools)

#load abunds_table
abunds_table <- list.files("data/big/species_abundance", full.names = T, recursive = T, pattern = ".csv") %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'path',
          col_types = cols(
            family_common_name = col_character(),
            common_name = col_character(),
            metric_desc = col_character(),
            date = col_date(format = ""),
            value = col_double(),
            month = col_character(),
            region = col_character(),
            x = col_double(),
            y = col_double()
          )) %>% 
  mutate(path = basename(path) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  mutate(x = round(x, 2),
         y = round(y, 2)) %>% 
  rename(abundance = value) %>% 
  mutate(geo_id = str_c(x, y, sep = "_")) %>% 
  select(-path)

glimpse(abunds_table)

abunds_table %>% 
  write_csv("data/big/abunds_table.csv")