library(tidyverse)
library(lobstr)

ebird_files <-
  list.files("/Users/conorotompkins/Library/Application Support/org.R-project.R/R/ebirdst",
             full.names = T, recursive = T) %>% 
  set_names() %>% 
  enframe(name = "path", value = "file") %>% 
  mutate(file = basename(file))

ebird_files %>% 
  mutate(file_size = map_dbl(path, file.size)) %>% 
  summarize(file_size_gb = sum(file_size) / 10^9)