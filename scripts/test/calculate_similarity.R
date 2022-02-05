library(tidyverse)
library(sf)
library(tigris)
library(vroom)
library(lubridate)
library(hrbrthemes)
library(tools)
library(widyr)

set.seed(1234)

options(tigris_use_cache = TRUE)

mollweide <- "+proj=moll +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84"

original_raster_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs"

region_input <- "Pennsylvania, New Jersey"

region_shape <- states(cb = T) %>% 
  filter(str_detect(region_input, NAME)) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

abunds_table <- list.files("data/big/species_abundance", full.names = T) %>% 
  #keep(str_detect(., "Dark-eyed Junco")) %>% 
  set_names() %>% 
  map_dfr(vroom, delim = ",", .id = 'comName', col_type = cols(.default = "c")) %>% 
  mutate(comName = basename(comName) %>% file_path_sans_ext,
         month = month(date, label = T)) %>% 
  rename(abundance = value) %>% 
  mutate(abundance = parse_number(abundance),
         abundance = coalesce(abundance, 0),
         #round to get rid of discrepancies
         x_num = parse_number(x) %>% round(2),
         y_num = parse_number(y) %>% round(2)) %>% 
  arrange(comName, x, y)

abunds_table %>% 
  group_by(comName) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears == 0)

false_appearance <- abunds_table %>% 
  group_by(comName) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears == 0) %>% 
  ungroup() %>% 
  distinct(comName)

abunds_table %>% 
  semi_join(false_appearance, by = "comName")

abunds_table %>% 
  count(comName) %>% 
  distinct(n)

abunds_table %>% 
  distinct(x, y) %>% 
  count(x, y, sort = T) %>% 
  distinct(n)

abunds_table %>% 
  distinct(x_num, y_num) %>% 
  count(x_num, y_num, sort = T) %>% 
  ggplot(aes(x_num, y_num, size = n)) +
  geom_point()

#birds with "different" coordinates still have similar map
abunds_table %>% 
  #semi_join(false_appearance) %>% 
  filter(comName %in% c("Common Shelduck", "Red-necked Stint")) %>% 
  distinct(x_num, y_num) %>% 
  count(x_num, y_num, sort = T) %>% 
  ggplot(aes(x_num, y_num, size = n)) +
  geom_point()

#each x,y occurs once within a species
abunds_table %>% 
  distinct(comName, x, y) %>% 
  count(comName, x, y) %>% 
  distinct(n)

#each date occurs once within a species
abunds_table %>% 
  distinct(comName, date) %>% 
  count(comName, date) %>% 
  distinct(n)

#each x, y occurs once within a date
abunds_table %>% 
  distinct(date, x, y) %>% 
  count(date, x, y) %>% 
  distinct(n)

#compare coordinates across species
geo_id_table <- abunds_table %>% 
  select(comName, x, y) %>% 
  mutate(geo_id = str_c(x, y, sep = "_"))

geo_id_table %>% 
  count(geo_id, sort = T) %>% 
  distinct(n)

geo_id_table %>% 
  group_by(geo_id) %>% 
  filter(n() == 104) %>% 
  ungroup() %>% 
  distinct(comName)

#some species have basically the same coordinates with differences after many decimal places

#x have variable length
geo_id_table %>% 
  mutate(x_len = str_length(x),
         y_len = str_length(y)) %>% 
  count(x_len)

#y have consistent length
geo_id_table %>% 
  mutate(x_len = str_length(x),
         y_len = str_length(y)) %>% 
  count(y_len)

test_1 <- geo_id_table %>% 
  filter(comName == "Song Sparrow") %>% 
  #select(-comName) %>% 
  mutate(x_len = str_length(x),
         y_len = str_length(y))
  
test_2 <- geo_id_table %>% 
  filter(comName == "Common Shelduck") %>% 
  #select(-comName) %>% 
  mutate(x_len = str_length(x),
         y_len = str_length(y))

test_1 %>% 
  select(x, y) %>% 
  anti_join(select(test_2, x, y),
            by = c("x", "y")) %>% 
  distinct(x, y)

#1007690.315618213

test_1 %>% 
  select(comName, x, y) %>% 
  full_join(select(test_2, comName, x, y),
            by = c("x", "y"))%>% 
  View()

all.equal(test_1, test_2)

#create abundance summary
abundance_summary <- abunds_table %>%
  anti_join(false_appearance, by = "comName") %>% 
  group_by(comName, month, x_num, y_num) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>%
  ungroup() %>% 
  mutate(species_id = str_c(comName, sep = "_"),
         geo_id = str_c(x_num, y_num, sep = "_")) %>% 
  st_as_sf(coords = c("x_num", "y_num"), crs = mollweide) %>% 
  #st_filter(region_shape_moll, join = st_intersects) %>% 
  select(-species_id)

#each species has 3576 rows
abundance_summary %>% 
  st_drop_geometry() %>% 
  count(comName) %>% 
  distinct(n)

#no missing common_name
abundance_summary %>% 
  filter(is.na(comName))

#no missing months
abundance_summary %>% 
  filter(is.na(month))

abundance_summary %>%
  filter(comName == "Cape May Warbler"#,
         #month == "Sep"
         ) %>% 
  ggplot() +
  geom_sf(data = region_shape_moll, color = "black") +
  geom_sf(aes(color = abundance)) +
  scale_color_viridis_c() +
  facet_wrap(~month)

#no issue with duplicate x,y
abundance_summary %>%
  filter(comName %in% c("Cape May Warbler", "American Black Duck"),
         month == "Sep") %>% 
  separate(geo_id, into = c("x", "y"), sep = "_") %>% 
  mutate(across(c(x, y), parse_number)) %>% 
  ggplot(aes(x, y)) +
  geom_jitter(alpha = .3, width = 10^3.5, height = 10^3.5) +
  facet_wrap(~comName, ncol = 1)

#rm(abunds_table)

pairwise_dist_f <- function(x){
  
  pairwise_dist(tbl = x, item = geo_id, feature = comName, value = abundance,
                diag = T, upper = T)
  
}

pairwise_corr_f <- function(x){
  
  pairwise_cor(tbl = x, item = geo_id, feature = comName, value = abundance,
               diag = T, upper = T)
}

#each geo_id_1 in similarity_index should have 342 geo_id_2s
abundance_summary %>% 
  st_drop_geometry() %>% 
  distinct(geo_id) %>% 
  nrow()

similarity_index <- abundance_summary %>% 
  st_drop_geometry() %>% 
  #filter(month == "Apr") %>% 
  select(month, geo_id, comName, abundance) %>% 
  #for future development
  group_nest(month) %>%
  mutate(dist_data = map(data, pairwise_corr_f)) %>% 
  select(-data) %>% 
  unnest(dist_data) %>% 
  #pairwise_dist(geo_id, comName, abundance, diag = T, upper = T) %>% 
  rename(geo_id_1 = item1,
         geo_id_2 = item2) %>% 
  select(month, everything()) %>% 
  arrange(geo_id_1, geo_id_2)

#298 geo_id_1s
similarity_index %>% 
  distinct(geo_id_1) %>% 
  count(geo_id_1) %>% 
  nrow()

#298 geo_id_1s
similarity_index %>% 
  distinct(geo_id_2) %>% 
  count(geo_id_2) %>% 
  nrow()

#each geo_id_1 appears 3576 
similarity_index %>% 
  count(geo_id_1) %>% 
  distinct(n)

#each geo_id_2 appears 3576 
similarity_index %>% 
  count(geo_id_2) %>% 
  distinct(n)

similarity_index %>% 
  ggplot(aes(correlation)) +
  geom_histogram() +
  facet_wrap(~month)

#this is the issue with not using upper = T in matrix function
test_geo_id_1 <- similarity_index %>% 
  #filter(geo_id_1 == "1007690.31561821_4762108.24422306") %>% 
  distinct(geo_id_1) %>% 
  filter(row_number() %in% sample(1:198, 5))

test_geo_id_1_sf <- test_geo_id_1 %>% 
  separate(geo_id_1, "_", into = c("x", "y")) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide)

similarity_index %>% 
  mutate(compare_geo_id = geo_id_1 == geo_id_2) %>% 
  #filter(compare_geo_id == T)
  semi_join(test_geo_id_1) %>% 
  #pivot_longer(cols = c(geo_id_1, geo_id_2)) %>% 
  separate(geo_id_2, "_", into = c("x", "y")) %>% 
  st_as_sf(coords = c("x", "y"), crs = mollweide) %>% 
  ggplot() +
  geom_sf(data = region_shape_moll) +
  geom_sf(aes(color = distance, stroke = compare_geo_id)) + 
  #geom_sf(data = test_geo_id_1_sf, color = "red") +
  facet_wrap(~geo_id_1) +
  scale_color_viridis_c() +
  scale_discrete_manual(
    aesthetics = "stroke",
    values = c(1, .1)
  )
  


similarity_index %>% 
  write_csv("data/big/similarity_index.csv")
