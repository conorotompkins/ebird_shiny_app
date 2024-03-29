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

region_input <- read_csv("data/big/target_region.csv")

region_shape <- states(cb = T) %>% 
  semi_join(region_input, by = c("NAME" = "state_name")) %>% 
  st_transform(crs = original_raster_crs) %>% 
  summarize()

region_bbox <- region_shape %>% 
  sf::st_bbox(crs = original_raster_crs)

region_shape_moll <- region_shape %>% 
  st_transform(mollweide)

region_shape_moll %>% 
  ggplot() +
  geom_sf()

abunds_table <- vroom("data/big/abunds_table.csv",
                      col_types = cols(
                        .default = "c"
                      )) %>% 
  mutate(abundance = parse_number(abundance),
         abundance = coalesce(abundance, 0),
         #round to get rid of discrepancies
         x_num = parse_number(x) %>% round(2),
         y_num = parse_number(y) %>% round(2)) %>% 
  arrange(common_name, x, y)

abunds_table %>% 
  group_by(common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears == 0)

false_appearance <- abunds_table %>% 
  group_by(common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears == 0) %>% 
  ungroup() %>% 
  distinct(common_name)

abunds_table %>% 
  semi_join(false_appearance, by = "common_name")

abunds_table %>% 
  count(common_name) %>% 
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
  filter(common_name %in% c("Common Shelduck", "Red-necked Stint")) %>% 
  distinct(x_num, y_num) %>% 
  count(x_num, y_num, sort = T) %>% 
  ggplot(aes(x_num, y_num, size = n)) +
  geom_point()

#each x,y occurs once within a species
abunds_table %>% 
  distinct(common_name, x, y) %>% 
  count(common_name, x, y) %>% 
  distinct(n)

#each date occurs once within a species
abunds_table %>% 
  distinct(common_name, date) %>% 
  count(common_name, date) %>% 
  distinct(n)

#each x, y occurs once within a date
abunds_table %>% 
  distinct(date, x, y) %>% 
  count(date, x, y) %>% 
  distinct(n)

#compare coordinates across species
geo_id_table <- abunds_table %>% 
  select(common_name, x, y) %>% 
  mutate(geo_id = str_c(x, y, sep = "_"))

geo_id_table %>% 
  count(geo_id, sort = T) %>% 
  distinct(n)

geo_id_table %>% 
  group_by(geo_id) %>% 
  filter(n() == 104) %>% 
  ungroup() %>% 
  distinct(common_name)

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
  filter(common_name == "Song Sparrow") %>% 
  #select(-comName) %>% 
  mutate(x_len = str_length(x),
         y_len = str_length(y))
  
test_2 <- geo_id_table %>% 
  filter(common_name == "Common Shelduck") %>% 
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
  select(common_name, x, y) %>% 
  full_join(select(test_2, common_name, x, y),
            by = c("x", "y"))%>% 
  View()

all.equal(test_1, test_2)

#create abundance summary
abundance_summary <- abunds_table %>%
  anti_join(false_appearance, by = "common_name") %>% 
  group_by(common_name, month, x_num, y_num) %>% 
  summarize(abundance = mean(abundance, na.rm = T)) %>%
  ungroup() %>% 
  mutate(species_id = str_c(common_name, sep = "_"),
         geo_id = str_c(x_num, y_num, sep = "_")) %>% 
  st_as_sf(coords = c("x_num", "y_num"), crs = mollweide) %>% 
  #st_filter(region_shape_moll, join = st_intersects) %>% 
  select(-species_id)

#each species has 11148 rows
abundance_summary %>% 
  st_drop_geometry() %>% 
  count(common_name) %>% 
  distinct(n)

#no missing common_name
abundance_summary %>% 
  filter(is.na(common_name))

#no missing months
abundance_summary %>% 
  filter(is.na(common_name))

abundance_summary %>%
  filter(common_name == "Ovenbird"#,
         #month == "Sep"
         ) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  ggplot() +
  geom_sf(data = region_shape_moll, color = "black") +
  geom_sf(aes(color = abundance)) +
  scale_color_viridis_c() +
  facet_wrap(~month)

#no issue with duplicate x,y
abundance_summary %>%
  filter(common_name %in% c("Ovenbird", "American Black Duck"),
         month == "Sep") %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  separate(geo_id, into = c("x", "y"), sep = "_") %>% 
  mutate(across(c(x, y), parse_number)) %>% 
  ggplot(aes(x, y)) +
  geom_jitter(alpha = .3, width = 10^3.5, height = 10^3.5) +
  facet_wrap(~common_name, ncol = 1)

#rm(abunds_table)

pairwise_dist_f <- function(x){
  
  pairwise_dist(tbl = x, item = geo_id, feature = common_name, value = abundance,
                diag = T, upper = T)
  
}

pairwise_corr_f <- function(x){
  
  pairwise_cor(tbl = x, item = geo_id, feature = common_name, value = abundance,
               diag = T, upper = T)
}

#each geo_id_1 in similarity_index should have 929 geo_id_2s
abundance_summary %>% 
  st_drop_geometry() %>% 
  distinct(geo_id) %>% 
  nrow()

similarity_index <- abundance_summary %>% 
  st_drop_geometry() %>% 
  #filter(month == "Apr") %>% 
  select(month, geo_id, common_name, abundance) %>% 
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

#929 geo_id_1s
similarity_index %>% 
  distinct(geo_id_1) %>% 
  count(geo_id_1) %>% 
  nrow()

#929 geo_id_1s
similarity_index %>% 
  distinct(geo_id_2) %>% 
  count(geo_id_2) %>% 
  nrow()

#each geo_id_1 appears 11148 
similarity_index %>% 
  count(geo_id_1) %>% 
  distinct(n)

#each geo_id_2 appears 11148 
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
  geom_sf(aes(color = correlation, stroke = compare_geo_id)) + 
  #geom_sf(data = test_geo_id_1_sf, color = "red") +
  facet_wrap(~geo_id_1) +
  scale_color_viridis_c() +
  scale_discrete_manual(
    aesthetics = "stroke",
    values = c(1, .1)
  )
  


similarity_index %>% 
  write_csv("data/big/similarity_index.csv")