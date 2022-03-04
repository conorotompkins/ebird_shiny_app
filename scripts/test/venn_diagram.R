library(tidyverse)
library(vroom)
library(utils)
library(tools)
library(lubridate)
library(ggVennDiagram)
library(sf)
library(janitor)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/create_venn_diagram.R")

genes <- paste("gene",1:1000,sep="")
set.seed(20210419)
x <- list(A=sample(genes,300),
          B=sample(genes,525),
          C=sample(genes,440),
          D=sample(genes,350))

ggVennDiagram(x, show_intersect = T)

base_venn <- ggVennDiagram(x)

str(base_venn)

ggVennDiagram(x) + scale_fill_gradient(low="blue",high = "red")

input_month <- "Mar"

similarity_index <- vroom("data/big/similarity_index.csv") %>% 
  filter(month == input_month)

similarity_geo <- similarity_index %>% 
  prep_similarity_index(45) %>% 
  mutate(month = fct_relevel(month, month.abb))

similarity_geo %>% 
  distinct(x, y, geo_index_compare) %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text()

abunds_table <- vroom("data/big/abunds_table.csv") %>% 
  filter(month == input_month)

abunds_table %>% 
  count(month)

reference_id <- 45

reference <- similarity_geo %>% 
  filter(geo_index_reference == reference_id) %>% 
  distinct(geo_index_reference, geo_id_reference) %>% 
  pull(geo_id_reference)

compare_id <- 238

compare <- similarity_geo %>% 
  filter(geo_index_compare == compare_id) %>% 
  distinct(geo_index_compare, geo_id_compare) %>% 
  pull(geo_id_compare)

abunds_table %>%
  distinct(geo_id, x, y) %>% 
  mutate(flag = geo_id %in% c(reference, compare)) %>% 
  ggplot(aes(x, y, color = flag)) +
  geom_point()

reference_list <- abunds_table %>% 
  filter(geo_id == reference) %>% 
  group_by(common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(common_name) %>% 
  pull(common_name)

compare_list <- abunds_table %>% 
  filter(geo_id == compare) %>% 
  group_by(common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(common_name) %>% 
  pull(common_name)

venn_list <- list("Reference" = reference_list, "Compare" = compare_list)

ggVennDiagram(venn_list) +
  labs(fill = "Distinct Species") +
  scale_fill_stepsn(colors = grey.colors(5),
                    breaks = c(seq(from = 0, to = 90, by = 20), Inf),
                    guide = guide_colorbar(direction = "horizontal",
                                           title.position = "bottom")) +
  # scale_fill_gradient_n(low = "grey",
  #                     high = "black",
  #                     guide = guide_colorbar(direction = "horizontal",
  #                                        title.position = "bottom")) +
  scale_color_manual(values = c("#FFFFFF", "#FFFFFF")) +
  theme(legend.position = "bottom")

reference_df <- abunds_table %>% 
  filter(geo_id == reference) %>% 
  group_by(family_common_name, common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(family_common_name, common_name) %>% 
  mutate(type = "Reference")

compare_df <- abunds_table %>% 
  filter(geo_id == compare) %>% 
  group_by(family_common_name, common_name) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(family_common_name, common_name) %>% 
  mutate(type = "Compare")

reference_df_segment <- anti_join(reference_df, compare_df, by = c("family_common_name", "common_name"))

compare_df_segment <- anti_join(compare_df, reference_df, by = c("family_common_name", "common_name"))

both_df_segment <- reference_df %>% 
  semi_join(compare_df, by = c("family_common_name", "common_name")) %>% 
  mutate(type = "Both")

reference_df %>% 
  semi_join(compare_df, by = c("family_common_name", "common_name"))

compare_df %>% 
  semi_join(reference_df, by = c("family_common_name", "common_name"))

reference_df %>% 
  anti_join(compare_df, by = c("family_common_name", "common_name"))

compare_df %>% 
  anti_join(reference_df, by = c("family_common_name", "common_name"))

venn_df <- bind_rows(reference_df_segment, compare_df_segment, both_df_segment)

venn_df %>% 
  count(type, family_common_name) %>% 
  arrange(type, desc(n)) %>% 
  group_by(type) %>% 
  summarize(n = sum(n)) %>% 
  adorn_totals(where = "row")

create_venn_diagram(reference_id, compare_id, similarity_df = similarity_geo, table = abunds_table)

Venn(venn_list) %>% 
  process_data() %>% 
  .@region %>% 
  distinct(name)

Venn(venn_list) %>% 
  process_data() %>% 
  .@region %>% 
  mutate(pct = count / sum(count),
         pct = round(pct * 100, 1),
         venn_label = str_c(count, "\n", pct, "%")) %>% 
  ggplot(aes(fill = name)) +
  geom_sf() +
  geom_sf_label(aes(label = venn_label))





