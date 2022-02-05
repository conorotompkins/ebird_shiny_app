library(tidyverse)
library(vroom)
library(utils)
library(tools)
library(lubridate)
library(ggVennDiagram)
library(sf)

source("scripts/functions/prep_similarity_index.R")
source("scripts/functions/create_venn_diagram.R")


genes <- paste("gene",1:1000,sep="")
set.seed(20210419)
x <- list(A=sample(genes,300),
          B=sample(genes,525),
          C=sample(genes,440),
          D=sample(genes,350))

ggVennDiagram(x) + scale_fill_gradient(low="blue",high = "red")

similarity_index <- vroom("data/big/similarity_index.csv")

similarity_geo <- similarity_index %>% 
  prep_similarity_index(45) %>% 
  mutate(month = fct_relevel(month, month.abb))

similarity_geo %>% 
  distinct(x, y, geo_index_compare) %>% 
  ggplot(aes(x, y, label = geo_index_compare)) +
  geom_text()

abunds_table <- vroom("data/big/abunds_table.csv")

reference <- similarity_geo %>% 
  filter(geo_index_reference == 45) %>% 
  distinct(geo_index_reference, geo_id_reference) %>% 
  pull(geo_id_reference)

compare <- similarity_geo %>% 
  #mutate(geo_id_compare = str_c(x, y, sep = "_")) %>% 
  filter(geo_index_compare == 26) %>% 
  distinct(geo_index_compare, geo_id_compare) %>% 
  pull(geo_id_compare)

abunds_table %>%
  distinct(geo_id, x, y) %>% 
  mutate(flag = geo_id %in% c(reference, compare)) %>% 
  ggplot(aes(x, y, color = flag)) +
  geom_point()



reference_list <- abunds_table %>% 
  filter(geo_id == reference) %>% 
  group_by(comName) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(comName) %>% 
  pull(comName)

compare_list <- abunds_table %>% 
  filter(geo_id == compare) %>% 
  group_by(comName) %>% 
  summarize(appears = sum(abundance > 0)) %>% 
  filter(appears > 0) %>% 
  ungroup() %>% 
  distinct(comName) %>% 
  pull(comName)

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

create_venn_diagram(45, 200, similarity_df = similarity_geo, table = abunds_table)
