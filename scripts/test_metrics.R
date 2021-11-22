source("scripts/functions/pull_species_metric.R")

library(ggtern)
library(ggforce)
library(concaveman)
library(patchwork)

species_table <- crossing(species = c("Northern Cardinal", 
                             "Pileated Woodpecker", 
                             "Red-bellied Woodpecker",
                             "Hairy Woodpecker",
                             "Downy Woodpecker"
                             ),
               metric = c("count", "abundance", "occurrence"))

species_table <- species_table %>% 
  mutate(data = map2(species, metric, ~get_species_metric(.x, .y)))

species_table_unnested <- species_table %>%
  unnest(data)

glimpse(species_table_unnested)

species_table_unnested %>% 
  #filter(metric_desc == "count") %>% 
  ggplot(aes(value, fill = species)) +
  geom_histogram() +
  facet_wrap(~metric_desc, scales = "free")

plot_1 <- species_table_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  ggplot(aes(occurrence, count, color = species, fill = species)) +
  #geom_mark_hull(alpha = .2, color = NA) +
  geom_point(alpha = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 3)) +
  theme_bw()

plot_2 <- species_table_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  ggplot(aes(occurrence, abundance, color = species, fill = species)) +
  #geom_mark_hull(alpha = .2, color = NA) +
  geom_point(alpha = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 3)) +
  theme_bw()

plot_3 <- species_table_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  ggplot(aes(count, abundance, color = species, fill = species)) +
  geom_abline() +
  #geom_mark_hull(alpha = .2, color = NA) +
  geom_point(alpha = 1) +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0, 3)) +
  theme_bw()

layout <- "
AACC
BBCC
"
plot_1 + plot_2 + plot_3 + plot_layout(guides = 'collect', design = layout)

species_table_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  select(species, month, count, occurrence, abundance) %>% 
  mutate(test = count * occurrence) %>% 
  ggplot(aes(abundance, test)) +
  geom_point() +
  geom_abline() +
  tune::coord_obs_pred()


test_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  #filter(month == "Jan") %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  ggtern(aes(occurrence, count, abundance, color = species)) +
  geom_point() +
  #geom_encircle() +
  scale_color_viridis_d() +
  Tlab("Count") + Llab("Occurrence") + Rlab("Abundance") +
  Tarrowlab("Higher Count") + Larrowlab("Higher Occurrence %") + Rarrowlab("Higher Abundance") +
  theme_bw() +
  theme_showarrows()

test_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  #filter(month == "Jan") %>% 
  ggtern(aes(occurrence, count, abundance, fill = species)) +
  stat_density_tern(
    geom='polygon',
    aes(alpha=..level..),
    bins=5,
    color=NA) +
  scale_fill_viridis_d()  +
  #facet_wrap(~month) +
  theme_bw()

test_unnested %>% 
  select(species, month, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, month),
              names_from = metric_desc,
              values_from = value) %>% 
  ggplot(aes(month, count, color = species, group = species)) +
  geom_line()
