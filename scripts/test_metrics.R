source("scripts/functions/pull_species_metric.R")

library(ggtern)
library(ggforce)
library(concaveman)
library(patchwork)
library(hrbrthemes)

theme_set(theme_ipsum())

species_table <- crossing(species = c("Northern Cardinal"),
                          metric = c("occurrence", "count", "abundance"),
                          resolution = c("hr"))

species_metrics <- species_table %>% 
  mutate(data = pmap(list(species, metric, resolution), ~get_species_metric(..1, ..2, ..3))) %>% 
  mutate(resolution = fct_relevel(resolution, c("hr", "mr", "lr"))) %>% 
  arrange(species, metric, resolution)

species_metrics_unnested <- species_metrics %>%
  unnest(data)

glimpse(species_metrics_unnested)

species_metrics_unnested %>% 
  #filter(metric_desc == "count") %>% 
  ggplot(aes(value, fill = species)) +
  geom_histogram() +
  facet_wrap(~metric_desc, scales = "free")

species_metric_wide <- species_metrics_unnested %>% 
  select(species, date, x, y, metric_desc, value) %>% 
  pivot_wider(id_cols = c(species, date, x, y, metric_desc),
              names_from = metric_desc,
              values_from = value) 

species_metric_wide %>% 
  ggplot(aes(occurrence, count)) +
  geom_point()

plot_1 <- species_metric_wide %>% 
  drop_na(occurrence, count) %>% 
  ggplot(aes(occurrence, count)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  scale_x_percent() +
  coord_cartesian(ylim = c(0, 12)) +
  guides(fill = "none") +
  theme_bw()

plot_2 <- species_metric_wide %>% 
  drop_na() %>% 
  ggplot(aes(occurrence, abundance)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  scale_x_percent() +
  coord_cartesian(ylim = c(0, 12)) +
  guides(fill = "none") +
  theme_bw()

plot_3 <- species_metric_wide %>%
  drop_na() %>% 
  ggplot(aes(count, abundance)) +
  geom_density_2d_filled(contour_var = "ndensity") +
  geom_abline() +
  coord_cartesian(xlim = c(0, 12),
                  ylim = c(0, 12)) +
  guides(fill = "none") +
  theme_bw()

layout <- "
AACC
BBCC
"
plot_1 + plot_2 + plot_3 + 
  plot_layout(guides = 'collect', design = layout) +
  plot_annotation(title = "Northern Cardinal in Allegheny County Pennsylvania")

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
