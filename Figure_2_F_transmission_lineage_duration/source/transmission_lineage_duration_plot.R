library(tidyverse)
library(magrittr)
library(ggpubr)

#xxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Creating a Transmission lineage duration rain cloud or dotplot for ---------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxx

TLD_tab <- read_tsv("input/longest_chain_from_closest_transmission_no_singletons.tsv")

#xxxxxxxxxxxxxxxxxxxxxxxxx
# Piechart --------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxx

Introductions_tab <- read_tsv("input/Question1_introductions.tsv") %>% 
  filter(tip_count > 1)

Introductions_tab %>% 
  group_by(to_aggregated) %>% 
  summarise(mean_domestic = mean(domestic),
            mean_between_focal = mean(between_focal),
            mean_non_focal = mean(non_focal)) %>% 
  mutate(sums = rowSums(select(., -to_aggregated)))

Introductions_tab %>% 
  # create a new col: capital not_capital
  mutate(to_aggregated_v2 = case_when(grepl("not_capital", to_aggregated) ~ "not_capital",
                                      .default = "capital")) %>% 
  group_by(to_aggregated_v2) %>% 
  summarise(mean_domestic = mean(domestic),
            mean_between_focal = mean(between_focal),
            mean_non_focal = mean(non_focal)) %>% 
  mutate(sums = rowSums(select(., -to_aggregated_v2)))

Introductions_tab %>% 
  summarise(mean_domestic = mean(domestic),
            mean_between_focal = mean(between_focal),
            mean_non_focal = mean(non_focal)) %>% 
  mutate(sums = rowSums(select(., contains("mean"))))

#xxxxxxxxxxxxxxxxxxxxxxxxx
# Boxplots, wilcoxon ------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxx

City_edges_tab <- read_tsv("input/city_edges.tsv") %>% 
  filter(!is.na(Question2A))



City_edges_tab %>%
  ggplot(aes(x = Question2A, y = weight)) +
  geom_boxplot() +
  geom_jitter() +
  # wilcoxon test
  # stat_compare_means(label = "p.format") +
  stat_compare_means(comparisons = list(c("no_capital", "with_capital")), 
                     label = "p.signif") +
  theme_linedraw() +
  # remove the vertical grid lines
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


