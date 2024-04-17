library(tidyverse)
library(magrittr)
library(ggpubr) # stat_pvalue_manual
#xxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Boxplots of area under the bacterial growth curves -----------------
#xxxxxxxxxxxxxxxxxxxxxxxxxx


# Description: 
# Area under the bacterial growth curves either in the absence
# (untreated) or in the presence of different phages alone or in combinations.
# Data points represent the mean area under the bacterial growth curve value of
# three technical replicates measured with each ST2-KL3 CRAB clinical isolates
# (n = 41 distinct isolates, except for the 3-phage combinations, where n = 27).
# A.u stands for the arbitrary unit. ** p ⋜ 0.05, *** p = 0.0001, **** p <
# 0.0001 from two-sided Kruskal-Wallis test. Abbreviations: H – Highwayman, S –
# Silvergun, F – Fanak, Po – Porter, N - Navy-v2, Ph – PhT2-v2.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
B_tab <- read_tsv("input/Fig6B.tsv") %>% 
    # longer
    pivot_longer(!bacteria, names_to = "Phage", values_to = "Area") %>%
    # remove 1st column
    select(-bacteria) %>% 
    # remove rows with Area NA
    filter(!is.na(Area)) %>% 
    # convert Phage to factor
    mutate(Phage = factor(Phage, levels = c("untreated", "H", "S", "HS", "HSPo", 
                                            "HSPh", "HSN", "HSF", "HSFPo", 
                                            "HSFPh")))

#xxxxxxxxxxxxxx
## Perform Kruskal-Wallis test ----
#xxxxxxxxxxxxxx
# Perform Kruskal-Wallis tests for all pairwise combinations of groups

# create a result tab for plotting like this
x <- rstatix::t_test(data = B_tab, Area ~ Phage, 
                                        p.adjust.method = "BH")

B_kruskal_results <- combn(as.character(unique(B_tab$Phage)), 2, function(x) {
    result <- kruskal.test(Area ~ Phage, data = filter(B_tab, Phage %in% x))
    broom::tidy(result)
}, simplify = FALSE)

# Combine results into a single tibble
B_kruskal_results %<>% bind_rows(., .id = "combination") %>%
    select(-method)

#xxxxxxxxxx
### add columns to match the result of rstatix::t_test ----
#xxxxxxxxxx
# add group1 group2 columns
Combinations <- combn(as.character(unique(B_tab$Phage)), 2) %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble() %>% 
    rename(group1 = V1, group2 = V2) %>% 
    # add .y. column containing Area
    mutate(.y. = "Area") %>% 
    relocate(.y., .before = group1)

B_kruskal_results %<>% 
    select(-combination) %>%
    # rename p.value to p
    rename(p = p.value) %>%
    bind_cols(Combinations, .) %>%
    # adjusting p-values
    mutate(p.adj = p.adjust(p, method = "BH"),
           p.adj.signif = ifelse(p.adj < 0.0001, "****",
                          ifelse(p.adj < 0.001, "***",
                          ifelse(p.adj < 0.01, "**",
                          ifelse(p.adj < 0.05, "*", "ns")))))

rm(Combinations, x)

B_kruskal_results %>% 
    write_tsv("output/Fig6B_kruskal_results.tsv")

#xxxxxxxxxx
## Boxplots ----
#xxxxxxxxxx
B_tab %>% 
    ggplot() +
    geom_boxplot(aes(x = Phage, y = Area, fill = Phage, color = Phage), outliers = FALSE) +
    geom_jitter(aes(x = Phage, y = Area, fill = Phage, color = Phage), width = 0.2) +
    scale_fill_manual(values = Colour_list$Fig6B_alpha) +
    scale_color_manual(values = Colour_list$Fig6B) +
    scale_y_continuous(labels = scales::comma) +
    # significant Kruskal-Wallis tests
    stat_pvalue_manual(B_kruskal_results, label = "p.adj.signif", hide.ns = TRUE,
                       y.position = 80, tip.length = 0.01, step.increase = 0.05,
                       vjust = 1) +
    labs(x = NULL, y = expression(paste("Area under bacterial growth curve OD"[600], " (a.u.)"))) +
    theme_linedraw() +
    theme(legend.position = "none",
          # axis font size
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          # remove the horizontal grid lines
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())




