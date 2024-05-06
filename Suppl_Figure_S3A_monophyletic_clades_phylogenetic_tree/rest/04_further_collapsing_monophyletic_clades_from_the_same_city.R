library(tidyverse)
library(tidytree)
library(ggtree)
library(ape)
library(adephylo)
#xxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Further collapsing monophyletic clades -----------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx

all_clade_tips_v2 <- listTips(tree_data_collapsed@phylo)

nodes_monophyletic_same_city <- c()
nodes_monophyletic_same_city_names <- c()
nodes_monophyletic_diff_city <- c()
for(i in 1:length(all_clade_tips_v2)) {
  cities <- tree_tbl_collapsed %>% 
    filter(label %in% names(all_clade_tips_v2[[i]])) %>% 
    filter(!is.na(city_pooled_new)) %>%
    group_by(city_pooled_new) %>% 
    group_keys() %>% 
    pull()
  if(length(cities) == 1) {
    nodes_monophyletic_same_city <- c(nodes_monophyletic_same_city, names(all_clade_tips_v2)[i])
    # add city as name to the node
    nodes_monophyletic_same_city_names <- c(nodes_monophyletic_same_city_names, cities)
  } else {
    nodes_monophyletic_diff_city <- c(nodes_monophyletic_diff_city, names(all_clade_tips_v2)[i])
  }
}
rm(i)
nodes_monophyletic_same_city
NULL
rm(all_clade_tips_v2, nodes_monophyletic_same_city, nodes_monophyletic_same_city_names)
