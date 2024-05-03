library(tidyverse)
library(tidytree)
library(ggtree)
library(ggnewscale)
source("source/get_colors.R")
#xxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Plot 16 clades of dated_tree_with_state_changes.pdf ----------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Clades to zoom in -------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Get nodes of clades
nodes <- read_tsv("input/longest_chain_from_closest_transmission_no_singletons.tsv") %>% 
  filter(group == "Non-focal") %>% 
  select(closest_sc_node) %>% 
  arrange(closest_sc_node) %>%
  pull()

# Plot clades one-by-one to be able to save it with proper size
# change from 1 to 16

# height of file
h = c(4, 14, 30, 5, 16, 20, 16, 4, 60, 4, 5, 14, 5, 7, 4, 5)
# width of file
w = c(7, 12, 20, 7, 10, 15, 12, 4, 20, 7, 6, 10, 7, 7, 4, 7)

for(n in 1:length(nodes)) {
  tree_clade <- tree_subset(tree, node = nodes[n], levels_back = 0)
  tree_tbl_clade <- as_tibble(tree_clade)
  
  ## basic tree ----
  p_clade <- ggtree(tree_clade, aes(color = get(args$target)), mrsd = max_date, size = 1) +
    theme_tree2() +
    scale_x_ggtree(breaks = seq(from = year(min_date), to = year(max_date)+1, by = 1)) +
    scale_color_manual(values = geo_cols$color, limits = geo_cols[[args$target]]) +
    geom_label(aes(label = city_pooled), size = 4, alpha = 0.5, vjust = -0.5, hjust = 0) +
    geom_tiplab(align = TRUE, size = 4, alpha = 1, hjust = 0, offset = 2) +
    # add space to tiplab
    hexpand(0.3) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14, angle = 90, vjust = 0.5, hjust = 1))
  
  ggsave(file = paste0("output/dated_tree_with_state_changes_clade_", nodes[n], ".png"), 
         p_clade, height = h[n], width = w[n], limitsize = FALSE)
  
  ggsave(file = paste0("output/dated_tree_with_state_changes_clade_", nodes[n], ".pdf"), 
         p_clade, height = h[n], width = w[n], limitsize = FALSE)
}

rm(n)
