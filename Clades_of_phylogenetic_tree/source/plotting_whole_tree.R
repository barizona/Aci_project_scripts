library(tidyverse)
library(tidytree)
library(ggtree)
library(ggnewscale)
#xxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Plot the whole tree as dated_tree_with_state_changes.pdf ----------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxx
# Input ---------------------------------------------------------------------
#xxxxxxxxxxxx
tree_tbl <- readRDS("input/tree_tbl.rds")

# create new names for city_pooled
tree_tbl %>% 
  group_by(city_pooled) %>%
  group_keys() %>% 
  write_tsv("output/city_pooled.tsv")

tbl_factors <- read_tsv("output/city_pooled_filled.tsv")

tree_tbl <- tbl_factors %>% 
  left_join(tree_tbl, ., by = "city_pooled") %>% 
  # convert to city_pooled_new to factor
  mutate(city_pooled_new = factor(city_pooled_new, levels = tbl_factors$city_pooled_new))
  
tree <-  as.treedata(tree_tbl)

min_date <- min(tree_tbl$collection_day, na.rm = TRUE)
max_date <- max(tree_tbl$collection_day, na.rm = TRUE)


#xxxxxxxxxxxx
# Big tree plot -----------------------------------------------------------
#xxxxxxxxxxxx
## basic tree ----
p <- ggtree(tree, aes(color = city_pooled_new), mrsd = max_date) +
  theme_tree2() +
  scale_x_ggtree(breaks = seq(from = year(min_date), to = year(max_date), by = 2)) +
  scale_color_manual(values = tbl_factors$color_new, limits = tbl_factors$city_pooled_new) +
  geom_label(aes(label = city_pooled_new), size = 2, alpha = 0.5) +
  geom_tiplab(align = TRUE, size = 2, alpha = 1) +
  theme(legend.position = "none")

ggsave(
  file = "output/dated_tree_with_state_changes_new_colours.pdf", p, 
  height = 0.25*nrow(tree_tbl),
  width = 0.01*nrow(tree_tbl),
  units = "cm",
  limitsize = FALSE
)

