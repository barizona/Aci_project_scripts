library(tidyverse)
library(tidytree)
library(ggtree)
library(ape)
library(adephylo) # listTips

#xxxxxxxxxxxxxxxxxxxxx
# AIM: Fig S3 A: Collapsing those clades that are monophyletic and all tips from the same city and plot --------
#xxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxx
# Input ---------------------------------------------------------------------
#xxxxxxxxxxxx

tree_tbl <- readRDS("input/tree_tbl.rds")

# create new names for city_pooled
tree_tbl %>% 
  group_by(city_pooled) %>%
  group_keys() %>% 
  write_tsv("output/city_pooled.tsv")

# manually add these colours to the output/city_pooled_filled.tsv
# HU colours
interpolated_palette <- c("darkred", "darkorange", "khaki3", "orchid1", "orchid4") %>%
  colorRampPalette()
interpolated_palette(15)
# [1] "#8B0000" "#AC2800" "#CD5000" "#EE7800" "#F79410" "#E9A431" "#DBB552" "#CDC673"
# [9] "#DBB299" "#E99FC0" "#F78CE6" "#EE7AE9" "#CD69C9" "#AC58A9" "#8B4789"

# RO colours
interpolated_palette <- c("midnightblue", "mediumblue", "skyblue", "turquoise3", "steelblue4") %>%
  colorRampPalette()
interpolated_palette(11)
# [1] "#191970" "#0F0F95" "#0505BA" "#1B29D3" "#517BDF" "#87CEEB" "#50CADE" "#1AC6D3"
# [9] "#0AB1BF" "#208AA5" "#36648B"

# RS colours
interpolated_palette <- c("#00441B", "darkgreen", "olivedrab1", "seagreen") %>%
  colorRampPalette()
interpolated_palette(7)
# [1] "#00441B" "#00540D" "#006400" "#60B11F" "#C0FF3E" "#77C54A" "#2E8B57"

# BA colours
# "peru", "saddlebrown"

rm(interpolated_palette)

tbl_factors <- read_tsv("output/city_pooled_filled.tsv")

tree_tbl <- tbl_factors %>% 
  left_join(tree_tbl, ., by = "city_pooled") %>% 
  # convert to city_pooled_new to factor
  mutate(city_pooled_new = factor(city_pooled_new, levels = tbl_factors$city_pooled_new))

tree <-  as.treedata(tree_tbl)

#xxxxxxxxxxxxxxxxxxxxx
# Monophyletic clades from the same city ----------------------------------
#xxxxxxxxxxxxxxxxxxxxx

all_clade_tips <- listTips(tree@phylo)

nodes_monophyletic_same_city <- c()
nodes_monophyletic_same_city_names <- c()

for(i in 1:length(all_clade_tips)) {
  cities <- tree_tbl %>% 
    filter(label %in% names(all_clade_tips[[i]])) %>% 
    filter(!is.na(city_pooled_new)) %>%
    group_by(city_pooled_new) %>% 
    group_keys() %>% 
    pull()
  if(length(cities) == 1) {
    nodes_monophyletic_same_city <- c(nodes_monophyletic_same_city, names(all_clade_tips)[i])
    # add city as name to the node
    nodes_monophyletic_same_city_names <- c(nodes_monophyletic_same_city_names, cities)
  } 
}

# add names to the nodes
names(nodes_monophyletic_same_city) <- nodes_monophyletic_same_city_names

# convert to node nrs.
nodes_monophyletic_same_city_nr <- tree_tbl %>% 
  filter(label %in% nodes_monophyletic_same_city) %>%
  select(node) %>%
  pull()
names(nodes_monophyletic_same_city_nr) <- names(nodes_monophyletic_same_city)

rm(i, nodes_monophyletic_same_city_names, nodes_monophyletic_same_city, 
   all_clade_tips, cities)

#xxxxxxxxxxxxxxxxx
## Remove nodes that are contained in other nodes -----
#xxxxxxxxxxxxxxxxx

nodes_monophyletic_same_city_nr_v2 <- nodes_monophyletic_same_city_nr

for(i in 1:length(nodes_monophyletic_same_city_nr)) {
  n <- tree_tbl %>% 
    filter(node == nodes_monophyletic_same_city_nr[i]) %>%
    filter(parent %in% nodes_monophyletic_same_city_nr) %>% 
    nrow()
  if(n > 0) { nodes_monophyletic_same_city_nr_v2[i] <- NA }
  rm(n)
}

# remove NAs
nodes_monophyletic_same_city_nr_v2 <- 
  nodes_monophyletic_same_city_nr_v2[!is.na(nodes_monophyletic_same_city_nr_v2)]

# convert to node names
nodes_monophyletic_same_city_v2 <- tree_tbl %>% 
  filter(node %in% nodes_monophyletic_same_city_nr_v2) %>%
  select(label) %>%
  pull()
names(nodes_monophyletic_same_city_v2) <- names(nodes_monophyletic_same_city_nr_v2)

rm(i, nodes_monophyletic_same_city_nr, nodes_monophyletic_same_city_nr_v2)

#xxxxxxxxxxxxxxxxxxxxx
## Drop tips from the tree that are in the monophyletic clades from the same city ----
#xxxxxxxxxxxxxxxxxxxxx
# all tips to drop
tips_to_drop_all <- all_clade_tips[nodes_monophyletic_same_city_v2] %>% 
  unlist() %>% 
  names() %>%
  # remove text before .
  sub(".*?\\.", "", .)

# keep the first tip of every nodes_monophyletic_same_city_v2 clade
tips_to_keep <- all_clade_tips[nodes_monophyletic_same_city_v2] %>% 
  map(~ .[1]) %>% 
  unlist() %>% 
  names() %>%
  # remove text before .
  sub(".*?\\.", "", .)

tips_to_drop <- setdiff(tips_to_drop_all, tips_to_keep)

# all tips to drop
tree_collapsed <- drop.tip(phy = tree@phylo, tip = tips_to_drop)

# convert to tibble and add data
tree_tbl_collapsed <- as_tibble(tree_collapsed) %>% 
  # merge with tree_tbl
  left_join(., select(tree_tbl, -parent, -node, -branch.length), by = "label") %>% 
  # add a new column indicating if the tip belongs to a collapsed monophyletic clade
  mutate(collapsed = ifelse(label %in% tips_to_keep, TRUE, FALSE))

# for the collapsed clades, change the label to Clade_1: nr. of tips, Clade_2: nr. of tips, etc.
tips_to_keep_clade_names <- paste("Clade", 1:length(tips_to_keep), sep = " ")
# paste the number of tips in the clade
tips_to_keep_clade_names <- paste0(tips_to_keep_clade_names, " - ", 
                                   map_int(all_clade_tips[nodes_monophyletic_same_city_v2], 
                                           length))
names(tips_to_keep_clade_names) <- tips_to_keep

tree_tbl_collapsed <- tips_to_keep_clade_names %>% 
  enframe(name = "label", value = "clade_label") %>% 
  left_join(tree_tbl_collapsed, ., by = "label")

# overwrite the label column with the clade_label column when collapsed == TRUE
tree_tbl_collapsed$label <- ifelse(tree_tbl_collapsed$collapsed, 
                                   tree_tbl_collapsed$clade_label, 
                                   tree_tbl_collapsed$label)

# convert to tree data
tree_data_collapsed <-  as.treedata(tree_tbl_collapsed)

rm(tips_to_drop_all, tips_to_keep, tips_to_drop, tips_to_keep_clade_names, tree, 
   tree_tbl, tree_collapsed, nodes_monophyletic_same_city_v2)

#xxxxxxxxxxxxxxxxxxx
# Plot --------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxx

min_date <- min(tree_tbl_collapsed$collection_day, na.rm = TRUE)
max_date <- max(tree_tbl_collapsed$collection_day, na.rm = TRUE)


#xxxxxxxx
## fan tree using different colours with legend without labels ----
#xxxxxxxx
p_collapsed_fan <- tree_data_collapsed %>% 
  ggtree(aes(color = city_pooled_new), mrsd = max_date, layout = "fan", 
         open.angle = 7) +
  theme_tree2(text = element_text(size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = tbl_factors$color_new, 
                     limits = tbl_factors$city_pooled_new) +
  # add "normal" tip names
  geom_tiplab(aes(subset = collapsed == FALSE), align = TRUE, family = "Arial", 
              size = 2, offset = 12) +
  # add "collapsed" tip names
  geom_tiplab(aes(subset = collapsed == TRUE), align = TRUE, family = "Arial", 
              fontface = "italic", size = 2, offset = 0) +
  scale_x_ggtree(breaks = seq(from = 1975, to = year(max_date)+1, 
                              by = 5)) +
  # remove "a"-s from legend (change them to space \U00A0)
  guides(color = guide_legend(override.aes = list(label = "\U00A0", 
                                                  family = "Arial",
                                                  size = 0, 
                                                  linewidth = 1), 
                              ncol = 8)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        # add dotted line to dates
        panel.grid.major.x = element_line(color = "grey20", 
                                          linetype = "dotted", linewidth = 0.3))

# rotate the tree to fit the time scale
p_collapsed_fan <- p_collapsed_fan %>% 
  rotate_tree(2)

ggsave(file = "output/dated_tree_with_state_changes_collapsed_fan.pdf", 
       p_collapsed_fan,
       height = 0.05*nrow(tree_tbl_collapsed),
       width = 0.04*nrow(tree_tbl_collapsed),
       units = "cm", limitsize = FALSE, device = cairo_ps)

# the saved pdf is opened in Okular, saved as EPS, then opened with Inksape with
# "Replace PDF fonts by the closest-named installed fonts" which replaced
# ArialMS fonts to Arial. Then the timescale were installed by hand.

#xxxxxxxx
## save a basic tree to edit the time scale on fan tree ----
#xxxxxxxx
p_collapsed_basic <- tree_data_collapsed %>% 
  ggtree(aes(color = city_pooled_new), mrsd = max_date) +
  theme_tree2(text = element_text(size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = tbl_factors$color_new, 
                     limits = tbl_factors$city_pooled_new) +
  # add "normal" tip names
  geom_tiplab(aes(subset = collapsed == FALSE), align = TRUE, size = 2, 
              alpha = 1, offset = 12) +
  # add "collapsed" tip names
  geom_tiplab(aes(subset = collapsed == TRUE), align = TRUE, size = 2, 
              alpha = 1, offset = 0, fontface = "italic") +
  scale_x_ggtree(breaks = seq(from = 1975, to = year(max_date)+1, 
                              by = 5)) +
  # remove "a"-s from legend (change them to space \U00A0)
  guides(color = guide_legend(override.aes = list(label = "\U00A0", size = 0, 
                                                  linewidth = 2), ncol = 8)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        # add dotted line to dates
        panel.grid.major.x = element_line(color = "grey20", 
                                          linetype = "dotted", linewidth = 0.3))

ggsave(file = "output/dated_tree_with_state_changes_collapsed.pdf", 
       p_collapsed_basic,
       height = 0.15*nrow(tree_tbl_collapsed),
       width = 0.08*nrow(tree_tbl_collapsed),
       units = "cm", limitsize = FALSE)

sessionInfo() %>%
  capture.output() %>%
  writeLines("output/sessionInfo.txt")
