library(tidyverse)
library(tidytree)
library(ggtree)
library(ape)
library(adephylo)
#xxxxxxxxxxxxxxxxxxxxx
# AIM: Collapsing those clades that are monophyletic and all tips from the same city --------
#xxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxx
# Monophyletic clades from the same city ----------------------------------
#xxxxxxxxxxxxxxxxxxxxx

all_clade_tips <- listTips(tree@phylo)

names(all_clade_tips) %>% head()
# "Node_1" "Node_2" "Node_3" "Node_4" "Node_5" "Node_6"
all_clade_tips$Node_1 %>% head()
# GCF_000805035.1 GCF_001432755.1 GCF_021175075.1 GCA_016480565.1 GCA_016489465.1 
# 7169               1               7               2               6 
# GCA_016489985.1 
# 5 

length(all_clade_tips)
# 7719

# lengths
all_clade_tips_lengths <- lapply(all_clade_tips, length)

nodes_monophyletic_same_city <- c()
nodes_monophyletic_same_city_names <- c()
nodes_monophyletic_diff_city <- c()
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
  } else {
    nodes_monophyletic_diff_city <- c(nodes_monophyletic_diff_city, names(all_clade_tips)[i])
  }
}
rm(i)
length(nodes_monophyletic_same_city)
# 7442
length(nodes_monophyletic_same_city_names)
# 7442
length(nodes_monophyletic_diff_city)
# 277
length(nodes_monophyletic_same_city) + length(nodes_monophyletic_diff_city) == length(all_clade_tips)
# TRUE

# add names to the nodes
names(nodes_monophyletic_same_city) <- nodes_monophyletic_same_city_names

rm(nodes_monophyletic_same_city_names, nodes_monophyletic_diff_city)

# convert to node nrs.
nodes_monophyletic_same_city_nr <- tree_tbl %>% 
  filter(label %in% nodes_monophyletic_same_city) %>%
  select(node) %>%
  pull()

names(nodes_monophyletic_same_city_nr) <- names(nodes_monophyletic_same_city)

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
rm(i)
# remove NAs
nodes_monophyletic_same_city_nr_v2 <- nodes_monophyletic_same_city_nr_v2[!is.na(nodes_monophyletic_same_city_nr_v2)]

length(nodes_monophyletic_same_city_nr_v2)
# 161

# convert to node names
nodes_monophyletic_same_city_v2 <- tree_tbl %>% 
  filter(node %in% nodes_monophyletic_same_city_nr_v2) %>%
  select(label) %>%
  pull()

names(nodes_monophyletic_same_city_v2) <- names(nodes_monophyletic_same_city_nr_v2)

#xxxxxxxxxxxxxxxxxxxxx
## Drop tips from the tree that are in the monophyletic clades from the same city
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
tips_to_keep_clade_names <- paste0(tips_to_keep_clade_names, " - ", map_int(all_clade_tips[nodes_monophyletic_same_city_v2], length))
names(tips_to_keep_clade_names) <- tips_to_keep

tree_tbl_collapsed <- tips_to_keep_clade_names %>% 
  enframe(name = "label", value = "clade_label") %>% 
  left_join(tree_tbl_collapsed, ., by = "label")

# overwrite the label column with the clade_label column when collapsed == TRUE
tree_tbl_collapsed$label <- ifelse(tree_tbl_collapsed$collapsed, tree_tbl_collapsed$clade_label, tree_tbl_collapsed$label)

# convert to tree data
tree_data_collapsed <-  as.treedata(tree_tbl_collapsed)

min_date <- min(tree_tbl_collapsed$collection_day, na.rm = TRUE)
max_date <- max(tree_tbl_collapsed$collection_day, na.rm = TRUE)

#xxxxxxxxxxxxxxxxxxx
# Plot --------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxx

#xxxxxxxx
## basic tree ----
#xxxxxxxx
p_collapsed <- tree_data_collapsed %>% 
  ggtree(aes(color = city_pooled_new), mrsd = max_date) +
  theme_tree2(text = element_text(size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_ggtree(breaks = seq(from = year(min_date), to = year(max_date), by = 2)) +
  scale_color_manual(values = tbl_factors$color_new, limits = tbl_factors$city_pooled_new) +
  geom_label(aes(label = city_pooled_new), size = 2, alpha = 0.5) +
  # add "normal" tip names
  geom_tiplab(aes(subset = collapsed == FALSE), align = TRUE, size = 2, alpha = 1, offset = 3) +
  # add "collapsed" tip names
  geom_tiplab(aes(subset = collapsed == TRUE), align = TRUE, size = 2, alpha = 1, offset = 1, fontface = "bold.italic") +
  theme(legend.position = "none")

ggsave(file = "output/dated_tree_with_state_changes_collapsed.pdf", p_collapsed,
       height = 0.15*nrow(tree_tbl_collapsed),
       width = 0.08*nrow(tree_tbl_collapsed),
       units = "cm", limitsize = FALSE)

#xxxxxxxx
## fan tree using different colours with legend without labels ----
#xxxxxxxx
p_collapsed_fan_v2 <- tree_data_collapsed %>% 
  ggtree(aes(color = city_pooled_new), mrsd = max_date, layout = "fan", open.angle = 7) +
  theme_tree2(text = element_text(size = 10),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = tbl_factors$color_new, limits = tbl_factors$city_pooled_new) +
    # add "normal" tip names
  geom_tiplab(aes(subset = collapsed == FALSE), align = TRUE, size = 2, alpha = 1, offset = 12) +
  # add "collapsed" tip names
  geom_tiplab(aes(subset = collapsed == TRUE), align = TRUE, size = 2, alpha = 1, offset = 0, fontface = "italic") +
  scale_x_ggtree(breaks = seq(from = year(min_date), to = year(max_date), by = 5)) +
  # remove "a"-s from legend (change them to space \U00A0)
  guides(color = guide_legend(override.aes = list(label = "\U00A0", size = 0, linewidth = 2), ncol = 8)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 6),
        # add dotted line to dates
        panel.grid.major.x = element_line(color = "grey20", 
                                          linetype = "dotted", linewidth = 0.3))

ggsave(file = "output/dated_tree_with_state_changes_collapsed_fan.pdf", p_collapsed_fan_v2,
       height = 0.05*nrow(tree_tbl_collapsed),
       width = 0.04*nrow(tree_tbl_collapsed),
       units = "cm", limitsize = FALSE)


rm(all_clade_tips)


# # HU colours
# interpolated_palette <- c("darkred", "darkorange", "khaki3", "orchid1", "orchid4") %>% 
#   colorRampPalette()
# interpolated_palette(15)
# [1] "#8B0000" "#AC2800" "#CD5000" "#EE7800" "#F79410" "#E9A431" "#DBB552" "#CDC673"
# [9] "#DBB299" "#E99FC0" "#F78CE6" "#EE7AE9" "#CD69C9" "#AC58A9" "#8B4789"
# 
# # RO colours
# interpolated_palette <- c("midnightblue", "mediumblue", "skyblue", "turquoise3", "steelblue4") %>% 
#   colorRampPalette()
# interpolated_palette(11)
# [1] "#191970" "#0F0F95" "#0505BA" "#1B29D3" "#517BDF" "#87CEEB" "#50CADE" "#1AC6D3"
# [9] "#0AB1BF" "#208AA5" "#36648B"
# 
# # RS colours
# interpolated_palette <- c("#00441B", "darkgreen", "olivedrab1", "seagreen") %>% 
#   colorRampPalette()
# interpolated_palette(7)
# [1] "#00441B" "#00540D" "#006400" "#60B11F" "#C0FF3E" "#77C54A" "#2E8B57"
# 
# # BA colours
# "peru", "saddlebrown"

