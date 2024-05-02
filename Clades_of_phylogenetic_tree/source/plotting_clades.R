library(tidyverse)
library(tidytree)
library(ggtree)
library(ggnewscale)
source("source/get_colors.R")
#xxxxxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Plot 16 clades of dated_tree_with_state_changes.pdf ----------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx

args <- list(target = "city_pooled",
             heatmap_vars = c("city_pooled", "country", "continent", 
                              "k_serotype"))

#xxxxxxxxxxxx
# Input ---------------------------------------------------------------------
#xxxxxxxxxxxx
tree_tbl <- readRDS("input/tree_tbl.rds")

index <- which(is.na(tree_tbl$branch.length))

if (length(index) != 1) {
  stop("Could not find tree root.")
}

geo_cols <- get_colors(tree_tbl[1:(index-1), ], args$target)

tree_tbl <- left_join(tree_tbl, geo_cols, by = args$target)

tree <-  as.treedata(tree_tbl)

min_date <- min(tree_tbl$collection_day, na.rm = TRUE)
max_date <- max(tree_tbl$collection_day, na.rm = TRUE)

#xxxxxxxxxxxx
# Prepare heatmap matrices and colors -----------------------------------------
#xxxxxxxxxxxx
mats <- list()
cls <- list()
for (i in 1:length(args$heatmap_vars)) {
  mats[[i]] <- as.matrix(tree_tbl[, args$heatmap_vars[i]])
  rownames(mats[[i]]) <- tree_tbl$label
  colnames(mats[[i]]) <- args$heatmap_vars[i]
  if (args$heatmap_vars[i] == args$target) {
    cls[[i]] <- geo_cols$color
    names(cls[[i]]) <- geo_cols$city_pooled
  } else {
    set.seed(0)
    coldf <-  get_colors(tree_tbl[1:(index-1), ], args$heatmap_vars[i])
    cls[[i]] <- coldf$color
    names(cls[[i]]) <- coldf[[args$heatmap_vars[i]]]
  }
}
rm(i)

index_ambiguous_nodes <- grep("\\|", tree_tbl[[args$target]])

index_transmission_nodes <- vector()
for (i in 1:nrow(tree_tbl)) {
  target_child <- tree_tbl[[args$target]][i]
  target_parent <- tree_tbl[[args$target]][which(tree_tbl$node == tree_tbl$parent[i])]
  if (target_child == target_parent) next() else {
    index_transmission_nodes <- c(index_transmission_nodes, i)
  }
}
rm(i)

index_trnodes <- which(tree_tbl$node %in% index_transmission_nodes)

trlabel <- vector()
for (i in 1:nrow(tree_tbl)) {
  if (tree_tbl$node[i] %in% index_trnodes) {
    trlabel <- c(trlabel, paste0(
      "INTRO?\n",
      tree_tbl$collection_day[which(tree_tbl$node == tree_tbl$parent[i])],
      "/",
      tree_tbl$collection_day[i]
    ))
  } else {
    trlabel <- c(trlabel, NA_character_)
  }
}
rm(i)

#xxxxxxxxxxxx
# Big tree plot -----------------------------------------------------------
#xxxxxxxxxxxx
## basic tree ----
p <- ggtree(tree, aes(color = get(args$target)), mrsd = max_date) +
  theme_tree2() +
  scale_x_ggtree(breaks = seq(from = year(min_date), to = year(max_date), by = 2)) +
  scale_color_manual(values = geo_cols$color, limits = geo_cols[[args$target]]) +
  geom_point2(aes(subset = (node %in% index_ambiguous_nodes)),
              shape = 21, size = 10, fill = "orange", alpha = 0.5) +
  geom_label2(aes(x = branch, 
                  subset = (tree_tbl$node %in% index_transmission_nodes),
                  label = trlabel),
              size = 2, col = "black", fill = "red", alpha = 0.5) +
  geom_label(aes(label = city_pooled), size = 2, alpha = 0.5) +
  geom_tiplab(align = TRUE, size = 2, alpha = 1) +
  theme(legend.position = "none")

## heatmap to tree ----
p2 <- p
for (i in 1:length(mats)) {
  p2 <- gheatmap(p2, mats[[i]], offset = 1 + (i-1) * 1, width = 0.02,
                 colnames_offset_y = 0, colnames_position = "top",
                 legend_title = args$heatmap_vars) +
  labs(fill = args$heatmap_vars[i]) +
  scale_fill_manual(values = cls[[i]])
  if (i < length(mats)) {
    p2 <- p2 + new_scale_fill()
  }
}
p2 <- p2 + theme(legend.position = "none")
rm(i)

# ggsave(
#   file = "output/dated_tree_with_state_changes.pdf",
#   height = 0.25*nrow(tree_tbl),
#   width = 0.01*nrow(tree_tbl),
#   units = "cm",
#   limitsize = FALSE
# )


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
n <- 16
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

h = c(4, 14, 30, 5, 16, 20, 16, 4, 60, 4, 5, 14, 5, 7, 4, 5)
w = c(7, 12, 20, 7, 10, 15, 12, 4, 20, 7, 6, 10, 7, 7, 4, 7)
ggsave(file = paste0("output/dated_tree_with_state_changes_clade_", nodes[n], ".png"), 
       p_clade, height = h[n], width = w[n], limitsize = FALSE)

ggsave(file = paste0("output/dated_tree_with_state_changes_clade_", nodes[n], ".pdf"), 
       p_clade, height = h[n], width = w[n], limitsize = FALSE)

