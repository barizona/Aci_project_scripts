library(tidyverse)
library(castor) # geographic_acf
library(ggpointdensity)
library(ggpubr)

set.seed(1)

#xxxxxxxxxxxxxxxxxxxxxxx
# Inputs ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxx
## Tree ----
#xxxxxxxxxxx
# read the treedater output tree
dated_tree <- readRDS("input/dated_tree.rds")

# write out as newick tree
treeio::write.tree(dated_tree, "output/dated_tree.nwk")

# read the newick tree
dated_tree <- treeio::read.tree("output/dated_tree.nwk")

#xxxxxxxxxxx
## Metadata ----
#xxxxxxxxxxx
metadata <- readRDS("input/aci_study.rds") %>%
  as_tibble() %>%
  # keep only the rows of the metadata that are in the dated_tree$tip.label
  filter(assembly %in% dated_tree$tip.label) %>%
  # arrange the rows in the same order as the dated_tree$tip.label
  arrange(factor(assembly, levels = dated_tree$tip.label))

# check if the tips of the tree and the content of the assembly column are the same
all.equal(dated_tree$tip.label, metadata$assembly)
# [1] TRUE

#xxxxxxxxxx
## Keep only those genomes where the city column is filled in the metadata ----
#xxxxxxxxxx

metadata_with_city <- metadata %>%
  filter(!is.na(city))

dated_tree_with_city <- dated_tree %>%
  ape::keep.tip(phy = ., tip = metadata_with_city$assembly)

#xxxxxxxxxxxxxxxxxxxxxx
# Phylogenetic autocorrelation of geographic locations -----------------------
#xxxxxxxxxxxxxxxxxxxxxx
autocorr_result <- geographic_acf(trees = dated_tree_with_city,
                                  tip_latitudes = metadata_with_city$lat,
                                  tip_longitudes = metadata_with_city$lon,
                                  # every tip pair of every tree is included exactly once
                                  Npairs = 1000000,
                                  Nbins = NULL,
                                  min_phylodistance = 0,
                                  max_phylodistance = NULL,
                                  uniform_grid = FALSE,
                                  phylodistance_grid = NULL)

# Convert autocorr_result to tibble for plotting
autocorr_result_tab <- autocorr_result %>%
  as_tibble() %>%
  select(-success) %>%
  # exclude rows where autocorrelations is NaN
  filter(!is.nan(autocorrelations)) %>%
  # exclude rows where std_autocorrelations is NaN
  filter(!is.nan(std_autocorrelations)) %>%
  # exclude rows where mean_geodistances is NaN
  filter(!is.nan(mean_geodistances)) %>%
  # exclude rows where std_geodistances is NaN
  filter(!is.nan(std_geodistances)) %>%
  # create a Year column
  mutate(Year = phylodistances / 2)

#xxxxxxxxxx
## Plot of all years -----
#xxxxxxxxxx

p <- autocorr_result_tab %>%
  # slice_sample(n = 30000) %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              span = 0.3, color = "gray30", se = FALSE) +
  scale_y_continuous(limits = c(-1, 1)) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

ggsave("output/autocorr_result_with_city.png", p, 
       width = 7, height = 5, dpi = 300)
ggsave("output/autocorr_result_with_city.pdf", p, 
       width = 7, height = 5)

#xxxxxxxxxx
## Plot of the first 20 years -----
#xxxxxxxxxx

p2 <- autocorr_result_tab %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              span = 0.5, color = "gray30", se = TRUE) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(-1, 1)) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

ggsave("output/autocorr_result_with_city_20years.png", p2, 
       width = 7, height = 5, dpi = 300)
ggsave("output/autocorr_result_with_city_20years.pdf", p2, 
       width = 7, height = 5)
