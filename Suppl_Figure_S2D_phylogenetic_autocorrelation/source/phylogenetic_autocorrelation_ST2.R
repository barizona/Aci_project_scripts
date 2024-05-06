library(tidyverse)
library(castor) # geographic_acf
library(ggpointdensity)
library(ggpubr)

set.seed(1)

#xxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Figure S2D: Calculating phylogenetic autocorrelation plot ----------------------
#xxxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxxxx
# Inputs ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx
#xxxxxxxxxxx
## Tree ----
#xxxxxxxxxxx
# read the treedater output tree
dated_tree <- readRDS("input/dated_tree.rds")

# write out as newick tree
treeio::write.tree(dated_tree, "input/dated_tree.nwk")

# read the newick tree
dated_tree <- treeio::read.tree("input/dated_tree.nwk")

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

geodate_tab <- read_tsv("input/aci_crab_ds_geodate2_no_pop.tsv") %>% 
  filter(filtered == TRUE & downsampled == TRUE & mlst == "ST2")

# check if the geodate_tab contain all tips of the tree
# setdiff(geodate_tab$assembly, dated_tree$tip.label)

#xxxxxxxxxx
## Keep only those genomes where the city column is filled in the metadata ----
# and kept after geodate filtering
#xxxxxxxxxx

# keep only those filtered == TRUE and downsampled == TRUE in geodate_tab
to_keep <- geodate_tab %>%
  select(assembly) %>%
  pull()

# filtering metadata
metadata_with_city <- metadata %>%
  filter(!is.na(city)) %>% 
  filter(assembly %in% to_keep)
rm(to_keep)

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
  scale_colour_distiller(palette = "Spectral", name = "Number of\nneighbouring\npoints") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              span = 0.9, color = "gray30", se = FALSE) +
  scale_y_continuous(limits = c(-1, 1)) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_linedraw()

ggsave("output/autocorr_result_with_city_span_0.9.png", 
       p, width = 7.5, height = 5, dpi = 300)
ggsave("output/autocorr_result_with_city_span_0.9.pdf", 
       p, width = 7.5, height = 5)

sessionInfo() %>%
  capture.output() %>%
  writeLines("output/sessionInfo.txt")
