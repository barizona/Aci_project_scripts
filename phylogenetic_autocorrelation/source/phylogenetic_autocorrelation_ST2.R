library(castor)
library(tidyverse)
library(ape)
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
  keep.tip(phy = ., tip = metadata_with_city$assembly)

#xxxxxxxxxxxxxxxxxxxxxx
# All tips with city -- phylogenetic autocorrelation of geographic locations -----------
#xxxxxxxxxxxxxxxxxxxxxx
autocorr_result <- geographic_acf(trees = dated_tree_with_city,
                                  tip_latitudes = metadata_with_city$lat,
                                  tip_longitudes = metadata_with_city$lon,
                                  # every tip pair of every tree is included exactly once
                                  Npairs = Inf,
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
## Plot -----
#xxxxxxxxxx

plot_A <- autocorr_result_tab %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              color = "gray30", se = FALSE) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

#xxxxxxxxxxxxxxxxxxxxxx
# Eastern Asia tips -- phylogenetic autocorrelation of geographic locations -----------
#xxxxxxxxxxxxxxxxxxxxxx

metadata_with_city_EA <- metadata_with_city %>%
  filter(region23 == "eastern_asia")

dated_tree_with_city_EA <- dated_tree_with_city %>%
  keep.tip(phy = ., tip = metadata_with_city_EA$assembly)

autocorr_result_EA <- geographic_acf(trees = dated_tree_with_city_EA,
                                     tip_latitudes = metadata_with_city_EA$lat,
                                     tip_longitudes = metadata_with_city_EA$lon,
                                     # every tip pair of every tree is included exactly once
                                     Npairs = Inf,
                                     Nbins = NULL,
                                     min_phylodistance = 0,
                                     max_phylodistance = NULL,
                                     uniform_grid = FALSE,
                                     phylodistance_grid = NULL)

# Convert autocorr_result to tibble for plotting
autocorr_result_tab_EA <- autocorr_result_EA %>%
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

plot_B <- autocorr_result_tab_EA %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              color = "gray30", se = FALSE) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

#xxxxxxxxxxxxxxxxxxxxxx
# Northern America tips -- phylogenetic autocorrelation of geographic locations -----------
#xxxxxxxxxxxxxxxxxxxxxx

metadata_with_city_NA <- metadata_with_city %>%
  filter(region23 == "northern_america")

dated_tree_with_city_NA <- dated_tree_with_city %>%
  keep.tip(phy = ., tip = metadata_with_city_NA$assembly)

autocorr_result_NA <- geographic_acf(trees = dated_tree_with_city_NA,
                                     tip_latitudes = metadata_with_city_NA$lat,
                                     tip_longitudes = metadata_with_city_NA$lon,
                                     # every tip pair of every tree is included exactly once
                                     Npairs = Inf,
                                     Nbins = NULL,
                                     min_phylodistance = 0,
                                     max_phylodistance = NULL,
                                     uniform_grid = FALSE,
                                     phylodistance_grid = NULL)

# Convert autocorr_result to tibble for plotting
autocorr_result_tab_NA <- autocorr_result_NA %>%
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

plot_C <- autocorr_result_tab_NA %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              color = "gray30", se = FALSE) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

#xxxxxxxxxxxxxxxxxxxxxx
# Eastern Europe tips -- phylogenetic autocorrelation of geographic locations -----------
#xxxxxxxxxxxxxxxxxxxxxx

metadata_with_city_EE <- metadata_with_city %>%
  filter(region23 == "eastern_europe")

dated_tree_with_city_EE <- dated_tree_with_city %>%
  keep.tip(phy = ., tip = metadata_with_city_EE$assembly)

autocorr_result_EE <- geographic_acf(trees = dated_tree_with_city_EE,
                                     tip_latitudes = metadata_with_city_EE$lat,
                                     tip_longitudes = metadata_with_city_EE$lon,
                                     # every tip pair of every tree is included exactly once
                                     Npairs = Inf,
                                     Nbins = NULL,
                                     min_phylodistance = 0,
                                     max_phylodistance = NULL,
                                     uniform_grid = FALSE,
                                     phylodistance_grid = NULL)

# Convert autocorr_result to tibble for plotting
autocorr_result_tab_EE <- autocorr_result_EE %>%
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

plot_D <- autocorr_result_tab_EE %>%
  ggplot(aes(x = Year, y = autocorrelations)) +
  geom_pointdensity(size = 0.5) +
  scale_colour_distiller(palette = "Spectral", name = "Nr. of\nneighbours") +
  # regression
  geom_smooth(method = "loess", formula = y ~ x,
              color = "gray30", se = FALSE) +
  stat_cor(method = "spearman", cor.coef.name = "rho",
           label.x.npc = "left", label.y.npc = "bottom", show.legend = FALSE) +
  labs(x = "MRCA (years)", y = "Autocorrelation") +
  theme_minimal()

#xxxxxxxxxxxxxxxxxxxxxxxxx
# Combining plots to a single one -----------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxx

plot <- ggarrange(plot_A, plot_B, plot_C, plot_D,
                  ncol = 2, nrow = 2,
                  labels = c("A", "B", "C", "D"))

ggsave("output/autocorr_result_with_city_EA_NA_EE.png", width = 12, height = 10, dpi = 300)
ggsave("output/autocorr_result_with_city_EA_NA_EE.pdf", width = 12, height = 10)

save.image()


