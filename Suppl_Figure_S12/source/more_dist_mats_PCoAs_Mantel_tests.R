library(tidyverse)
library(cluster) # pam
library(ggrepel) # for geom_text_repel
library(ggtext) # for superscripts: theme(... element_markdown())
library(cowplot)

# library(factoextra) # fviz_cluster: Visualize Clustering Results

#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot PCoA to of distance matrix for Supplementary Figure 12 ---------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description: 
# Differences in cell surface properties between recently diverged
# isolates of ST636-KL40 strain correlate with phage susceptibility profile
# differences.

#xxxxxxxxxxxxxxxxxxxx
# A -----------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description:
# Principal coordinate analysis (PCoA) plot of nine ST636-KL40 isolates from
# Eastern and Southern Europe derived from the Fourier-transformed infrared
# measurement using the IR Biotyper instrument (Supplementary Table 23,
# Methods). Each isolate has at least one pair of isolates that are
# phylogenetically closely related (that is, diverged within two years).
# Principal coordinates 1 and 2 explain 53% and 24% of the variation. The
# isolates are coloured based on their Partitioning Around Medoids (PAM)
# clustering into four clusters.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read the large distance matrix from lower tsv
# header only, the first column contains no names but distances, 0 diagonal is needed
distance_matrix_A <- read.table(
  "input/less_samples_lower_distance_matrix.tsv", 
  header = TRUE, sep = "\t") %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)

# row names for the PCoA
row_names_A <- file("input/less_samples_lower_distance_matrix.tsv") %>% 
  scan(., what = "", nlines = 1, sep="\t", quote = "\"",) 

# change "." in labels to space
dendextend::labels(distance_matrix_A) <- row_names_A

rm(row_names_A)

#xxxxxxxx
## k-medoids or PAM (Partitioning Around Meroids) ----
#xxxxxxxx
# comparing 4 clusters using euclidean
pam_A <- pam(distance_matrix_A, k = 4)

# convert it to tibble
pam_A_tab <- pam_A$cluster %>% 
  enframe("Sample", "cluster") %>% 
  # cluster as factor
  mutate(cluster = factor(cluster))

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_A <- ape::pcoa(distance_matrix_A, correction = "none")

# relative eigen values for axis names
axis_labs_A <- c("Axis.1" = paste0("PC1: ", round(pcoa_A$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_A$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
# * PCoA plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
p1 <- pcoa_A$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_A$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # scale_color_manual(name = NULL, values = Colors) +
  # rename axes
  labs(x = axis_labs[1], y = axis_labs[2]) +
  # add title
  ggtitle("Infrared signal") +
  # legend title
  labs(color = "PAM cluster of infrared signal") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12),
        legend.position = "top",
        legend.text = element_text(size = 12),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # add margin to x-axis title
        axis.title.x = element_text(size = 14, margin = margin(t = 10)), 
        # add margin to y-axis title
        axis.title.y = element_text(size = 14, margin = margin(r = 10))) 

# saving legend to a variable
legend <- get_legend(p1)
# removing legend from the plot
p1 <- p1 + theme(legend.position = "none")

rm(axis_labs)

#xxxxxxxxxxxxxxxxxxxxxxx
# Phage data - full matrix ------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxx
# * Jaccard distance from the binary phage data ----
#xxxxxxxxxxxx
# read data
bin_matrix <- read.table("input/less_samples_aci_phage_binary_fullmatrix.tsv", 
                         header = TRUE, sep = "\t", row.names = 1)

phage_dist <- ade4::dist.binary(bin_matrix, upper = TRUE, method = 1)

#xxxxxxxx
# * k-medoids or PAM (Partitioning Around Meroids) ----
#xxxxxxxx
# comparing 4 clusters using euclidean
pam(phage_dist, k = 4)
# Aci 200 Aci 222   Aci 4 Aci 126 Aci 184 Aci 191 Aci 223 Aci 240  Aci 72 
#       1       2       3       3       1       2       2       1       4

#xxxxxxxxxx
# * PCoA ----
#xxxxxxxxxx
pcoa_phage <- ape::pcoa(phage_dist, correction = "none")

# relative eigen values for axis names
axis_labs <- c("Axis.1" = paste0("PC1: ", round(pcoa_phage$values$Relative_eig[1], 3)),
               "Axis.2" = paste0("PC2: ", round(pcoa_phage$values$Relative_eig[2], 3)))

#xxxxxxxxxx
# * PCoA plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
p2 <- pcoa_phage$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_phage$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # scale_color_manual(name = NULL, values = Colors) +
  # rename axes
  labs(x = axis_labs[1], y = axis_labs[2]) +
  # add title
  ggtitle("Phage susceptibility profile") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12),
        legend.position = "none",
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # add margin to x-axis title
        axis.title.x = element_text(size = 14, margin = margin(t = 10)), 
        # add margin to y-axis title
        axis.title.y = element_text(size = 14, margin = margin(r = 10))) 

rm(axis_labs)


#xxxxxxxxxxxxxxxxxxxxxxxx
# Genetic distances for small ---------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx
# full table with all info
all_dist <- read_tsv("input/all_phagedist_phylodist.tsv")

names_small <- labels(distance_matrix_A) %>% 
  # delete spaces
  str_remove_all(" ")

# change Aci126 to Aci126_Belgrade5
names_small[which(names_small == "Aci126")] <- "Aci126_Belgrade5"

# filter to only the small dataset
gen_dist <- all_dist %>% 
  filter((A1 %in% names_small) & (A2 %in% names_small)) %>% 
  # add space to names
  mutate(A1 = str_replace_all(A1, "Aci", "Aci "),
         A2 = str_replace_all(A2, "Aci", "Aci ")) %>%
  # change to Aci 126_Belgrade5 to Aci 126
  mutate(A1 = str_replace_all(A1, "Aci 126_Belgrade5", "Aci 126"),
         A2 = str_replace_all(A2, "Aci 126_Belgrade5", "Aci 126")) %>%
  # select partistic distance
  select(A1, A2, patristic) 

# make it full (A B dist -> and B A dist)
gen_dist <- gen_dist %>% 
  # change names
  rename(A2 = A1, A1 = A2) %>%
  # reorder columns
  select(A1, A2, patristic) %>% 
  # bind to the original
  bind_rows(gen_dist, .) %>% 
  # convert to a lower triangle distance matrix
  spread(A2, patristic, fill = 0) %>% 
  # remove the first column with names
  select(-A1) %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)
  
rm(names_small, all_dist)

#xxxxxxxx
# * k-medoids or PAM (Partitioning Around Meroids) ----
#xxxxxxxx
# comparing 4 clusters using euclidean
pam(gen_dist, k = 4)
# Aci 126 Aci 184 Aci 191 Aci 200 Aci 222 Aci 223 Aci 240   Aci 4  Aci 72 
#       1       2       2       2       2       2       2       3       4 

#xxxxxxxxxx
# * PCoA ----
#xxxxxxxxxx
pcoa_gen <- ape::pcoa(gen_dist, correction = "none")

# relative eigen values for axis names
axis_labs <- c("Axis.1" = paste0("PC1: ", round(pcoa_gen$values$Relative_eig[1], 3)),
               "Axis.2" = paste0("PC2: ", round(pcoa_gen$values$Relative_eig[2], 3)))

#xxxxxxxxxx
# * PCoA plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
p3 <- pcoa_gen$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_gen$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # scale_color_manual(name = NULL, values = Colors) +
  # rename axes
  labs(x = axis_labs[1], y = axis_labs[2]) +
  # add title
  ggtitle("Genetic distance") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12),
        legend.position = "none",
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # add margin to x-axis title
        axis.title.x = element_text(size = 14, margin = margin(t = 10)), 
        # add margin to y-axis title
        axis.title.y = element_text(size = 14, margin = margin(r = 10))) 

rm(axis_labs)

#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# 3 PCoA plots together -----------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

# legend title PAM cluster of infrared signal
# A: infrared signal
# B: phage susceptibility profile
# C: genetic distance

p <- plot_grid(p1, p2, p3, nrow = 1,
               labels = c("A", "B", "C"))

plot_grid(legend, p, ncol = 1, rel_heights = c(0.1, 1))

# ggsave("output/pcoa_Aer_dataset_and_phageJaccard_and_geneticsDist.png", width = 15, height = 5)
ggsave("output/pcoa_Aer_dataset_and_phageJaccard_and_geneticsDist.pdf", width = 12, height = 4)

rm(p1, p2, p3, legend, p)




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Comparing the three distance matrices ---------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
vegan::mantel(xdis = distance_matrix_A, 
              ydis = phage_dist, 
              method = "pearson", 
              permutations = 100000,
              parallel = getOption("mc.cores"))
# Mantel statistic r: 0.4081 
# Significance: 0.01157 
# Upper quantiles of permutations (null model):
#   90%   95% 97.5%   99% 
#   0.228 0.294 0.351 0.419 

vegan::mantel(xdis = distance_matrix_A, 
              ydis = gen_dist, 
              method = "pearson", 
              permutations = 100000,
              parallel = getOption("mc.cores"))
# Mantel statistic r: -0.02499 
# Significance: 0.49909 
# Upper quantiles of permutations (null model):
#   90%   95% 97.5%   99% 
#   0.363 0.444 0.517 0.701

vegan::mantel(xdis = phage_dist, 
              ydis = gen_dist, 
              method = "pearson", 
              permutations = 100000,
              parallel = getOption("mc.cores"))
# Mantel statistic r:  0.11 
# Significance: 0.27113 
# Upper quantiles of permutations (null model):
#   90%   95% 97.5%   99% 
#   0.241 0.306 0.357 0.466
