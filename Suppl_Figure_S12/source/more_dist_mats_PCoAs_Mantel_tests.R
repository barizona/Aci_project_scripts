library(tidyverse)
library(ggrepel) # for geom_text_repel
library(ggtext) # for superscripts: theme(... element_markdown())
library(cowplot)
library(vegan) # mantel

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
pam_A <- cluster::pam(distance_matrix_A, k = 4)

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
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_A <- pcoa_A$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_A$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # rename axes
  labs(x = axis_labs_A[1], y = axis_labs_A[2]) +
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
legend <- get_legend(plot_A)
# removing legend from the plot
plot_A <- plot_A + theme(legend.position = "none")

#xxxxxxxxxxxxxxxxxxxxxxx
# B --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# PCoA plot of the phage susceptibility profile differences
# calculated with the Jaccard index. Principal coordinates 1 and 2 explain 33%
# and 23% of the variation. The isolates are coloured based on the PAM
# clustering into four clusters of the samples in Fig S12A. Differences in the
# surface properties derived from the Fourier-transformed infrared signal
# correlate with phage susceptibility profile differences calculated with the
# Jaccard index (Mantel test, r = 0.408, p-value = 0.011, n = 9, number of
# permutations: 10,000, method: Pearson correlation). Measurements were carried
# out in three technical replicates.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
bin_matrix_B <- read.table("input/less_samples_aci_phage_binary_fullmatrix.tsv", 
                           header = TRUE, sep = "\t", row.names = 1)

# Jaccard distance from the binary phage data
distance_matrix_B <- ade4::dist.binary(bin_matrix_B, upper = TRUE, method = 1)

rm(bin_matrix_B)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_B <- ape::pcoa(distance_matrix_B, correction = "none")

# relative eigen values for axis names
axis_labs_B <- c("Axis.1" = paste0("PC1: ", round(pcoa_B$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_B$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_B <- pcoa_B$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_B$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # rename axes
  labs(x = axis_labs_B[1], y = axis_labs_B[2]) +
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

#xxxxxxxxxxxxxxxxxxxxxxxx
# C ---------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# PCoA plot of the genetic distances for the isolates. Principal
# coordinates 1 and 2 explain 53% and 25% of the variation. The isolates are
# coloured based on the PAM clustering into four clusters of the samples in Fig
# S12A. Correlation cannot be observed between the Fourier-transformed infrared
# signal and the genetic distances for the isolates (Mantel test, r = -0.025,
# p-value = 0.499, n = 9, number of permutations: 10,000, method: Pearson
# correlation). 

#xxxxxxxxxx
## Input ---- 
#xxxxxxxxxx
# full table with all info
all_dist <- read_tsv("input/all_phagedist_phylodist.tsv")

names_C <- labels(distance_matrix_A) %>% 
  # delete spaces
  str_remove_all(" ")

# change Aci126 to Aci126_Belgrade5
names_C[which(names_C == "Aci126")] <- "Aci126_Belgrade5"

# filter to only the small dataset
distance_matrix_C <- all_dist %>% 
  filter((A1 %in% names_C) & (A2 %in% names_C)) %>% 
  # add space to names
  mutate(A1 = str_replace_all(A1, "Aci", "Aci "),
         A2 = str_replace_all(A2, "Aci", "Aci ")) %>%
  # change to Aci 126_Belgrade5 to Aci 126
  mutate(A1 = str_replace_all(A1, "Aci 126_Belgrade5", "Aci 126"),
         A2 = str_replace_all(A2, "Aci 126_Belgrade5", "Aci 126")) %>%
  # select partistic distance
  select(A1, A2, patristic) 

# make it full (A B dist -> and B A dist)
distance_matrix_C <- distance_matrix_C %>% 
  # change names
  rename(A2 = A1, A1 = A2) %>%
  # reorder columns
  select(A1, A2, patristic) %>% 
  # bind to the original
  bind_rows(distance_matrix_C, .) %>% 
  # convert to a lower triangle distance matrix
  spread(A2, patristic, fill = 0) %>% 
  # remove the first column with names
  select(-A1) %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)
  
rm(names_C, all_dist)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_C <- ape::pcoa(distance_matrix_C, correction = "none")

# relative eigen values for axis names
axis_labs_C <- c("Axis.1" = paste0("PC1: ", round(pcoa_C$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_C$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_C <- pcoa_C$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa_C$vectors), .) %>% 
  # add pam clusters
  left_join(., pam_A_tab) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # scale_color_manual(name = NULL, values = Colors) +
  # rename axes
  labs(x = axis_labs_C[1], y = axis_labs_C[2]) +
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

#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# Creating the complex figure: putting the 3 PCoA plots together --------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

# legend title PAM cluster of infrared signal
# A: infrared signal
# B: phage susceptibility profile
# C: genetic distance

p <- plot_grid(plot_A, plot_B, plot_C, nrow = 1,
               labels = c("A", "B", "C"))

plot_grid(legend, p, ncol = 1, rel_heights = c(0.1, 1))

ggsave("output/pcoa_Aer_dataset_and_phageJaccard_and_geneticsDist.png", 
       width = 15, height = 5)
ggsave("output/pcoa_Aer_dataset_and_phageJaccard_and_geneticsDist.pdf", 
       width = 12, height = 4)






#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Comparing the distance matrices with Mantel tests ---------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

set.seed(1)

#xxxxxxxxxx
## A vs B ----
#xxxxxxxxxx
mantel(xdis = distance_matrix_A, 
       ydis = distance_matrix_B, 
       method = "pearson", 
       permutations = 100000,
       parallel = getOption("mc.cores"))

# Mantel statistic r: 0.4081 
# Significance: 0.01134 
# Upper quantiles of permutations (null model):
#     90%   95% 97.5%   99% 
#   0.231 0.295 0.352 0.417 

#xxxxxxxxxx
## A vs C ----
#xxxxxxxxxx
mantel(xdis = distance_matrix_A, 
       ydis = distance_matrix_C, 
       method = "pearson", 
       permutations = 100000,
       parallel = getOption("mc.cores"))

# Mantel statistic r: -0.02499 
# Significance: 0.49813 
# 
# Upper quantiles of permutations (null model):
#     90%   95% 97.5%   99% 
#   0.364 0.446 0.521 0.702 


