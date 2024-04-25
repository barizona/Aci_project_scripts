library(tidyverse)
library(ggrepel) # for geom_text_repel
library(cowplot)
library(vegan) # for mantel

#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot PCoA to of distance matrix for Supplementary Figure 12 ---------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description: 
# Differences in cell surface properties between recently diverged
# isolates of ST636-KL40 strain correlate with phage susceptibility profile
# differences.

set.seed(1)

#xxxxxxxxxxxxxxxxxxxxxxx
# A --------------------------------------------------------------------------
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
bin_matrix_A <- read.table(
  "input/ST636_KL40_phage_susceptibility_profile_binary_full_matrix.tsv", 
  header = TRUE, sep = "\t", row.names = 1)

# Jaccard distance from the binary phage susceptibility data
distance_matrix_A <- ade4::dist.binary(bin_matrix_A, upper = TRUE, method = 1)

rm(bin_matrix_A)

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
  geom_text_repel(size = 4, show.legend = FALSE, 
                  box.padding = 0.5, max.overlaps = 100) +
  # colouring scheme
  scale_color_brewer(palette = "Set1") +
  # rename axes and legend title
  labs(x = axis_labs_A[1], y = axis_labs_A[2],
       color = "PAM cluster of phage susceptibility profile") +
  # add title
  ggtitle("Phage susceptibility profile") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12),
        # legend
        legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        # add extra margin to fit legend title
        # legend.margin=margin(l = 10, r = 10, unit = "cm"),
        legend.box.margin = margin(r = 20),
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

### saving legend to a variable ----
# get_legend(plot_A)
legend <- get_plot_component(plot_A, "guide-box-top", return_all = FALSE)
# removing legend from the plot
plot_A <- plot_A + theme(legend.position = "none")

#xxxxxxxxxxxxxxxxxxxx
# B -----------------------------------------------------------------------
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
distance_matrix_B <- read.table(
  "input/ST636_KL40_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv", 
  header = TRUE, sep = "\t") %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)

# row names for the PCoA
row_names_B <- file(
  "input/ST636_KL40_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv") %>% 
  scan(., what = "", nlines = 1, sep="\t", quote = "\"",) 

# change "." in labels to space
dendextend::labels(distance_matrix_B) <- row_names_B

rm(row_names_B)

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
  geom_text_repel(size = 4, show.legend = FALSE, 
                  box.padding = 0.5, max.overlaps = 100) +
  # colouring scheme
  scale_color_brewer(palette = "Set1") +
  # rename axes
  labs(x = axis_labs_B[1], y = axis_labs_B[2]) +
  # add title
  ggtitle("Infrared signal") +
  theme_linedraw() +
  theme(axis.text = element_text(size = 12),
        legend.position = "none",
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

#xxxxxxxxxxxxxxxxxxxxxxxx
# C ---------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# PCoA plot of the phylogenetic distances for the isolates. Principal
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

# rename Aci126 to Aci126_Belgrade5
names_C[which(names_C == "Aci126")] <- "Aci126_Belgrade5"

# filter to only the dataset in focus
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
  geom_text_repel(size = 4, show.legend = FALSE, 
                  box.padding = 0.5, max.overlaps = 100) +
  # colouring scheme
  scale_color_brewer(palette = "Set1") +
  # rename axes
  labs(x = axis_labs_C[1], y = axis_labs_C[2]) +
  # add title
  ggtitle("Phylogenetic distance") +
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
# A: phage susceptibility profile
# B: infrared signal
# C: phylogenetic distance

p <- plot_grid(plot_A, NULL, plot_B, NULL, plot_C, nrow = 1,
               labels = c("A", "", "B", "", "C"),
               rel_widths = c(1, 0.05, 1, 0.05, 1))
# add legend
p <- plot_grid(legend, NULL, p, 
               ncol = 1, rel_heights = c(0.1, 0.05, 1))

ggsave("output/Fig_S12_pcoas.pdf", p, width = 13, height = 4.5)
# to convert the pdf to good resolution png:
# convert -density 300 -trim output/Fig_S12_pcoas.pdf -quality 100 output/Fig_S12_pcoas.png



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# Comparing the distance matrices with Mantel tests ---------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

# comparing all the 5 distance matrices 
# $statistic -> r
# $signif  -> p-value

# a list of all distance matrices
distance_matrices_list <- list("A" = distance_matrix_A, 
                               "B" = distance_matrix_B, 
                               "C" = distance_matrix_C)

# all combinations of distance matrices
Mantel_test_results <- combn(names(distance_matrices_list), 2) %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble()

for(i in 1:nrow(Mantel_test_results)) {
  Mantel_test_results$r[i] <- mantel(xdis = distance_matrices_list[[Mantel_test_results$V1[i]]], 
                                     ydis = distance_matrices_list[[Mantel_test_results$V2[i]]], 
                                     method = "pearson", 
                                     permutations = 100000,
                                     parallel = getOption("mc.cores"))$statistic
  Mantel_test_results$p[i] <- mantel(xdis = distance_matrices_list[[Mantel_test_results$V1[i]]], 
                                     ydis = distance_matrices_list[[Mantel_test_results$V2[i]]], 
                                     method = "pearson", 
                                     permutations = 100000,
                                     parallel = getOption("mc.cores"))$signif
}

rm(i)

Mantel_test_results %>% write_tsv("output/Mantel_test_results_pearson.tsv")
