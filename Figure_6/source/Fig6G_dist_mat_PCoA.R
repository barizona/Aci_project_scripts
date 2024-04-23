library(tidyverse)
library(ggrepel) # for geom_text_repel
library(ggtext) # for superscripts: theme(... element_markdown())
library(ggpubr)
#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot PCoA to of distance matrix for Figure 6 G ---------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description:
# Principle coordinate analysis plot derived from Fourier-transformed infrared
# measurements (see Methods, Supplementary Table 23) differentiates
# phage-resistant ST2-KL3 isolates that harbour loss-of-function mutations
# within the CPS biosynthetic pathway from those that do not (i.e. wt and
# resistant lines harbouring mutation exclusively in the lgt2 gene).

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read the large distance matrix from lower tsv
# header only, the first column contains no names but distances, 0 diagonal is 
# needed
G_distance_matrix <- read.table(
  "input/Fig6G_ST2_KL3_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv", 
  header = TRUE, sep = "\t") %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)

# row names for the PCoA
G_row_names <- file(
  "input/Fig6G_ST2_KL3_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv") %>% 
  scan(., what = "", nlines = 1, sep="\t", quote = "\"") 

# change "." in labels to space
dendextend::labels(G_distance_matrix) <- G_row_names
rm(G_row_names)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
G_pcoa <- ape::pcoa(G_distance_matrix, correction = "none")

#xxxxxxxxxx
## Variables for plotting ----
#xxxxxxxxxx
# colouring table
G_meta <- read_tsv(
  "input/Fig6G_ST2_KL3_Fourier_transformed_infrared_measurements_metadata.tsv") %>% 
  # convert `Phage resistant` to markdown
  mutate(`Phage resistant` = case_match(`Phage resistant`, 
                                        "H" ~ "H<sup>R</sup>",
                                        "HS" ~ "HS<sup>R</sup>",
                                        "HSF" ~ "HSF<sup>R</sup>",
                                        "HSFPh" ~ "HSFPh<sup>R</sup>",
                                        "wt" ~ "wt")) %>%
  # factorize `Phage resistant` for correct order
  mutate(`Phage resistant` = factor(`Phage resistant`, 
                                    levels = c("H<sup>R</sup>", 
                                               "HS<sup>R</sup>",
                                               "HSF<sup>R</sup>", 
                                               "HSFPh<sup>R</sup>", 
                                               "wt")))

# relative Eigen values for axis names
G_axis_labs <- c("Axis.1" = paste0("PC1: ", 
                                   round(G_pcoa$values$Relative_eig[1], 3)),
                 "Axis.2" = paste0("PC2: ", 
                                   round(G_pcoa$values$Relative_eig[2], 3)))

# converting for ggplot
# 1st and 2nd axis
G_tab <- G_pcoa$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(G_pcoa$vectors), .) %>% 
  # add metadata
  left_join(., G_meta)

rm(G_distance_matrix)

#xxxxxxxxxx
# Plot - coloured ------------------------------------------------------------
#xxxxxxxxxx
p_G <- G_tab %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, 
             colour = `Phage resistant`)) +
  geom_point(size = 1.2) +
  geom_text_repel(show.legend = FALSE, size = 2.5) +
  # no title for legend
  scale_color_manual(name = NULL, values = Colour_list$Fig6G,
                     labels = paste("<span style='color:",
                                    Colour_list$Fig6G,
                                    "'>",
                                    levels(G_tab$`Phage resistant`),
                                    "</span>")) +
  # rename axes
  labs(x = G_axis_labs[1], y = G_axis_labs[2]) +
  theme_linedraw(base_size = 14) +
        # legend
  theme(legend.position = "top",
        legend.text = element_markdown(size = 8),
        legend.key.size = unit(0, "cm"),
        # move the legend closer to the plot
        legend.margin = margin(t = 0, r = 0, b = -8, l = 0),
        # axis font size
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) 

#xxxxxxxxxx
# Plot - grayscale ------------------------------------------------------------
#xxxxxxxxxx

# shapes for grayscale figure
G_shape_vect <- c(15, 17, 18, 19, 20)
names(G_shape_vect) <- names(Colour_list$Fig6G_gray)

# converting for ggplot
# 1st and 2nd axis
p_Gg <- G_pcoa$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(G_pcoa$vectors), .) %>% 
  # add metadata
  left_join(., G_meta) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, 
             colour = `Phage resistant`, shape = `Phage resistant`)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # colour scale, no title for legend
  scale_color_manual(name = NULL, values = Colour_list$Fig6G_gray) +
  # shape scale, no title for legend
  scale_shape_manual(name = NULL, values = G_shape_vect) +
  # rename axes
  labs(x = G_axis_labs[1], y = G_axis_labs[2]) +
  theme_linedraw() +
  # axis numbers
  theme(axis.text = element_text(size = 12),
        legend.position = "top",
        legend.text = element_markdown(size = 12),
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

ggsave("output/Fig6G_pcoa_ST2_KL3_Fourier_transformed_infrared_measurements_grayscale.png", 
       p_Gg, width = 5, height = 5)
ggsave("output/Fig6G_pcoa_ST2_KL3_Fourier_transformed_infrared_measurements_grayscale.pdf", 
       p_Gg, width = 5, height = 5)

rm(G_shape_vect, p_Gg)
