library(tidyverse)
library(ggrepel) # for geom_text_repel
library(ggtext) # for superscripts: theme(... element_markdown())
#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot PCoA to of distance matrix for Figure 6 F ---------------------------
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
distance_matrix <- read.table(
  "input/ST2_KL3_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv", 
  header = TRUE, sep = "\t") %>% 
  as.matrix() %>% 
  as.dist(., upper = TRUE)

# row names for the PCoA
row_names <- file(
  "input/ST2_KL3_Fourier_transformed_infrared_measurements_lower_distance_matrix.tsv") %>% 
  scan(., what = "", nlines = 1, sep="\t", quote = "\"") 

# change "." in labels to space
dendextend::labels(distance_matrix) <- row_names
rm(row_names)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa <- ape::pcoa(distance_matrix, correction = "none")

#xxxxxxxxxx
## Variables for plotting ----
#xxxxxxxxxx
# colouring table
meta <- read_tsv(
  "input/ST2_KL3_Fourier_transformed_infrared_measurements_metadata.tsv") %>% 
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

# Colours for scale colour manual
colour_vect <- meta %>% 
  select(`Phage resistant`, Colour) %>%
  distinct() %>% 
  arrange(`Phage resistant`) %>%
  deframe()

# grayscale
colour_vect_gr <- c("black", "gray15", "gray30", "gray45", "gray60")
names(colour_vect_gr) <- names(colour_vect)

# relative Eigen values for axis names
axis_labs <- c("Axis.1" = paste0("PC1: ", 
                                 round(pcoa$values$Relative_eig[1], 3)),
               "Axis.2" = paste0("PC2: ", 
                                 round(pcoa$values$Relative_eig[2], 3)))

#xxxxxxxxxx
# Plot - coloured ------------------------------------------------------------
#xxxxxxxxxx

# converting for ggplot
# 1st and 2nd axis
pcoa$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa$vectors), .) %>% 
  # add metadata
  left_join(., meta) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, 
             colour = `Phage resistant`)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # no title for legend
  scale_color_manual(name = NULL, values = colour_vect) +
  # rename axes
  labs(x = axis_labs[1], y = axis_labs[2]) +
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

ggsave("output/pcoa_ST2_KL3_Fourier_transformed_infrared_measurements.png", width = 5, height = 5)
ggsave("output/pcoa_ST2_KL3_Fourier_transformed_infrared_measurements.pdf", width = 5, height = 5)

sessionInfo() %>% 
  capture.output() %>% 
  writeLines("output/sessionInfo.txt")

#xxxxxxxxxx
# Plot - grayscale ------------------------------------------------------------
#xxxxxxxxxx

shape_vect <- c(15, 17, 18, 19, 20)
names(shape_vect) <- names(colour_vect)

# converting for ggplot
# 1st and 2nd axis
pcoa$vectors[,1:2] %>% 
  as.data.frame() %>% 
  tibble() %>% 
  # add rownames
  bind_cols(Sample = rownames(pcoa$vectors), .) %>% 
  # add metadata
  left_join(., meta) %>% 
  ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, 
             colour = `Phage resistant`, shape = `Phage resistant`)) +
  geom_point(size = 3) +
  geom_text_repel(size = 4, show.legend = FALSE) +
  # colour scale, no title for legend
  scale_color_manual(name = NULL, values = colour_vect_gr) +
  # shape scale, no title for legend
  scale_shape_manual(name = NULL, values = shape_vect) +
  # rename axes
  labs(x = axis_labs[1], y = axis_labs[2]) +
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

ggsave("output/pcoa_ST2_KL3_Fourier_transformed_infrared_measurements_grayscale.png", width = 5, height = 5)
ggsave("output/pcoa_ST2_KL3_Fourier_transformed_infrared_measurements_grayscale.pdf", width = 5, height = 5)

sessionInfo() %>% 
  capture.output() %>% 
  writeLines("output/sessionInfo.txt")
