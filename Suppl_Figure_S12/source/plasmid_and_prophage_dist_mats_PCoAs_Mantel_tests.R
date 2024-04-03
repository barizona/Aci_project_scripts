library(tidyverse)
library(ggrepel) # for geom_text_repel
library(cowplot)
library(vegan) # mantel

#xxxxxxxxxxxxxxxxxxxxxxx
# D --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# Plasmid distance

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
distance_matrix_D_all <- read.table("input/st636plasmidvegdist.tsv", 
    header = TRUE, sep = "\t") %>% 
    # remove the first column
    select(-X) %>% 
    as.matrix() %>% 
    as.dist(., upper = TRUE)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_D_all <- ape::pcoa(distance_matrix_D_all, correction = "none")

# relative eigen values for axis names
axis_labs_D_all <- c("Axis.1" = paste0("PC1: ", round(pcoa_D_all$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_D_all$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_D_all <- pcoa_D_all$vectors[,1:2] %>% 
    as.data.frame() %>% 
    tibble() %>% 
    # add rownames
    bind_cols(Sample = rownames(pcoa_D_all$vectors), .) %>% 
    ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample)) +
    geom_point(size = 3) +
    geom_text_repel(size = 4, show.legend = FALSE,
                    box.padding = 0.5, max.overlaps = 100) +
    # rename axes
    labs(x = axis_labs_D_all[1], y = axis_labs_D_all[2]) +
    # add title
    ggtitle("Plasmid profile") +
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
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

#xxxxxxxxxxxxxxxxxxxxxxx
# E --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# Prophage distance

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
distance_matrix_E_all <- read.table("input/st636prophagevegdist.tsv", 
                                    header = TRUE, sep = "\t") %>% 
    # remove the first column
    select(-X) %>% 
    as.matrix() %>% 
    as.dist(., upper = TRUE)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_E_all <- ape::pcoa(distance_matrix_E_all, correction = "none")

# relative eigen values for axis names
axis_labs_E_all <- c("Axis.1" = paste0("PC1: ", round(pcoa_E_all$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_E_all$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_E_all <- pcoa_E_all$vectors[,1:2] %>% 
    as.data.frame() %>% 
    tibble() %>% 
    # add rownames
    bind_cols(Sample = rownames(pcoa_E_all$vectors), .) %>% 
    ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample)) +
    geom_point(size = 3) +
    geom_text_repel(size = 4, show.legend = FALSE,
                    box.padding = 0.5, max.overlaps = 100) +
    # rename axes
    labs(x = axis_labs_E_all[1], y = axis_labs_E_all[2]) +
    # add title
    ggtitle("Prophage profile") +
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
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
# D: Plasmid profile
# E: Prophage profile

plot_grid(plot_D_all, plot_E_all, nrow = 1)

ggsave("output/DE_all_pcoas_ST636_KL40.png", width = 15, height = 7)
