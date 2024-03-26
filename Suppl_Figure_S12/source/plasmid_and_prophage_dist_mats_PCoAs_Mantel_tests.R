#xxxxxxxxxxxxxxxxxxxxxxx
# D --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# Plasmid distance

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
distance_matrix_D <- read.table(
    "input/st636plasmidvegdist.tsv", 
    header = TRUE, sep = "\t") %>% 
    # delete the first column
    select(-X) %>%
    as.matrix() %>% 
    as.dist(., upper = TRUE)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_D <- ape::pcoa(distance_matrix_D, correction = "none")

# relative eigen values for axis names
axis_labs_D <- c("Axis.1" = paste0("PC1: ", round(pcoa_D$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_D$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_D <- pcoa_D$vectors[,1:2] %>% 
    as.data.frame() %>% 
    tibble() %>% 
    # add rownames
    bind_cols(Sample = rownames(pcoa_D$vectors), .) %>% 
    # add pam clusters
    left_join(., pam_A_tab) %>% 
    ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
    geom_point(size = 3) +
    geom_text_repel(size = 4, show.legend = FALSE) +
    # rename axes
    labs(x = axis_labs_D[1], y = axis_labs_D[2]) +
    # add title
    ggtitle("Phage susceptibility profile") +
    theme_linedraw() +
    theme(axis.text = element_text(size = 12),
          legend.position = "none",
          # remove the vertical grid lines
          panel.grid.major.x = element_Dlank(),
          panel.grid.minor.x = element_Dlank(),
          # remove the horizontal grid lines
          panel.grid.major.y = element_Dlank(),
          panel.grid.minor.y = element_Dlank(),
          # add margin to x-axis title
          axis.title.x = element_text(size = 14, margin = margin(t = 10)), 
          # add margin to y-axis title
          axis.title.y = element_text(size = 14, margin = margin(r = 10))) 
