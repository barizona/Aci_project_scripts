#xxxxxxxxxxxxxxxxxxxxxxx
# D --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# Plasmid distance

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
distance_matrix_D <- read.table("input/st636plasmidvegdist.tsv", 
    header = TRUE, sep = "\t")

# rename samples to match the pam_A_tab
# add a space after "Aci" in the sample names
# column names
colnames(distance_matrix_D) <- colnames(distance_matrix_D) %>% 
    str_replace_all("Aci", "Aci ") %>% 
    str_replace_all("Aci 126_Belgrade5", "Aci 126")

# row names
distance_matrix_D$X <- distance_matrix_D$X %>% 
    str_replace_all("Aci", "Aci ") %>% 
    str_replace_all("Aci 126_Belgrade5", "Aci 126")

# filter only those samples that are in the pam_A_tab
# order pam_A_tab$Sample numerically and create a vector of sample names
Names <- pam_A_tab$Sample %>% 
    str_replace_all("Aci ", "") %>% 
    as.numeric() %>% 
    sort() %>% 
    paste0("Aci ", .)

# create the distance matrix
distance_matrix_D <- distance_matrix_D %>% 
    filter(X %in% Names) %>% 
    select(Names) %>% 
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
    ggtitle("Plasmid profile") +
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

#xxxxxxxxxxxxxxxxxxxxxxx
# E --------------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

# Description: 
# Prophage distance

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read data
distance_matrix_E <- read.table("input/st636prophagevegdist.tsv", 
                                header = TRUE, sep = "\t")

# rename samples to match the pam_A_tab
# add a space after "Aci" in the sample names
# column names
colnames(distance_matrix_E) <- colnames(distance_matrix_E) %>% 
    str_replace_all("Aci", "Aci ") %>% 
    str_replace_all("Aci 126_Belgrade5", "Aci 126")

# row names
distance_matrix_E$X <- distance_matrix_E$X %>% 
    str_replace_all("Aci", "Aci ") %>% 
    str_replace_all("Aci 126_Belgrade5", "Aci 126")

# filter only those samples that are in the pam_A_tab (Names)
# create the distance matrix
distance_matrix_E <- distance_matrix_E %>% 
    filter(X %in% Names) %>% 
    select(Names) %>% 
    as.matrix() %>% 
    as.dist(., upper = TRUE)

#xxxxxxxxxx
## PCoA ----
#xxxxxxxxxx
pcoa_E <- ape::pcoa(distance_matrix_E, correction = "none")

# relative eigen values for axis names
axis_labs_E <- c("Axis.1" = paste0("PC1: ", round(pcoa_E$values$Relative_eig[1], 
                                                  3)),
                 "Axis.2" = paste0("PC2: ", round(pcoa_E$values$Relative_eig[2], 
                                                  3)))

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx
# converting for ggplot
# 1st and 2nd axis
plot_E <- pcoa_E$vectors[,1:2] %>% 
    as.data.frame() %>% 
    tibble() %>% 
    # add rownames
    bind_cols(Sample = rownames(pcoa_E$vectors), .) %>% 
    # add pam clusters
    left_join(., pam_A_tab) %>%
    ggplot(aes(x = `Axis.1`, y = `Axis.2`, label = Sample, color = cluster)) +
    geom_point(size = 3) +
    geom_text_repel(size = 4, show.legend = FALSE) +
    # rename axes
    labs(x = axis_labs_E[1], y = axis_labs_E[2]) +
    # add title
    ggtitle("Prophage profile") +
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
# D: Plasmid profile
# E: Prophage profile

p2 <- plot_grid(plot_D, plot_E, nrow = 1,
               labels = c("D", "E"))

plot_grid(legend, p2, ncol = 1, rel_heights = c(0.1, 1))

ggsave("output/DE_pcoas_ST636_KL40.png", width = 8, height = 4)
ggsave("output/DE_pcoas_ST636_KL40.pdf", width = 8, height = 4)


