library(tidyverse)

#xxxxxxxxxxxxxxxxxxxxx
# AIM: heatmap of phage sensitivity ---------------------------------------
#xxxxxxxxxxxxxxxxxxxxx

A_tab <- read_tsv("input/Fig7A_phage_sensitivity.tsv") %>% 
    # convert to longer
    pivot_longer(cols = -Phage, names_to = "Sample", values_to = "Sensitivity") %>% 
    # convert to factor
    mutate(Sensitivity = factor(Sensitivity, levels = c(1, 0)))

#xxxxxxxxxxxxxx
# Plot heatmap ----
#xxxxxxxxxxxxxx
p_A <- A_tab %>% 
    ggplot(aes(x = Sample, y = Phage, fill = factor(Sensitivity))) +
    geom_tile(color = "black") +
    scale_fill_manual(values = Colour_list$Fig7A, 
                      labels = c("1" = "sensitive", "0" = "non-sensitive")) +
    theme_void() +
    labs(x = "Phage", y = "", fill = "Sensitivity") +
    theme(legend.position = "top",
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.4, "cm"),
          # move the legend closer to the plot
          # legend.margin = margin(t = 0, r = 0, b = -8, l = 0),
          # axis font
          axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 8, angle = 0, vjust = 0, hjust = 1,
                                     margin = margin(r = -5)))

