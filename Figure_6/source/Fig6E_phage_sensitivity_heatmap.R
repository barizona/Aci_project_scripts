library(tidyverse)
library(magrittr)
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxx
# AIM: heatmap of phage sensitivity Figure 6E ---------------------------------
#xxxxxxxxxxxxxxxxxxxxx

E_tab <- read_tsv("input/Fig6E_phage_sensitivity.tsv") %>% 
    # factorize Phage
    mutate(Phage = factor(Phage, levels = c("Ph", "F", "S", "H"))) %>%
    # arrange by Phage
    arrange(Phage) %>%
    # colouring Phage
    mutate(Phage = paste("<span style='color:", Colour_list$Fig6Ephage_axis, "'>", Phage, "</span>"))

E_tab %<>%
    mutate(Phage = factor(Phage, levels = E_tab$Phage)) %>% 
    # colouring colnames except Phage
    rename_at(vars(-Phage), ~paste("<span style='color:", Colour_list$Fig6Eaci_axis, "'>", .x, "</span>")) %>%
    # convert to longer
    pivot_longer(cols = -Phage, names_to = "Sample", values_to = "Sensitivity")

E_tab %<>%
    # convert Sample and Sensitivity to factor
    mutate(Sample = factor(Sample, levels = E_tab$Sample[1:4]), 
           Sensitivity = factor(Sensitivity, levels = c(1, 0)))

#xxxxxxxxxxxxxx
# Plot heatmap ----
#xxxxxxxxxxxxxx
p_E <- E_tab %>% 
    ggplot(aes(x = Sample, y = Phage, fill = factor(Sensitivity))) +
    geom_tile(color = "black") +
    scale_fill_manual(name = NULL, values = Colour_list$Fig6E, 
                      labels = c("1" = "sensitive", "0" = "resistant")) +
    # legend to 2 rows
    guides(fill = guide_legend(nrow = 2)) +
    theme_void() +
    labs(x = "Phage", y = "", fill = "Sensitivity") +
    theme(legend.position = "top",
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.4, "cm"),
          # axis font
          axis.text.x = element_markdown(size = 8, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_markdown(size = 8))

