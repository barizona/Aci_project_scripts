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
# 
#     
#     
#     levels = c("<span style='color: #EF3E54 '> Ph </span>",
#                "<span style='color: #CAE084 '> F </span>",
#                "<span style='color: #234AA6 '> S </span>",
#                "<span style='color: #E6A4B5 '> H </span>"))
    
# E_tab %<>% 
#     # convert Sample and Sensitivity to factor
#     mutate(Sample = factor(Sample, levels = E_tab$Sample[1:4]), 
#            Sensitivity = factor(Sensitivity, levels = c(1, 0)))
    
    
# E_tab %>% 
# # colouring and factor
#     mutate(Phage = factor(paste("<span style='color:", Colour_list$Fig6Ephage_axis, "'>", unique(E_tab$Phage), "</span>")))
#     # convert to factor
#     mutate(Sensitivity = factor(Sensitivity, levels = c(1, 0)),
#            Phage = factor(Phage, levels = c("Ph", "F", "S", "H")),
#            Sample = factor(Sample, levels = c("Aci 110", "A110-1", "A110-2",
#                                               "A110-G1")))
#     # 
# 
# factor(paste("<span style='color:", Colour_list$Fig6Ephage_axis, "'>", unique(E_tab$Phage), "</span>"), 
#        levels = c("<span style='color: #E6A4B5 '> Ph </span>",
#                   "<span style='color: #234AA6 '> F </span>",
#                   "<span style='color: #CAE084 '> S </span>",
#                   ))

#xxxxxxxxxxxxxx
# Plot heatmap ----
#xxxxxxxxxxxxxx
p_E <- E_tab %>% 
    ggplot(aes(x = Sample, y = Phage, fill = factor(Sensitivity))) +
    geom_tile(color = "black") +
    scale_fill_manual(name = NULL, values = Colour_list$Fig6E, 
                      labels = c("1" = "sensitive", "0" = "non-sensitive")) +
    # legend to 2 rows
    guides(fill = guide_legend(nrow = 2)) +
    theme_void() +
    labs(x = "Phage", y = "", fill = "Sensitivity") +
    theme(legend.position = "top",
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.4, "cm"),
          # axis font
          axis.text.x = element_markdown(size = 8, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_markdown(size = 8, angle = 0, vjust = 0, hjust = 1))

