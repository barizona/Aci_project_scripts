library(tidyverse)
library(magrittr)
library(ggtext) # for superscripts: theme(... element_markdown())

set.seed(1)
#xxxxxxxxxxxxxxxxxxxx
# AIM: mean-variance plot of phage absorption for Figure 6 F -------------------
#xxxxxxxxxxxxxxxxxxxx

# Description: 
# E, Phage adsorption assay with the phage H, S, F, and Ph for a
# wild-type ST2-KL3 isolate (Aci 110) and for its three phage-resistant
# derivatives: Aci 110-1, Aci 110-2, and Aci 110-G1 which are resistant to H,
# HS, and HSFPh, respectively. Results show the log10 reduction in free phage
# titres at a maximum adsorption time point in comparison to the t0 time point
# (dashed line) after mixing phages and host bacteria (for details see Methods
# and Supplementary Table 22). Data are mean ± standard deviation, n = 3
# technical replicates.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
# read the wide table
F_tab <- read_tsv("input/Fig6F_Phage_adsorption_assay.tsv")

# format the table for plotting
F_tab %<>% 
    # convert it to long format
    pivot_longer(!isolate, names_to = "value_type", values_to = "log10_FC") %>% 
    # create a new column for phage
    separate(value_type, into = c("phage", "replicate"), sep = "_") %>%
    # mutate isolate and phage to factors
    mutate(isolate = factor(isolate, levels = c("Aci 110", "A110-1", 
                                                "A110-2", "A110-G1")),
           phage = factor(phage, levels = c("H", "S", "F", "Ph")))

#xxxxxxxxxxxxxxxxxx
# Mean line with values -------------------------------------------------------
#xxxxxxxxxxxxxxxxxx

p_F <- F_tab %>% 
    ggplot(aes(x = phage, y = log10_FC, colour = phage)) +
    # add a line at y = 0
    geom_hline(yintercept = 0, linetype = "solid", colour = "gray70") +
    geom_point(position = position_jitterdodge(jitter.width = 1, 
                                               dodge.width = 1), size = 1.2) +
    # mean bars
    stat_summary(aes(ymax = after_stat(y), ymin = after_stat(y)),
                 fun = mean, geom = "errorbar", width = 0.8, linewidth = 0.6,
                 colour = "gray40") +
    scale_colour_manual(values = Colour_list$Fig6F, name = "Phage:") +
    facet_wrap(~isolate, nrow = 1) +
    # change y axis label to "log10 fold change in free phages"
    labs(x = "", y = expression(paste("log"[10], " fold change in free phage titers"))) +
    theme_linedraw(base_size = 14) +
    # colour the x axis values and ticks to transparent
    theme(axis.text.x = element_text(color = "transparent"),
          axis.ticks.x = element_line(color = "transparent"),
          # legend
          legend.position = "top",
          legend.key.size = unit(0, "cm"),
          # move the legend closer to the plot
          legend.margin = margin(t = 0, r = 0, b = -8, l = 0),
          # font
          text = element_text(size = 8, family = "Arial"),
          axis.text.y = element_text(size = 8),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          # remove the horizontal grid lines
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          # no background for facet headers
          strip.background = element_rect(fill = NA, color = "black"),
          strip.text = element_text(size = 8, color = "black", face = "bold", 
                                    family = "Arial"),
          # reduce spacing between facets
          panel.spacing = unit(0, "lines"))

#xxxxxxxxxxx
## Change facet header text colour -------
#xxxxxxxxxxx
p_F <- ggplot_gtable(ggplot_build(p_F))

strips <- which(grepl('strip-', p_F$layout$name))

for (i in seq_along(strips)) {
    k <- which(grepl('rect', p_F$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    l <- which(grepl('titleGrob', p_F$grobs[[strips[i]]]$grobs[[1]]$childrenOrder))
    # background colour
    # p_F$grobs[[strips[i]]]$grobs[[1]]$children[[k]]$gp$fill <- Colour_list$Fig6Faxis[i]
    # text colour
    p_F$grobs[[strips[i]]]$grobs[[1]]$children[[l]]$children[[1]]$gp$col <- Colour_list$Fig6Faxis[i]
}

rm(strips, k, l, i)

# convert back to ggplot object
p_F <- ggplotify::as.ggplot(p_F)



 
