library(tidyverse)
library(cowplot)
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Creating the complex figure --------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

# A: p_A
# B: p_B
# C: p_C
# D: 


composit_A <- plot_grid(p_A1st, p_A1st, p_A1st,
                        nrow = 1, align = "h", axis = "l")

p <- plot_grid(p_Alegend, composit_A, ncol = 1, rel_heights = c(0.1, 0.9))

# Each figure should fit on a single 8.5” x 11” page
ggsave("output/Figure7.pdf", p, units = "in", height = 3, width = 8.5)
