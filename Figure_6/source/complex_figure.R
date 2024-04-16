library(tidyverse)
# library(patchwork)
library(cowplot)

set.seed(1)
#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Creating the complex figure --------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

# A: 
# B: 
# C: 
# D: 
grConvert::convertPicture("input/Fig6D.pdf", "input/Fig6D_cairo.svg")
p_D <- grImport2::readPicture("input/Fig6D_cairo.svg")

# E: 
# F:
# G: 
p_G <- png::readPNG("input/Fig6G_cells_tem_white_bg_v2.png", native = TRUE) %>% 
    grid::rasterGrob()

composit_EFG <- plot_grid(p_E, p_F, p_G, 
          labels = c("E", "F", "G"), label_fontface = "plain", label_size = 12,
          nrow = 1, align = "h")

plot_grid(grImport2::pictureGrob(p_D), composit_EFG, 
          labels = c("D", ""), label_fontface = "plain", label_size = 12,
          ncol = 1)



# Each figure should fit on a single 8.5” x 11” page
ggsave("output/Figure6.pdf", units = "in", height = 6, width = 8.5)
# ggsave("output/Figure6.pdf", units = "in", height = 8, width = 12)
