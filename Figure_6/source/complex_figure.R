library(tidyverse)
library(cowplot)
library(ggtext) # for superscripts: theme(... element_markdown())

set.seed(1)
#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Creating the complex figure --------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

# A: p_A
# B: p_B
# C: p_C
# D: 
grConvert::convertPicture("input/Fig6D.pdf", "input/Fig6D_cairo.svg")
p_D <- grImport2::readPicture("input/Fig6D_cairo.svg")
# E: p_E
# F: P_F
# G: 
p_G <- png::readPNG("input/Fig6G_cells_tem_white_bg_v2.png", native = TRUE) %>% 
    grid::rasterGrob()

composit_ABC <- plot_grid(p_A, p_B, p_C,
                          labels = c("A", "B", "C"), label_fontface = "plain", label_size = 12,
                          nrow = 1, align = "h")

composit_EFG <- plot_grid(p_E, p_F, p_G, 
                          labels = c("E", "F", "G"), label_fontface = "plain", label_size = 12,
                          nrow = 1, align = "h")

p <- plot_grid(composit_ABC, grImport2::pictureGrob(p_D), composit_EFG, 
               labels = c("", "D", ""), label_fontface = "plain", label_size = 12,
               ncol = 1)

# Each figure should fit on a single 8.5” x 11” page
ggsave("output/Figure6.pdf", p, units = "in", height = 9, width = 8.5)

sessionInfo() %>%
  capture.output() %>%
  writeLines("output/Fig6_sessionInfo.txt")
