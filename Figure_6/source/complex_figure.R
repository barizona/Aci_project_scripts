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
# G: p_G
# H: 
p_H <- png::readPNG("input/Fig6H_cells_tem_white_bg.png", native = TRUE) %>% 
    grid::rasterGrob(height = unit(2.54, "in"), y = 0.53)

composit_ABC <- plot_grid(p_A, p_B, p_C,
                          labels = c("A", "B", "C"), label_fontface = "plain", label_size = 12,
                          nrow = 1, align = "h")


composit_DE <- plot_grid(grImport2::pictureGrob(p_D), 
                         plot_grid(NULL, p_E, NULL, ncol = 1, align = "h", rel_heights = c(0.22, 1, 0.22)), 
                         NULL,
                         labels = c("D", "E", ""), label_fontface = "plain", label_size = 12,
                         nrow = 1, rel_widths = c(1, 0.15, 0.03))

composit_FGH <- plot_grid(p_F, p_G, p_H, 
                          labels = c("F", "G", "H"), label_fontface = "plain", label_size = 12,
                          nrow = 1, align = "h")

p <- plot_grid(composit_ABC, composit_DE, composit_FGH, 
               ncol = 1)


# Each figure should fit on a single 8.5” x 11” page
ggsave("output/Figure6.pdf", p, units = "in", height = 8.3, width = 8.5)

# to convert the pdf to good resolution png:
# convert -density 300 -trim output/Figure6.pdf -quality 100 output/Figure6.png

sessionInfo() %>%
  capture.output() %>%
  writeLines("output/sessionInfo.txt")
