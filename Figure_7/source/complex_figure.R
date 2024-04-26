library(tidyverse)
library(cowplot)
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxxxxxxxx
# AIM: Creating the complex figure --------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxxxxx

composit_ABC <- plot_grid(p_A, NULL, p_B, NULL, p_C,
                          labels = c("A", "B", "", "C",""), label_fontface = "plain", label_size = 12,
                          nrow = 1, align = "h", axis = "l", rel_widths = c(1, 0.1, 1, 0.1, 1)) %>% 
    plot_grid(p_ABClegend, NULL, ., ncol = 1, rel_heights = c(0.1, 0.01, 0.9))

composit_DEF <- plot_grid(p_D, NULL, p_E, NULL, p_F,
                          labels = c("D", "", "E", "", "F"), label_fontface = "plain", label_size = 12,
                          nrow = 1, 
                          align = "h", axis = "t",
                          rel_widths = c(0.5, 0.05, 1, 0.1, 1))


p <- plot_grid(composit_ABC, composit_DEF, ncol = 1, rel_heights = c(0.8, 1))

# Each figure should fit on a single 8.5” x 11” page
ggsave("output/Figure7.pdf", p, units = "in", height = 6, width = 8.5)

# to convert the pdf to good resolution png:
# convert -density 300 -trim output/Figure7.pdf -quality 100 output/Figure7.png
