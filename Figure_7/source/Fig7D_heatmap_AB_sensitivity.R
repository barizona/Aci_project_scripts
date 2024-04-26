library(tidyverse)
library(ggtext) # for superscripts: theme(... element_markdown())
#xxxxxxxxxxxxxxxxxxxxxx

#xxxxxxxxxxxxxxxxxxxxx
# AIM: AB sensitivity heatmap, Figure 7D --------------------------------------
#xxxxxxxxxxxxxxxxxxxxx

# Description: 
# Antibiotic sensitivity profile of the HSFPh cocktail-resistant
# lines. The intensity of the orange shading indicates the median (n≥3) log2
# fold change in the MIC of the cocktail-resistant lines in comparison to their
# wild-type counterparts. The 5 antibiotics with
# clinical relevance are MER-meropenem, IMI-Imipenem, COL-colistin,
# LEV-levofloxacin, T-S- a combination of Trimethoprim and Sulfamethoxazole (1:5
# ratio). The star indicates a transition from the resistant state to the
# intermediate state according to the EUCAST clinical breakpoint.

#xxxxxxxxxxxxxxxxxxxxxxx
# Input ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx
D_tab <- read_tsv("input/Fig7D_AB_sensitivity.tsv") %>% 
    # conevrt to long
    pivot_longer(cols = -AB, names_to = "Strain", values_to = "Value") %>% 
    # convert to factor
    mutate(AB = factor(AB, levels = c("COL", "T-S", "LEV", "IMI", "MER")),
           Strain = factor(Strain, levels = c("A110-G1", "A107-E1")))

#xxxxxxxxxxxxxxxxxxxxxxx
# Plot --------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

p_D <- D_tab %>% 
    ggplot(aes(x = Strain, y = AB, fill = Value)) +
    geom_tile(color = "black") +
    # grayscale
    scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
    # add stars to the 5th row
    geom_text(data = D_tab %>% filter(AB == "MER"), aes(label = "*"), 
              size = 8, vjust = 0.7, color = "white") +
    labs(x = NULL, y = NULL, fill = "log<sup>2</sup> fold change in MIC") +
    guides(fill = guide_colorbar(title.position = "top")) +
    theme_void() +
    theme(legend.position = "top",
          legend.title = element_markdown(size = 8),
          legend.text = element_text(size = 8),
          # axis font
          axis.text.x = element_text(size = 8, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 8))


