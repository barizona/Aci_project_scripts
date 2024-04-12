library(tidyverse)
library(magrittr)
#xxxxxxxxxxxxxxxxxxxx
# AIM: mean-variance plot of phage absorption for Figure 6 E -------------------
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
E_tab <- read_tsv("input/Fig6E_Phage_adsorption_assay.tsv")

# format the table for plotting
E_tab %<>% 
    # convert it to long format
    pivot_longer(!isolate, names_to = "value_type", values_to = "log10_FC") %>% 
    # create a new column for phage
    separate(value_type, into = c("phage", "type"), sep = "_") %>%
    # convert it to wider format having mean, lower and upper sd-s in columns
    pivot_wider(names_from = type, values_from = log10_FC)

#xxxxxxxxxxxxxxxxxx
# Errorbar plot -----------------------------------------------------------
#xxxxxxxxxxxxxxxxxx

E_tab %>% 
    ggplot(aes(x = isolate, y = mean, color = phage)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = higher),
                  width = 0.5, position = position_dodge(width = 0.5)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") + 
    theme_bw()


    
