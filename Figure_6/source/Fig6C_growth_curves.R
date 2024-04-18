library(tidyverse)
library(magrittr)
library(scales)
#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot groth curces Figure 6 C ---------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description: 
# Growth curves (OD600) were measured for 72 hours for 25 randomly
# selected ST2-KL3 isolates in the absence (untreated) or in the presence of the
# phage cocktail HSFPh. Plotted values represent the mean ± CI 95%. Panel A, B,
# and C data are available in Supplementary Table 19.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
C_tab <- read_tsv("input/Fig6C.tsv") %>% 
    # convert it to long format
    pivot_longer(!time, names_to = "Treatment_rep", values_to = "Growth") %>% 
    # create a new column for Treatment and Replicate
    separate(Treatment_rep, into = c("Treatment", "Replicate"), sep = "_") %>%
    mutate(Treatment = factor(Treatment, levels = c("untreated", "HSFPh")),
           Replicate = as.numeric(Replicate))

# calculate means and 95% confidence intervals by time and Replicates
C_tab %<>% 
    group_by(time, Treatment) %>% 
    # calculate means and 95% confidence intervals by time and Replicates
    summarise(mean = mean(Growth), CI = 1.96 * sd(Growth)/sqrt(n()))

# time to hh:00
C_tab %<>% 
    mutate(time = as.POSIXct(time, format = "%H:%M"))

#xxxxxxxxxxxxxxxxxxxxxxxx
## Growth cure plot ------
#xxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxx
# for perfect x axis
#xxxxxx
# Find the minimum and maximum time values
min_time <- min(C_tab$time)
max_time <- max(C_tab$time)

# Define the limits for the x-axis
C_x_limits <- as.POSIXct(c("1970-01-01 00:00:00", "1970-01-04 00:00:00"), format = "%Y-%m-%d %H:%M:%S")

# Define breaks and labels for the x-axis
C_x_breaks <- seq(min_time, max_time, by = "12 hours")
C_x_labels <- format(C_x_breaks, "%H:%M")
C_x_labels
# [1] "00:00" "12:00" "00:00" "12:00" "00:00" "12:00" "00:00"

# Replace 
C_x_labels = c("00:00", "12:00", "24:00", "36:00", "48:00", "60:00", "72:00")

rm(min_time, max_time)

p_C <- C_tab %>% 
    ggplot(aes(x = time, group = Treatment)) + 
    geom_line(aes(y = mean, color = Treatment), linewidth = 1) + 
    geom_ribbon(aes(y = mean, ymin = mean - CI, ymax = mean + CI, fill = Treatment), 
                alpha = 0.2) +
    scale_color_manual(values = Colour_list$Fig6C, name = NULL) +
    scale_fill_manual(values = Colour_list$Fig6C, name = NULL) +
    # x axis with hours only
    scale_x_datetime(limits = C_x_limits, breaks = C_x_breaks, labels = C_x_labels) +
    labs(x = "Time (hours)", y = expression(paste("OD"[600]))) +
    theme_linedraw(base_size = 14) +
    theme(# legend
        legend.position = c(0.8, 0.7),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"),
        # axis font size
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # legend background to transparent
        legend.background = element_rect(fill = "transparent"))
