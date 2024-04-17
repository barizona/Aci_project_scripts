library(tidyverse)
library(magrittr)
library(scales)
#xxxxxxxxxxxxxxxxxxxx
# AIM: Plot groth curces Figure 6 A ---------------------------
#xxxxxxxxxxxxxxxxxxxx

# Description: 
# Growth curves (OD600) measured for 24 hours for 41 randomly
# selected ST2-KL3 European isolates either in the absence (untreated) or in the
# presence of the Highwayman (H) and Silvergun (S) phages alone, or in
# combination (HS). Plotted values represent the mean ± CI 95%.

#xxxxxxxxxx
## Input ----
#xxxxxxxxxx
A_tab <- read_tsv("input/Fig6A.tsv") %>% 
    # convert it to long format
    pivot_longer(!time, names_to = "Treatment_rep", values_to = "Growth") %>% 
    # create a new column for Treatment and Replicate
    separate(Treatment_rep, into = c("Treatment", "Replicate"), sep = "_") %>% 
    mutate(Treatment = factor(Treatment, levels = c("untreated", "H", "S", "HS")),
           Replicate = as.numeric(Replicate))

# calculate means and 95% confidence intervals by time and Replicates
A_tab %<>% 
    group_by(time, Treatment) %>% 
    # calculate means and 95% confidence intervals by time and Replicates
    summarise(mean = mean(Growth), CI = 1.96 * sd(Growth)/sqrt(n()))

A_tab %>% 
    select(time) %>% 
    unique() %>% 
    mutate(time = as.POSIXct(time, format = "%H:%M")) %>% 
    print(n = 100)

# time to hh:00
A_tab %<>% 
    mutate(time = as.POSIXct(time, format = "%H:%M"))

#xxxxxxxxxxxxxxxxxxxxxxxx
## Growth cure plot ------
#xxxxxxxxxxxxxxxxxxxxxxxx

#xxxxxx
# for perfect x axis
#xxxxxx
# Find the minimum and maximum time values
min_time <- min(A_tab$time)
max_time <- max(A_tab$time)

# Define the limits for the x-axis
A_x_limits <- as.POSIXct(c("1970-01-01 00:00:00", "1970-01-02 00:00:00"), format = "%Y-%m-%d %H:%M:%S")

# Define breaks and labels for the x-axis
A_x_breaks <- seq(min_time, max_time, by = "4 hours")
A_x_labels <- format(A_x_breaks, "%H:%M")

# Replace "00:00" with "24:00" in the labels
A_x_labels[length(A_x_labels)] <- "24:00"

rm(min_time, max_time)

p_A <- A_tab %>% 
    ggplot(aes(x = time, group = Treatment)) + 
    geom_line(aes(y = mean, color = Treatment), size = 1) + 
    geom_ribbon(aes(y = mean, ymin = mean - CI, ymax = mean + CI, fill = Treatment), 
                alpha = 0.2) +
    scale_color_manual(values = Colour_list$Fig6A, name = NULL) +
    scale_fill_manual(values = Colour_list$Fig6A, name = NULL) +
    # x axis with hours only
    scale_x_datetime(limits = A_x_limits, breaks = A_x_breaks, labels = A_x_labels) +
    labs(x = "Time (hours)", y = expression(paste("OD"[600]))) +
    theme_linedraw(base_size = 14) +
    theme(# legend
          legend.position = c(0.1, 0.8),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.5, "cm"),
          # axis font size
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8),
          # remove the vertical grid lines
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          # remove the horizontal grid lines
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          # reduce spacing between facets
          panel.spacing = unit(0, "lines"))
