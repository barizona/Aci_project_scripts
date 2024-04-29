library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # get_legend
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxx
# AIM: Survival plot of PBS, Aci 110 (wt), Aci 110 (wt) + HS, Aci 110 (wt) + HSFPh, Figure 7F ----------------
#xxxxxxxxxxxxxxxxxxxxx

# Description: 
# Kaplan-Meier curves showing that Highwayman phage alone (MOI 6),
# and the cocktail HSFPh (MOI 6:1:1:1) saved 80% and 100% of the
# mice, respectively, following infection with Aci-15 ST2-KL3 isolate as opposed
# to the untreated animals  that died (CFU = 109, n = 5 mice/group) ***
# indicates p < 0.00027 from the Log-rank test.

#xxxxxxxxxxxxxxxxxxxxxxx
# Input ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

F_tab <- read_tsv("input/Fig7F_mouse_exp_20240402.tsv") %>% 
    filter(Strain %in% c("Aci 15", "Aci 15 + H_60min", 
                         "Aci 15 + cocktail_60min")) %>% 
    filter(Treatment == "10_9") %>%
    # change the strain names
    mutate(Strain = case_match(Strain, 
                               "Aci 15 + H_60min" ~ "Aci 15 + H",
                               "Aci 15 + cocktail_60min" ~ "Aci 15 + HSFPh",
                               .default = Strain)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("Aci 15", "Aci 15 + H",
                                              "Aci 15 + HSFPh")))

# fit
F_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                      data = F_tab)

#xxxxxxxxxx
## Plot data ----
#xxxxxxxxxx
p_F_v1 <- F_fit_data %>% 
    ggsurvplot(data = F_tab, 
               axes.offset = FALSE,
               pval = FALSE, 
               conf.int = FALSE, 
               risk.table = FALSE, 
               size = 0.6,
               title = NULL,
               xlim = c(0, 7), break.time.by = 1, 
               ylim = c(-0.05, 1.05), break.y.by = 0.2,
               conf.int.alpha = 0.2,
               xlab = "Time (day)",
               ylab = "Survival probability",
               legend.title = "",
               legend.labs = c("Aci 15", "Aci 15 + H",
                               "Aci 15 + HSFPh"),
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx

# read image
logo <- png::readPNG("input/Mouse.png", native = TRUE) %>% 
    grid::rasterGrob(height = unit(0.4, "in"), y = 0.35, x = 0.4, just = "left")

p_F <- p_F_v1$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7F) +
    # remove y lab
    ylab(NULL) +
    # add image
    annotation_custom(logo, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    # legend to 2 rows
    guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(# legend
        legend.position = "top",
        legend.text = element_text(size = 8),
        legend.margin = margin(0, 0, 0, 0),
        # axis font size
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


rm(p_F_v1, logo)

