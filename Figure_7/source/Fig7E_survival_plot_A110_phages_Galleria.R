library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # get_legend
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxx
# AIM: Survival plot of PBS, Aci 110 (wt), Aci 110 (wt) + HS, Aci 110 (wt) + HSFPh, Figure 7E ----------------
#xxxxxxxxxxxxxxxxxxxxx

# Description: 
# Kaplan-Meier curves show that both the cocktail HS (MOI 10:10),
# and the cocktail HSFPh (MOI 10:1:0.1:0.1) significantly
# improve the survival of G. mellonella larvae infected with Aci 110 ST2-KL3
# isolate as compared to the untreated larvae (CFU = 9*106, n = 10
# larvae/group, ≥ 3 biological replicates/group, PBS – larvae injected only with
# PBS). **** indicates p < 0.0001 from a two-sided Log-rank test.

#xxxxxxxxxxxxxxxxxxxxxxx
# Input ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx

E_tab <- read_tsv("input/Fig7_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", "Aci 110 + HS 10", 
                         "Aci 110 + H10 + S1 + FP 0.1")) %>%
    # change the strain names
    mutate(Strain = case_match(Strain, 
                               "Aci 110" ~ "Aci 110 (wt)",
                               "Aci 110 + HS 10" ~ "Aci 110 (wt) + HS",
                               "Aci 110 + H10 + S1 + FP 0.1" ~ "Aci 110 (wt) + HSFPh",
                               .default = Strain)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("PBS", "Aci 110 (wt)", 
                                              "Aci 110 (wt) + HS", 
                                              "Aci 110 (wt) + HSFPh")))

# fit
E_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                      data = E_tab)
#xxxxxxx
## pairwise two-sided Log-rank test ----
#xxxxxxx
E_log_rank_test <- pairwise_survdiff(Surv(
    time = Time, event = Event) ~ Strain, data = E_tab, 
    rho = 0, p.adjust.method = "none")

# convert to stars indicating the level of significance
E_signif <- E_log_rank_test$p.value[c("Aci 110 (wt) + HS", "Aci 110 (wt) + HSFPh"), 
                                    "Aci 110 (wt)"] %>% 
    as_tibble() %>% 
    # create p.adj.signif column
    mutate(p_signif = ifelse(value < 0.0001, "****",
                             ifelse(value < 0.001, "***",
                                    ifelse(value < 0.01, "**",
                                           ifelse(value < 0.05, "*", "ns"))))) %>% 
    select(p_signif) %>% 
    pull()
names(E_signif) <- c("Aci 110 (wt) + HS", "Aci 110 (wt) + HSFPh")

#xxxxxxxxxx
## Plot data ----
#xxxxxxxxxx
p_E_v1 <- E_fit_data %>% 
    ggsurvplot(data = E_tab, 
               axes.offset = FALSE,
               pval = FALSE, 
               conf.int = 0.95, 
               risk.table = FALSE, 
               size = 0.6,
               title = NULL,
               xlim = c(0, 54), break.time.by = 6, 
               ylim = c(-0.05, 1.05), break.y.by = 0.2,
               conf.int.alpha = 0.2,
               xlab = "Time (h)",
               ylab = "Survival probability",
               legend.title = "",
               legend.labs = c("PBS", "Aci 110 (wt)", 
                               "Aci 110 (wt) + HS", 
                               "Aci 110 (wt) + HSFPh"),
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

# start and end points for segment
E_start_end <- p_E_v1$data.survplot %>% 
    filter(time == 48 & (Strain == "Aci 110 (wt)")) %>% 
    select(surv) %>% 
    pull()

E_start_end <- p_E_v1$data.survplot %>% 
    filter(time == 48 & (Strain == "Aci 110 (wt) + HS")) %>% 
    select(surv) %>% 
    pull() %>% 
    c(E_start_end, .)

E_start_end <- p_E_v1$data.survplot %>% 
    filter(time == 48 & (Strain == "Aci 110 (wt) + HSFPh")) %>% 
    select(surv) %>% 
    pull() %>% 
    c(E_start_end, .)

names(E_start_end) <- c("Aci 110 (wt)", "Aci 110 (wt) + HS", "Aci 110 (wt) + HSFPh")

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx

# read image
logo <- png::readPNG("input/Galleria_mellonella.png", native = TRUE) %>% 
    grid::rasterGrob(height = unit(0.4, "in"), y = 0.35, x = 0.02, just = "left")

p_E <- p_E_v1$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7E) +
    scale_fill_manual(values = Colour_list$Fig7E_alpha) +
    # indicate significance
    # from Aci 110 (wt) to Aci 110 (wt) + HSFPh
    annotate("segment", x = 51, xend = 51, 
             y = E_start_end["Aci 110 (wt)"], 
             yend = E_start_end["Aci 110 (wt) + HSFPh"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 50, xend = 51, 
             y = E_start_end["Aci 110 (wt)"], 
             yend = E_start_end["Aci 110 (wt)"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 50, xend = 51, 
             y = E_start_end["Aci 110 (wt) + HSFPh"], 
             yend = E_start_end["Aci 110 (wt) + HSFPh"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("text", x = 53, y = mean(c(E_start_end["Aci 110 (wt)"], 
                                        E_start_end["Aci 110 (wt) + HSFPh"])), 
             angle = 90, label = E_signif["Aci 110 (wt) + HSFPh"], size = 5, 
             hjust = 0.5, vjust = 0.5) +
    # from Aci 110 (wt) to Aci 110 (wt) + HS
    annotate("segment", x = 53, xend = 53, 
             y = E_start_end["Aci 110 (wt)"], 
             yend = E_start_end["Aci 110 (wt) + HS"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 52, xend = 53, 
             y = E_start_end["Aci 110 (wt)"], 
             yend = E_start_end["Aci 110 (wt)"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 52, xend = 53, 
             y = E_start_end["Aci 110 (wt) + HS"], 
             yend = E_start_end["Aci 110 (wt) + HS"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("text", x = 55, y = mean(c(E_start_end["Aci 110 (wt)"], 
                                        E_start_end["Aci 110 (wt) + HS"])), 
             angle = 90, label = E_signif["Aci 110 (wt) + HS"], size = 5, 
             hjust = 0.5, vjust = 0.5) +
    # add image
    annotation_custom(logo, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
    # legend to 2 rows
    guides(colour = guide_legend(nrow = 2, byrow = TRUE),
           fill = guide_legend(nrow = 2, byrow = TRUE), name = "") +
    theme(# legend
        legend.position = "top",
        legend.margin = margin(0, 0, 0, 0),
        # font
        text = element_text(size = 8, family = "Arial"),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# new scale for x-axis
p_E <- p_E + 
    scale_x_continuous(breaks = seq(0, 48, 6), labels = seq(0, 48, 6))

rm(E_signif, E_start_end, p_E_v1, logo)

