library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # get_legend
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxx
# AIM: Survival plot Figure 7A (1st, 2nd, 3rd) -------------------------------
#xxxxxxxxxxxxxxxxxxxxx

# Description: Kaplan-Meier curves showing the survival of G. mellonella larvae
# after infection with either phage-resistant ST2-KL3 CRAB lines or with
# wild-type counterparts. HSFPh- and HS-resistant lines (A110-G1, A110-2)
# have decreased virulence compared to their wild-type counterparts, in contrast
# to the H-resistant line (A110-1). **** indicates p < 0.0001 from
# two-sided Log-rank test, n = 10 larvae/group, ≥ 3 biological replicates/group,
# PBS means larvae injected only with PBS, inoculum size = 9*106 CFU.

#xxxxxxxxxxxxxxxxxxxxxxx
# 1st A110-1 ------------------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxx
# changing the strain names
A_var_aci <- "A110-1"

A1st_tab <- read_tsv("input/Fig7BD_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", A_var_aci)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("PBS", "Aci 110", A_var_aci)))

# fit
A1st_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                         data = A1st_tab)
#xxxxxxx
## pairwise two-sided Log-rank test ----
#xxxxxxx
A1st_log_rank_test <- pairwise_survdiff(Surv(
    time = Time, event = Event) ~ Strain, data = A1st_tab, 
    rho = 0, p.adjust.method = "none")

# convert to stars indicating the level of significance
A_signif <- A1st_log_rank_test$p.value[A_var_aci,] %>% 
    as_tibble() %>% 
    # create p.adj.signif column
    mutate(p_signif = ifelse(value < 0.0001, "****",
                             ifelse(value < 0.001, "***",
                                    ifelse(value < 0.01, "**",
                                           ifelse(value < 0.05, "*", "ns"))))) %>% 
    select(p_signif) %>% 
    pull()
names(A_signif) <- c("PBS", "Aci 110")

#xxxxxxxxxx
## Plot data ----
#xxxxxxxxxx
p_A1st_v1 <- A1st_fit_data %>% 
    ggsurvplot(data = A1st_tab, 
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
               legend.labs = c("PBS", "Aci 110", A_var_aci),
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

# start and end points for segment
A_start_end <- p_A1st_v1$data.survplot %>% 
    filter(time == 48 & (Strain == "PBS")) %>% 
    select(surv) %>% 
    pull()

A_start_end <- p_A1st_v1$data.survplot %>% 
    filter(time == 48 & (Strain == "Aci 110")) %>% 
    select(surv) %>% 
    pull() %>% 
    c(A_start_end, .)

A_start_end <- p_A1st_v1$data.survplot %>% 
    filter(time == 48 & (Strain == A_var_aci)) %>% 
    select(surv) %>% 
    pull() %>% 
    c(A_start_end, .)

names(A_start_end) <- c("PBS", "Aci 110", A_var_aci)

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx

p_A1st <- p_A1st_v1$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7A1st) +
    scale_fill_manual(values = Colour_list$Fig7A1st_alpha) +
    # indicate significance
    # from Aci 110 wt
    annotate("segment", x = 51, xend = 51, 
             y = A_start_end["Aci 110"], yend = A_start_end[3], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 50, xend = 51, 
             y = A_start_end["Aci 110"], yend = A_start_end["Aci 110"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 50, xend = 51, 
             y = A_start_end[3], yend = A_start_end[3], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("text", x = 53, y = mean(c(A_start_end["Aci 110"], A_start_end[3])), 
             angle = 90, label = A_signif["Aci 110"], size = 5, 
             hjust = 0.5, vjust = 0.5) +
    # from PBS wt
    annotate("segment", x = 53, xend = 53, 
             y = A_start_end["PBS"], yend = A_start_end[3], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 52, xend = 53, 
             y = A_start_end["PBS"], yend = A_start_end["PBS"], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("segment", x = 52, xend = 53, 
             y = A_start_end[3], yend = A_start_end[3], 
             color = "black", linewidth = 0.3, lineend = "round") +
    annotate("text", x = 55, y = mean(c(A_start_end["PBS"], A_start_end[3])), 
             angle = 90, label = A_signif["PBS"], size = 5, 
             hjust = 0.5, vjust = 0.5) +
    theme(# legend
        legend.position = "none",
        # axis font size
        # axis.text.x = element_text(),
        # axis.ticks.x = element_line(),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# new scale for x-axis
p_A1st <- p_A1st + 
    scale_x_continuous(breaks = seq(0, 48, 6), labels = seq(0, 48, 6))

rm(A_var_aci, A_signif, A_start_end)

# TODO: remove y lab from 2nd, 3rd
# ylab(NULL)

#xxxxxxxxxxxxxxxxxxxxxxxx
# A single legend for all plots -------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx
Alegend_tab <- read_tsv("input/Fig7BD_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", "A110-1", "A110-2", "A110-G1")) %>%
    # change Strain names
    mutate(Strain = case_match(Strain,
                               "Aci 110" ~ "Aci 110 wt",
                               "A110-1" ~ "A110-1 H<sup>R</sup>",
                               "A110-2" ~ "A110-2 HS<sup>R</sup>",
                               "A110-G1" ~ "A110-G1 HSFPh<sup>R</sup>",
                               .default = Strain)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("PBS", "Aci 110 wt", 
                                              "A110-1 H<sup>R</sup>",
                                              "A110-2 HS<sup>R</sup>",
                                              "A110-G1 HSFPh<sup>R</sup>")))

# fit
Alegend_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                         data = Alegend_tab)

#xxxxxxxxxx
## Plot data ----
#xxxxxxxxxx
p_Alegend_v1 <- Alegend_fit_data %>% 
    ggsurvplot(data = A1st_tab, 
               axes.offset = FALSE,
               pval = FALSE, 
               conf.int = 0.95, 
               risk.table = FALSE, 
               title = NULL,
               xlim = c(0, 53), break.time.by = 6, 
               ylim = c(0, 1), break.y.by = 0.2,
               conf.int.alpha = 0.2,
               xlab = "Time (h)",
               ylab = "Survival probability",
               legend = "top",
               legend.labs = levels(Alegend_tab$Strain),
               legend.title = "",
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

# extract the table to plot
Alegend_tab_plot <- p_Alegend_v1$data.survplot %>% 
    tibble() %>% 
    # factorize Strain
    mutate(Strain = factor(Strain, levels = levels(Alegend_tab$Strain)))
    
rm(p_Alegend_v1)


p_Alegend <- Alegend_tab_plot %>% 
    ggplot(aes(x = time, group = Strain)) + 
    geom_line(aes(y = surv, color = Strain), linewidth = 0.6) + 
    geom_ribbon(aes(y = surv, ymin = lower, ymax = upper, fill = Strain), 
                alpha = 0.2) +
    # colour
    scale_color_manual(name = NULL, values = Colour_list$Fig7Alegend) +
    scale_fill_manual(name = NULL, values = Colour_list$Fig7Alegend_alpha) +
    theme(# legend
        legend.position = "top",
        legend.text = element_markdown(size = 8))

p_Alegend <- get_legend(p_Alegend)

