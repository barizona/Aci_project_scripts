library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # stat_pvalue_manual
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

A1st_tab <- read_tsv("input/Fig7BD_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", "A110-1")) %>% 
    # change Strain names
    mutate(Strain = case_match(Strain,
                               "Aci 110" ~ "Aci 110 wt",
                               "A110-1" ~ "A110-1 H<sup>R</sup>",
                                .default = Strain)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("PBS", "Aci 110 wt", 
                                              "A110-1 H<sup>R</sup>")))

# fit
A1st_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                         data = A1st_tab)

#xxxxxxx
## pairwise two-sided Log-rank test ----
#xxxxxxx
A1st_log_rank_test <- pairwise_survdiff(Surv(
    time = Time, event = Event) ~ Strain, data = A1st_tab, 
    rho = 0, p.adjust.method = "none")

# create a tibble with a single row to compare to Aci 110 wt
A1st_log_rank_test_tab <- 
    A1st_log_rank_test$p.value["A110-1 H<sup>R</sup>","Aci 110 wt"] %>% 
    as_tibble() %>% 
    # rename column
    rename(p.adj = value) %>%
    # create p.adj.signif column
    mutate(p.adj.signif = ifelse(p.adj < 0.0001, "****",
                          ifelse(p.adj < 0.001, "***",
                          ifelse(p.adj < 0.01, "**",
                          ifelse(p.adj < 0.05, "*", "ns"))))) %>% 
    # add group1 group2
    mutate(group1 = "A110-1 H<sup>R</sup>",
           group2 = "Aci 110 wt")
    


# TODO: create a table and plot like here:
# https://rpkgs.datanovia.com/ggpubr/reference/stat_pvalue_manual.html
# library(ggsurvplot)
# library(ggpubr)
# 
# # Example data and plot
# fit <- survfit(Surv(time, status) ~ sex, data = lung)
# ggsurv <- ggsurvplot(fit)
# 
# # Vertical positioning of p-values
# ggsurv + stat_pvalue_manual(pval = 0.05, geom = "text", aes(label = paste("p =", ..p.value.format..)),
#                             position = "identity", vjust = 1, hjust = 0.5, size = 3, direction = "vertical")

# https://www.datanovia.com/en/blog/how-to-add-p-values-onto-horizontal-ggplots/

#xxxxxxxxxx
## plot ----
#xxxxxxxxxx
p_A1st <- A1st_fit_data %>% 
    ggsurvplot(data = A1st_tab, 
               axes.offset = FALSE,
               pval = FALSE, 
               conf.int = 0.95, 
               risk.table = FALSE, 
               title = NULL,
               xlim = c(0, 48), break.time.by = 6, 
               ylim = c(0, 1), break.y.by = 0.2,
               conf.int.alpha = 0.2,
               xlab = "Time (h)",
               ylab = "Survival probability",
               font.x = 9,
               font.y = 9, 
               font.tickslab = 8,
               legend = "top",
               legend.labs = c("PBS", "Aci 110 wt", "A110-1 H<sup>R</sup>"),
               legend.title = "",
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

p_A1st$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7A1st) +
    scale_fill_manual(values = Colour_list$Fig7A1st_alpha) +
    # add space to x axis for indicating the significance
    xlim(0, 50) +
    # indicate the p-value
    # stat_pvalue_manual(A1st_log_rank_test_tab, label = "p.adj.signif", 
    #                    hide.ns = TRUE, tip.length = 0.02, step.increase = 0.07,
    #                    vjust = 0.8, size = 3, 
    #                    x.position = 1, y.position = 1, coord.flip = TRUE) + 
    annotate("segment", x = 48, xend = 48, y = 0.2, yend = 0.8,  # Adjust these values for positioning
             color = "black", size = 1) +
    annotate("text", x = 49, y = 0.8, label = "****", size = 3) +
    theme(# legend
        legend.text = element_markdown(size = 8),
        # remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())



# TODO: continue with annotate
# change the tick labs on x axis (48)
# delete the p-val tab
