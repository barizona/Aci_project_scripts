library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # stat_pvalue_manual
#xxxxxxxxxxxxxxxxxxxxx
# AIM: Survival plot ---------------------------------------
#xxxxxxxxxxxxxxxxxxxxx

# Description: Kaplan-Meier curves showing the survival of G. mellonella larvae
# after infection with either phage-resistant ST2-KL3 CRAB lines or with
# wild-type counterparts. HSFPh- and HS-resistant lines (A110-G1, A110-2)
# have decreased virulence compared to their wild-type counterparts, in contrast
# to the H-resistant line (A110-1). **** indicates p < 0.0001 from
# two-sided Log-rank test, n = 10 larvae/group, ≥ 3 biological replicates/group,
# PBS means larvae injected only with PBS, inoculum size = 9*106 CFU.

B_tab <- read_tsv("input/Fig7BD_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", "A110-1", "A110-2", "A110-G1")) %>% 
    # convert to factor
    mutate(Strain = factor(Strain, levels=c("PBS", "Aci 110", "A110-1", 
                                            "A110-2", "A110-G1")))


B_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                      data = B_tab)

# pairwise two-sided Log-rank test
pairwise_survdiff(Surv(time = Time, event = Event) ~ Strain, data = B_tab, 
                  rho = 0, p.adjust.method = "none")
#             PBS     Aci 110 A110-1  A110-2
#     Aci 110 < 2e-16 -       -       -     
#     A110-1  1.4e-12 0.0022  -       -     
#     A110-2  0.4109  < 2e-16 8.4e-11 -     
#     A110-G1 0.5808  < 2e-16 2.4e-09 0.2443

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


p_B <- B_fit_data %>% 
    ggsurvplot(data = B_tab, 
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
               legend.labs = c("PBS", "Aci 110", "A110-1", 
                               "A110-2", "A110-G1"),
               legend.title = "",
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

p_B
p_B$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7B) +
    scale_fill_manual(values = Colour_list$Fig7B_alpha) +
    # signifinace
    stat_pvalue_manual(pval = 0.05, geom = "text", aes(label = paste("p =", ..p.value.format..)),
                                                   position = "identity", vjust = 1, hjust = 0.5, size = 3, direction = "vertical")
    theme(# remove the vertical grid lines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # remove the horizontal grid lines
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())



# TODO: add signifinace to the plot
