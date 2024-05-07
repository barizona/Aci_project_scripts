library(tidyverse)
library(survival) # survfit
library(survminer) # ggsurvplot
library(ggpubr) # get_legend
library(ggtext) # for superscripts: theme(... element_markdown())

#xxxxxxxxxxxxxxxxxxxxxxxx
# A single legend for ABC plots -------------------------------------------
#xxxxxxxxxxxxxxxxxxxxxxxx
ABClegend_tab <- read_tsv("input/Fig7_Exp_20230911_19.tsv") %>% 
    filter(Strain %in% c("PBS", "Aci 110", "A110-1", "A110-2", "A110-G1")) %>%
    # change Strain names
    mutate(Strain = case_match(Strain,
                               "Aci 110" ~ "Aci 110 (wt)",
                               "A110-1" ~ "A110-1 (H<sup>R</sup>)",
                               "A110-2" ~ "A110-2 (HS<sup>R</sup>)",
                               "A110-G1" ~ "A110-G1 (HSFPh<sup>R</sup>)",
                               .default = Strain)) %>%
    # convert to factor
    mutate(Strain = factor(Strain, levels = c("PBS", "Aci 110 (wt)", 
                                              "A110-G1 (HSFPh<sup>R</sup>)",
                                              "A110-2 (HS<sup>R</sup>)",
                                              "A110-1 (H<sup>R</sup>)")))

# fit
ABClegend_fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                              data = ABClegend_tab)

#xxxxxxxxxx
## Plot data ----
#xxxxxxxxxx
p_ABClegend_v1 <- ABClegend_fit_data %>% 
    ggsurvplot(data = ABClegend_tab, 
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
               legend.labs = levels(ABClegend_tab$Strain),
               legend.title = "",
               font.legend = list(size = 10),
               ggtheme = theme_linedraw())

#xxxxxxxxxx
## Plot ----
#xxxxxxxxxx

p_ABClegend <- p_ABClegend_v1$plot +
    # colour
    scale_color_manual(values = Colour_list$Fig7ABClegend) +
    scale_fill_manual(values = Colour_list$Fig7ABClegend_alpha) +
    theme(# legend
          legend.position = "top",
          legend.text = element_markdown(size = 8, family = "Arial"))


### saving legend to a variable ----
p_ABClegend <- cowplot::get_plot_component(p_ABClegend, "guide-box-top", return_all = FALSE)

rm(p_ABClegend_v1)
