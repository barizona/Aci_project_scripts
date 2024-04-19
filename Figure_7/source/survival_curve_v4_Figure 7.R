library(tidyverse)

library(survminer)
library(survival)

library(ggsci)

library(viridis)
library(ggtext)

#####################
# Fig 7/A left subfigure #####
#### A110-G1 ####

data <- read_delim("Exp_20230911_19.csv", ",", 
                   escape_double = FALSE, trim_ws = TRUE)

phr <- data %>% filter(Strain=="PBS" | Strain=="Aci 110" | Strain=="A110-G1")

phr <- phr %>% 
  mutate(Strain=factor(Strain, ordered = TRUE,
                       levels=c("PBS", "Aci 110", "A110-G1")))

cols_line <- c("#A67B5B", "#ADB6B6ff", "#FFB547FF" )
cols_name <- c("#A67B5B", "#ADB6B6ff", "#FFB547FF") 

labels <- function(x, cols_name) {
  glue::glue("<span style = 'color: {cols_name}'>{x}</span>")
}

fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                    data = phr)

gg <- ggsurvplot(fit_data, data = phr, 
                 axes.offset = F,
                 pval = F, conf.int = 0.95, 
                 risk.table = FALSE, 
                 title = NULL,
                 xlim = c(0,48), break.time.by = 6, 
                 ylim = c(0,1), break.y.by = 0.2,
                 palette= cols_line, 
                 conf.int.alpha = 0.2,
                 xlab="\nTime (h)",
                 ylab="Survival probability\n",
                 font.x = c(15),
                 font.y = c(15), font.tickslab = c(12),
                 legend = "top",
                 legend.labs = c("PBS", "Aci 110", "A110-G1"),
                 legend.title = "",
                 font.legend = list(size = 10))
#guides(colour = guide_legend(nrow = 2))

#gg <- gg$plot +
#theme(legend.text = element_markdown())

##### colouring the legend text

gg <- gg$plot +
  scale_color_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  scale_fill_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  theme(legend.text = element_markdown())


gg_decon <- ggplot_build(gg)
ng <- length(unique(gg_decon$data[[1]]$colour))

# modify the lines 
gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour))-2)), y))
# modify the censor point
gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour)))-2), y))
# from https://ggplot2-book.org/internals.html
gtable <- ggplot_gtable(gg_decon)

png(filename = "110_G1_top.png",  units = "mm", width = 90, height = 90, bg = "transparent", res=800)
grid::grid.newpage()
grid::grid.draw(gtable)
dev.off()

####### Fig 7/A middle subfigure #####
####### A110-2 #####

data <- read_delim("Exp_20230911_19.csv", ",", 
                   escape_double = FALSE, trim_ws = TRUE)


data %>% distinct(Strain)

data <- data %>% filter(Strain=="PBS" | Strain=="Aci 110" | Strain== "A110-2") 


data <- data %>% 
  mutate(Strain=factor(Strain, ordered = TRUE,
                       levels=c("PBS", "Aci 110", "A110-2")))

cols_line <- c("#A67B5B", "#ADB6B6ff", "#E762D7FF")
cols_name <- c("#A67B5B", "#ADB6B6ff", "#E762D7FF")

labels <- function(x, cols_name) {
  glue::glue("<span style = 'color: {cols_name}'>{x}</span>")
}

fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                    data = data)

gg <- ggsurvplot(fit_data, data = data, 
                 axes.offset = F,
                 pval = F, conf.int = 0.95, 
                 #linetype = "strata", # Change line type by groups
                 #surv.median.line = "hv", # Specify median survival
                 #ggtheme = theme_bw(), # Change ggplot2 theme
                 risk.table = FALSE, 
                 title = "",
                 xlim = c(0,48), break.time.by = 6, 
                 ylim = c(0,1), break.y.by = 0.2,
                 palette= cols_line, 
                 conf.int.alpha = 0.2,
                 xlab="\nTime (h)",
                 #ylab="Survival probability\n",
                 ylab = "",
                 font.x = c(15),
                 font.y = c(15), font.tickslab = c(12),
                 legend = "top",
                 legend.labs = c("PBS","Aci 110","A110-2"),
                 legend.title = "",
                 font.legend = list(size = 10))

#guides(colour = guide_legend(nrow = 2))

#gg <- gg$plot +
#theme(legend.text = element_markdown())


##### colouring the legend text

gg <- gg$plot +
  scale_color_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  scale_fill_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  theme(legend.text = element_markdown())


gg_decon <- ggplot_build(gg)
ng <- length(unique(gg_decon$data[[1]]$colour))

# modify the lines 
gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour))-2)), y))
# modify the censor point
gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour)))-2), y))
# from https://ggplot2-book.org/internals.html
gtable <- ggplot_gtable(gg_decon)

png(filename = "A110_2_top.png",  units = "mm", width = 90, height = 90, bg = "transparent", res=800)
grid::grid.newpage()
grid::grid.draw(gtable)
dev.off()

####################
####### Fig 7/A right subfigure #####
####### A110-1 #####


data <- read_delim("Exp_20230911_19.csv", ",", 
                   escape_double = FALSE, trim_ws = TRUE)


data <- data %>% filter(Strain=="PBS" | Strain=="Aci 110" | Strain== "A110-1") 
data %>% distinct(Strain)


data <- data %>% 
  mutate(Strain=factor(Strain, ordered = TRUE,
                       levels=c("PBS", "Aci 110", "A110-1")))

#data2 <- data %>% 
#mutate(Strain=factor(Strain, ordered = TRUE, levels=c("PBS", "A110-1"))) # p < 0.0001

#data3 <- data %>% 
#mutate(Strain=factor(Strain, ordered = TRUE, levels=c("Aci 110", "A110-1"))) # p = 0.0022

labels <- function(x, cols_name) {
  glue::glue("<span style = 'color: {cols_name}'>{x}</span>")
}

cols_line <- c("#A67B5B", "#ADB6B6ff", "#ED0000FF")
cols_name <- c("#A67B5B", "#ADB6B6ff", "#ED0000FF")


fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                    data = data)

gg <- ggsurvplot(fit_data, data = data, 
                 axes.offset = F,
                 pval = F, conf.int = 0.95, 
                 #linetype = "strata", # Change line type by groups
                 #surv.median.line = "hv", # Specify median survival
                 #ggtheme = theme_bw(), # Change ggplot2 theme
                 risk.table = FALSE, 
                 title = "",
                 xlim = c(0,48), break.time.by = 6, 
                 ylim = c(0,1), break.y.by = 0.2,
                 palette= cols_line, 
                 conf.int.alpha = 0.2,
                 xlab="\nTime (h)",
                 #ylab="Survival probability\n",
                 ylab = "",
                 font.x = c(15),
                 font.y = c(15), font.tickslab = c(12),
                 legend = "top",
                 legend.labs = c("PBS","Aci 110","A110-1"),
                 legend.title = "",
                 font.legend = list(size = 10))
#guides(colour = guide_legend(nrow = 2))

# without coloring the legend text

#gg <- gg$plot +
#theme(legend.text = element_markdown())

##### colouring the legend text

gg <- gg$plot +
  scale_color_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  scale_fill_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  theme(legend.text = element_markdown())


gg_decon <- ggplot_build(gg)
ng <- length(unique(gg_decon$data[[1]]$colour))

# modify the lines 
gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour))-2)), y))
# modify the censor point
gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour)))-2), y))
# from https://ggplot2-book.org/internals.html
gtable <- ggplot_gtable(gg_decon)

png(filename = "A110_1_top_b.png",  units = "mm", width = 90, height = 90, bg = "transparent", res=800)
grid::grid.newpage()
grid::grid.draw(gtable)
dev.off()

############
### Fig 7/C #####
### larvae experiment ####


data <- read_delim("Exp_20230911_19.csv", ",", 
                   escape_double = FALSE, trim_ws = TRUE)

data %>% distinct(Strain)

# without cocktail control (phage control from larvae and mice goes to supplementary)

data <- data %>% filter(Strain=="PBS" | Strain=="Aci 110" | Strain=="Aci 110 + HS 10" |  Strain=="Aci 110 + H10 + S1 + FP 0.1" )



data <- data %>% mutate(Strain=factor(Strain, ordered = TRUE,
                                      levels=c("PBS", "Aci 110", "Aci 110 + HS 10","Aci 110 + H10 + S1 + FP 0.1")))


cols_line <- c("#A67B5B", "#ADB6B6ff","#E762D7FF", "#FFB547FF" )
cols_name <- c("#A67B5B", "#ADB6B6ff","#E762D7FF", "#FFB547FF" )

labels <- function(x, cols_name) {
  glue::glue("<span style = 'color: {cols_name}'>{x}</span>")
}

fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                    data = data)


gg <- ggsurvplot(fit_data, data = data,
                 axes.offset = F,
                 pval = F, 
                 #conf.int = 0.95,
                 conf.int = F,
                 #linetype = "strata", # Change line type by groups
                 #surv.median.line = "hv", # Specify median survival
                 #ggtheme = theme_bw(), # Change ggplot2 theme
                 risk.table = FALSE, 
                 title = "",
                 xlim = c(0,48), break.time.by = 6, 
                 ylim = c(0,1), break.y.by = 0.2,
                 palette=cols_line, 
                 conf.int.alpha = 0.2,
                 xlab="\nTime (h)",
                 ylab="Survival probability\n",
                 font.x = c(15),
                 font.y = c(15), font.tickslab = c(12),
                 legend = "top",
                 legend.labs = c("PBS", "Aci 110", "Aci 110 + HS", "Aci 110 + HSFPh"), 
                 legend.title = "",
                 font.legend = list(size = 10))+
  guides(colour = guide_legend(nrow = 2))


##### colouring the legend text

gg <- gg$plot +
  scale_color_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  scale_fill_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  theme(legend.text = element_markdown())


gg_decon <- ggplot_build(gg)
ng <- length(unique(gg_decon$data[[1]]$colour))

# modify the lines 
gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour))-2)), y))
# modify the censor point
gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour)))-2), y))
# from https://ggplot2-book.org/internals.html
gtable <- ggplot_gtable(gg_decon)


png(filename = "Aci110_cocktail_HS_treatment_b.png",  units = "mm", width = 90, height = 90, bg = "transparent", res=800)
grid::grid.newpage()
grid::grid.draw(gtable)
dev.off()


############################
##### Fig 7/D
#### Mouse exp 20240402  #####
############################


data <- read_delim("mouse_exp_20240402.csv", ",", 
                   escape_double = FALSE, trim_ws = TRUE)

data %>% distinct(Strain)

#### plotting only the 1 h treatments #####

data <- data %>% filter(Treatment == "10_9") %>%
  filter (Strain == "Aci 15" | Strain == "Aci 15 + H_60min" | Strain == "Aci 15 + cocktail_60min")  %>%
  mutate(Strain=factor(Strain, ordered = TRUE, levels=c("Aci 15","Aci 15 + H_60min","Aci 15 + cocktail_60min")))


fit_data <- survfit(Surv(time = Time, event = Event) ~ Strain,
                    data = data)

#cols <- c("#D2AF81FF", "#ADB6B6ff", "#FFB547FF", "#FD7446FF")

#cols_line <- c("#D2B48C", "#ADB6B6ff","#ED0000FF", "#FFB547FF" )
#cols_name <- c("#D2B48C", "#ADB6B6ff","#ED0000FF", "#FFB547FF" )

cols_line <- c("#ADB6B6ff","#ED0000FF", "#FFB547FF" )
cols_name <- c("#ADB6B6ff","#ED0000FF", "#FFB547FF" )

labels <- function(x, cols_name) {
  glue::glue("<span style = 'color: {cols_name}'>{x}</span>")
}


gg <- ggsurvplot(fit_data, data = data,
                 #axes.offset = F,
                 pval = F, conf.int = F, 
                 #linetype = "strata", # Change line type by groups
                 #surv.median.line = "hv", # Specify median survival
                 #ggtheme = theme_bw(), # Change ggplot2 theme
                 risk.table = FALSE, 
                 title = "",
                 xlim = c(0,7), break.time.by = 1, 
                 ylim = c(0,1), break.y.by = 0.2,
                 palette=cols_line,
                 #conf.int.alpha = 0.1,
                 xlab="\nTime (day)",
                 #ylab="Survival probability\n",
                 ylab = NULL,
                 font.x = c(15),
                 font.y = c(15), font.tickslab = c(12),
                 legend = "top",
                 #legend.labs = c("HSFPh", "Aci 15","Aci 15 + H","Aci 15 + HSFPh"),
                 legend.labs = c("Aci 15","Aci 15 + H","Aci 15 + HSFPh"),
                 legend.title = "",
                 font.legend = list(size = 10))+
  guides(colour = guide_legend(nrow = 2))

# p = 0.00027

##### colouring the legend text

gg <- gg$plot +
  scale_color_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  scale_fill_manual(values = cols_line, labels = ~labels(.x, cols_name)) +
  theme(legend.text = element_markdown())


gg_decon <- ggplot_build(gg)
ng <- length(unique(gg_decon$data[[1]]$colour))

# modify the lines 
gg_decon$data[[1]] <-  gg_decon$data[[1]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour))-2)), y))
# modify the censor point
gg_decon$data[[3]] <-  gg_decon$data[[3]] %>%
  mutate(y=ifelse(y==1, 1 - (.01*(as.numeric(as.factor(colour)))-2), y))
# from https://ggplot2-book.org/internals.html
gtable <- ggplot_gtable(gg_decon)


png(filename = "Fig_7D_Mouse_exp_20240402_noaxisoffset.png",  units = "mm", width = 90, height = 90, bg = "transparent", res=800)
grid::grid.newpage()
grid::grid.draw(gtable)
dev.off()
