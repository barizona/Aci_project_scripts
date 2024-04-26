library(tidyverse)
#xxxxxxxxxxxxxxxxxxxx
# Colouring ----------------------------------------------------------
#xxxxxxxxxxxxxxxxxxxx

# read table with colouring information
Colouring_tab <- read_tsv("../Colouring_tab.tsv")

# convert the table to a list
Colour_list <- list()

Loop_var <- colnames(Colouring_tab) %>% 
  grep("Fig.*_name", ., value = TRUE) %>% 
  # remove _name
  gsub("_name", "", .)

for(i in Loop_var){
  x <- Colouring_tab %>% 
    select(Colours_hex, !!sym(paste0(i, "_number")), 
           !!sym(paste0(i, "_name"))) %>% 
    arrange(!!sym(paste0(i, "_number"))) %>% 
    filter(!is.na(!!sym(paste0(i, "_number")))) %>% 
    select(-!!sym(paste0(i, "_number")))
  
  # convert the table to a list element
  Colour_list[[i]] <- x$Colours_hex
  names(Colour_list[[i]]) <- pull(x[, paste0(i, "_name")])
}

rm(i, Loop_var, x, Colouring_tab)

# Figure 7A change alpha
Colour_list$Fig7A_alpha <- adjustcolor(Colour_list$Fig7A, alpha.f = 0.5)
names(Colour_list$Fig7A_alpha) <- names(Colour_list$Fig7A)
# Figure 7B change alpha
Colour_list$Fig7B_alpha <- adjustcolor(Colour_list$Fig7B, alpha.f = 0.5)
names(Colour_list$Fig7B_alpha) <- names(Colour_list$Fig7B)
# Figure 7C change alpha
Colour_list$Fig7C_alpha <- adjustcolor(Colour_list$Fig7C, alpha.f = 0.5)
names(Colour_list$Fig7C_alpha) <- names(Colour_list$Fig7C)
# # Figure 7ABC legend change alpha
Colour_list$Fig7ABClegend_alpha <- adjustcolor(Colour_list$Fig7ABClegend, alpha.f = 0.5)
names(Colour_list$Fig7ABClegend_alpha) <- names(Colour_list$Fig7ABClegend)
# Figure 7E change alpha
Colour_list$Fig7E_alpha <- adjustcolor(Colour_list$Fig7E, alpha.f = 0.5)
names(Colour_list$Fig7E_alpha) <- names(Colour_list$Fig7E)
