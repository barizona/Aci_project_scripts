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

# TODO: szetszedni 1st, 2nd 3rd
# Figure 7A change alpha
Colour_list$Fig7B_alpha <- adjustcolor(Colour_list$Fig7B, alpha.f = 0.5)
names(Colour_list$Fig7B_alpha) <- names(Colour_list$Fig7B)

# TODO: chagne the names of aci on A
# Aci 110 H^R
