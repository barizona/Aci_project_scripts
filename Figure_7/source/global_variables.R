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
# 1st
Colour_list$Fig7A1st_alpha <- adjustcolor(Colour_list$Fig7A1st, alpha.f = 0.5)
names(Colour_list$Fig7A1st_alpha) <- names(Colour_list$Fig7A1st)
# 2nd
Colour_list$Fig7A2nd_alpha <- adjustcolor(Colour_list$Fig7A2nd, alpha.f = 0.5)
names(Colour_list$Fig7A2nd_alpha) <- names(Colour_list$Fig7A2nd)
# 3rd
Colour_list$Fig7A3rd_alpha <- adjustcolor(Colour_list$Fig7A3rd, alpha.f = 0.5)
names(Colour_list$Fig7A3rd_alpha) <- names(Colour_list$Fig7A3rd)
# legend
Colour_list$Fig7Alegend_alpha <- adjustcolor(Colour_list$Fig7Alegend, alpha.f = 0.5)
names(Colour_list$Fig7Alegend_alpha) <- names(Colour_list$Fig7Alegend)
