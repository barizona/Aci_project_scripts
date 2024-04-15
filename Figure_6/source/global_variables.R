# colour

Colour_list <- list()

# Use a few colours from the Prism palette
Colour_list$prism_colors <- rcartocolor::carto_pal(name = "Prism", n = 12)
# 
c("#5F4690", # 1                                  
  "#1D6996", # 2                                           S^R   
  "#38A6A5", # 3                                3 F
  "#0F8554", # 4    3 Aci110-2     2 HS^R
  "#73AF48", # 5                                1 H
  "#EDAD08", # 6                   3 HSF^R
  "#E17C05", # 7    2 Aci110-1     1 H^R
  "#CC503E", # 8                                2 S
  "#94346E", # 9    
  "#6F4070", # 10                               4 Ph
  "#994E95", # 11   4 Aci110-G1    4 HSFPh^R       
  "#666666") # 12   1 Aci110       5 wt

# Figure 6E x axis
Colour_list$Aci110_x_axis <- Colour_list$prism_colors[c(12, 7, 4, 11)]
names(Colour_list$Aci110_x_axis) <- c("Aci110", "Aci110-1", "Aci110-2", "Aci110-G1")

# Figure 6E content
Colour_list$Aci110_content <- Colour_list$prism_colors[c(5, 8, 3, 10)]
names(Colour_list$Aci110_content) <- c("H", "S", "F", "Ph")

# Figure 6F colour version
Colour_list$phage_resistance <- Colour_list$prism_colors[c(7, 4, 6, 11, 12)]
names(Colour_list$phage_resistance) <- c("H<sup>R</sup>", "HS<sup>R</sup>", 
                                        "HSF<sup>R</sup>", "HSFPh<sup>R</sup>", 
                                        "wt")
# Figure 6F grayscale version
Colour_list$phage_resistance_gray <- c("black", "gray15", "gray30", "gray45", "gray60")
names(Colour_list$phage_resistance_gray) <- names(Colour_list$phage_resistance)


# theme
