## Advanced Data Visualization (QSS 19) Spring 2024
## R Homework 2
## Name: Emma Ricci-De Lucca
## Date: April 2 - April 15, 2024

library(tidyverse) # everything!
library(colorspace) # conversion of RGB to hex code, among other things. 
install.packages("colorBlindness")
library(colorBlindness) # cool color blind tester. 
install.packages("gridExtra")
library(gridExtra) # This is for multiple plots on one visualization. 
library(scales)
library(readxl)
library(lubridate)
library(ggplot2)
install.packages("ggridges")
library(ggridges)
install.packages("ggtext")
library(ggtext)
install.packages("gghalves")
library(gghalves)
install.packages("waffle")
library(waffle)
install.packages('plotrix')
library('plotrix')
library(cowplot)
install.packages("ggrepel")
library(ggrepel)

## Exercise - Recreation problem for W.E.B DuBois' famous plot from the 1900 Paris Exhibition
web <- read.csv('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/week 2/dubois_data.csv') # import data
glimpse(web)


dubois <- c("#dc273f","#ffcd36","#efe3d4","#6f81ae","#be9f87","#e1d2c1") # chosen from color picker; adjusting alpha makes the color transparent
names <- c("AGRICULTURE, FISHERIES, AND MINING.","DOMESTIC AND PERSONAL SERVICE.","MANUFACTURING AND MECHANICAL INDUSTRIES.","PROFESSIONS.","TRADE AND TRANSPORTATION")

# web$Order <- factor(web$Occupation, levels = c("empty","Agriculture, Fisheries and Mining","Domestic and Personal Service","Manufacturing and Mechanical Industries","Professions","Trade and Transportation","NA"))

webd_plot <- web %>%
  mutate(Group = ifelse(Group == "Negroes", "African Americans", "Whites")) %>% # replace some language
  add_row(Percentage = 63.0, Group = "African Americans", Occupation = "empty") %>% # add an empty pie slice for the left-hand empty space
  add_row(Percentage = 63.0, Group = "Whites", Occupation = "empty") %>% # add another empty pie slice for the right-hand empty space
  arrange(Group,Occupation) %>%
  mutate(lbls = ifelse(Percentage != 63, paste0(Percentage, "%"), "")) %>%
  ggplot(aes(y = Percentage, fill = Occupation, x = 0, group = Group)) + # plot the slices for each occupation and their corresponding percentages
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y" , start = -4.15) + # adjusting the start of the slices to rotate them to the right place
  theme_void() +
  scale_fill_manual(values = dubois) + # bring in the correct colors
  labs(title = "OCCUPATIONS OF AFRICAN AMERICANS AND WHITES IN GEORGIA.") +
  geom_text(aes(label = paste0(lbls), x = 0.35),
            position = position_stack(vjust = 0.5),
            family = "Andale Mono",
            size = 2.5) + # add labels and adjust positioning
  theme(legend.position = "none",
        plot.title = element_text(vjust = -1, family = "Andale Mono"))

webd_plot

x_legend <- c(5,5,105,105,105) # this is where I want the points to go that will correspond to the legend circle color (on the x axis)
x_text1 <- c(9,9) # this is where I want the text labeling the points to go (on the x axis) on the left side
x_text2 <- c(101,101,101) # text for the right side
y_legend <- c(60,40,65,50,35) # coordinate for points on the y axis
y_text1 <- c(61.5,42.5) # text for left-hand side
y_text2 <- c(62.5,50,33) # text for right-hand side
text_legend <- c("AGRICULTURE, FISHERIES,","MANUFACTURING AND","PERSONAL SERVICE.","PROFESSIONS.","TRANSPORTATION.") # labels for each color
data_legend <- data.frame(x_legend, y_legend) # create a data frame for the circles

legend_plot <- data_legend %>%
  ggplot(aes(x = x_legend, y = y_legend)) + # plot the circles
  geom_point(size = 10, aes(color = text_legend)) + # adjust the size and make the outline brown, make color an aesthetic to fill later
  scale_y_continuous(limits = c(-5,100)) + # set the axes for the legend plot
  scale_x_continuous(limits = c(0,110)) +
  annotate("text", x = x_text1, y = y_text1, label = text_legend[1:2], size = 3, hjust = 0, family = "Andale Mono") + # add text next to each circle, corresponding to the legend
  annotate("text", x = x_text2, y = y_text2, label = text_legend[3:5], size = 3, hjust = 1, family = "Andale Mono") +
  annotate("text", x = 18, y = 58.5, label = "AND MINING.", size = 3, family = "Andale Mono") +
  annotate("text", x = 11, y = 39, label = "MECHANICAL INDUSTRIES.", size = 3, hjust = 0, family = "Andale Mono") +
  annotate("text", x = 97.5, y = 66, label = "DOMESTIC AND", size = 3, hjust = 1, family = "Andale Mono") +
  annotate("text", x = 96.5, y = 36.5, label = "TRADE AND", size = 3, hjust = 1, family = "Andale Mono") +
  annotate("text", x = 55, y = 94, label = "AFRICAN AMERICANS.", size = 5, family = "Andale Mono") +
  annotate("text", x = 55, y = -3, label = "WHITES.", size = 5, family = "Andale Mono") +
  scale_color_manual(values = c("#dc273f","#6f81ae","#ffcd36","#be9f87","#e1d2c1")) + # custom colors from color picker
  theme_void() +
  theme(legend.position = "None")

legend_plot

# customization ggdraw, cowplots to overlay the two plots
ggdraw() +
  draw_plot(webd_plot, x = 0, y = 0, scale = 1) +
  draw_plot(legend_plot) +
  theme(plot.background = element_rect(fill = '#efe3d4'))


