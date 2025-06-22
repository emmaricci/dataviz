## Advanced Data Visualization (QSS 19) Spring 2024
## Homework 3
## R Review III: Web Scraping, Maps, Animations, and More
## Name: Emma Ricci-De Lucca
## Date: April 25 - May 9th, 2024

library(tidyverse)
library(rvest)
install.packages("gganimate")
library(gganimate)
library(readxl)
library(sf)
install.packages("maps")
library(maps)
install.packages("ggspatial")
library(ggspatial)
library(lubridate)
library(ggplot2)
library(gridExtra)
install.packages("magick")
library(magick)
library(grid)
library(cowplot)
install.packages("gifski_renderer")
library(gifski_renderer)
library(colorspace)
library(scales)
install.packages("rnaturalearth")
library("rnaturalearth")
install.packages("rnaturalearthdata")
library("rnaturalearthdata")
install.packages("rnaturalearthhires")
library(rnaturalearthhires)
install.packages("devtools")
library(devtools)
devtools::install_github("ropensci/rnaturalearthhires")

## Exercise - Animated Plots
# Problem 1

# animate it as a line plot over time with a leading point
memo_animate <- memo_count %>% # pulling from object made in exercise 1 from web scraping
  ggplot(aes(x = year, y = count, color = year)) + # x should be the year and y should be the total number of documents in that year
  geom_path() +
  geom_point() +
  labs(title ="Number of Presidential Proclamations per Year fom 1789 to 2024",
       x = "Year",
       y = "Total Number of Documents") +
  transition_reveal(year) + # make the point follow the y value
  theme_minimal()

animate(memo_animate, nframes = 100, fps = 8, width = NULL, height = NULL, renderer = gifski_renderer()) # animate the plot with the 100 frames created. adjust the speed and plot size


# Plot
wiid <- read_excel('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/Week 1/adv_hw1_data/wiid_2022_data/WIID_30JUN2022_0.xlsx') # import wiid data
glimpse(wiid)

decile_dat <- wiid %>%
  pivot_longer(names_to = "decile", # use pivot longer to stack the decile variables on top of each other
               values_to = "percent",
               d1:d10) %>%
  select(country, year, decile, percent) %>%
  mutate(deciles = factor(decile, levels = c("d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10"), ordered = TRUE)) %>% # use factors to keep the order of deciles from d0 to d10
  filter(country == "Canada" | country == "United States") %>% # only look at US and Canada
  group_by(country, deciles) %>%
  summarise(mean_decile = mean(percent, na.rm = TRUE)) %>% # calculate average across deciles for all years, exclusing NA values
  ungroup()

decile_dat
  
# decile_colors1 <- sequential_hcl(10, "Blues 2")
# decile_colors2 <- sequential_hcl(10, "Reds 2")
# show_col(decile_colors1)
# show_col(decile_colors2)

decile_rose <- decile_dat %>%
  ggplot(aes(x = deciles, fill = country, y = mean_decile)) + # categorical variable fill with deciles and y value with percentages
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "x") + # rose plot
  theme_minimal() +
  transition_manual(deciles,
                    cumulative = TRUE) +
  labs(x = "Decile",
       y = "Percentages of Income",
       fill = " Country",
       title = "USA vs Canada Average Income Decile") +
  enter_grow()

rose_gif <- animate(decile_rose, nframes = 101, fps = 10, renderer = gifski_renderer())
rose_gif
anim_save(filename="Decileroseplot.gif", rose_gif)

