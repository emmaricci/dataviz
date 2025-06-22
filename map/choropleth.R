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


# Exercise - Mapping
map_USA <- ne_countries(scale = "large", # medium resolution
                   returnclass = "sf",
                   country = "united states of america")  # just USA

# grab lakes, rivers, streams
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
lakes <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf")
#streams <- ne_download(scale = "large", type = "streams", category = "physical", returnclass = "sf") # this isn't working
road <- ne_download(scale = "large", type = "roads", category = "cultural", returnclass = "sf")

# states in the Northeast of the USA
ne_states <- c("Maine", "New Hampshire", "Vermont", "Massachusetts", 
               "Rhode Island", "Connecticut", "New York", "New Jersey", 
               "Pennsylvania", "Delaware", "Maryland")

ne_us <- map_USA[map_USA$name %in% ne_states, ] # grab them from the natural earth map

# map of northeast of USA
map_USA %>%
  ggplot() +
  geom_sf(data = ne_us, fill = "lightblue", color = "gray50", size = 0.2) +
  geom_sf(data = rivers, color = 'blue', linewidth = 0.2) +  # rivers as lines
  geom_sf(data = lakes, fill = 'lightblue') +  # lakes as color fills
  geom_sf(data = road, color = "gray", size = 0.3) + # add roads
  coord_sf(xlim = c(-90,-65), ylim = c(33,48)) + # center around NH and VT
  theme_minimal() +
  ggtitle("Northeast United States")


