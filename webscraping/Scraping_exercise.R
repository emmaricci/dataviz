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


## Exercise 1 - Web Scraping
# Problem 1
# read url and store it in new object
proclams <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/proclamations?items_per_page=60&page="
# copying the URL after clicking on view 60 results per page

page_urls <- paste0(proclams, 0:155) # create a list with all of the pages of proclamations, with 9309 items and 60 displaying on each page, then there are 156 pages
page_urls

length(page_urls)
class(page_urls)

# Jane helped me with the following three lines of code
proc_df <- lapply(1:length(page_urls), function(i) # iterate over a sequence of indices and for each index, perform the following operations
{
  print(paste0 ("Executing ", i, " of ", length(page_urls))) # print the progress of the operation, which may take a while
  
  page_html <- read_html(page_urls[i]) # read the html content from all of the URLs (aka from all the pages)
  
  proclams_memo <- page_html %>% # create new object that stores values extracted from html
    html_elements(".date-display-single") %>% # upon inspection, the class housing the information is as follows
    html_text() %>% # extract the text from the html class
    as_tibble() %>% # put the data scraped into a tibble
    setNames("date") %>% # label the column
    mutate(DATE = mdy(date), year = year(DATE)) %>% # lubridate date to extract the year
  
  return(proclams_memo)
}) %>%
  map_dfr(as.data.frame) # take the results from the above function and put it into a single data frame

view(proc_df)
# count the number of memos each year
memo_count <- proc_df %>%
  group_by(year) %>%
  summarise(count = n())

memo_count

# Problem 2
movies <- read_html("https://editorial.rottentomatoes.com/guide/100-best-classic-movies/1") # read in the url for 100 best classic movies

# get the name of the movie
names <- movies %>%
  html_nodes(css = ".article_movie_title a") %>% # class for just the title and the corresponding tag
  html_text() %>% # extract the text
  as_tibble() %>% # put it in a tibble
  rename(title = value) # rename the column to title
glimpse(names)

# get the year of the movie
years <- movies %>%
  html_nodes(css = ".start-year") %>%
  html_text() %>%
  as_tibble() %>%
  rename(year = value) %>%
  #mutate(year = as.numeric(str_extract(year, "\\d{4}"))) # can remove parentheses with a stringr function by extracting numbers and converting to numeric
glimpse(years)

# get the rank of the movie
rankings <- movies %>%
  html_nodes(css = ".countdown-index") %>%
  html_text() %>%
  as_tibble() %>%
  rename(rank = value) %>%
  # mutate(rank = as.double(str_remove(rank, "#"))) # can remove the pound sign before the ranking number using stringr function
glimpse(rankings)

# get the image of the movie
images <- movies %>%
  html_nodes(css = ".article_poster") %>%
  html_attr("src") %>% # HTML elements extracting source of external resources such as images
  as_tibble() %>%
  rename(image = value)
glimpse(images)

# get the critics' consensus text of the movie
critics <- movies %>%
  html_nodes(css = ".critics-consensus") %>%
  html_text() %>%
  as_tibble() %>%
  rename(critic = value)
glimpse(critics)

# join all the tibbles in one data frame
movies_df <- cbind(names, years, rankings, images, critics) %>%
  as_tibble() %>%
  filter(year > 1939 & year < 1951) %>% # only want top movies in the 1940s
  arrange(rank) %>% # the order of the data should be according to rank, not year
  slice(1:10) # only want the top 10 movies (aka the first 10 rows)

View(movies_df)

create_movie_plot <- function(rank, title, year, image, critic) { # define function to create a plot for a single movie
  img <- image_read(image) #read the image
  
  plot <- ggplot() + # Create ggplot with image
    annotation_custom(rasterGrob(img, interpolate = TRUE), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) + # use custom annotations to create graphical object from image and position in entire plotting area   
    theme_minimal() +
    labs(title = paste(title, "(", year, ")"), subtitle = str_wrap(critic, width = 100)) + # insert title, year with parentheses, and the critics' consensus as a subtitle that wraps
    theme(plot.subtitle = element_text(family = "sans", size = 5, hjust = 0, vjust = 1)) # make the subtitle smaller and left-justified
    
    return(plot)
}

plots <- lapply(1:nrow(movies_df), function(i) { # run through each row of the data frame (aka all top 10 movie entries)
  create_movie_plot(movies_df$rank[i], movies_df$title[i], movies_df$year[i], movies_df$image[i], movies_df$critic[i]) # pull in all the necessary information 
})

grid.arrange(top = "Top 10 Classic Movies from the 1940s",
             bottom = "Source: Rotten Tomatoes",
             grobs = plots, ncol = 2) # arrange the plots in two columns of 5
