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

## Exercise 1 - Color theory in practice
# 1. make my own sequential color palette
n <- 6
color_p1 <- sequential_hcl(n, "Purple-Blue")
show_col(color_p1)

#2. make a diverging color palette
color_p2 <- diverging_hcl(n, "Green-Orange")
show_col(color_p2)

#3. make a qualitative color palette
color_p3 <- qualitative_hcl(n, "Dark 3")
show_col(color_p3)

#4. make tweaks with plot from previous homework
data <- read_excel('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/Week 1/adv_hw1_data/wiid_2022_data/WIID_30JUN2022_0.xlsx') # import wiid data
glimpse(data)
data %>%
  filter(oecd == "OECD") %>%
  mutate(europe = ifelse(region_un == "Europe", "Europe", "Non-Europe")) %>%
  group_by(region_un, europe) %>%
  summarize(mean(gini))
data$logGDP <- log(data$gdp) # calculate the log of GDP
plot1 <- data %>%
  ggplot(aes(x = gini, y = logGDP, color = region_un)) + # make color as an aesthetic
  geom_point(size = 0.5) + # make point size smaller
  labs(x = "Gini score",
       y = "GDP per capita (natural log scale)",
       color = "UN Region",
       title = "Relationship between gini score and gdp") + # label aesthetics, axes, title
  scale_color_manual(values = color_p1) + # change the color of the categories according to color palette defined above
  guides(color = guide_legend(override.aes = list(size = 5)))+ # make legend symbol larger
  theme_bw() #white background and grey gridlines

colorBlindness::cvdPlot(plot1)


## Exercise 2 - More Plots, with a touch of customization
# 1. create a density ridgline plot
chicago <- read.csv('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/Week 1/adv_hw1_data/chi_town.csv') # import Chicago data
chicago$DATE <- ymd(chicago$DATE) # convert date type from character to date
glimpse(chicago)

# I replaced the code from the homework with more streamlined code that is done with piping
chicago %>%
  mutate(year = year(DATE)) %>% # extract year
  mutate(month = month(DATE)) %>% # extract month
  mutate(TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)) %>% # take the average of TMIN and TMAX for when TAVG does not have a value
  mutate(decade = case_when(year < 1970 ~ "1960s",
                             year > 1969 & year < 1980 ~ "1970s",
                             year > 1979 & year < 1990 ~ "1980s",
                             year > 1989 & year < 2000 ~ "1990s",
                            year > 1999 & year < 2010 ~ "2000s",
                            year > 2009 & year < 2020 ~ "2010s",
                             TRUE ~ "other" )) %>% # dividing the yearly data into decades
  select(decade, TAVG, year, month, everything()) %>%
  filter(year >= 1960 & year < 2020) %>% # only look from 1960 to 2019
  filter(month == 6) %>% # choose june (month 6 of calendar year)
  ggplot(aes(x = TAVG, y = decade, fill = decade)) + # make the plot using ave temps and decades creates
  geom_density_ridges()+
  labs(x = "Average Temperature (F)",
       y = "Decades",
       title = "**Chicago Average Temperature in June Across Time** <br>
         <span style = 'font-size:8pt;'>The years spanned in this graph is grouped by decade, from <span style = 'color:#ffba08;'> **1960s**, <span style = 'color:#f48c06;'> **1970s**, <span style = 'color:#d00000;'> **1980s**, <span style = 'color:#9d0208;'> **1990s**, <span style = 'color:#6a040f;'> **2000s**, <span style = 'color:#000000;'>to <span style = 'color:#370617;'> **2010s**.<span style = 'color:#000000;'> As you can see from the density ridgeline plot below, the average temperature seems to increase over time throughout the decades. </span>"
  )+
  scale_fill_manual(values = c("#ffba08","#f48c06","#d00000","#9d0208","#6a040f","#370617"))+ # I chose these colors to imply the "heating up" of temperatures over time
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal() +
  theme(legend.position = "none",
    plot.title = element_textbox_simple(family = "Verdana", size = 10, lineheight = 1, padding = margin(0,0,5,0)),
    axis.title.x  = element_text(family = "Verdana"),
    axis.title.y  = element_text(family = "Verdana"),
    axis.text.x   = element_text(family = "Verdana"),
    axis.text.y   = element_text(family = "Verdana"))

# 2. make a raincloud plot
chicago %>%
  mutate(year = year(DATE)) %>% # extract year
  mutate(month = month(DATE)) %>% # extract month
  mutate(TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)) %>% # take the average of TMIN and TMAX for when TAVG does not have a value
  mutate(decade = case_when(year < 1970 ~ "1960s",
                            year > 1969 & year < 1980 ~ "1970s",
                            year > 1979 & year < 1990 ~ "1980s",
                            year > 1989 & year < 2000 ~ "1990s",
                            year > 1999 & year < 2010 ~ "2000s",
                            year > 2009 & year < 2020 ~ "2010s",
                            TRUE ~ "other" )) %>% # dividing the yearly data into decades
  select(decade, TAVG, year, month, everything()) %>%
  filter(year >= 1960 & year < 2020) %>% # only look from 1960 to 2019
  filter(month == 6) %>% # choose june (month 6 of calendar year)
  ggplot(aes(x = TAVG, y = decade, fill = decade)) + # make the plot using ave temps and decades creates
  geom_violin()+
  labs(x = "Average Temperature (F)",
       y = "Decades",
       title = "**Chicago Average Temperature in June Across Time** <br>
         <span style = 'font-size:8pt;'>The years spanned in this graph is grouped by decade, from <span style = 'color:#6B0077;'> **1960s**, <span style = 'color:#73579B;'> **1970s**, <span style = 'color:#828BBC;'> **1980s**, <span style = 'color:#9CB8D6;'> **1990s**, <span style = 'color:#C1DDE8;'> **2000s**, <span style = 'color:#000000;'>to <span style = 'color:#F1F1F1;'> **2010s**.<span style = 'color:#000000;'> As you can see from the density ridgeline plot below, the average temperature seems to increase over time throughout the decades. </span>"
  )+
  scale_fill_manual(values = color_p1)+ # I replaced the previous red sequential with a different color palette generated before, as instructued
  coord_cartesian(clip = "off") + # To avoid cut off
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_textbox_simple(family = "Verdana", size = 10, lineheight = 1, padding = margin(0,0,5,0)),
        axis.title.x  = element_text(family = "Verdana"),
        axis.title.y  = element_text(family = "Verdana"),
        axis.text.x   = element_text(family = "Verdana"),
        axis.text.y   = element_text(family = "Verdana"))

# 3. make two side-by-side waffle plots
# want to show percentage of days above and below the average month-day temperature from two periods

plot1 <- chicago %>%
  mutate(year = year(DATE)) %>% # extract year
  mutate(month = month(DATE)) %>% # extract month
  mutate(day = day(DATE)) %>% # extract day
  mutate(TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)) %>% # take the average of TMIN and TMAX for when TAVG does not have a value
  group_by(day) %>%
  filter(day == 1 & month == 6) %>% # choosing june 1st to compare
  filter(year >= 1960 & year <= 1964) %>% # only look from 1960 to 2019
  mutate(ave = mean(TAVG)) %>% # find the average across all years from that specific date
  mutate(dev = TAVG - ave) %>% # find the deviation of each year from the average
  mutate(count = ifelse(dev > 0, 1,-1)) %>% # if temp is above the average (aka the difference is greater than 0), then assign a 1
  select(count, dev, ave, day, month, year, everything()) %>%
  ggplot(aes(fill = count, values = n)) +
  scale_fill_manual(values = c("#a40000","#0b03fc"), labels = c("Above", "Below"))+
  geom_waffle(color = "white", size = 3)+ # make the inside borders white and thicker
  ##labs(title = "**Chicago Average Temperature in June <br> from 1960 to 1964 (left) and from 2016 to 2020 (right)** <br> <span style = 'font-size:10pt;'>The waffle plot depicts the percentage of days in which the temperature is  <span style = 'color:#a40000;'>**above** <span style = 'color:#000000;'>or <span style = 'color:#0b03fc;'>**below**  <br><span style = 'color:#000000;'>the average temperature for June 1st across the entire time period. </span>")+
  coord_equal()+ # I didn't like the way the default plot appeared, so making some superficial changes here and below
  theme(panel.background = element_blank(), # remove background panel
        axis.text.x=element_blank(), # remove axis text
        axis.text.y=element_blank(),
        axis.ticks=element_blank(), # remove tick marks
        plot.title = element_markdown(lineheight = 1.5),
        legend.title = element_blank()) # enable markdown for the title
plot1
  
plot2 <- chicago %>%
  mutate(year = year(DATE)) %>% # extract year
  mutate(month = month(DATE)) %>% # extract month
  mutate(day = day(DATE)) %>% # extract day
  mutate(TAVG = ifelse(is.na(TAVG), (TMAX + TMIN) / 2, TAVG)) %>% # take the average of TMIN and TMAX for when TAVG does not have a value
  group_by(day) %>%
  filter(day == 1 & month == 6) %>%
  filter(year >= 2016 & year <= 2020) %>% # only look from 1960 to 2019
  mutate(ave = mean(TAVG)) %>% # find the average across all years from that specific date
  mutate(dev = TAVG - ave) %>% # find the deviation of each year from the average
  mutate(count = ifelse(dev > 0, 1,-1)) %>% # if temp is above the average (aka the difference is greater than 0), then assign a 1
  select(count, dev, ave, day, month, year, everything()) %>%
  ggplot(aes(fill = count, values = n)) +
  scale_fill_manual(values = c("#a40000","#0b03fc"), labels = c("Above", "Below"))+
  geom_waffle(color = "white", size = 3,show.legend = FALSE)+ # make the inside borders white and thicker
  ## labs(title = "**Chicago Average Temperature in June from 2016 to 2020** <br> <span style = 'font-size:8pt;'>The waffle plot depicts the percentage of days in which the temperature is  <span style = 'color:#a40000;'>**above** <span style = 'color:#000000;'>or <span style = 'color:#0b03fc;'>**below** <span style = 'color:#000000;'>the average temperature for June 1st across the entire time period. </span>" )+
  coord_equal()+ # I didn't like the way the default plot appeared, so making some superficial changes here and below
  theme(panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown())
plot2

grid.arrange(plot1, plot2, ncol=2, # put the two plots side by side
             top = c("Chicago Average Temperature in June from 1960 to 1964 (left) and from 2016 to 2020 (right)"),
                     bottom = c("The waffle plot depicts the percentage of days in which the temperature is above below the average temperature for June 1st across the entire time period."))
