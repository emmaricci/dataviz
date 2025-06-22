## Advanced Data Visualization (QSS 19) Spring 2024
## R Homework 1
## Name: Emma Ricci-De Lucca
## Date: March 26 - April 2nd, 2024

## Basics working with data
# 1.investigating the base R dataset called diamonds
data("diamonds")
dim(diamonds) # the dimensions are 53940 by 10

# 2. finding class of color variable
class(diamonds$color) # color is an ordered factor variable
ditypeof(diamonds[3]) # color is stored as a list

# 3. importing WIID data
install.packages("readxl")
library(readxl)
data <- read_excel('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/Week 1/adv_hw1_data/wiid_2022_data/WIID_30JUN2022_0.xlsx')
head(data)
tail(data)
glimpse(data)

# 4. getting the minimum and maximum Gini scores for the Americas by UN sub-region
data %>%
  filter(region_un_sub == c("Northern America","Central America","South America")) %>% # only looking at Americas
  group_by(region_un_sub) %>%     # divide by sub-region
  summarize(min(gini), max(gini))  # find max's and min's of gini

# 5. creating a new variable that divides countries into European and non-European countries
data %>%
  filter(oecd == "OECD") %>%
  mutate(europe = ifelse(region_un == "Europe", "Europe", "Non-Europe")) %>%
  group_by(region_un, europe) %>%
  summarize(mean(gini))

## Basics in plotting
chicago <- read.csv('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/Week 1/adv_hw1_data/chi_town.csv')
install.packages('lubridate')
library(lubridate)
# convert date type from character to date
chicago$DATE <- ymd(chicago$DATE)

# 1. creating a smoothed line plot
# define range of dates to plot
date_min <- as_date("1960-01-01"); date_min; class(date_min)
date_max <- date("1980-12-31"); date_max; class(date_max)

# subset data frame to include only the desired range of dates (1960 to 1980)
data_subset <- chicago[chicago$DATE >= date_min & chicago$DATE <= date_max, ]; data_subset

# for the dates without a TAVG, approximate by taking the mean of the TMIN and the TMAX
data_subset$TAVG[is.na(data_subset$TAVG)] <- rowMeans(data_subset[, c("TMIN", "TMAX")], na.rm = TRUE)

# extract the month and day
data_subset$month <- month(data_subset$DATE)
data_subset$day <- day(data_subset$DATE)
glimpse(data_subset)

# take average of TAVG across all days of each year
data_subset <- data_subset %>%
  group_by(month, day) %>%
  mutate(TAVG_new = mean(TAVG))

months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec") # create a vector with names of the month

# plot
data_subset %>%
   ggplot(aes(x = month, y = TAVG_new)) +
   geom_smooth(method = "loess", span = 0.4) +   # Smooth line using with span set to 0.4
   #geom_point(color = "blue4", size = 0.5) +
   scale_y_continuous(limits = c(10,80), breaks = round(seq(min(data_subset$TAVG), max(data_subset$TAVG), by = 10),1)) +
   scale_x_discrete(labels = months, limits = 1:12) + # label the x axis by month
   labs(x = "Month",
        y = "Average Temperature (F)",
        title = "Average Temperatures in Chicago from 1960 to 1980")

# 2. create a ribbon plot that shades the area between the maximum temperatures and minimum temperatures
# by each day of the year, averaged over the years. Use a color/fill of your choice.
data_subset %>%
  ggplot(aes(x = month, y = TAVG_new)) +
  geom_ribbon(aes(ymin = TMIN, ymax = TMAX), fill = "thistle3") +
  #geom_line(color="yellow")+
  geom_smooth(method = "auto", color = "purple4")+
  scale_y_continuous(limits = c(0,100))+
  scale_x_discrete(labels = months, limits = 1:12) +
  labs(x = "Month",
       y = "Average Temperature (F)",
       title = "Average Temperatures in Chicago from 1960 to 1980")
## I'm not sure how to make the ribbon look consistent (aka not as choppy)
## I don't quite know why the geom_ribbon isn't connecting from one t_min or t_max to another across the different months
## I wonder if there is a better way to input the data to make the ribbon look nicer

# 3. Using WIID data, investigate the relationship between Gini scores (gini) and GDP per capita (gdp)
# calculate log of gdp
data$logGDP <- log(data$gdp)

# plot scatter plot
# set a color aesthetic for UN region. Change the base colors to highlight the differences between Africa and the other UN regions
# I made Africa a shade of yellow and the other UN regions shades of green and grey
thayer_palette <- c("#f5c351","#24553e","forestgreen","#a1d25f","grey") #replaced the following two colors because they were too hard to see #e1e1e6, #12312c
  
data %>%
  ggplot(aes(x = gini, y = logGDP, color = region_un)) + # make color as an aesthetic
  geom_point(size = 0.5) + # make point size smaller
  labs(x = "Gini score",
     y = "GDP per capita (natural log scale)",
     color = "UN Region",
     title = "Relationship between gini score and gdp") + # label aesthetics, axes, title
  scale_color_manual(values = thayer_palette) + # change the color of the categories according to color palette defined above
  guides(color = guide_legend(override.aes = list(size = 5)))+ # make legend symbol larger
  theme_bw() #white background and grey gridlines

# 4. Investigate and create my own theme
# make a new color palette from which to select
coolers <- c("gold2","lightskyblue2","#B2E8D6","#D8CCE8","darkslateblue","#daffe7","#6E8E7A", "#A6D4DB")

# using the data from the previous problem to test out a different theme
data %>%
  ggplot(aes(x = gini, y = logGDP, color = region_un)) + # make color as an aesthetic
  geom_point(size = 0.5) + # make point size smaller
  labs(x = "Gini score",
       y = "GDP per capita (natural log scale)",
       color = "UN Region",
       title = "Relationship between gini score and gdp") + # label aesthetics, axes, title
  scale_color_manual(values = coolers) + # change the color of the categories according to color palette defined above
  guides(color = guide_legend(override.aes = list(size = 5)))+ # make legend symbol larger
  theme_bw() + #white background and grey gridlines
  theme(plot.title  = element_text(family = "Verdana",face = "bold"),  # change some fonts and styles
              axis.title.x  = element_text(family = "Verdana"),
              axis.title.y  = element_text(family = "Verdana"),
              axis.text.x   = element_text(family = "Verdana"),
              axis.text.y   = element_text(family = "Verdana"),
              legend.title = element_text(family = "Verdana"),
              legend.text = element_text(family = "Avenir"),
              legend.position = "bottom")
