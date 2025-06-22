## Advanced Data Visualization (QSS 19) Spring 2024
## Project 2
## Recreation of plot in the wild
## Name: Emma Ricci-De Lucca
## Date: May, 2024

# prep the libraries
library(tidyr)        # tidying up messy data sets
library(dplyr)        # piping, filtering, mutation, selecting, summarizing, etc
library(tidyverse)    # includes dplyr, ggplot2, and others
library(rvest)        # web scraping
library(ggplot2)      # plotting
library(readxl)       # reading excel documents
library(devtools)     # to install packages
library(ggtext)       # extending plotting with additional text features (like markdown)
library(stringr)      # functions for strings
library(colorspace)   # to make color palettes
library(grid)         # for arranging plots
library(ggridges)     # for geom ridges plot
library(waffle)       # for geom waffle plot

# data scrubbing from CDC
cdc_url <- read_html("https://www.cdc.gov/reproductivehealth/maternal-mortality/erase-mm/data-mmrc.html")

cdc_data <- cdc_url %>%
  html_nodes(css = "table") %>%
  html_table()

cdc_data


# race and ethnicity
purple_pal <- rev(c("#E0C3FC","#DAB6FC","#CBB2FE","#BBADFF","#9FA0FF","#8187DC","#757BC8"))
  
cdc_data[[1]]
data_subset <- cdc_data[[1]][-c(1, 9:22), ] # remove the irrelevant rows
data_subset

# NEED TO ADJUST FOR POPULATION SIZES ACCORDING TO RACE/ETHNIC REMOGRAPHIC USING US CENSUS DATA (2023)
population <- c(1018) # sample size for collected data from CDC
census <- c(.191, 0.013, 0.063, 0.136, 0.003, 0.589, 0.03)
race_pop <- population * census
race_pop

# plot 1 horizontal bar chart
bar_plot <- data_subset %>%
  `colnames<-`(c("Race", "N", "Percent")) %>%
  mutate(Races = c("Hispanic", "American Indian or Alaska Native", "Asian", "Black", "Native Hawaiian and Other Pacific Islander", "White", "Other/multiple races")) %>%
  mutate(Races = str_wrap(Races, width = 23)) %>%
  mutate(N = as.numeric(N)) %>%
  mutate(N_real = N / race_pop * 100) %>%
  mutate(Perc_real = N_real / 785.4 * 100) %>%
  ggplot(aes(x = reorder(Races, -N_real), y = N_real, fill = reorder(Races, -N_real))) + # reorder the bars in ascending order
  geom_col(width = 0.85) +
  coord_flip() +
  theme(legend.position = "none") +
  scale_fill_manual(values = purple_pal) + # custom color palette
  labs(x = "Race or Ethnicity",
       y = "Number of Maternal Deaths",
       title = "**Distribution of US pregnancy-related deaths by race and ethnicity (2017-2019)**", 
       caption = "Trost et al. 2022, Centers for Disease Control and Prevention") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_textbox_simple(family = "Verdana", size = 10, lineheight = 1.25, margin = margin(b = 15, t = 10)),
        axis.title  = element_blank(),
        axis.text.y = element_text(family = "Verdana", color = "black"),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(), # remove all major and minor lines
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(20, 20, 20, 20))

bar_plot

bar_plot +
  annotate("text", x = 1, y = 238.4, label = "29%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 2, y = 208.4, label = "25%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 3, y = 100.4, label = "11.3%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 4, y = 90.4, label = "9.92%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 5, y = 86.4, label = "9.43%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 6, y = 80.4, label = "8.66%", color = "black", family = "Verdana", size = 3.5) +
  annotate("text", x = 7, y = 65.4, label = "6.75%", color = "black", family = "Verdana", size = 3.5) 
  

# timeline mortality
cdc_data[[3]]
## BAR CHART
timeline_plot <- cdc_data[[3]] %>%
  `colnames<-`(c("Stage", "N", "Percent")) %>% # rename the columns
  mutate(Days = c("0", "280", "286", "328","693")) %>% # add in continuous variable for the stages
  mutate(Stage = factor(Stage, levels = rev(unique(Stage)))) %>% # make stages a factor
  ggplot(aes(y = N, x = 0)) +
  geom_col(aes(fill = Stage), width = 1) + # plot bar chart
  coord_flip() + # make it horizontal
  scale_fill_manual(values = c("#3F74CA","#5f8bd3","#7fa3dc","#b6e5c8","#83A691")) + # custom color palette
  labs(title = "**Distribution of US pregnancy-related deaths by timing of death (2017-2019)** 
       <br><span style = 'font-size:8pt;'>Among pregnancy-related deaths, <span style = 'color:#3F74CA;'>**65.3%**<span style = 'color:#000000;'> occur during the <span style = 'color:#3F74CA;'>**postpartum period.** </span>",
       caption = "Trost et al. 2022, Centers for Disease Control and Prevention") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_textbox_simple(family = "Verdana", size = 10, lineheight = 1.25, margin = margin(b = 30, t = 10)),
        axis.title.x  = element_blank(),
        axis.title.y  = element_blank(),
        panel.grid.major = element_blank(), # remove all major and minor lines
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        axis.text = element_blank())

timeline_plot

Stages <- c("During pregnancy", "Day of delivery", "1-6", "7-42", "43-365")
Stages <- str_wrap(Stages, width = 11)

timeline_plot +
  annotate("text", x = -.8, y = 100, label = Stages[1], color = "#83A691", family = "Verdana", vjust = 0, hjust = 0.5, fontface = 2) +
  annotate("text", x = -.8, y = 280, label = Stages[2], color = "#b6e5c8", family = "Verdana", vjust = 0, hjust = 0.5, fontface = 2) +
  annotate("text", x = -.68, y = 420, label = Stages[3], color = "#7fa3dc", family = "Verdana", vjust = 0, hjust = 0.5, fontface = 2) +
  annotate("text", x = -.68, y = 580, label = Stages[4], color = "#5f8bd3", family = "Verdana", vjust = 0, hjust = 0.5, fontface = 2) +
  annotate("text", x = -.68, y = 850, label = Stages[5], color = "#3F74CA", family = "Verdana", vjust = 0, hjust = 0.5, fontface = 2) +
  annotate("rect", xmin = -0.9, xmax = -0.75, ymin = 350, ymax = 1002, alpha = 1, fill = "#3F74CA") +
  annotate("text", x = -0.85, y = 570, label = "days postpartum", color = "white", family = "Verdana", vjust = 0, hjust = 0, fontface = 2)+
  annotate("text", x = 0, y = 100, label = "21.6%", color = "white", family = "Verdana", vjust = 0, hjust = 0.5) +
  annotate("text", x = 0, y = 280, label = "13.2%", color = "white", family = "Verdana", vjust = 0, hjust = 0.5) +
  annotate("text", x = 0, y = 418, label = "12%", color = "white", family = "Verdana", vjust = 0, hjust = 0.5) +
  annotate("text", x = 0, y = 580, label = "23.3%", color = "white", family = "Verdana", vjust = 0, hjust = 0.5) +
  annotate("text", x = 0, y = 850, label = "30%", color = "white", family = "Verdana", vjust = 0, hjust = 0.5)



# causes of mortality
cdc_data[[4]]
waffle_data <- cdc_data[[4]][-c(1:2), c(-4:-15)]
waffle_data

tidy_waffle <- waffle_data %>%
  `colnames<-`(c("Condition", "N", "Percent")) %>%
  mutate(Condition = str_replace_all(Condition, "[^[:alpha:]\\s]", "")) # get rid of the numbers at the end which were the references
tidy_waffle

waffle_df <- data.frame(label = rep(new_waffle$Condition, new_waffle$N))
waffle_df

cpalette <- c("#6A040F","#9D0208","#D00000","#E85D04","#F48C06","#FFBA08","#FFDD00","#EEEF20", "#D4D700", "#AACC00", "#80B918", "#2B9348", "#19582b")
Conditions <- c("Mental health conditions", "Hemorrhage", "Cardiac and coronary conditions", "Infection", "Embolismthrombotic", "Cardiomyopathy", "Hypertensive disorders of pregnancy", "Amniotic fluid embolism","Injury", "Cerebrovascular accident", "Cancer", "Metabolicendocrine conditions", "Pulmonary conditions")   

waffle_df %>%
  mutate(labelz = factor(label, ordered = TRUE, levels = Conditions)) %>%
  ggplot(aes(fill = labelz, values = n)) +
  scale_fill_manual(values = cpalette) +
  geom_waffle(n_cols = 25, n_rows = 38, color = "white", size = 1) +
  coord_equal() +
  labs(title ="**Distribution of US pregnancy-related deaths by condition (2017-2019)** 
       <br><span style = 'font-size:8pt;'><span style = 'color:#370617;'>**Mental health conditions**<span style = 'color:#000000;'> are the leading cause of maternal mortality amongst women in the US.</span>",
       caption = "Trost et al. 2022, Centers for Disease Control and Prevention") +
  theme(legend.position = "bottom",
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_markdown(lineheight = 1.5),
        legend.title = element_blank())  



