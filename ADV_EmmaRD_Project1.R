## Advanced Data Visualization (QSS 19) Spring 2024
## Project 1
## Recreation of plot in the wild
## Name: Emma Ricci-De Lucca
## Date: May, 2024

# prep the libraries
library(tidyr)
library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(readxl)
library(devtools)
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggtext)
library(showtext)
library(stringr)


##  SANKEY DIAGRAM
# perinatal depression treatment cascade
treatment_cascade <- read_excel('/Users/emmariccidelucca/Dropbox/Dartmouth College/Spring 2024/qss 19/proj 1 and 2/Perinatal Depression Treatment Cascade Data_CareDecrement.xlsx')
glimpse(treatment_cascade)

# steps of the care continuum
steps <- c("All", "AND", "PPD", "Unrecognized", "Untreated", "InadequateT", "Unremitted")
steps <- factor(steps, levels = stages, ordered = TRUE) # I want them to show up in this order
colorp <- c("#D8CCE8", "#B6E5C8", "#364156", "#A6D4DB", "#364156", "#364156", "#364156") # custom color palette


# x axis labels
names <- c('Prevalence', 'Diagnosis', 'Treatment', 'Adequate trial of treatment', 'Remission')
names<- str_wrap(names, width = 15) # make the long labels wrap so that they don't overlap on the x axis
gaps <- c("All", "AND", "PPD", "Unrecognized", "Untreated", "Inadequate Treatment", "Unremitted")
gaps <- str_wrap(gaps, width = 10)

# use the make long function to manipulate the data for ggsankey
cascade_df <- treatment_cascade %>%
  make_long(PrevTOT, Dx, Tx, AdTx, Rem)
glimpse(cascade_df)


sankey_plot <- cascade_df %>%
  ggplot(aes(x = x,
             next_x = next_x,
             node = node,
             next_node = next_node,
             fill = factor(node),
             label = node)) +
  geom_sankey(flow.alpha = 0.5,               # transparency
              node.color = "white") +
  theme_sankey(base_size = 18) +
  labs(x = "Stage of Care",
       y = "Percent of Women with Depression") +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 11, family = "Verdana", lineheight = 1.2),
        axis.title.x  = element_text(family = "Verdana", size = 9, face = "bold"),
        axis.text.x = element_text(family = "Verdana", size = 9, color = "black"),
        axis.ticks.x = element_line(linewidth = 0.5),
        axis.line.x = element_line(linewidth = 0.5, arrow = arrow(type='closed', length = unit(5,'pt'))),
        axis.title.y  = element_text(family = "Verdana", size = 9, face = "bold"),
        plot.caption = element_markdown(size = 8, family = "Verdana")) +
  scale_fill_manual(values = colorp) +
  labs(title = "**Perinatal Depression Treatment Cascade** <br>
       <span style = 'font-size:8pt;'>The cumulative shortfalls in clinical recognition, initiation of treatment, adequacy of treatment, and treatment response for women with <span style = 'color:#B6E5C8;'>**antenatal (AND)** <span style = 'color:#000000;'>and <span style = 'color:#A6D4DB;'>**postpartum depression (PPD)**
        <br><span style = 'color:#000000;'>Of <span style = 'color:#D8CCE8;'>**all women who suffer from perinatal depression**<span style = 'color:#364156;'>, **50%-70%** of women go **undetected**, **85%** go **untreated**, **91%-93%** are **not adequately treated**, and **95%-97%** continue to suffer from symptoms **without remission**. </span>", 
       caption = "Data Source: Cox et al. (2016)") +
  scale_x_discrete(labels = names) 


sankey_plot


# add annotations for addition labels to emphasize gap in care continuum
sankey_plot +
  annotate("rect", xmin = 5.055, xmax = 6, ymin = -42, ymax = 150, alpha = 0.5, fill = "#364156")+
  annotate("text", x = 2.5, y = 98, label = gaps[4], color = "white", family = "Verdana") +
  annotate("text", x = 3.5, y = 45, label = gaps[5], color = "white", family = "Verdana") +
  annotate("text", x = 4.5, y = 35, label = gaps[6], color = "white", family = "Verdana") +
  annotate("text", x = 5.5, y = 45, label = gaps[7], color = "white", family = "Verdana")





