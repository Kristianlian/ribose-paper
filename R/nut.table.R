#### Nutrition table

# Author: SCM/DH
# Edited: Kristian Lian - small changes, mainly different organizing etc.
# Project: Ribose

# This script creates a table for the nutrition data

# Packages
library(tidyverse)
library(knitr)
library(readr)
library(tinytex)
library(tidyr)
library(kableExtra)
library(rmarkdown)

# Data

nut.weight <- readRDS("./data/data-gen/dxa/nut.weight.RDS")


# Nutrition table with pro/weight. The table shows mean and SD of total intake 
# of grams per supplement per time point.

nuttable <- nut.weight %>%
  pivot_longer(names_to = "variable",
               values_to = "values", col= fat_glucose:proprkg_placebo) %>%
  group_by(timepoint, variable) %>%
  summarise(m = mean(values, na.rm = TRUE),
            s= sd(values, na.rm = TRUE)) %>%
  ungroup() %>%
  separate(variable, into = c("variable", "group")) %>%
  mutate(stat = paste0(round(m, 1), " (", round(s, 1), ")")) %>%
  select(timepoint, variable, group, stat,) %>%
  pivot_wider(names_from = variable,
              values_from = stat) %>%
  select(timepoint, group, calories, carbohydrates, fat, protein,
         proprkg) %>%
  #changed names of group for simplicity 
  mutate(group = if_else(group %in% c("glucose"),
                         "G",
                         if_else(group %in% c("placebo"),
                                 "P",
                                 ""))) 

nuttable %>%
  kable()
# kable_styling(bootstrap_options = c("Striped", "hover", "condensed"), full_width = FALSE) %>%
#footnote("Table (): Nutrition status pr. group, pr timepoint. G = glucose,
#       P = placebo. The data is shown in mean and sd")

#kable extra for fotnote.

saveRDS(nuttable, file = "./data/data-gen/dxa/nuttable.RDS")




