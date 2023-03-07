### Nutrition data cleanup
#
#
## Author: SCM/DH
## Edited: KL
#
#
## Project: Ribose
#
#
## Purpose: This script cleans up the nutrition data
#
#
## Packages
library(readxl);library(tidyverse)

# Data
fp.weight <- read_excel("data/dxa/ribose_dxa.xlsx") %>%
  select(weight, subject) 

nut.dat <- read_excel("data/dxa/Ribose_nutrition_result.xlsx")


# Data handling/cleaning

nut.clean <- nut.dat %>%
  select(timepoint, subject, group, calories, fat, carbohydrates, protein, sup_pro, sup_gluc, kcal_glu) %>%
  mutate(timepoint = if_else(timepoint %in% c("T1","T2"), 
                             "D1",
                             if_else(timepoint %in% c("3", "4"),
                                     "D2",
                                     if_else(timepoint %in% c("5", "6"),
                                             "D3",
                                             if_else(timepoint %in% c("7", "8"),
                                                     "D4",
                                                     if_else(timepoint %in% c("9", "10"),
                                                             "D5",
                                                             if_else(timepoint %in% c("T3", "T4"),
                                                                     "D6",
                                                                     "na"))))))) %>%
  group_by(timepoint, subject, group) %>%
  inner_join(fp.weight) %>%
  summarise(protein = sum(protein + sup_pro),
            fat = sum(fat),
            calories = sum(calories + kcal_glu),
            carbohydrates = sum(carbohydrates + sup_gluc),
            weight = sum(weight)) %>%
  mutate(proprkg = (protein/weight))
  

saveRDS(nut.clean, "./data/data-gen/nutrition/nut.clean.RDS")


