#### Nutrition data handling

# Author: SCM/DH
# Edited: Kristian Lian - small changes, mainly different organizing etc.
# Project: Ribose

# This script tidies up the nutrition data set and saves two data frames (fp.weight and nutha) for later analyses.

# Packages
library(readxl);library(tidyverse)

# Data
fp.weight <-read_excel("data/dxa/ribose_dxa.xlsx") %>%
  select(weight, subject) %>%
  print()

saveRDS(fp.weight, "./data/data-gen/dxa/fp.weight.RDS")

dxa.res <- read_excel("data/dxa/Ribose_nutrition_result.xlsx")

nutri.weight <- read_excel("data/dxa/Ribose_nutrition_result.xlsx")
  

# Data handling/cleaning

nutha <- dxa.res %>%
  select(timepoint, subject, group, calories, fat, carbohydrates, protein, sup_pro, sup_gluc, kcal_glu) %>%
  mutate(timepoint = if_else(timepoint %in% c("T1","T2"), 
                             "Day 1",
                             if_else(timepoint %in% c("3", "4"),
                                     "Day 2",
                                     if_else(timepoint %in% c("5", "6"),
                                             "Day 3",
                                             if_else(timepoint %in% c("7", "8"),
                                                     "Day 4",
                                                     if_else(timepoint %in% c("9", "10"),
                                                             "Day 5",
                                                             if_else(timepoint %in% c("T3", "T4"),
                                                                     "Day 6",
                                                                     "na"))))))) %>%
  group_by(timepoint, subject, group) %>%
  inner_join(fp.weight) %>%
  summarise(protein = sum(protein + sup_pro),
            fat = sum(fat),
            calories = sum(calories + kcal_glu),
            carbohydrates = sum(carbohydrates + sup_gluc),
            weight = sum(weight)) %>%
  mutate(proprkg = (protein/weight))

saveRDS(nutha, "./data/data-gen/dxa/nutha.RDS")


nut.weight <- nutri.weight %>%
  select(timepoint, meal, subject, protein, fat, calories,
         carbohydrates, group, sup_pro, sup_gluc, kcal_glu) %>%
  #changed names to compare groups each paired days
  mutate(timepoint = if_else(timepoint %in% c("T1","T2"), 
                             "Day 1",
                             if_else(timepoint %in% c("3", "4"),
                                     "Day 2",
                                     if_else(timepoint %in% c("5", "6"),
                                             "Day 3",
                                             if_else(timepoint %in% c("7", "8"),
                                                     "Day 4",
                                                     if_else(timepoint %in% c("9", "10"),
                                                             "Day 5",
                                                             if_else(timepoint %in% c("T3", "T4"),
                                                                     "Day 6",
                                                                     "na"))))))) %>%
  group_by(timepoint, subject, group) %>%
  #take in weight
  inner_join(fp.weight) %>%
  #summarise each variable to get total pr timepoint.
  #Whey protein supplement is added to total protein ingestion
  summarise(protein = sum(protein + sup_pro),
            fat = sum(fat),
            calories = sum(calories + kcal_glu),
            carbohydrates = sum(carbohydrates + sup_gluc),
            weight = sum(weight)) %>%
  #summarise to get protein pr weight.
  mutate(proprkg = (protein/weight)) %>%
  pivot_wider(names_from = group,
              values_from = c(fat, protein, calories, carbohydrates, weight, proprkg))

saveRDS(nut.weight, "./data/data-gen/dxa/nut.weight.RDS")



