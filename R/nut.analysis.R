#### Nutrition analysis

# Author: SCM/DH
# Edited: Kristian Lian - small changes, mainly different organizing etc.
# Project: Ribose

# This script analyses nutrition data extracted from "MyFitnessPal" and imported via excel. Macro-nutrient intake per participant per day
# is calculated to investigate whether there where different intakes between training days.

# Packages
library(knitr);library(rmarkdown);library(tidyverse);library(readr);
library(tinytex);library(tidyr);library(broom);library(arsenal);library(lme4);library(dplyr);
library(lmerTest);library(emmeans);library(magrittr);library(dabestr) #library(dbplyr)?

# Data

fp.weight <- readRDS("./data/data-gen/dxa/fp.weight.RDS")

nut.res <- readRDS("./data/data-gen/dxa/nutha.RDS")

## Macro nutrient analysis
# The code beneath tests whether there are differences in macro nutrient intake from pre to post, 
# divided into "protein", "fat" and "carbohydrates", and total calories in a linear model. The main interest is if there is difference between glucose and 
# placebo.

# Protein

pro.dat <- nut.res %>%
  select(timepoint, subject, group, protein)

prolm <- lmer(protein ~ timepoint + timepoint:group + (1|subject), data = pro.dat)

plot(prolm)

summary(prolm)

# Fat

fat.dat <- nut.res %>%
  select(timepoint, subject, group, fat)

fatlm <- lmer(fat  ~ timepoint + timepoint:group +(1|subject), data = fat.dat)

plot(fatlm)

summary(fatlm)

# Carbohydrates
carb.dat <- nut.res %>%
  select(timepoint, subject, group, carbohydrates)


carblm <- lmer(carbohydrates  ~ timepoint + timepoint:group + (1|subject), data = carb.dat)

plot(carblm)

summary(carblm)

# Total calories a day

cal.dat <- nut.res %>%
  select(timepoint, subject, group, calories,) 


callm <- lmer(calories  ~ 0 + timepoint + timepoint:group + (1|subject), data = cal.dat)

plot(callm)

summary(callm)

# Protein per body weight

prowe <- nut.res %>%
  select(timepoint, subject, group, proprkg)

prokglm <- lmer(proprkg  ~ 0 + timepoint + timepoint:group + (1|subject), data = prowe)

plot(prokglm)

summary(prokglm)



