#### Nutrition change analysis

# Author: KL
# Project: Ribose

# This script calculates change scores and models the change for protein, protein/kg/body weight,
# fat, carbohydrates and calories.

# Packages
library(tidyverse); library(lme4); library(lmerTest); library(emmeans)


# Data

fp.weight <- readRDS("./data/data-gen/dxa/fp.weight.RDS")

dxa.res <- read_excel("data/dxa/Ribose_nutrition_result.xlsx")

# Data handling

# Data handling/cleaning

nut.res <- dxa.res %>%
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

saveRDS(nutha, "./data/data-gen/dxa/nutha.RDS")

### Macro nutrient change analysis
# The code beneath tests whether there are differences in macro nutrient intake from pre to post, 
# divided into "protein", "fat" and "carbohydrates", and total calories in a linear model. The main interest is if there is difference between glucose and 
# placebo.

## Protein

# Log transforming for analysis
pro.lchange <- nut.res %>%
  select(time = timepoint, subject, supplement = group, protein) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = protein) %>%
  ungroup() %>%
  mutate(change.d2 = log(D2)-log(D1),
         change.d3 = log(D3)-log(D1),
         change.d4 = log(D4)-log(D1),
         change.d5 = log(D5)-log(D1),
         change.d6 = log(D6)-log(D1),
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(pro.lchange, "./data/data-gen/nutrition/pro.lchange.RDS")

# Untransformed change for figures
pro.change <- nut.res %>%
  select(time = timepoint, subject, supplement = group, protein) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = protein) %>%
  ungroup() %>%
  mutate(change.d2 = D2-D1,
         change.d3 = D3-D1,
         change.d4 = D4-D1,
         change.d5 = D5-D1,
         change.d6 = D6-D1,
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6))

saveRDS(pro.change, "./data/data-gen/nutrition/pro.change.RDS")


pro.lmod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = pro.lchange)
pro.mod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = pro.change)

pro.lres <- plot(pro.lmod)
pro.lsum <- summary(pro.lmod)

pro.res <- plot(pro.mod)
pro.sum <- summary(pro.mod)

## Fat
# Log-transformed for analysis
fat.lchange <- nut.res %>%
  select(time = timepoint, subject, supplement = group, fat) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = fat) %>%
  ungroup() %>%
  mutate(change.d2 = log(D2)-log(D1),
         change.d3 = log(D3)-log(D1),
         change.d4 = log(D4)-log(D1),
         change.d5 = log(D5)-log(D1),
         change.d6 = log(D6)-log(D1),
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(fat.lchange, "./data/data-gen/nutrition/fat.lchange.RDS")

# Untransformed for figures
fat.change <- nut.res %>%
  select(time = timepoint, subject, supplement = group, fat) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = fat) %>%
  ungroup() %>%
  mutate(change.d2 = D2-D1,
         change.d3 = D3-D1,
         change.d4 = D4-D1,
         change.d5 = D5-D1,
         change.d6 = D6-D1,
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6))

saveRDS(fat.change, "./data/data-gen/nutrition/fat.change.RDS")

fat.lmod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = fat.lchange)
fat.mod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = fat.change)

fat.lres <- plot(fat.lmod)
fat.lsum <- summary(fat.lmod)
fat.res <- plot(fat.mod)
fat.sum <- summary(fat.mod)

## Carbohydrates
# Log-transformed for analysis

cho.lchange <- nut.res %>%
  select(time = timepoint, subject, supplement = group, cho = carbohydrates) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = cho) %>%
  ungroup() %>%
  mutate(change.d2 = log(D2)-log(D1),
         change.d3 = log(D3)-log(D1),
         change.d4 = log(D4)-log(D1),
         change.d5 = log(D5)-log(D1),
         change.d6 = log(D6)-log(D1),
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(cho.lchange, "./data/data-gen/nutrition/cho.lchange.RDS")

# Untransformed for figures
cho.change <- nut.res %>%
  select(time = timepoint, subject, supplement = group, cho = carbohydrates) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = cho) %>%
  ungroup() %>%
  mutate(change.d2 = D2-D1,
         change.d3 = D3-D1,
         change.d4 = D4-D1,
         change.d5 = D5-D1,
         change.d6 = D6-D1,
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6))

saveRDS(cho.change, "./data/data-gen/nutrition/cho.change.RDS")

cho.lmod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cho.lchange)
cho.mod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cho.change)


cho.lres <- plot(cho.lmod)
cho.lsum <- summary(cho.lmod)
cho.res <- plot(cho.mod)
cho.sum <- summary(cho.mod)

## Total calories a day
# Log-transformed for analysis
cal.lchange <- nut.res %>%
  select(time = timepoint, subject, supplement = group, calories) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = calories) %>%
  ungroup() %>%
  mutate(change.d2 = log(D2)-log(D1),
         change.d3 = log(D3)-log(D1),
         change.d4 = log(D4)-log(D1),
         change.d5 = log(D5)-log(D1),
         change.d6 = log(D6)-log(D1),
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(cal.lchange, "./data/data-gen/nutrition/cal.lchange.RDS")

# Untransformed for figures
cal.change <- nut.res %>%
  select(time = timepoint, subject, supplement = group, calories) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = calories) %>%
  ungroup() %>%
  mutate(change.d2 = D2-D1,
         change.d3 = D3-D1,
         change.d4 = D4-D1,
         change.d5 = D5-D1,
         change.d6 = D6-D1,
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(cal.change, "./data/data-gen/nutrition/cal.change.RDS")

cal.lmod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cal.lchange)
cal.mod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cal.change)

cal.lres <- plot(cal.lmod)
cal.lsum <- summary(cal.lmod)
cal.res <- plot(cal.mod)
cal.sum <- summary(cal.mod)

## Protein per body weight
# Log-transformed for analysis
proprkg.lchange <- nut.res %>%
  select(time = timepoint, subject, supplement = group, proprkg) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = proprkg) %>%
  ungroup() %>%
  mutate(change.d2 = log(D2)-log(D1),
         change.d3 = log(D3)-log(D1),
         change.d4 = log(D4)-log(D1),
         change.d5 = log(D5)-log(D1),
         change.d6 = log(D6)-log(D1),
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(proprkg.lchange, "./data/data-gen/nutrition/propr.lchange.RDS")

# Untransformed for figures
proprkg.change <- nut.res %>%
  select(time = timepoint, subject, supplement = group, proprkg) %>%
  group_by(subject, time, supplement) %>%
  #summarise(mean.pro = mean(protein, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = proprkg) %>%
  ungroup() %>%
  mutate(change.d2 = D2-D1,
         change.d3 = D3-D1,
         change.d4 = D4-D1,
         change.d5 = D5-D1,
         change.d6 = D6-D1,
         d1 = D1-mean(D1, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, d1, change.d2, change.d3, change.d4, change.d5, change.d6) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.d2:change.d6)) 

saveRDS(properkg.change, "./data/data-gen/nutrition/propr.change.RDS")

propr.lmod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = proprkg.lchange)
propr.mod <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = proprkg.change)

propr.lres <- plot(propr.lmod)
propr.lsum <- summary(propr.lmod)
propr.res <- plot(propr.mod)
propr.sum <- summary(propr.mod)

## Getting emmeans
# Protein
protein.lemm <- confint(emmeans(pro.lmod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(protein.lemm, "./data/data-gen/nutrition/prot.lemm.RDS")

protein.emm <- confint(emmeans(pro.mod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(protein.emm, "./data/data-gen/nutrition/prot.emm.RDS")

# Fat
fat.lemm <- confint(emmeans(fat.lmod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(fat.lemm, "./data/data-gen/nutrition/fat.lemm.RDS")

fat.emm <- confint(emmeans(fat.mod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(fat.emm, "./data/data-gen/nutrition/fat.emm.RDS")

# Carbs
cho.lemm <- confint(emmeans(cho.lmod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cho.lemm, "./data/data-gen/nutrition/cho.lemm.RDS")

cho.emm <- confint(emmeans(cho.mod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cho.emm, "./data/data-gen/nutrition/cho.emm.RDS")

# Calories
cal.lemm <- confint(emmeans(cal.lmod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cal.lemm, "./data/data-gen/nutrition/cal.lemm.RDS")

cal.emm <- confint(emmeans(cal.mod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cal.emm, "./data/data-gen/nutrition/cal.emm.RDS")

# Protein per kg body weight
propr.lemm <- confint(emmeans(propr.lmod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(propr.lemm, "./data/data-gen/nutrition/propr.lemm.RDS")

propr.emm <- confint(emmeans(propr.mod, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(propr.emm, "./data/data-gen/nutrition/propr.emm.RDS")







