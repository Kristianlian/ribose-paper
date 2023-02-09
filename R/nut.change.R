#### Nutrition change analysis

# Author: KL
# Project: Ribose

# This script calculates change scores and models the change for protein, protein/kg/body weight,
# fat, carbohydrates and calories.

# Packages
library(tidyverse); library(lme4); library(lmerTest); library(emmeans)


# Data

fp.weight <- readRDS("./data/data-gen/dxa/fp.weight.RDS")

nut.res <- readRDS("./data/data-gen/dxa/nutha.RDS")

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

# Un-logged change for figures
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


m1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = pro.lchange)
m1.1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = pro.change)

plot(m1)
plot(m1.1)
summary(m1)
summary(m1.1)

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

# Un-logged for figures
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

m2 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = fat.lchange)
m2.1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = fat.change)

plot(m2)
plot(m2.1)
summary(m2)
summary(m2.1)

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

# Unlogged for figures
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

m3 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cho.lchange)
m3.1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cho.change)


plot(m3)
plot(m3.1)
summary(m3)
summary(m3.1)

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

# Unlogged for figures
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

m4 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cal.lchange)
m4.1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = cal.change)

plot(m4)
plot(m4.1)
summary(m4)
summary(m4.1)

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

# Unlogged for figures
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

m5 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = proprkg.lchange)
m5.1 <- lmerTest::lmer(change ~ 0 + d1 + time + supplement:time + (1|subject),
                     data = proprkg.change)

plot(m5)
plot(m5.1)
summary(m5)
summary(m5.1)

## Getting emmeans
# Protein
protein.lemm <- confint.m1 <- confint(emmeans(m1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(protein.lemm, "./data/data-gen/nutrition/prot.lemm.RDS")

protein.emm <- confint.m1.1 <- confint(emmeans(m1.1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(protein.emm, "./data/data-gen/nutrition/prot.emm.RDS")

# Fat
fat.lemm <- confint.m2 <- confint(emmeans(m2, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(fat.lemm, "./data/data-gen/nutrition/fat.lemm.RDS")

fat.emm <- confint.m2.1 <- confint(emmeans(m2.1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(fat.emm, "./data/data-gen/nutrition/fat.emm.RDS")


# Carbs
cho.lemm <- confint.m3 <- confint(emmeans(m3, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cho.lemm, "./data/data-gen/nutrition/cho.lemm.RDS")

cho.emm <- confint.m3.1 <- confint(emmeans(m3.1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cho.emm, "./data/data-gen/nutrition/cho.emm.RDS")

# Calories
cal.lemm <- confint.m4 <- confint(emmeans(m4, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cal.lemm, "./data/data-gen/nutrition/cal.lemm.RDS")

cal.emm <- confint.m4.1 <- confint(emmeans(m4.1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(cal.emm, "./data/data-gen/nutrition/cal.emm.RDS")

# Protein per kg body weight
propr.lemm <- confint.m5 <- confint(emmeans(m5, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(propr.lemm, "./data/data-gen/nutrition/propr.lemm.RDS")

propr.emm <- confint.m5.1 <- confint(emmeans(m5.1, specs = ~"supplement|time")) %>%
  data.frame()
saveRDS(propr.emm, "./data/data-gen/nutrition/propr.emm.RDS")


