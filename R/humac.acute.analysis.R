#### Humac "acute" data baseline analysis

## Author: Kristian Lian/DH
## Project: Ribose

# Purpose: This script plots mean torque per supplement (both through intervention and pre vs. post) results from the ribose project, 
# and calculates a baseline analysis between supplement legs, at each speed (isometric, 60 d/s and 240 d/s).

# Packages
library(tidyverse);library(nlme);library(lme4);library(emmeans)

# Data

humac <- readRDS("./data/data-gen/humac/humac.clean.RDS")


## Change-data
# The code beneath summarizes the mean values at each time, grouped by subject, time and supplement, creating a wider data set with observations of 
# participants glucose measurements per time point.
# Then, mutate() is used to calculate change scores, where each timepoint is log-transformed and compared to baseline. baseline = baseline - mean(baseline,
# na.rm = TRUE) mean centers the baseline values. Subject, supplement, baseline and change scores are then selected and pivoted for modeling. The data set is
# filtered according to test exercise (isometric, isokinetic 60 or isokinetic 240)

# Isometric
isom.dat <- humac %>%
  filter(test == "isom",
         time %in% c("test3", "test4")) # %>%
 # print()

change.isom <- isom.dat %>%
  dplyr::select(subject, acute, supplement, peak.torque) %>%
  group_by(subject, acute, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = acute, 
              values_from = peak.torque) %>%
  #print()
  
  ungroup() %>%
  mutate(change.2 = log(post30min)-log(rest),
         change.3 = log(post2h)-log(rest),
         change.4 = log(post23h)-log(rest),
         baseline = rest - mean(rest, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
#  print()

saveRDS(change.isom, "./data/data-gen/humac/isom.lchange.ac.RDS")

# Isok.60

isok60.dat <- humac %>%
  filter(test == "isok.60",
         time %in% c("test3", "test4")) #%>%
  #print()

change.60 <- isok60.dat %>%
  dplyr::select(subject, acute, supplement, peak.torque) %>%
  group_by(subject, acute, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = acute, 
              values_from = peak.torque) %>%
  #print()
  
  ungroup() %>%
  mutate(change.2 = log(post30min)-log(rest),
         change.3 = log(post2h)-log(rest),
         change.4 = log(post23h)-log(rest),
         baseline = rest - mean(rest, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
#  print()

saveRDS(change.60, "./data/data-gen/humac/iso60.lchange.ac.RDS")

## Isok.240

isok240.dat <- humac %>%
  filter(test == "isok.240",
         time %in% c("test3", "test4")) #%>%
  #print()

change.240 <- isok240.dat %>%
  dplyr::select(subject, acute, supplement, peak.torque) %>%
  group_by(subject, acute, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = acute, 
              values_from = peak.torque) %>%
  #print()
  
  ungroup() %>%
  mutate(change.2 = log(post30min)-log(rest),
         change.3 = log(post2h)-log(rest),
         change.4 = log(post23h)-log(rest),
         baseline = rest - mean(rest, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
#  print()

saveRDS(change.240, "./data/data-gen/humac/iso240.lchange.ac.RDS")

## Linear mixed effects model
# This model tries to explain the change by time and supplement, accounting for potential differences in baseline values and that the same participants
# are measured at multiple time points. 
# It produces results on both the time effect and the difference between the groups at any timepoint. We are interested in the difference between groups.

# Isometric
m.isom <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = change.isom)
plot(m.isom)

summary(m.isom)

# Isok.60
m.60 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = change.60)
plot(m.60)

summary(m.60)

# Isok.240
m.240 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = change.240)
plot(m.240)

summary(m.240)

## Fold-change estimated means
# Gets estimated means from the model, these are average increase at pre = 0 (the average pre value).
# These are log-fold change values (changeble with the mutate function)

# Isometric

confint.m1 <- confint(emmeans(m.isom, specs = ~"supplement|time")) %>%
  data.frame() %>%
  print()

saveRDS(confint.m1, "./data/data-gen/humac/emm.ac.isom.RDS")

# Isok.60

confint.m2 <- confint(emmeans(m.60, specs = ~"supplement|time")) %>%
  data.frame() %>%
  print()

saveRDS(confint.m2, "./data/data-gen/humac/emm.ac.60.RDS")

# Isok.240

confint.m3 <- confint(emmeans(m.240, specs = ~"supplement|time")) %>%
  data.frame()

saveRDS(confint.m3, "./data/data-gen/humac/emm.ac.240.RDS")


