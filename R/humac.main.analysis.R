### Humac main analysis
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script analyses humac data from baseline, post 2 session test,
# post 4 session test and post 5 RT sessions test via change-score comparisons
#
#
## Associated scripts: humac.cleanup.R
#
#
# Packages
library(tidyverse);library(nlme);library(lme4);library(emmeans)

# Data

rest.dat <- readRDS("./data/data-gen/humac/rest.dat.RDS")


## Change-data
# The code beneath summarizes the mean values at each time, grouped by subject, time and supplement, creating a wider data set with observations of 
# participants glucose measurements per time point.
# Then, mutate() is used to calculate change scores, where each timepoint is log-transformed and compared to baseline. baseline = baseline - mean(baseline,
# na.rm = TRUE) mean centers the baseline values. Subject, supplement, baseline and change scores are then selected and pivoted for modeling. The data set is
# filtered according to test exercise (isometric, isokinetic 60 or isokinetic 240).


# Isometric
isom.dat <- rest.dat %>%
  filter(test == "isom") #%>%
 # print()

change.isom <- isom.dat %>%
  dplyr::select(subject, time, supplement, peak.torque) %>%
  group_by(subject, time, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = peak.torque) %>%
  
  ungroup() %>%
  mutate(change.2 = log(test1)-log(baseline),
         change.3 = log(test2)-log(baseline),
         change.4 = log(test3)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
#  print()

saveRDS(change.isom, "./data/data-gen/humac/isom.lchange.RDS")

# Isok 60

isok60.dat <- rest.dat %>%
  filter(test == "isok.60") # %>%
 # print()

change.60 <- isok60.dat %>%
  dplyr::select(subject, time, supplement, peak.torque) %>%
  group_by(subject, time, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = peak.torque) %>%
  
  ungroup() %>%
  mutate(change.2 = log(test1)-log(baseline),
         change.3 = log(test2)-log(baseline),
         change.4 = log(test3)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
  #print()

saveRDS(change.60, "./data/data-gen/humac/isok60.lchange.RDS")

## Isok.240

isok240.dat <- rest.dat %>%
  filter(test == "isok.240") # %>%
  #print()

change.240 <- isok240.dat %>%
  dplyr::select(subject, time, supplement, peak.torque) %>%
  group_by(subject, time, supplement) %>%
  summarise(peak.torque = mean(peak.torque, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = peak.torque) %>%
  
  ungroup() %>%
  mutate(change.2 = log(test1)-log(baseline),
         change.3 = log(test2)-log(baseline),
         change.4 = log(test3)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.4)) #%>%
  #print()

saveRDS(change.240, "./data/data-gen/humac/isok240.lchange.RDS")

## Linear mixed effects model
# This model tries to explain the change by time and supplement, accounting for potential differences in baseline values and that the same participants
# are measured at multiple time points. 
# It produces results on both the time effect and the difference between the groups at any timepoint. We are interested in the difference between groups.

# Mean of all subjects 

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

confint.isom <- confint(emmeans(m.isom, specs = ~"supplement|time")) %>%
  data.frame() 

saveRDS(confint.isom, "./data/data-gen/humac/emm.isom.RDS")

# Isok.60

confint.60 <- confint(emmeans(m.60, specs = ~"supplement|time")) %>%
  data.frame()# %>%
#  print()

saveRDS(confint.60, "./data/data-gen/humac/emm.60.RDS")

# Isok.240

confint.240 <- confint(emmeans(m.240, specs = ~"supplement|time")) %>%
  data.frame()

saveRDS(confint.240, "./data/data-gen/humac/emm.240.RDS")




