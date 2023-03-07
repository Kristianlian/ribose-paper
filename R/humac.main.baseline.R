### Humac baseline analysis
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script analyses baseline measurements from humac
#
#
## Associated scripts: humac.cleanup.R
#
#
# Packages
library(tidyverse);library(nlme);library(lme4);library(emmeans)

# Data

humac <- readRDS("./data/data-gen/humac/humac.clean.RDS")

## Baseline analysis - comparison of the two legs
# A baseline analysis comparing peak torque for each exercise at baseline between the two legs via a paired t.test, and providing a summary of mean peak
# torque and sd

# Isometric
base.isom <- humac %>%
  filter(time == "baseline",
         test == "isom") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  pivot_wider(names_from = supplement,
              values_from = peak.torque) #%>%
 # print()

isom.ttest <- t.test(base.isom$glucose, base.isom$placebo, paired = TRUE)

isom.summary <- humac %>%
  filter(time == "baseline",
         test == "isom") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  mutate(m = mean(peak.torque),
         s = sd(peak.torque)) #%>%
  #print()

saveRDS(isom.summary, "./data/data-gen/humac/isom.sum.RDS")

# Isok 60

base.60 <- humac %>%
  filter(time == "baseline",
         test == "isok.60") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  pivot_wider(names_from = supplement,
              values_from = peak.torque) #%>%
  #print()

isok60.ttest <- t.test(base.60$glucose, base.60$placebo, paired = TRUE)

isok60.summary <- humac %>%
  filter(time == "baseline",
         test == "isok.60") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  mutate(m = mean(peak.torque),
         s = sd(peak.torque)) #%>%
#  print()

saveRDS(isok60.summary, "./data/data-gen/humac/isok60.sum.RDS")

# Isok 240

base.240 <- humac %>%
  filter(time == "baseline",
         test == "isok.240") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  pivot_wider(names_from = supplement,
              values_from = peak.torque) #%>%
#  print()

isok240.ttest <- t.test(base.240$glucose, base.240$placebo, paired = TRUE)

isok240.summary <- humac %>%
  filter(time == "baseline",
         test == "isok.240") %>%
  select(subject, time, test, supplement, peak.torque) %>%
  group_by(supplement) %>%
  mutate(m = mean(peak.torque),
         s = sd(peak.torque)) #%>%
  #print()

saveRDS(isok240.summary, "./data/data-gen/humac/isok240.sum.RDS")
