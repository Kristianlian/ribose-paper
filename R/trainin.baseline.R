## Training intensity baseline analysis

# Author: Kristian Lian
# Project: Ribose


# This script summarises training intensity and tests for baseline differences.

# Packages
library(tidyverse);library(readxl)


# Data

tot.load <- readRDS("./data/data-gen/training/tot.load.RDS")

tot.rm <- read_excel("./data/tests/ribose.1rm.xlsx", na = "NA") %>%
  select(subject, supplement, exercise, rm) 

## Baseline analysis - comparison of the two legs
# A baseline analysis comparing training intensity at baseline sessions between the two legs via a paired t.test, and providing a summary of mean training
# intensity and sd

base.joined <- tot.load %>%
  full_join(tot.rm) %>%
  mutate(p.rm = (load/rm)*100) %>%
  filter(time == "baseline") %>%
  select(subject, time, supplement, p.rm, exercise) %>%
  group_by(subject, supplement, exercise) %>%
  summarise(m = mean(p.rm)) %>%
  pivot_wider(names_from = supplement,
              values_from = m) #%>%
  #print()

rm.ttest <- t.test(base.joined$glucose, base.joined$placebo, paired = TRUE)

rm.summary <- tot.load %>%
  full_join(tot.rm) %>%
  mutate(p.rm = (load/rm)*100) %>%
  filter(time == "baseline") %>%
  select(subject, time, supplement, p.rm, exercise) %>%
  group_by(supplement, exercise) %>%
  summarise(m = mean(p.rm),
            s = sd(p.rm)) %>%
  print()

## Join data by full_join() and calulates %1RM as a measure of training intensity

tot.joined <- tot.load %>%
  full_join(tot.rm) %>%
  mutate(p.rm = (load/rm)*100) %>%
  print()

saveRDS(tot.joined, "./data/data-gen/training/tot.joined.RDS")

mean.joined <- tot.joined %>%
  select(subject, supplement, time, p.rm) %>%
  group_by(supplement, time) %>%
  summarise(mean.prm = mean(p.rm),
            sd.prm = sd(p.rm)) %>%
  print()

saveRDS(mean.joined, "./data/data-gen/training/mean.joined.RDS")


