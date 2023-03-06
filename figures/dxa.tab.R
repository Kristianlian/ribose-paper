###### Baseline tab
#
#
## Author: KL/SCM
#
#
## Purpose:


## Packages
library(readxl); library(tidyverse)

## Data
dxa.dat <- read_excel("./data/dxa/ribose_dxa.xlsx")
leg.dat <- read_excel("./data/dxa/ribose_dxaprleg.xlsx")
code <- read_excel("./data/sup.xlsx")

## Gendercount - from the script dxa.R by SCM

gendercount <- dxa.dat %>%
  count(sex)

## Per subject

dxa.dat %>%
  select(-timepoint) %>%
  # Convert g to kg
  mutate(leanmass = leanmassg/1000) %>%
  select(subject, sex, age, height, weight, leanmass) %>%
  group_by(sex) %>%
  summarise(mean.age = mean(age, na.rm = TRUE),
            mean.height = mean(height, na.rm = TRUE),
            mean.weight = mean(weight, na.rm = TRUE),
            mean.leanm = mean(leanmass, na.rm = TRUE),
            sd.a = sd(age, na.rm = TRUE),
            sd.h = sd(height, na.rm = TRUE),
            sd.w = sd(weight, na.rm = TRUE),
            sd.l = sd(leanmass, na.rm = TRUE)) %>%
  mutate(a.stat = paste0(round(mean.age, 2), " ± ", round(sd.a, 2)),
         h.stat = paste0(round(mean.height, 2), " ± ", round(sd.h, 2)),
         w.stat = paste0(round(mean.weight, 2), " ± ", round(sd.w, 2)),
         l.stat = paste0(round(mean.leanm, 2), " ± ", round(sd.l, 2))) %>%
  full_join(gendercount) %>%
  select(sex, n, a.stat, h.stat, w.stat, l.stat, )

## Per leg

leg.desc <- leg.dat %>%
  full_join(code) %>%
  select(subject, sex, supplement, leanmassg, fatmassg, totalmasskg) %>%
  # Convert g to kg
  mutate(leanmass = leanmassg/1000,
         fatmass = fatmassg/1000) %>%
  select(-leanmassg, fatmassg) %>%
  group_by(supplement) %>%
  summarise(mean.lm = mean(leanmass, na.rm = TRUE),
            mean.fm = mean(fatmass, na.rm = TRUE),
            mean.tm = mean(totalmasskg, na.rm = TRUE),
            sd.lm = sd(leanmass, na.rm = TRUE),
            sd.fm = sd(fatmass, na.rm = TRUE),
            sd.tm = sd(totalmasskg, na.rm = TRUE)) %>%
  mutate(lm.stat = paste0(round(mean.lm, 2), " ± ", round(sd.lm, 2)),
         fm.stat = paste0(round(mean.fm, 2), " ± ", round(sd.fm, 2)),
         tm.stat = paste0(round(mean.tm, 2), " ± ", round(sd.tm, 2))) %>%
  mutate(supplement = if_else(supplement == "glucose",
                              "Glucose",
                              if_else(supplement == "placebo",
                                      "Placebo", supplement))) %>%
  select(supplement, lm.stat, fm.stat, tm.stat) %>%
  print()






