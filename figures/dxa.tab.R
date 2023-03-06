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
  count(sex) %>%
  pivot_wider(names_from = sex,
              values_from = n)


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
            sd.tm = sd(totalmasskg, na.rm = TRUE))




