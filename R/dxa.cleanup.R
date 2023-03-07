### Baseline DXA cleanup
#
#
## Author: SCM
## Edited: KL
#
#
## Project: Ribose
#
#
## Purpose: This script cleans up DXA data grouped by leg (i.e. glu/plac)
#
#
## Packages

library(readxl); library(tidyverse)

## Data

dxa.dat <- read_excel("./data/dxa/ribose_dxaprleg.xlsx")


dxa.clean <- dxa.dat %>%
  select(subject, sex, leg, leanmassg, fatmassg, totalmasskg) %>%
  #make grams into kg
  mutate(leanmass = leanmassg/1000,
         fatmass = fatmassg/1000)%>%
  select(subject, sex, leg, leanmass, fatmass, totalmasskg) 

saveRDS(dxa.clean, "./data/data-gen/dxa/dxa.clean.RDS")

