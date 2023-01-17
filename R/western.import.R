#### Import and normalization of quantified Western Blot data

## Author: Kristian Lian

## Purpose: The purpose of this script is to import the quantified western data, and normalize total protein by subtracting
## background. The script also mean summarises signal data. This script creates and saves the data frames "total.protein"
## and "mean.signal".

# Packages
library(readxl);library(tidyverse)

# Data import
western.dat <- read_excel("data/protein/western.quant.xlsx")


# Summarising mean total protein and backgroun per gel

# The "mean.tp" and "mean.bg" codes calculates the mean and SD of total protein and background in each well, on each gel.
# This is needed to normalize total protein with the "total.protein" code.

mean.tp <- western.dat %>%
  select(gel, well, tp1, tp2) %>%
  #group_by(gel, well) %>%
  pivot_longer(names_to = "measure",
               values_to = "mean.grey",
               cols = (tp1:tp2)) %>%
  group_by(gel, well) %>%
  summarise(tp.mean = mean(mean.grey, na.rm = TRUE),
            tp.sd = sd(mean.grey, na.rm = TRUE))


mean.bg <- western.dat %>%
  select(gel, well, bg1, bg2, bg3, bg4) %>%
  pivot_longer(names_to = "measure",
               values_to = "mean.grey",
               cols = (bg1:bg4)) %>%
  group_by(gel, well) %>%
  summarise(bg.mean = mean(mean.grey, na.rm = TRUE),
            bg.sd = sd(mean.grey, na.rm = TRUE))


# This code joins together the mean.tp and mean.bg data frames, then groups by gel and well, so that total protein 
# can be normalized for each well on each separate gel

total.protein <- mean.tp %>%
  full_join(mean.bg) %>%
  group_by(gel, well) %>%
  summarise(total.protein = tp.mean-bg.mean) %>%
  print()

saveRDS(total.protein, "./data/data-gen/protein/total.protein.RDS")  

## Mean signal

cmyc.sum <- western.dat %>%
  select(gel, well, cmyc.sig, cmyc2.sig) %>%
  #print()
  pivot_longer(names_to = "target",
               values_to = "signal",
               cols = (cmyc.sig:cmyc2.sig)) %>%
  group_by(gel, well) %>%
  summarise(cmyc.mean = mean(signal, na.rm = TRUE),
            cmyc.sd = sd(signal, na.rm = TRUE)) %>%
  
  print()

saveRDS(cmyc.sum, "./data/data-gen/protein/cmyc.sum.RDS")  


ubf.sum <- western.dat %>%
  select(gel, well, ubf.sig, ubf2.sig) %>%
  pivot_longer(names_to = "target",
               values_to = "signal",
               cols = (ubf.sig:ubf2.sig)) %>%
  group_by(gel, well) %>%
  summarise(ubf.mean = mean(signal, na.rm = TRUE),
            ubf.sd = sd(signal, na.rm = TRUE)) %>%
  print()
  
saveRDS(ubf.sum, "./data/data-gen/protein/ubf.sum.RDS")


rps6.sum <- western.dat %>%
  select(gel, well, rps6.sig, rps62.sig) %>%
  pivot_longer(names_to = "target",
               values_to = "signal",
               cols = (rps6.sig:rps62.sig)) %>%
  group_by(gel, well) %>%
  summarise(rps6.mean = mean(signal, na.rm = TRUE),
            rps6.sd = sd(signal, na.rm = TRUE)) %>%
  print()

saveRDS(rps6.sum, "./data/data-gen/protein/rps6.sum.RDS")

mean.signal <- cmyc.sum %>%
  full_join(ubf.sum) %>%
  full_join(rps6.sum) %>%
  full_join(western.dat) %>%
  select(gel, well, subject, time, leg, cmyc.mean, ubf.mean, rps6.mean) %>%
  print()




