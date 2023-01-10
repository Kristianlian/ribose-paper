#### Blood glucose change fig

## Author: Kristian Lian
## Project: Ribose

## This script analyses is equal to mean-bloodglucose-change, except from not calculating log-fold change. Instead, absolute change scores are calculated
# to illustrate the changes in absolute plasma glucose values. For description of the codes, see mean-bloodglucose-change.

# Packages
library(dplyr)
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(emmeans)
library(tidyr)


## Data handling

gluc.dat <- read_excel("./data/glucose/fingerstick.xlsx", na = "NA")
gluc.dat$sample_time <- as.character(gluc.dat$sample_time)

glu.dat <- gluc.dat %>%
  mutate(time = if_else(sample_time == "0",
                        "baseline",
                        if_else(sample_time == "45",
                                "min45",
                                if_else(sample_time == "90",
                                        "min90",
                                        if_else(sample_time == "120",
                                                "min120",
                                                if_else(sample_time == "135",
                                                        "min135",
                                                        if_else(sample_time == "150",
                                                                "min150",
                                                                if_else(sample_time == "270",
                                                                        "min270", sample_time))))))),
         time = factor(time, levels = c("baseline", "min45", "min90", "min120", "min135", "min150", "min270"))) %>%
  mutate(glu = as.numeric(glu),
         lak = as.numeric(lak)) %>%
  print()

## Change data

change_dat.glu <- glu.dat %>%
  # filter(subject != "107") %>%
  dplyr::select(subject, time, glu, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(glu = mean(glu, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = glu) %>%
  #print()
  
  ungroup() %>%
  mutate(change.45 = min45-baseline,
         change.90 = min90-baseline,
         change.120 = min120-baseline,
         change.135 = min135-baseline,
         change.150 = min150-baseline,
         change.270 = min270-baseline,
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.45, change.90, change.120, change.135, 
         change.150, change.270) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.45:change.270)) %>%
  print()

## Linear mixed effects model

m1 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = change_dat.glu)
plot(m1)

summary(m1)


## Emmeans

gluc.change <- confint.m1 <- confint(emmeans(m1, specs = ~"supplement|time")) %>%
  data.frame()


saveRDS(gluc.change, "./data/data-gen/glucose/gluc.change.RDS")


