### Insulin analysis

## Author: Kristian Lian
# Project: Ribose

## Packages
library(dplyr)
library(tidyverse)
library(readxl)
library(nlme)
library(lme4)
library(emmeans)
library(tidyr)

## Data

ins.dat <- read_excel("./data/glucose/ribose.insulin.xlsx", na = "NA")


code <- read_excel("data/code_key.xlsx", na = "NA") %>%
  select(-leg) %>%
  print()

## Data handling


ins.dat$sample.time <- as.character(ins.dat$sample.time)

ins.dat2 <- ins.dat %>%
  inner_join(code) %>%
  mutate(sample.time = as.character(sample.time)) %>%
  mutate(time = if_else(sample.time == "0",
                        "pre",
                        if_else(sample.time == "0.1",
                                "baseline",
                                if_else(sample.time == "45",
                                "min45",
                                if_else(sample.time == "90",
                                        "min90",
                                        if_else(sample.time == "120",
                                                "min120",
                                                if_else(sample.time == "270",
                                                        "min270", sample.time))))))) %>%
  mutate(time = factor(time, levels = c("pre", "baseline", "min45", "min90", "min120", "min270")),
         c.pep = as.numeric(c.pep)) %>%
  print()

## Baseline analysis
# Comparing pre measurements to post day baseline

base.ins <- ins.dat2 %>%
  filter(time %in% c("pre", "baseline")) %>%
  select(subject, time, supplement, c.pep) %>%
  group_by(supplement) %>%
  pivot_wider(names_from = time, 
              values_from = c.pep) %>%
  print()

ins.ttest <- t.test(base.ins$pre, base.ins$baseline, paired = TRUE)
  
## Change data

ins.change <- ins.dat2 %>%
  #filter(subject != "102") %>%
  dplyr::select(subject, time, c.pep, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(mean.pep = mean(c.pep, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = mean.pep) %>%
  ungroup() %>%
  mutate(change.45 = min45-baseline,
         change.90 = min90-baseline,
         change.120 = min120-baseline,
         change.270 = min270-baseline,
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  select(subject, supplement, baseline, change.45, change.90, change.120, change.270) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.45:change.270)) %>%
  print()

ins.lchange <- ins.dat2 %>%
  #filter(subject != "102") %>%
  dplyr::select(subject, time, c.pep, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(mean.pep = mean(c.pep, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = mean.pep) %>%
  ungroup() %>%
  mutate(change.45 = log(min45)-log(baseline),
         change.90 = log(min90)-log(baseline),
         change.120 = log(min120)-log(baseline),
         change.270 = log(min270)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  select(subject, supplement, baseline, change.45, change.90, change.120, change.270) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.45:change.270)) %>%
  print()

## Linear mixed effects model

m1 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = ins.lchange) # Main analysis, using log-transformed data due to heteroscedasticity

m2 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = ins.change) # For emmeans and figure illustration

plot(m1)
plot(m2)

summary(m1)

## Emmeans

library(pbkrtest)

# For analysis
insulin.change <- confint.m1 <- confint(emmeans(m1, specs = ~"supplement|time")) %>%
  data.frame()
  
saveRDS(insulin.change, "./data/data-gen/glucose/insulin.change.RDS")

# For figure
insulin.figchange <- confint.m2 <- confint(emmeans(m2, specs = ~"supplement|time")) %>%
  data.frame()

saveRDS(insulin.figchange, "./data/data-gen/glucose/insulin.figchange.RDS")


