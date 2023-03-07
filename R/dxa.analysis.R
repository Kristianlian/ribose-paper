### Baseline analysis DXA
#
#
## Author: SCM
## Edited: KL
#
#
## Project: Ribose
#
#
## Purpose: This script contains baseline analyses of the clean DXA data
## (script: dxa.cleanup.R)
#
#
## Packages

library(tidyverse); library(nlme); library(lme4); library(emmeans)

## Data

dxa.clean <- readRDS("./data/data-gen/dxa/dxa.clean.RDS") %>%
  select(-sex) 


## Baseline analysis models
# Lean mass between legs

leanmass.lm <- lmer(leanmass ~ leg + leg:subject + (1|subject), data = dxa.clean)

plot(leanmass.lm)
summary(leanmass.lm)

emmeans(leanmass.lm, specs = ~ "leg") %>%
  confint()


# Fat mass between legs

fat.lm <- lmer(fatmass ~ leg + leg:subject +(1|subject), data = dxa.clean)

plot(fat.lm)
summary(fat.lm)

emmeans(fat.lm, specs = ~ "leg") %>%
  confint()


# Totalmass between legs

total.lm <- lmer(totalmasskg ~leg + leg:subject + (1|subject), data = dxa.clean)

plot(total.lm)
summary(total.lm)

emmeans(total.lm, specs = ~ "leg") %>%
  confint() 



