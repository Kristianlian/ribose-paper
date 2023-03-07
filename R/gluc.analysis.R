### Plasma glucose analysis
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script analyses the cleaned up glucose data, by change-score
# comparisons
#
#
## Associated scripts: gluc.cleanup.R
#
#
## Packages
library(tidyverse); library(dplyr); library(nlme); library(lme4); library(emmeans); library(tidyr)

## Data

gluc.clean <- readRDS("./data/data-gen/glucose/gluc.clean.RDS")

# Un-transformed
change_dat.glu <- gluc.clean %>%
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

saveRDS(change_dat.glu, "./data/data-gen/glucose/glu.change.RDS")

# Log-transformed
glu.logchange <- gluc.clean %>%
  # filter(subject != "107") %>%
  dplyr::select(subject, time, glu, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(glu = mean(glu, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = glu) %>%
  #print()
  
  ungroup() %>%
  mutate(change.45 = log(min45)-log(baseline),
         change.90 = log(min90)-log(baseline),
         change.120 = log(min120)-log(baseline),
         change.135 = log(min135)-log(baseline),
         change.150 = log(min150)-log(baseline),
         change.270 = log(min270)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.45, change.90, change.120, change.135, 
         change.150, change.270) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.45:change.270)) %>%
  print()

saveRDS(glu.logchange, "./data/data-gen/glucose/glu.logchange.RDS")


## Linear mixed effects model

m1 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = change_dat.glu)

m2 <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = glu.logchange)

plot(m1)
plot(m2)

summary(m1)
summary(m2)

## Emmeans

gluc.change <- confint.m1 <- confint(emmeans(m1, specs = ~"supplement|time")) %>%
  data.frame()

gluc.logchange <- confint.m2 <- confint(emmeans(m2, specs = ~"supplement|time")) %>%
  data.frame()


saveRDS(gluc.change, "./data/data-gen/glucose/gluc.emm.RDS")
saveRDS(gluc.change, "./data/data-gen/glucose/gluc.logemm.RDS")

