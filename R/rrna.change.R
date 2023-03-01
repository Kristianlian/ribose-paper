## rRNA change analysis
#### qPCR-analysis

# Author: Daniel Hammarström & Kristian Lian


# This script loads the qclean.RDS, derived from the "qpcr.analysis" script. The script also calculate change score for each of the rRNA's,
# baseline analyses and models the change from pre to post in a linear mixed effects model.

# Packages
library(tidyverse)
library(readxl)
library(nlme)
library(emmeans)

# Data
qdat <-readRDS("./data/data-gen/rna/qclean.RDS")


## Change data - preliminary handling, filtering out each rRNA for separate analysis

rrna18 <- qdat %>%
  filter(target == "trg_18s") %>%
  print()

rrna28 <- qdat %>%
  filter(target == "trg_28s") %>%
  print()

rrna5.8 <- qdat %>%
  filter(target == "trg_5.8s") %>%
  print()

rrna5 <- qdat %>%
  filter(target == "trg_5s") %>%
  print()

rrna47 <- qdat %>%
  filter(target == "trg_47s") %>%
  print()

#### Baseline analysis and change scores per rRNA
### A baseline analysis comparing rRNA expression at baseline between the two legs via a paired t.test, and providing a summary of mean rRNA expression
### and sd
## Change scores per rRNA
# The code beneath summarizes the mean values at each time, grouped by subject, time and supplement, creating a wider data set with observations of 
# participants glucose measurements per time point.
# Then, mutate() is used to calculate change scores, where each timepoint is log-transformed and compared to baseline. baseline = baseline - mean(baseline,
# na.rm = TRUE) mean centers the baseline values. Subject, supplement, baseline and change scores are then selected and pivoted for modeling.

# 18S
change.18 <- rrna18 %>%
  dplyr::select(subject, time, rep, nf.expr, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = nf.expr) %>%
  ungroup() %>%
  # print()
  mutate(change = Post-Pre,
         pre = Pre - mean(Pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(change.18, "./data/data-gen/rna/lchange.18.RDS")

base.18 <- change.18 %>%
  select(subject, supplement, Pre) %>%
  pivot_wider(names_from = supplement,
              values_from = Pre) %>%
  print()

ttest.18 <- t.test(base.18$GLUCOSE, base.18$PLACEBO, paired = TRUE)

# 28S
change.28 <- rrna28 %>%
  dplyr::select(subject, time, rep, nf.expr, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = nf.expr) %>%
  ungroup() %>%
  # print()
  mutate(change = Post-Pre,
         pre = Pre - mean(Pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(change.28, "./data/data-gen/rna/lchange.28.RDS")

base.28 <- change.28 %>%
  select(subject, supplement, Pre) %>%
  pivot_wider(names_from = supplement,
              values_from = Pre) %>%
  print()

ttest.28 <- t.test(base.28$GLUCOSE, base.28$PLACEBO, paired = TRUE)

# 5.8S
change.58 <- rrna5.8 %>%
  dplyr::select(subject, time, rep, nf.expr, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = nf.expr) %>%
  ungroup() %>%
  # print()
  mutate(change = Post-Pre,
         pre = Pre - mean(Pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(change.58, "./data/data-gen/rna/lchange.58.RDS")

base.5.8 <- change.58 %>%
  select(subject, supplement, Pre) %>%
  pivot_wider(names_from = supplement,
              values_from = Pre) %>%
  print()

ttest.5.8 <- t.test(base.5.8$GLUCOSE, base.5.8$PLACEBO, paired = TRUE)

# 5S
change.5 <- rrna5 %>%
  dplyr::select(subject, time, rep, nf.expr, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = nf.expr) %>%
  ungroup() %>%
  # print()
  mutate(change = Post-Pre,
         pre = Pre - mean(Pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(change.5, "./data/data-gen/rna/lchange.5.RDS")

base.5 <- change.5 %>%
  select(subject, supplement, Pre) %>%
  pivot_wider(names_from = supplement,
              values_from = Pre) %>%
  print()

ttest.5 <- t.test(base.5$GLUCOSE, base.5$PLACEBO, paired = TRUE)

# 47S

change.47 <- rrna47 %>%
  dplyr::select(subject, time, rep, nf.expr, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = nf.expr) %>%
  ungroup() %>%
  # print()
  mutate(change = Post-Pre,
         pre = Pre - mean(Pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(change.47, "./data/data-gen/rna/lchange.47.RDS")

base.47 <- change.47 %>%
  select(subject, supplement, Pre) %>%
  pivot_wider(names_from = supplement,
              values_from = Pre) %>%
  print()

ttest.47 <- t.test(base.47$GLUCOSE, base.47$PLACEBO, paired = TRUE)

# Linear mixed effects model
# This model sets an intercept per participant (mixed model), and controls for differences in pre/baseline values. It then tries to explain the changes in
# groups by the supplement provided, for each rRNA separately.

# 18S
m1 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
                     data = change.18)

plot(m1)

summary(m1)

# 28S
m2 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
                     data = change.28)

plot(m2)

summary(m2)

# 5.8S
m3 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
                     data = change.5.8)

plot(m3)

summary(m3)

# 5S
m4 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
                     data = change.5)

plot(m4)

summary(m4)

# 47S
m5 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
                     data = change.47)

plot(m5)

summary(m5)

## Fold-change estimated means
# Gets estimated means from the model, these are average increase at pre = 0 (the average pre value).
# These are log-fold change values (changeble with the mutate function). The emmeans frames are saved for figure illustration in a separate script.

# 18S
emm.18 <- confint(emmeans(m1, specs = ~"supplement")) %>%
  data.frame()

saveRDS(emm.18, "./data/data-gen/rna/emm.18.RDS")

# 28S
emm.28 <- confint(emmeans(m2, specs = ~"supplement")) %>%
  data.frame()

saveRDS(emm.28, "./data/data-gen/rna/emm.28.RDS")

# 5.8S
emm.5.8 <- confint(emmeans(m3, specs = ~"supplement")) %>%
  data.frame()

saveRDS(emm.5.8, "./data/data-gen/rna/emm.5.8.RDS")

# 5S
emm.5 <- confint(emmeans(m4, specs = ~"supplement")) %>%
  data.frame()

saveRDS(emm.5, "./data/data-gen/rna/emm.5.RDS")

# 47S
emm.47 <- confint(emmeans(m5, specs = ~"supplement")) %>%
  data.frame()

saveRDS(emm.47, "./data/data-gen/rna/emm.47.RDS")


