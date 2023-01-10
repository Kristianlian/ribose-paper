#### Session score analysis

# This script analyzes log-fold change in RPE through the intervention per supplement. a linear mixed effects models is used together with emmeans to 
# analyze the interaction between time- and/or supplement on changes in training intensity. RPE was measured through a 1-10
# point scale, 15min after each session (session score)

## Packages

library(tidyverse);library(lme4);library(broom);library(emmeans)

## Data

ss.dat <- readRDS("./data/data-gen/training/ss.dat.RDS")

## Change-data
# The code beneath summarizes the mean values at each time, grouped by subject, time and supplement, creating a wider data set with observations of 
# participants glucose measurements per time point.
# Then, mutate() is used to calculate change scores, where each timepoint is log-transformed and compared to baseline. baseline = baseline - mean(baseline,
# na.rm = TRUE) mean centers the baseline values. Subject, supplement, baseline and change scores are then selected and pivoted for modeling.

change_dat.ss <- ss.dat %>%
  dplyr::select(subject, time, session.score, supplement) %>%
  group_by(subject, time, supplement) %>%
  summarise(session.score = mean(session.score, na.rm = TRUE)) %>%
  pivot_wider(names_from = time, 
              values_from = session.score) %>%
  #print()
  
  ungroup() %>%
  mutate(change.2 = log(session2)-log(baseline),
         change.3 = log(session3)-log(baseline),
         change.4 = log(session4)-log(baseline),
         change.5 = log(session5)-log(baseline),
         change.post = log(post)-log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) %>%
  select(subject, supplement, baseline, change.2, change.3, change.4, change.5, change.post) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = (change.2:change.post)) #%>%
  #print()

## Linear mixed effects model
# This model tries to explain the change by time and supplement, accounting for potential differences in baseline values and that the same participants
# are measured at multiple time points. 
# It produces results on both the time effect and the difference between the groups at any timepoint. We are interested in the difference between groups.

m1 <- lmerTest::lmer(change ~ baseline + time + supplement:time + (1|subject),
                     data = change_dat.ss)
plot(m1)

summary(m1)


## Fold-change estimated means
# Gets estimated means from the model, these are average increase at pre = 0 (the average pre value).
# These are log-fold change values (changeble with the mutate function)

confint.m1 <- confint(emmeans(m1, specs = ~"supplement|time")) %>%
  data.frame() #%>%
  #print()

saveRDS(confint.m1, "./data/data-gen/training/emm.ss.RDS")




