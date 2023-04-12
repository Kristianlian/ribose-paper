### Strength index analysis
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script creates and analyses strength index combined of isometric and 
# isokinetic peak torque, to be used in fig 1 
#
#
## Packages

library(readxl);library(tidyverse)

## Handling the data by creating a new factor called time from timepoint. This factor combines any observation at T1 and T2 to baseline, etc. 
# The code also sorts the order of the factor time, from baseline to session 6, using time = factor(time, levels c()), and sets placebo to be compared to 
# glucose via supplement = factor(supplement, levels = c()). Acute code is called to set a new factor named acute, so that its possible to divid post 5th
# session data from post 6th session data

humac <- read_excel("./data/tests/ribose.humac.xlsx", na = "NA")


humac.clean2 <- humac %>%
  select(-date) %>%
  mutate(time = if_else(timepoint == "D-1",
                        "baseline",
                        if_else(timepoint %in% c("D4", "D5"),
                                "test2",
                                if_else(timepoint %in% c("D8", "D9"),
                                        "test3",
                                        if_else(timepoint %in% c("T3", "T4") & acute == "rest",
                                                "test4",
                                                if_else(timepoint %in% c("T3", "T4") & acute == "post30min",
                                                        "test5",
                                                        if_else(timepoint %in% c("T3", "T4") & acute == "post2h",
                                                                "test6",
                                                                if_else(timepoint %in% c("T4", "D13") & acute == "post23h", 
                                                                        "test7", timepoint)))))))) %>%
  mutate(time = factor(time, levels = c("baseline", "test2", "test3", "test4", "test5", "test6", "test7")),
         acute = factor(acute, levels = c("rest", "post30min", "post2h", "post23h")),
         supplement = factor(supplement, levels = c("glucose", "placebo"))) 

saveRDS(humac.clean2, "./data/data-gen/humac/humac.clean2.RDS")

strength.index <- humac.clean2 %>%
  group_by(time, supplement) %>%
  pivot_wider(names_from = test,
              values_from = peak.torque) %>%
  mutate(isom.i = isom / max(isom),
         isok60.i = isok.60 / max(isok.60),
         isok240.i = isok.240 / max(isok.240)) %>%
  ungroup() %>%
  group_by(subject, time, supplement) %>%
  summarise(isom.mi = mean(isom.i),
            isok60.mi = mean(isok60.i),
            isok240.mi = mean(isok240.i)) %>%
  pivot_longer(names_to = "test",
               values_to = "strength.i",
               cols = c(isom.mi:isok240.mi)) %>%
  ungroup() %>%
  group_by(subject, time, supplement) %>%
  summarise(strength = mean(strength.i))

saveRDS(strength.index, "./data/data-gen/humac/str.index.RDS")

acute.index <- strength.index %>%
  filter(time %in% c("test4", "test5", "test6", "test7")) 

### Change analysis

# Full data set
str.change <- strength.index %>%
  pivot_wider(names_from = time,
              values_from = strength) %>%
  mutate(change.2 = test2 - baseline,
         change.3 = test3 - baseline,
         change.4 = test4 - baseline,
         change.5 = test5 - baseline,
         change.6 = test6 - baseline,
         change.7 = test7 - baseline,
         baseline = baseline - mean(baseline, na.rm = TRUE)) %>%
  select(subject, supplement, baseline, change.2:change.7) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(change.2:change.7)) %>%
  print()

saveRDS(str.change, "./data/data-gen/humac/str.change.RDS")

str.lchange <- strength.index %>%
  pivot_wider(names_from = time,
              values_from = strength) %>%
  mutate(change.2 = log(test2) - log(baseline),
         change.3 = log(test3) - log(baseline),
         change.4 = log(test4) - log(baseline),
         change.5 = log(test5) - log(baseline),
         change.6 = log(test6) - log(baseline),
         change.7 = log(test7) - log(baseline),
         baseline = baseline - mean(baseline, na.rm = TRUE)) %>%
  select(subject, supplement, baseline, change.2:change.7) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(change.2:change.7)) %>%
  print()

saveRDS(str.lchange, "./data/data-gen/humac/str.lchange.RDS")

## Acute change: Post session 5 -> 23hrs post session 6

astr.change <- acute.index %>%
  pivot_wider(names_from = time,
              values_from = strength) %>%
  mutate(change.2 = test5 - test4,
         change.3 = test6 - test4,
         change.4 = test7 - test4,
         baseline = test4 - mean(test4, na.rm = TRUE)) %>%
  select(subject, supplement, baseline, change.2:change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(change.2:change.4)) %>%
  print()

saveRDS(astr.change, "./data/data-gen/humac/astr.change.RDS")


astr.lchange <- acute.index %>%
  pivot_wider(names_from = time,
              values_from = strength) %>%
  mutate(change.2 = log(test5) - log(test4),
         change.3 = log(test6) - log(test4),
         change.4 = log(test7) - log(test4),
         baseline = test4 - mean(test4, na.rm = TRUE)) %>%
  select(subject, supplement, baseline, change.2:change.4) %>%
  pivot_longer(names_to = "time",
               values_to = "change",
               cols = c(change.2:change.4)) %>%
  print()

saveRDS(astr.lchange, "./data/data-gen/humac/astr.lchange.RDS")

### Change model full data

m.str <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                         data = str.change)

saveRDS(m.str, "./data/data-gen/humac/strength.model.RDS")

m.lstr <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                        data = str.lchange)
plot(m.str)
plot(m.lstr)

summary(m.str)
summary(m.lstr)

emm.str <- confint(emmeans(m.str, specs = ~"supplement|time")) %>%
  data.frame() 

lemm.str <- confint(emmeans(m.lstr, specs = ~"supplement|time")) %>%
  data.frame() 

saveRDS(emm.str, "./data/data-gen/humac/emm.str.RDS")




### Change model acute

m.astr <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                        data = astr.change)

m.alstr <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                         data = astr.lchange)
plot(m.astr)
plot(m.alstr)

summary(m.astr)
summary(m.alstr)

emm.astr <- confint(emmeans(m.astr, specs = ~"supplement|time")) %>%
  data.frame() 

saveRDS(emm.astr, "./data/data-gen/humac/emm.astr.RDS")

lemm.astr <- confint(emmeans(m.alstr, specs = ~"supplement|time")) %>%
  data.frame() 

saveRDS(lemm.astr, "./data/data-gen/humac/lemm.astr.RDS")

  
  
  
  
  
    
