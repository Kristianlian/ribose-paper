#### Western Analysis


## Author: Kristian Lian

# Packages
library(emmeans);library(tidyverse);library(nlme)

# Data

prot.dat <- readRDS("./data/data-gen/protein/prot.datc.RDS")

# Change analysis

prot.dat %>%
  group_by(time, supplement, target) %>%
  #mutate(norm.sign = log(tpl/norm.sign)) %>% # Log transforms and normalises
  # norm.sign by tpl
  summarise(m = mean(norm.sign, na.rm = TRUE)) %>%
  ggplot(aes(time, m, color = supplement, group = supplement)) +
  geom_line() +
  geom_point() +
  facet_wrap( ~ target, scales = "free")

# Change score

pchange <- prot.dat %>%
  dplyr::select(subject, time, outlier, sample.id, target, norm.sign, supplement) %>%
  filter(outlier == "in") %>% # samples marked as out (outlier, with a higher residual than 2) are filtered out
  group_by(subject, time, target, supplement) %>%
  summarise(mns = mean(norm.sign, na.rm = TRUE)) %>%
  pivot_wider(names_from = time,
              values_from = mns) %>%
  ungroup() %>%
  mutate(change = log(post)-log(pre),
         pre = pre - mean(pre, na.rm = TRUE),
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(pchange, "./data/data-gen/protein/prot.change.RDS")

# By target

cmyc <- pchange %>%
  filter(target == "cmyc") %>%
  print()

ubf <- pchange %>%
  filter(target == "ubf",
         subject != "103") %>% # Has missing values
  print()

rps6 <- pchange %>%
  filter(target == "rps6") %>%
  print()

# c-Myc change

m1 <- lme(change ~ pre + supplement,
            random = list(subject = ~ 1),
            data = cmyc) # same model as in "western import", but with change scores of norm.sign

plot(m1)

summary(m1)

cmyc.emm <- emmeans(m1, specs = ~"supplement") %>%
  data.frame

saveRDS(cmyc.emm, "./data/data-gen/protein/cmyc.change.RDS")

# UBF change

m2 <- lme(change ~ pre + supplement,
            random = list(subject = ~ 1),
            data = ubf) # same model as in "western import", but with change (i.e., change in norm.sign) instead of norm.sign

plot(m2)

summary(m2)

ubf.emm <- emmeans(m2, specs = ~"supplement") %>%
  data.frame()

saveRDS(ubf.emm, "./data/data-gen/protein/ubf.change.RDS")

# rps6 change

m3 <- lme(change ~ pre + supplement,
            random = list(subject = ~ 1),
            data = rps6) # same model as in "western import", but with change (i.e., change in norm.sign) instead of norm.sign

plot(m3)

summary(m3)

rps6.emm <- emmeans(m3, specs = ~"supplement") %>%
  data.frame()

saveRDS(rps6.emm, "./data/data-gen/protein/rps6.change.RDS")
