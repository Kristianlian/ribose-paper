##### Western analysis
#
#
## Purpose: The purpose of this script is to analyse the cleaned up western data
#
# Author: KL/DH
#
# Packages
library(tidyverse); library(nlme); library(emmeans)
#
# Data
prot.dat <- readRDS("./data/data-gen/protein/prot.datc.RDS") # from the script: western.cleanup.R


### Modelling target signal per supplement 
# These models try to explain the normalised signal by time, supplement and time+supplement (?). The random = list command creates an individual 
# intercept for the included (e.g., subject, gel.sample). 

## c-Myc model

# Untransformed
cmyc.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
                random = list(subject = ~ 1),
                data = filter(prot.dat, target == "cmyc"))

summary(cmyc.mod)
plot(cmyc.mod, resid(., type = "p") ~ fitted(.))


# Log-transformed
cmyc.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
                 random = list(subject = ~ 1),
                 data = filter(prot.dat, target == "cmyc"))

summary(cmyc.lmod)
plot(cmyc.lmod, resid(., type = "p") ~ fitted(.))




## UBF model
# Untransformed
ubf.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
               random = list(subject = ~ 1),
               data = filter(prot.dat, target == "ubf"))


summary(ubf.mod)
plot(ubf.mod, resid(., type = "p") ~ fitted(.))

# Log-transformed
ubf.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
                random = list(subject = ~ 1),
                data = filter(prot.dat, target == "ubf"))


summary(ubf.lmod)
plot(ubf.lmod, resid(., type = "p") ~ fitted(.))


# RPS6 model
# Untransformed
rps6.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
                 random = list(subject = ~ 1),
                 data = filter(prot.dat, target == "rps6"))

summary(rps6.mod)

plot(rps6.mod, resid(., type = "p") ~ fitted(.))


# Log-transformed
rps6.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(prot.dat, target == "rps6"))

summary(rps6.lmod)

plot(rps6.lmod, resid(., type = "p") ~ fitted(.))


### Getting emmeans from the models

## c-Myc emmeans
# Untransformed
cmyc.emm <- emmeans(cmyc.mod, specs = ~ time|supplement) %>%
  data.frame()


cmyc.emm %>%  
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 


saveRDS(cmyc.emm, "./data/data-gen/protein/cmyc.emm.RDS")

# Log-transformed
cmyc.lemm <- emmeans(cmyc.lmod, specs = ~ time|supplement) %>%
  data.frame()


cmyc.lemm %>%  
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 


saveRDS(cmyc.lemm, "./data/data-gen/protein/cmyc.lemm.RDS")


## UBF emmeans
# Untransformed
ubf.emm <- emmeans(ubf.mod, specs = ~ time|supplement) %>%
  data.frame()

ubf.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(ubf.emm, "./data/data-gen/protein/ubf.emm.RDS")

# Log-transformed
ubf.lemm <- emmeans(ubf.lmod, specs = ~ time|supplement) %>%
  data.frame()

ubf.lemm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(ubf.lemm, "./data/data-gen/protein/ubf.lemm.RDS")


## RPS6
# Untransformed
rps6.emm <- emmeans(rps6.mod, specs = ~ time|supplement) %>%
  data.frame()

rps6.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(rps6.emm, "./data/data-gen/protein/rps6.emm.RDS")

# Log-transformed
rps6.lemm <- emmeans(rps6.lmod, specs = ~ time|supplement) %>%
  data.frame()

rps6.lemm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(rps6.lemm, "./data/data-gen/protein/rps6.lemm.RDS")





