#### Import and normalization of quantified Western Blot data

## Author: Kristian Lian

## Purpose: The purpose of this script is to import the quantified western data, and normalize total protein by subtracting
## background. The script also mean summarises signal data, and gathers and saves normalized total protein and mean signal
##in the data fram "west.dat"

# Packages
library(readxl);library(tidyverse)

# Data import
western.dat <- read_excel("data/protein/western.quant.xlsx")

sample.info <- western.dat %>%
  select(gel, well, sample.id, subject, sample.name, leg) %>%
  print()

# Summarising mean total protein and background per gel

# The "mean.tp" and "mean.bg" codes calculates the mean and SD of total protein and background in each well, on each gel.
# This is needed to normalize total protein with the "total.protein" code.

mean.tp <- western.dat %>%
  select(gel, sample.id, tp1:bg4) %>%
  pivot_longer(names_to = "var", 
               values_to = "mean_gray", 
               cols = tp1:bg4) %>%
  mutate(var = gsub('[0-9]+', '', var)) %>%
  group_by(gel, sample.id, var) %>%
  summarise(mean_gray = mean(mean_gray)) %>%
  pivot_wider(names_from = var, 
              values_from = mean_gray) %>%
  mutate(tp = tp - bg) %>%
  dplyr::select(gel, sample.id, tp) %>%
  print()
  
  


## Mean signal
#


dat <- western.dat %>%
  dplyr::select(gel, sample.id, subject, time, sample.name, leg, 
                cmyc.sig, cmyc2.sig, ubf.sig, ubf2.sig, rps6.sig, rps62.sig) %>%

  pivot_longer(names_to = "target", 
               values_to = "signal", 
               cols = cmyc.sig:rps62.sig) %>%
  mutate(target = gsub(".sig", "", target), 
         target = gsub("2", "", target)) %>%
  
  group_by(gel, sample.id, subject, leg, time, target) %>%
  summarise(signal = mean(signal, na.rm = TRUE)) %>%

  inner_join(mean.tp) %>%
  
  ### Normalization 
  
  group_by(gel, target) %>%
  
  mutate(signal = signal / max(signal, na.rm = TRUE), 
         tp = tp / max(tp), 
         norm.sign = signal) %>%
  # Adds pool on all samples per gel
  group_by(gel) %>%
         mutate(pool = if_else(subject == "pool", norm.sign, NA_real_), 
                pool = mean(pool, na.rm = TRUE)) %>%
  ungroup() %>%
 
  # Samples normalized per pool
  mutate(norm.sign = norm.sign / pool) %>% 

  filter(subject != "pool") %>%
  
  group_by(subject) %>%
  mutate(gel.sorted = if_else(gel == max(gel), "B", "A"), 
         time = factor(time, levels = c("pre", "post"))) %>%
  
  print()
  
  

########################## Working code above ###################################

##### Normalization work: Total protein

mean.tp <- western.dat %>%
  select(gel, sample.id, tp1:bg4) %>%
  pivot_longer(names_to = "var", 
               values_to = "mean_gray", 
               cols = tp1:bg4) %>%
  mutate(var = gsub('[0-9]+', '', var)) %>%
  group_by(gel, sample.id, var) %>%
  summarise(mean_gray = mean(mean_gray)) %>%
  pivot_wider(names_from = var, 
              values_from = mean_gray) %>%
  ## Lane normalization factor: TPS for each lane (tpl) divided by TPS Signal from lane with highest TPS signal
  mutate(tpl = tp - bg) %>%
  group_by(gel) %>%
  mutate(tpl = tpl/max(tpl)) %>%

  dplyr::select(gel, sample.id, tpl) %>%
  print()


## Mean signal
#


dat2 <- western.dat %>%
  dplyr::select(gel, sample.id, subject, time, sample.name, leg, 
                cmyc.sig, cmyc2.sig, ubf.sig, ubf2.sig, rps6.sig, rps62.sig) %>%
  
  pivot_longer(names_to = "target", 
               values_to = "signal", 
               cols = cmyc.sig:rps62.sig) %>%
  mutate(target = gsub(".sig", "", target), 
         target = gsub("2", "", target)) %>%
  
  group_by(gel, sample.id, sample.name, subject, leg, time, target) %>%
  summarise(signal = mean(signal, na.rm = TRUE)) %>%
  
  inner_join(mean.tp) %>%
  
  ### Normalization 
  
  group_by(gel, target) %>%
  
  mutate(signal = signal / max(signal)) %>%
  
  ungroup() %>%

  mutate(norm.sign = signal) %>%
  
  # Adds pool on all samples per gel
  group_by(gel, target) %>%
  mutate(pool = if_else(subject == "pool", norm.sign, NA_real_), 
         pool = mean(pool, na.rm = TRUE)) %>%
  ungroup() %>%
  #print()
  
  # Samples normalized per pool
  mutate(norm.sign = norm.sign / pool) %>% 
  
  filter(subject != "pool") %>%
  
  group_by(subject) %>%
  mutate(gel.sorted = if_else(gel == max(gel), "B", "A"), 
         time = factor(time, levels = c("pre", "post")), 
         gel.sample = paste0(gel.sorted, sample.id)) %>%
  
  ## Add code key data 
  mutate(sample.time = gsub("pro1", "", sample.name)) %>%

  inner_join(read_excel("data/code_key.xlsx") %>%
               mutate(sample.time = time, 
                      subject = as.character(subject)) %>%
               dplyr::select(subject, supplement, sample.time)) %>%
  dplyr::select(gel, sample.id, gel.sorted, subject, leg, time, target, supplement, tpl, norm.sign) %>%
  mutate(gel.sample = paste0(gel, sample.id), 
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%

 
  print()




dat2 %>%
  
  dplyr::select(gel.sorted, sample.id, target, norm.sign) %>%
  pivot_wider(names_from = gel.sorted, values_from = norm.sign) %>%
  ggplot(aes(log(A), log(B))) + geom_point() + facet_wrap(~target)
  
  
library(nlme); library(emmeans)



m0 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
         random = list(subject = ~ 1),
 
       
         data = filter(dat2, target == "cmyc"))

m1 <- lme(log(norm.sign) ~ tpl + target + target:time + target:supplement + target:time:supplement, 
         random = list(subject = ~ 1, 
                       gel.sample = ~ 1),
         weights = varExp(form = ~ tpl|target),
         method = "ML",
         data = dat2)


anova(m0, m1)



summary(m)

plot(m0, resid(., type = "p") ~ fitted(.))



emmeans(m0, specs = ~ time|supplement) %>%
  data.frame() %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 



