#### Import and normalization of quantified Western Blot data

## Author: Kristian Lian

## Purpose: The purpose of this script is to import the quantified western data, and normalize total protein by subtracting
## background. The script also mean summarises signal data, and gathers and saves normalized total protein and mean signal
##in the data fram "west.dat"

### Packages
library(readxl);library(tidyverse)

### Data import

western.dat <- read_excel("data/protein/western.quant.xlsx")

sample.info <- western.dat %>%
  select(gel, well, sample.id, subject, sample.name, leg)

### Summarising mean total protein and background per gel


mean.tp <- western.dat %>%
  select(gel, well, sample.id, tp1:bg4) %>%
  pivot_longer(names_to = "var", 
               values_to = "mean_gray", 
               cols = tp1:bg4) %>%
  mutate(var = gsub('[0-9]+', '', var)) %>%
  group_by(gel, well, sample.id, var) %>%
  
  ## Below, mean gray is summarised for total protein signal in lanes and the background between lanes
  summarise(mean_gray = mean(mean_gray)) %>%
  pivot_wider(names_from = var, 
              values_from = mean_gray) %>%
  
  ## Lane normalization factor: TPS for each lane (tpl) divided by TPS Signal from lane with highest TP signal
  # tpl/max(tpl) divides total protein signal in each lane by the highest total protein signal on the corresponding
  # gel, creating tpl, which is the loading normalization factor 
  mutate(tpl = tp - bg) %>%
  group_by(gel) %>%
  mutate(tpl = tpl/max(tpl)) %>% 

  dplyr::select(gel, well, sample.id, tpl) %>%
  print()


### Signal normalisation - this code normalises the each samples mean target signal by the pools


dat2 <- western.dat %>%
  dplyr::select(gel, well, sample.id, subject, time, sample.name, leg, 
                cmyc.sig, cmyc2.sig, ubf.sig, ubf2.sig, rps6.sig, rps62.sig) %>%
  pivot_longer(names_to = "target", 
               values_to = "signal", 
               cols = cmyc.sig:rps62.sig) %>%
  
  ## Removes ".sig" and "2.sig" from target signal variables, so they can be summarised
  mutate(target = gsub(".sig", "", target), 
         target = gsub("2", "", target)) %>% 
  inner_join(mean.tp) %>%
  
  
  ## Normalization 
  # We want to normalise per gel and target (group_by()), as there is variation between the gels, 
  # and we dont want to normalise e.g. cmyc by ubf and rps6. The mutate(signal =) scales down the signal, similar to the tpl normalisation.
  # mutate(norm.sign) creates the new variable "norm.sign", with values from down-scaled signal
  group_by(gel, target) %>% 
  mutate(signal = signal / max(signal)) %>% 
  ungroup() %>%
  #print()
  mutate(norm.sign = signal) %>% 
  
  
  ## Groups by and summarises mean signal, then imports the lane normalisation factor (tpl) 
  group_by(gel, sample.id, sample.name, subject, leg, time, target) %>%
  summarise(norm.sign = mean(norm.sign, na.rm = TRUE)) %>% 

  ## Adds pool on all samples per gel, so we can normalise the signal by pool via the mutate(norm.sign = norm.sign / pool).
  # Thereafter, "pool" is filtered out, as we no longer need this in the data frame.
  group_by(gel, target) %>%

  mutate(pool = if_else(subject == "pool", norm.sign, NA_real_), 
         pool = mean(pool, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(norm.sign = norm.sign / pool) %>%
  filter(subject != "pool") %>% 
  
  ## Mark duplicates
  # Each sample was run in duplicates, with gel 1 corresponding to gel 4, gel 2 to 5 and gel 3 to 6. This code creates the variable "gel.sorted",
  # serving as a duplicate marker. With this, we can compare the target signal on different gels, to control for the between-gel variation. 
  group_by(subject) %>%
  mutate(gel.sorted = if_else(gel == max(gel), "B", "A"), 
         time = factor(time, levels = c("pre", "post")), 
         gel.sample = paste0(gel.sorted, sample.id)) %>%
  
  ## Add code key data 
  # Now that we have normalised the signal, we want to import the code_key, i.e. which samples are from glucose and placebo. 
  # As such, the code beneath imports and merges code_key with our data frame.
  mutate(sample.time = gsub("pro1", "", sample.name)) %>%
  inner_join(read_excel("data/code_key.xlsx") %>%
               mutate(sample.time = time, 
                      subject = as.character(subject)) %>%
               dplyr::select(subject, supplement, sample.time)) %>%
  dplyr::select(gel, sample.id, gel.sorted, subject, leg, time, target, supplement,  norm.sign) %>%
  mutate(gel.sample = paste0(gel, sample.id), 
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  print()

saveRDS(dat2, "./data/data-gen/protein/prot.dat.RDS")


### Plotting target signal on duplicate A and B

dat2 %>%
  
  dplyr::select(gel.sorted, sample.id, target, norm.sign) %>%
  pivot_wider(names_from = gel.sorted, values_from = norm.sign) %>%
  ggplot(aes(log(A), log(B))) + geom_point() + facet_wrap(~target)
  

### Modelling target signal per supplement 
# These models try to explain the normalised signal by time, supplement and time+supplement (?). The random = list command creates an individual 
# intercept for the included (e.g., subject, gel.sample). 
  
library(nlme); library(emmeans)


# c-Myc model

m0 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
         random = list(subject = ~ 1),
         data = filter(dat2, target == "cmyc"))

summary(m0)

plot(m0, resid(., type = "p") ~ fitted(.))


# UBF model

m1 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(dat2, target == "ubf"))


summary(m1)

plot(m1, resid(., type = "p") ~ fitted(.))


# RPS6 model

m2 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(dat2, target == "rps6"))

summary(m2)

plot(m2, resid(., type = "p") ~ fitted(.))


### Getting emmeans from the models

# c-Myc emmeans

cmyc.emm <- emmeans(m0, specs = ~ time|supplement) %>%
  data.frame()


cmyc.emm %>%  
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 


saveRDS(cmyc.emm, "./data/data-gen/protein/cmyc.emm.RDS")


# UBF emmeans

ubf.emm <- emmeans(m1, specs = ~ time|supplement) %>%
  data.frame()
  
ubf.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(ubf.emm, "./data/data-gen/protein/ubf.emm.RDS")


# RPS6

rps6.emm <- emmeans(m2, specs = ~ time|supplement) %>%
  data.frame()

rps6.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

saveRDS(rps6.emm, "./data/data-gen/protein/rps6.emm.RDS")
