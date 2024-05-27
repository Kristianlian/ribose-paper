



# Packages
library(tidyverse); library(emmeans); library(lme4); library(buildmer)

#
# Data 
# from the script: western_import.R
prot.dat <- readRDS("./data/data-gen/protein/prot.dat.RDS") %>%
  mutate(sample.id = paste0(gel.sorted, sample.id)) %>%
  print()




prot.dat %>%
  select(-sample.id, -gel.sample) %>%
  pivot_wider(names_from = time, 
              values_from = norm.sign) %>%
  mutate(fc = post / pre) %>%
  
  group_by(subject, leg, target, supplement) %>%
  summarise(sd.fc = sd(fc), 
          mean.fc = mean(fc)) %>%
  
  ggplot(aes(supplement, mean.fc)) + geom_point() + facet_wrap(~ target)
  
  
  

