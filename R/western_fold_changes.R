



# Packages
library(tidyverse); library(emmeans); library(lme4); library(buildmer); library(writexl)

#
# Data 
# from the script: western_import.R
prot.dat <- readRDS("./data/data-gen/protein/prot.dat.RDS") %>%
  mutate(sample.id = paste0(gel.sorted, sample.id)) %>%
  print()




prot.raw <- prot.dat %>%
  select(-sample.id, -gel.sample) %>%
  pivot_wider(names_from = time, 
              values_from = norm.sign) %>%
  mutate(fc = post / pre) 

write_xlsx(prot.raw, "./data/data-gen/prot.raw.xlsx")

prot.fc <- prot.raw %>%
  group_by(subject, leg, target, supplement) %>%
  summarise(sd.fc = sd(fc), 
          mean.fc = mean(fc))

write_xlsx(prot.fc, "./data/data-gen/prot.fc.xlsx")


myc.fc <- prot.fc %>%
  filter(target == "cmyc")

write_xlsx(myc.fc, "./data/data-gen/myc.fc.xlsx")
  

prot.fc %>%
  ggplot(aes(supplement, mean.fc)) + geom_point() + facet_wrap(~ target)
  
  
  

