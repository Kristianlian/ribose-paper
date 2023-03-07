### Humac raw data cleaunp
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script cleans up raw dxa data
#
#
# Packages
library(readxl);library(tidyverse)

## Handling the data by creating a new factor called time from timepoint. This factor combines any observation at T1 and T2 to baseline, etc. 
# The code also sorts the order of the factor time, from baseline to session 6, using time = factor(time, levels c()), and sets placebo to be compared to 
# glucose via supplement = factor(supplement, levels = c()). Acute code is called to set a new factor named acute, so that its possible to divid post 5th
# session data from post 6th session data

humac <- read_excel("./data/tests/ribose.humac.xlsx", na = "NA")
  
  
humac.clean <- humac %>%  
mutate(time = if_else(timepoint == "D-1", 
                        "baseline", 
                        if_else(timepoint %in% c("D4", "D5"), 
                                "test1", 
                                if_else(timepoint %in% c("D8", "D9"), 
                                        "test2", 
                                        if_else(timepoint %in% c("T3", "T4") & acute %in% c("rest", "post30min", "post2h"),
                                                "test3",
                                                if_else(acute == "post23h", "test4", timepoint)))))) %>%
  mutate(time = factor(time, levels = c("baseline", "test1", "test2", "test3", "test4")), 
         acute = factor(acute, levels = c("rest", "post30min", "post2h", "post23h")), 
         supplement = factor(supplement, levels = c("placebo", "glucose"))) #%>%
 # print()

saveRDS(humac.clean, "./data/data-gen/humac/humac.clean.RDS")

rest.dat <- humac.clean %>%
  filter(acute == "rest" ) #%>%
#  print()

saveRDS(rest.dat, "./data/data-gen/humac/rest.dat.RDS")





