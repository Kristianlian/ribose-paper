#### Humac data 
# Data handling


## Author: Kristian Lian

# Purpose: This script tidies up all humac data for later analyses, and saves one data frame for pre-post 5 session results, and one data 
# fram for pre-post the 6th session

## Time-points
# D-1: Baseline, before any supplementation or training
# D4, D5, D8 and D9: Day 4, 5, 8 and 9 of the intervention, humac testing of the leg that performed
# RT the preceding day
# T3: Post testing leg #1 (leg that started the intervention). Leg #1 is tested four times at T3/T4:
# Test 1 leg 1: 1.5hrs after protein ingestion, 45min before RT (T3)
# Test 2 leg 1: 30min after RT (T3)
# Test 3 leg 1: 2hrs after RT (T3)
# Test 4 leg 1: ~23hrs after RT (T4)
# Test 1 serve as a post test for the 5 RT sessions and pre test before the sixth session, test 2,
# 3, and 4 serve as post test following sixth session
# T4 and 13 follow the same design for leg #2

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





