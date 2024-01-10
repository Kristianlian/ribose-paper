###### Baseline tab
#
#
## Author: KL
#
#
## Purpose:


## Packages
library(readxl); library(tidyverse); library(flextable)

## Data

leg.dat <- read_excel("./data/dxa/ribose_dxaprleg.xlsx")

dxa.dat <- read_excel("./data/dxa/ribose_dxa.xlsx")

humac.clean2 <- readRDS("./data/data-gen/humac/humac.clean2.RDS")

str.index <- readRDS("./data/data-gen/humac/str.index.RDS")


code <- read_excel("./data/sup.xlsx")


## DXA

# Counts the "n" of females and males, added in the last step before 
# making a flextable
gendercount <- dxa.dat %>%
  count(sex) %>%
  mutate(sex = if_else(sex == "female",
                       "Female",
                       if_else(sex == "male",
                               "Male", sex))) 

# Summarising and prepping dxa data
dxa.tab <- dxa.dat %>%
  select(subject, sex, age, height, weight, leanmassg, bodyfat) %>%
  mutate(leanmass = leanmassg/1000) %>%
  group_by(sex) %>%
  summarise(mean.a = mean(age),
            sd.a = sd(age),
            mean.h = mean(height),
            sd.h = sd(height),
            mean.w = mean(weight),
            sd.w = sd(weight),
            mean.lm = mean(leanmass),
            sd.lm = sd(leanmass),
            mean.bf = mean(bodyfat),
            sd.bf = sd(bodyfat)) %>%
  mutate(age = paste0(sprintf("%.1f", mean.a), " (", sprintf("%.1f",sd.a), ")"),
         height = paste0(sprintf("%.1f",mean.h), " (",sprintf("%.1f",sd.h), ")"),
         weight = paste0(sprintf("%.1f",mean.w), " (", sprintf("%.1f",sd.w), ")"),
         lm = paste0(sprintf("%.1f",mean.lm), " (", sprintf("%.1f",sd.lm), ")"),
         bf = paste0(sprintf("%.1f",mean.bf), " (", sprintf("%.1f",sd.bf), ")")) %>%
  mutate(sex = if_else(sex == "female",
                       "Female",
                       if_else(sex == "male",
                               "Male", sex))) %>%
  select(sex, age, height, weight, lm, bf) %>%
  pivot_longer(values_to = "mean",
               names_to = "measure",
               cols = c(age:bf)) 


# Gets gender per subject, to mark the subjects with gender in the humac 
# data

gender <- dxa.dat %>%
  select(subject, sex) 

# Summarising and prepping humac data
#humac.tab <- humac.clean2 %>%
#  full_join(gender) %>%
#  select(subject, sex, time, acute, measure = test, peak.torque) %>%
#  filter(time == "baseline" & acute == "rest") %>%
#  group_by(sex, measure) %>%
#  summarise(mean.pt = mean(peak.torque),
#            sd.pt = sd(peak.torque)) %>%
#  mutate(mean = paste0(sprintf("%.1f",mean.pt), " (", sprintf("%.1f",sd.pt), ")")) %>%
#  select(sex, measure, mean) %>%
#  mutate(sex = if_else(sex == "female",
#                       "Female",
#                       if_else(sex == "male",
#                               "Male", sex))) 


# Joining the data frames to be used in the flextable

dxa.flex <- dxa.tab %>%
  #full_join(humac.tab) %>%
  pivot_wider(values_from = mean,
              names_from = measure) %>%
  full_join(gendercount) %>%
  select(sex, n, age, height, weight, lm, bf) %>%
  
  ## Make it to flextable
  flextable() %>%
  ## Change headers
  compose(i = 1, j = c(1, 2, 3, 4, 5, 6, 7), part = "header", 
          value = c(as_paragraph("Sex"),
                    as_paragraph("n"), 
                    as_paragraph("Age (yrs)"), 
                    as_paragraph("Stature (cm)"), 
                    as_paragraph("Body mass (kg)"),
                    as_paragraph("Lean mass (kg)"),
                    as_paragraph("Body Fat (%)")))
                  #  as_paragraph("60\U00BA sec", as_sup("-1")), 
                   # as_paragraph("240\U00BA sec", as_sup("-1")), 
                   # as_paragraph("0\U00BA sec", as_sup("-1")))) %>%
 # add_header_row(values = c(" ", "Knee-extension peak torque"), colwidths = c(7, 3))


dxa.flex <- border(dxa.flex, 
                   i = NULL,
                   j = NULL,
                   border = NULL,
                   border.top = fp_border_default(color = "black"),
                   border.bottom = fp_border_default(color = "black"),
                   border.left = fp_border_default(color = "black"),
                   border.right = fp_border_default(color = "black"),
                   part = "all") 

dxa.flex
