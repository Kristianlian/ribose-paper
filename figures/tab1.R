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
  select(subject, sex, age, height, weight, leanmassg) %>%
  mutate(leanmass = leanmassg/1000) %>%
  group_by(sex) %>%
  summarise(mean.a = mean(age),
            sd.a = sd(age),
            mean.h = mean(height),
            sd.h = sd(height),
            mean.w = mean(weight),
            sd.w = sd(weight),
            mean.lm = mean(leanmass),
            sd.lm = sd(leanmass)) %>%
  mutate(age = paste0(round(mean.a, 2), " (", round(sd.a, 2), ")"),
         height = paste0(round(mean.h, 2), " (", round(sd.h, 2), ")"),
         weight = paste0(round(mean.w, 2), " (", round(sd.w, 2), ")"),
         lm = paste0(round(mean.lm, 2), " (", round(sd.lm, 2), ")")) %>%
  mutate(sex = if_else(sex == "female",
                       "Female",
                       if_else(sex == "male",
                               "Male", sex))) %>%
  select(sex, age, height, weight, lm) %>%
  pivot_longer(values_to = "mean",
               names_to = "measure",
               cols = c(age:lm)) 


# Gets gender per subject, to mark the subjects with gender in the humac 
# data

gender <- dxa.dat %>%
  select(subject, sex) %>%
  print()

# Summarising and prepping humac data
humac.tab <- humac.clean2 %>%
  full_join(gender) %>%
  select(subject, sex, time, acute, measure = test, peak.torque) %>%
  filter(time == "baseline" & acute == "rest") %>%
  group_by(sex, measure) %>%
  summarise(mean.pt = mean(peak.torque),
            sd.pt = sd(peak.torque)) %>%
  mutate(mean = paste0(round(mean.pt, 2), " (", round(sd.pt, 2), ")")) %>%
  select(sex, measure, mean) %>%
  mutate(sex = if_else(sex == "female",
                       "Female",
                       if_else(sex == "male",
                               "Male", sex))) 
 

# Joining the data frames to be used in the flextable
desc.dat <- dxa.tab %>%
  full_join(humac.tab) %>%
  pivot_wider(values_from = mean,
              names_from = measure) %>%
  full_join(gendercount) %>%
  select(sex, n, age, height, weight, lm, isok.60, isok.240, isom) %>%
  print()


### Flextable 
# A table of average total session volume and the change per supplement from baseline/session 1 until session 6

## Settings for the table
set_flextable_defaults(
  font.size = 10, theme_fun = theme_booktabs,
  padding = 6,
  background.color = "#EFEFEF")

desc.tab <- flextable(desc.dat)

## Sets decimal marks and digits
desc.tab <- colformat_double(desc.tab,
                            big.mark=",", digits = 2, na_str = "N/A")

## Headers 
# Labelling
desc.tab <- set_header_labels(desc.tab,
                              sex = "Sex", n = "n", age = "Age (SD)", height = "Height (SD)",
                              weight = "Weight (SD)", lm = "Leanmass (SD)", 
                              isok.60 = "Iso 60 (SD)", isok.240 = "Iso 240 (SD)", isom = "Isom (SD)")

# Bold font
desc.tab <- bold(desc.tab, bold = TRUE, part = "header")
# Aligning numbers and headers
desc.tab <- align_nottext_col(desc.tab, align = "center", header = TRUE, footer = FALSE)

## Body
# Increases width for selected columns (j = ..) or the whole table
desc.tab <- width(desc.tab, width = 1.25)
desc.tab <- width(desc.tab, j = 2, width = 0.75)
desc.tab <- width(desc.tab, j = 1, width = 0.65)




save_as_image(desc.tab, path = "./figures/tab1.png")


