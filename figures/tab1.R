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

humac.clean2 <- readRDS("./data/data-gen/humac/humac.clean2.RDS")

str.index <- readRDS("./data/data-gen/humac/str.index.RDS")


code <- read_excel("./data/sup.xlsx")


## DXA

dxa.tab <- leg.dat %>%
  full_join(code) %>%
  select(subject, sex, supplement, leanmassg, fatmassg, totalmasskg) %>%
  # Convert g to kg
  mutate(leanmass = leanmassg/1000,
         fatmass = fatmassg/1000) %>%
  select(-leanmassg, fatmassg) %>%
  group_by(supplement) %>%
  summarise(mean.lm = mean(leanmass, na.rm = TRUE),
            mean.fm = mean(fatmass, na.rm = TRUE),
            mean.tm = mean(totalmasskg, na.rm = TRUE),
            sd.lm = sd(leanmass, na.rm = TRUE),
            sd.fm = sd(fatmass, na.rm = TRUE),
            sd.tm = sd(totalmasskg, na.rm = TRUE)) %>%
  mutate(lm.stat = paste0(round(mean.lm, 2), " ± ", round(sd.lm, 2)),
         fm.stat = paste0(round(mean.fm, 2), " ± ", round(sd.fm, 2)),
         tm.stat = paste0(round(mean.tm, 2), " ± ", round(sd.tm, 2))) %>%
  mutate(supplement = if_else(supplement == "glucose",
                              "Glucose",
                              if_else(supplement == "placebo",
                                      "Placebo", supplement))) %>%
  select(supplement, lm.stat) 



## Strength
index.tab <- str.index %>%
  filter(time == "baseline") %>%
  group_by(supplement) %>%
  summarise(mean.str = mean(strength, na.rm = TRUE),
            sd.str = sd(strength, na.rm = TRUE)) %>%
  mutate(str.stat = paste0(round(mean.str, 2), " ± ", round(sd.str, 2))) %>%
  mutate(supplement = if_else(supplement == "glucose",
                              "Glucose",
                              if_else(supplement == "placebo",
                                      "Placebo", supplement))) %>%
  select(supplement, str.stat) 


humac.tab <- humac.clean2 %>%
  filter(time %in% c("rest", "baseline")) %>%
  group_by(supplement) %>%
  pivot_wider(values_from = "peak.torque",
              names_from = "test") %>%
  summarise(mean.isom = mean(isom, na.rm = TRUE),
            mean.60 = mean(isok.60, na.rm = TRUE),
            mean.240 = mean(isok.240, na.rm = TRUE),
            sd.isom = sd(isom, na.rm = TRUE),
            sd.60 = sd(isok.60, na.rm = TRUE),
            sd.240 = sd(isok.240, na.rm = TRUE)) %>%
  mutate(isom.stat = paste0(round(mean.isom, 2), " ± ", round(sd.isom, 2)),
         isok60.stat = paste0(round(mean.60, 2), " ± ", round(sd.60, 2)),
         isok240.stat = paste0(round(mean.240, 2), " ± ", round(sd.240, 2))) %>%
  mutate(supplement = if_else(supplement == "glucose",
                              "Glucose",
                              if_else(supplement == "placebo",
                                      "Placebo", supplement))) %>%
  select(supplement, isom.stat, isok60.stat, isok240.stat)



desc.dat <- dxa.tab %>%
  full_join(index.tab) %>%
  full_join(humac.tab) 


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
                              supplement = "Supplement", lm.stat = "LM (kg) ± SD", str.stat = "Strength ± SD",
                              isom.stat = "Iso 0 PT ± SD", isok60.stat = "Iso 60 PT ± SD", isok240.stat = "Iso 240 PT ± SD")

# Bold font
desc.tab <- bold(desc.tab, bold = TRUE, part = "header")
# Aligning numbers and headers
desc.tab <- align_nottext_col(desc.tab, align = "center", header = TRUE, footer = FALSE)

## Body
# Increases width for selected columns (j = ..) or the whole table
desc.tab <- width(desc.tab, width = 1.25)

## Background color
desc.tab <- bg(desc.tab, i = 2, j = NULL, bg = "gray", part = "body", source = j)


save_as_image(desc.tab, path = "./figures/tab1.png")


