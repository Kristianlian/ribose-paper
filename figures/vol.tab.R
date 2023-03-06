######## Training volume table

# Packages
library(tidyverse); library(flextable); library(emmeans)

# Data

vol.clean <- readRDS("./data/data-gen/training/vol.clean.RDS")

vol.change <- readRDS("./data/data-gen/training/vol.change.RDS")
  
# Flextable for absolute values

vol.summarised <- vol.clean  %>%
  group_by(time, supplement) %>%
  summarise(mean.vol = mean(tot.volume, na.rm = TRUE),
            sd.vol = sd(tot.volume, na.rm = TRUE)) %>%
  
  print()


# 
vol.ch <- vol.change %>%
  group_by(time, supplement) %>%
  summarise(mean.change = mean(change, na.rm = TRUE),
            sd.change = sd(change, na.rm = TRUE)) %>%
  ungroup() %>%
  add_row(time = "baseline", supplement = "placebo", mean.change = 0, sd.change = 0, .before = 1) %>%
  add_row(time = "baseline", supplement = "glucose", mean.change = 0, sd.change = 0, .before = 2) %>%
  mutate(time = if_else(time == "baseline",
                         "baseline",
                         if_else(time == "change.2",
                                 "session2",
                                 if_else(time == "change.3",
                                         "session3",
                                         if_else(time == "change.4",
                                                 "session4",
                                                 if_else(time == "change.5",
                                                         "session5",
                                                         if_else(time == "change.6",
                                                                 "session6", time))))))) %>%
  print()


joined.vol <- vol.summarised %>%
  full_join(vol.ch) %>%
  mutate(time = if_else(time == "baseline",
                        "1",
                        if_else(time == "session2",
                                "2",
                                if_else(time == "session3",
                                        "3",
                                        if_else(time == "session4",
                                                "4",
                                                if_else(time == "session5",
                                                        "5",
                                                        if_else(time == "session6",
                                                                "6", time)))))),
         supplement = if_else(supplement == "placebo",
                              "Placebo",
                              if_else(supplement == "glucose",
                                      "Glucose", supplement)))
  print()

# Flextable for change

set_flextable_defaults(
  font.size = 10, theme_fun = theme_booktabs,
  padding = 6,
  background.color = "#EFEFEF")

vol.tab <- flextable(joined.vol)

vol.tab <- colformat_double(vol.tab,
                 big.mark=",", digits = 2, na_str = "N/A")

vol.tab <- width(vol.tab, width = 1.02)

vol.tab <- set_header_labels(vol.tab,
                             time = "Session", supplement = "Supplement", mean.vol = "Avg. volume (kg)",
                             sd.vol = "SD", mean.change = "Avg. change (kg)", sd.change = "SD")


vol.tab

#################################### OLD CODE BELOW ##############################################

# Model

#m <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
#                     data = vol.change)
#plot(m)
#msum <- summary(m)
#
#vol.emm <- confint(emmeans(m, specs = ~"supplement|time")) %>%
#  data.frame()
#
#saveRDS(confint.m2, "./data/data-gen/training/vol.emm.RDS")
#
#vol.emm <- readRDS("./data/data-gen/training/vol.emm.RDS") %>%
#  select(-SE, -df) %>%
#  mutate(Time = if_else(time == "change.2",
#                        "2",
#                        if_else(time == "change.3",
#                                "3",
#                                if_else(time == "change.4",
#                                        "4",
#                                        if_else(time == "change.5",
#                                                "5",
#                                                if_else(time == "change.6",
#                                                        "6", time))))),
#         Supplement = if_else(supplement == "glucose",
#                              "Glucose",
#                              if_else(supplement == "placebo",
#                                      "Placebo", supplement))) %>%
#  select(Supplement, Time, emmean, lower.CL, upper.CL)



          