######## Training volume table

# Packages
library(tidyverse); library(flextable)

# Data

vol.abs <- readRDS("./data/data-gen/training/volume.absolute.RDS")

vol.change <- readRDS("./data/data-gen/training/vol.change.RDS")

# Model

m <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                     data = vol.change)
plot(m)
msum <- summary(m)

vol.emm <- readRDS("./data/data-gen/training/vol.emm.RDS") %>%
  select(-SE, -df) %>%
  mutate(Time = if_else(time == "change.2",
                        "2",
                        if_else(time == "change.3",
                                "3",
                                if_else(time == "change.4",
                                        "4",
                                        if_else(time == "change.5",
                                                "5",
                                                if_else(time == "change.6",
                                                        "6", time))))),
         Supplement = if_else(supplement == "glucose",
                              "Glucose",
                              if_else(supplement == "placebo",
                                      "Placebo", supplement))) %>%
  select(Supplement, Time, emmean, lower.CL, upper.CL)


# Flextable

set_flextable_defaults(
  font.size = 10, theme_fun = theme_booktabs,
  padding = 6,
  background.color = "#EFEFEF")

vol.tab <- flextable(vol.emm)


vol.tab <- width(vol.tab, width = 1.1)

vol.tab <- colformat_double(vol.tab,
                 big.mark=",", digits = 2, na_str = "N/A")


vol.tab <- set_header_labels(vol.tab,
                             Supplement = "Supplement", Time = "Session", emmean = "Total volume change (kg)",
                             lower.CL = "Lower", upper.CL = "Upper")


vol.tab








          