##### Training volume table
#
#
## Author: KL
#
#
## Purpose:
# The purpose of this script is to produce table 2, presenting average total 
# session volume and average change per RT session per supplement.  

## Packages
library(tidyverse); library(flextable)

## Data

vol.clean <- readRDS("./data/data-gen/training/vol.clean.RDS") 
# Cleaned up data, from the script "volume.change.R#

vol.change <- readRDS("./data/data-gen/training/vol.change.RDS")
# Total session volum change data, from the script "volume.change.R#
  
## Summarising

# First, summarizing total session volume per time point and supplement
vol.summarised <- vol.clean  %>%
  # Groups the data by time point and supplement, i.e. session 1, 2, 3.. and placebo or glucose
  group_by(time, supplement) %>%
  # Summarises mean and SD of total session volume
  summarise(mean.vol = mean(tot.volume, na.rm = TRUE),
            sd.vol = sd(tot.volume, na.rm = TRUE)) %>%
  mutate(tot.stat = paste0(round(mean.vol, 2), " ± ", round(sd.vol, 2))) %>%
  select(time, supplement, tot.stat) %>%
  print()
  


# Then, summarizing the change in total session volume per time point and supplement
vol.ch <- vol.change %>%
  group_by(time, supplement) %>%
  summarise(mean.change = mean(change, na.rm = TRUE),
            sd.change = sd(change, na.rm = TRUE)) %>%
  ungroup() %>%
  # Adds baseline rows set to 0
  add_row(time = "baseline", supplement = "placebo", mean.change = 0, sd.change = 0, .before = 1) %>%
  add_row(time = "baseline", supplement = "glucose", mean.change = 0, sd.change = 0, .before = 2) %>%
  # Changes the name of time points to correspond with "vol.summarised"
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
  mutate(ch.stat = paste0(round(mean.change, 2), " ± ", round(sd.change, 2))) %>%
  select(time, supplement, ch.stat) %>%
  print()
  
# Joining the two data frames
joined.vol <- vol.summarised %>%
  full_join(vol.ch) %>%
  # Changes the name of time points as I want them in the table
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

  
## Change analysis
# The same model can be found in "volume.change.R"
  
m <- lmerTest::lmer(change ~ 0 + baseline + time + supplement:time + (1|subject),
                      data = vol.change)
plot(m)
msum <- summary(m)  


### Flextable 
# A table of average total session volume and the change per supplement from baseline/session 1 until session 6

## Settings for the table
set_flextable_defaults(
  font.size = 10, theme_fun = theme_booktabs,
  padding = 6,
  background.color = "#EFEFEF")

vol.tab <- flextable(joined.vol)

## Sets decimal marks and digits
vol.tab <- colformat_double(vol.tab,
                 big.mark=",", digits = 2, na_str = "N/A")

## Headers 
# Labelling
vol.tab <- set_header_labels(vol.tab,
                             time = "Session", supplement = "Supplement", 
                             tot.stat = "Mean volume (kg) ± SD", ch.stat = "Mean change (kg) ± SD")
# Bold font
vol.tab <- bold(vol.tab, bold = TRUE, part = "header")
# Aligning numbers and headers
vol.tab <- align_nottext_col(vol.tab, align = "center", header = TRUE, footer = FALSE)

## Body
# Increases width for selected columns (j = ..)
vol.tab <- width(vol.tab, j = 1, width = 0.8)
vol.tab <- width(vol.tab, j = 2, width = 1.1)
vol.tab <- width(vol.tab, j = c(3,4), width = 2)

## Foot
# Footnote to indicate statistics 
vol.tab <- footnote(x = vol.tab,
                    i = 7:12,
                    j = 4,
                    ref_symbols = "#",
                    value = as_paragraph(" = p < 0.05, compared to baseline"))

## Background color
vol.tab <- bg(vol.tab, i = c(2, 4, 6, 8, 10, 12), j = NULL, bg = "gray", part = "body", source = j)


save_as_image(vol.tab, path = "./figures/tab2.png")


          