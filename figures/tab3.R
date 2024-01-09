###### Humac table with absolute values
#
#
## Author: KL
#
#
## Purpose:

## Packages
library(tidyverse); library(flextable)

## Data

# Baseline data
humac.clean2 <- readRDS("./data/data-gen/humac/humac.clean2.RDS")

# Pre-post five sessions data

isometric <- readRDS("./data/data-gen/humac/isom.absolutechange.RDS")
iso60 <- readRDS("./data/data-gen/humac/iso60.absolutechange.RDS")
iso240 <- readRDS("./data/data-gen/humac/iso240.absolutechange.RDS")

# Pre-post session six data (acute data)
isometric.acute <- readRDS("./data/data-gen/humac/isom.absolutechange.ac.RDS")
iso60.acute <- readRDS("./data/data-gen/humac/iso60.absolutechange.ac.RDS")
iso240.acute <- readRDS("./data/data-gen/humac/iso240.absolutechange.ac.RDS")

## Summarising

# Wide format

tabdat <- humac.clean2 |>
  select(subject, supplement, time, acute, measure = test, peak.torque) %>%
#  filter(time == "baseline" & acute == "rest") |>
  group_by(measure, supplement, time) |>
  summarise(mean.pt = mean(peak.torque),
            sd.pt = sd(peak.torque)) |>
  mutate(mean = paste0(sprintf("%.1f",mean.pt), " (", sprintf("%.1f",sd.pt), ")")) |>
  select(supplement, time, measure, mean) |>
  mutate(supplement = if_else(supplement == "glucose",
                       "Glucose",
                       if_else(supplement == "placebo",
                               "Placebo", supplement))) |>
  mutate(measure = if_else(measure == "isom",
                           "0",
                           if_else(measure == "isok.60",
                                   "60",
                                   if_else(measure == "isok.240",
                                           "240", measure)))) |>
  pivot_wider(names_from = time,
              values_from = mean)


## Making the table

humac.flex <- tabdat.wide |>
  flextable() |>
  compose(i = 1, j = c(1, 2, 3, 4, 5, 6, 7, 8, 9), part = "header",
          value = c(as_paragraph("Condition"),
                    as_paragraph("Velocity"),
                    as_paragraph("Baseline"),
                    as_paragraph("After 2RT"),
                    as_paragraph("After 4RT"),
                    as_paragraph("After 5RT"),
                    as_paragraph("After 6RT"),
                    as_paragraph("After 6RT"),
                    as_paragraph("After 6RT"))) |>
  add_body_row(values = c(" ", "Knee-extension peak torque (Nm)"), colwidths = c(2, 7))

humac.flex <- border(humac.flex, 
                     i = NULL,
                     j = NULL,
                     border = NULL,
                     border.top = fp_border_default(color = "black"),
                     border.bottom = fp_border_default(color = "black"),
                     border.left = fp_border_default(color = "black"),
                     border.right = fp_border_default(color = "black"),
                     part = "all") 

humac.flex <- footnote(humac.flex,
                       i = 4,
                       j = 6,
                       as_paragraph("Sig. difference"),
                       ref_symbols = "*",
                       part = "body", inline = TRUE)





############################### OLD CODE BELOW #################################################

# Joining data

#rest.change <- isometric |>
 # full_join(iso60) |>
  #full_join(iso240)

#acute.change <- isometric.acute |>
 # full_join(iso60.acute) |>
  #full_join(iso240.acute)

# Pre-post session five change (rest)

#rest.tabdat <- rest.change |>
#  group_by(supplement, time) |>
#  summarise(mean.c = mean(change),
#            sd.c = sd(change)) |>
#  mutate(mean = paste0(sprintf("%.1f",mean.c), " (", sprintf("%.1f",sd.c), ")")) |>
#  select(supplement, time, mean) |>
#  mutate(supplement = if_else(supplement == "glucose",
#                              "Glucose",
#                              if_else(supplement == "placebo",
#                                      "Placebo", supplement))) #|>
#  mutate(time = if_else(time == "change.2",
#                        "After 2 RT",
#                        if_else(time == "change.3",
#                                "After 4 RT",
#                                if_else(time == "change.4",
#                                        "After 5 RT", time))))



# Pre-post session 6 change (acute)

#acute.tabdat <- acute.change |>
#  group_by(supplement, time) |>
#  summarise(mean.c = mean(change),
#            sd.c = sd(change)) |>
#  mutate(mean = paste0(sprintf("%.1f",mean.c), " (", sprintf("%.1f",sd.c), ")")) |>
#  select(supplement, time, mean) |>
#  mutate(supplement = if_else(supplement == "glucose",
#                              "Glucose",
#                              if_else(supplement == "placebo",
#                                      "Placebo", supplement))) |>
#  mutate(time = if_else(time == "change.2",
#                        "change.5",
#                        if_else(time == "change.3",
#                                "change.6",
#                                if_else(time == "change.4",
#                                        "change.7", time))))
#
## Joining 
#
#humac.tab <- baseline |>
#  full_join(rest.tabdat) |>
#  full_join(acute.tabdat)
#




