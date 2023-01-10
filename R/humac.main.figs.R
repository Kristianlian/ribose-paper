#### Humac main data figures

## Author: Kristian Lian/DH
## Project: Ribose

# Purpose: This script creates the figures from the humac.main.analysis, plotting change from pre to post 5 session per supplement.

# Packages
library(tidyverse);library(emmeans)

# Data

emm.isom <- readRDS("./data/data-gen/humac/emm.isom.RDS")

emm.60 <- readRDS("./data/data-gen/humac/emm.60.RDS")

emm.240 <- readRDS("./data/data-gen/humac/emm.240.RDS")

## Figures

pos <- position_dodge(width = 0.2)

# Isometric

isom.fig <- emm.isom %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Isometric \n(nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

saveRDS(isom.fig, "./data/data-gen/humac/isom.main.fig.RDS")

# Isok 60

isok60.fig <- emm.60 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Isokinetic 60 \n(nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

saveRDS(isok60.fig, "./data/data-gen/humac/isok60.main.fig.RDS")


# Isok 240

isok240.fig <- emm.240 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "Time-Point", y = "Isokinetic 240 \n(nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

saveRDS(isok240.fig, "./data/data-gen/humac/isok240.main.fig.RDS")
