#### Nutrition change figs

# Author: KL
# Project: Ribose

# Purpose: This script plots figures from protein, fat, carbs, calories and propr kg emmeans

# Packages
library(tidyverse); library(emmeans); library(cowplot)

# Data

pro <- readRDS("./data/data-gen/nutrition/prot.emm.RDS")
fat <- readRDS("./data/data-gen/nutrition/fat.emm.RDS")
cho <- readRDS("./data/data-gen/nutrition/cho.emm.RDS")
cal <- readRDS("./data/data-gen/nutrition/cal.emm.RDS")
propr <- readRDS("./data/data-gen/nutrition/propr.emm.RDS")

## Figures

pos <- position_dodge(width = 0.2)

# Protein

pro.fig <- pro %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.d1" = "Day 1", "change.d2" = "Day 2", "change.d3" = "Day 3",
                            "change.d4" = "Day 4", "change.d5" = "Day 5", 
                            "change.d6" = "Day 6")) +
  labs(x = "", y = "Protein (g)", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Fat
fat.fig <- fat %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.d1" = "Day 1", "change.d2" = "Day 2", "change.d3" = "Day 3",
                            "change.d4" = "Day 4", "change.d5" = "Day 5", "change.d6" = "Day 6")) +
  labs(x = "", y = "Fat (g)", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Carbs
cho.fig <- cho %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.d1" = "Day 1", "change.d2" = "Day 2", "change.d3" = "Day 3",
                            "change.d4" = "Day 4", "change.d5" = "Day 5", "change.d6" = "Day 6")) +
  labs(x = "", y = "Carbohydrates (g)", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Calories
cal.fig <- cal %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.d1" = "Day 1", "change.d2" = "Day 2", "change.d3" = "Day 3",
                            "change.d4" = "Day 4", "change.d5" = "Day 5", "change.d6" = "Day 6")) +
  labs(x = "", y = "Calories", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Protein pr kg
propr.fig <- propr %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.d1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.d1" = "Day 1", "change.d2" = "Day 2", "change.d3" = "Day 3",
                            "change.d4" = "Day 4", "change.d5" = "Day 5", "change.d6" = "Day 6")) +
  labs(x = "", y = "Protein (g) \nper kg body weight\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Cowplot for gathering figures

legend <- get_legend(pro.fig + theme(legend.box.margin = margin(0, 0, 0,12)))

nut.fig <- plot_grid(pro.fig + theme(legend.position = "none"),
                     propr.fig + theme(legend.position = "none"),
                     cho.fig + theme(legend.position = "none"),
                     fat.fig + theme(legend.position = "none"),
                     cal.fig + theme(legend.position = "none"), 
                     legend, ncol = 2, nrow = 3)

saveRDS(nut.fig, "./data/data-gen/nutrition/nut.fig.RDS")
