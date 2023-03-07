#### C-peptide change figure

## Author: Kristian Lian
## Project: Ribose

## This script creates the figure "cpep.fig", illustrating mean change in c-peptide measurements   


# Packages
library(tidyverse);library(emmeans)

# Data
cpep.change <- readRDS("./data/data-gen/glucose/insulin.figchange.RDS")
cpep.lchange <- readRDS("./data/data-gen/glucose/insulin.change.RDS")

## Figure
# Plots fold change of estimated marginal means per supplement and saves the figures to be used in the thesis

pos <- position_dodge(width = 0.2)

# Fig without log-transformation

cpep.fig <- cpep.change %>%
  data.frame() %>%
  add_row(supplement = "PLACEBO", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "GLUCOSE", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  #mutate(time = factor(time, levels = c("change.45", "change.90", "change.120", "change.135", "change.150", "change.270"))) %>%
  #mutate(time = as.numeric(time)) %>%
  ggplot(aes(time.c, emmean, group = supplement, fill = supplement)) +
  annotate("text", x = c(120, 150), y = c(1000, 1000), label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  labs(x = "Time-point", y = "C-peptide levels \n(pmol/L change)\n", fill = "") +
  theme_classic() +
  # theme(plot.background = element_rect(fill = "gray80")) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 90, 120, 150, 270),
                     expand = expansion(0), labels = c("change.1" = "-120", "change.90" = "-30", "change.120" = "0", 
                                                       "change.150" = "30", "change.270" = "120")) +
  #scale_y_continuous(limits = c(0,1000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


saveRDS(cpep.fig, "./data/data-gen/glucose/cpep.fig.RDS")


# Log-transformed and reverse transformed

cpep.fig <- cpep.lchange %>%
  data.frame() %>%
  add_row(supplement = "PLACEBO", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "GLUCOSE", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  #mutate(time = factor(time, levels = c("change.45", "change.90", "change.120", "change.135", "change.150", "change.270"))) %>%
  #mutate(time = as.numeric(time)) %>%
  ggplot(aes(time.c, exp(emmean), group = supplement, fill = supplement)) +
  #annotate("text", x = c(120, 135, 150), y = c(2.5, 2.1, 2.1), label = "*") +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                size = 0.5,
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  #scale_x_discrete(labels=c("change.45" = "45", "change.90" = "90",
  #                         "change.120" = "120", "change.135" = "135", 
  #                        "change.150" = "150", "change.270" = "270")) +
  labs(x = "Time-point", y = "C-peptide levels \n(pmol/L change)\n", fill = "") +
  theme_classic() +
  # theme(plot.background = element_rect(fill = "gray80")) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 45, 90, 120, 270),
                     expand = expansion(0), labels = c("0" = "-120", "45" = "-90", "90" = "-30", "120" = "0", "270" = "120")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






