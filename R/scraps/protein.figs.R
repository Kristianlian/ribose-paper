## Protein figures

# This script produces the figures for cmyc, ubf and rps6

# Author: Kristian Lian

# Packages
library(emmeans)
library(tidyverse)
library(cowplot)

## Data

# cmyc
cmyc.emm <- readRDS("./data/data-gen/protein/cmyc.emm.RDS")

ubf.emm <- readRDS("./data/data-gen/protein/ubf.emm.RDS")

rps6.emm <- readRDS("./data/data-gen/protein/rps6.emm.RDS")

## Figures

# cmyc fig
plot.cmyc <- cmyc.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
 # annotate("text", x = "Post", y = 3.5, label = "p = 0.502", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "Time point", y = "c-Myc", fill = "") +
  theme_classic()

# ubf fig

plot.ubf <- ubf.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  # annotate("text", x = "Post", y = 3.5, label = "p = 0.502", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "Time point", y = "UBF", fill = "") +
  theme_classic()


# rps6

plot.rps6 <- rps6.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  # annotate("text", x = "Post", y = 3.5, label = "p = 0.502", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "Time point", y = "RPS6", fill = "") +
  theme_classic()


# Cowplot for gathering figures

legend <- get_legend(plot.cmyc + theme(legend.box.margin = margin(0, 0, 0,12)))


prot.joined <- plot_grid(plot.cmyc + theme(legend.position = "none"),
                         plot.ubf + theme(legend.position = "none"), 
                         plot.rps6 + theme(legend.position = "none"),
                         legend,
                         ncol = 2, nrow = 2)

saveRDS(prot.joined, "./data/data-gen/protein/prot.joined.RDS")
