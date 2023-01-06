## rRNA figures

# This script produces the figures for rRNA 18S, 28S, 5.8S, 5S and 47S

# Author: Kristian Lian

# Packages
library(emmeans)
library(tidyverse)
library(cowplot)

## Data

# 18S
emm.18 <- readRDS("./data/data-gen/rna/emm.18.RDS")

# 28S
emm.28 <- readRDS("./data/data-gen/rna/emm.28.RDS")

# 5.8S
emm.5.8 <- readRDS("./data/data-gen/rna/emm.5.8.RDS")

# 5S
emm.5 <- readRDS("./data/data-gen/rna/emm.5.RDS")

# 47S
emm.47 <- readRDS("./data/data-gen/rna/emm.47.RDS")

## Figures
# Plots fold change of estimated marginal means per supplement and saves the figures to be used in the thesis

# 18S
plot.18 <- emm.18 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "18S rRNA \n(fold change)\n", fill = "") +
  theme_classic()

saveRDS(plot.18, "./data/data-gen/rna/plot.18.RDS")

# 28S
plot.28 <- emm.28%>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.740", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "28S rRNA \n(fold change)\n", fill = "") +
  theme_classic()

saveRDS(plot.28, "./data/data-gen/rna/plot.28.RDS")

# 5.8S
plot.5.8 <- emm.5.8 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.935", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "5.8S rRNA \n(fold change)\n", fill = "") +
  theme_classic()

saveRDS(plot.5.8, "./data/data-gen/rna/plot.5.8.RDS")

# 5S
plot.5 <- emm.5 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.9, label = "p = 0.790", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "5S rRNA \n(fold change)\n", fill = "") +
  theme_classic()

saveRDS(plot.5, "./data/data-gen/rna/plot.5.RDS")

# 47S
plot.47 <- emm.47 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 3.5, label = "p = 0.502", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "Time-point", y = "47S pre-rRNA \n(fold change)\n", fill = "") +
  theme_classic()

saveRDS(plot.47, "./data/data-gen/rna/plot.47.RDS")

# Cowplot for gathering figures

legend <- get_legend(plot.47 + theme(legend.box.margin = margin(0, 0, 0,12)))


rrna.joined <- plot_grid(plot.18 + theme(legend.position = "none"),
          plot.28 + theme(legend.position = "none"), 
          plot.5.8 + theme(legend.position = "none"),
          plot.5 + theme(legend.position = "none"), 
          plot.47 + theme(legend.position = "none"),
          legend,
          ncol = 2, nrow = 3)

saveRDS(rrna.joined, "./data/data-gen/rna/rrna.joined.RDS")
