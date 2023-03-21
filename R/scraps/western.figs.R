## Western figures

# This script produces the figures for cmyc, ubf and rps6

# Author: Kristian Lian

# Packages
library(emmeans)
library(tidyverse)
library(cowplot)

# Data

cmyc <- readRDS("./data/data-gen/protein/cmyc.emm.RDS")

ubf <- readRDS("./data/data-gen/protein/ubf.emm.RDS")

rps6 <- readRDS("./data/data-gen/protein/rps6.emm.RDS")


## cmyc fig

cmyc.plot <- cmyc %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
 # geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "c-Myc", fill = "") +
  theme_classic()


ubf.plot <- ubf %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  #geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "UBF", fill = "") +
  theme_classic()

rps6.plot <- rps6 %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  #geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "rps6", fill = "") +
  theme_classic()


# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))


prot.joined <- plot_grid(cmyc.plot + theme(legend.position = "none"),
                         ubf.plot + theme(legend.position = "none"), 
                         rps6.plot + theme(legend.position = "none"),
                         legend,
                         ncol = 2, nrow = 2)

saveRDS(prot.joined, "./data/data-gen/rna/prot2.joined.RDS")
