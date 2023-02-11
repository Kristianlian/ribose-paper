### Ribose figure 3 ######
#
# This script produces the figures for cmyc, ubf and rps6
#
#
# Packages
library(emmeans)
library(tidyverse)
library(cowplot)
#
# Data

cmyc <- readRDS("./data/data-gen/protein/cmyc.change.RDS")

ubf <- readRDS("./data/data-gen/protein/ubf.change.RDS")

rps6 <- readRDS("./data/data-gen/protein/rps6.change.RDS")


## cmyc fig

cmyc.plot <- cmyc %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "c-Myc signal \n(fold change)\n", fill = "") +
  theme_classic()


ubf.plot <- ubf %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "UBF signal \n(fold change)\n", fill = "") +
  theme_classic()

rps6.plot <- rps6 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  #annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  labs(x = "", y = "rps6 signal \n(fold change)\n", fill = "") +
  theme_classic()


# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))

fig3 <- plot_grid(cmyc.plot + theme(legend.position = "none"),
                  ubf.plot + theme(legend.position = "none"),
                  rps6.plot + theme(legend.position = "none"),
                  legend,
                  ncol = 2, nrow = 2)

ggsave(
  file = "fig3.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "./figures",
  scale = 1,
  width = 6,
  height = 12,
  units = c("in", "cm", "mm", "px"),
  dpi = 600,
  limitsize = TRUE,
  bg = NULL
)

