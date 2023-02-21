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

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

labsize <- 8
textsize <- 6
htextsize <- 7
atext <- 2
keysize <- 2


## cmyc fig

cmyc.plot <- cmyc %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "c-Myc signal \n(fold change)\n", fill = "Supplement") + plot_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = htextsize),
        axis.text = element_text(size = textsize))
        

# UBF plot


ubf.plot <- ubf %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 3, label = "*", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "UBF signal \n(fold change)\n", fill = "") + plot_theme +
  theme(axis.title = element_text(size = htextsize),
      axis.text = element_text(size = textsize))


# rpS6 plot


rps6.plot <- rps6 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.87, label = "*", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "rps6 signal \n(fold change)\n", fill = "Supplement") + plot_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = htextsize),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = 5),
        legend.key = element_rect(fill = "white"))


# Cowplot for gathering figures


fig3 <- plot_grid(rps6.plot,
          cmyc.plot + theme(legend.position = "none"),
          ubf.plot + theme(legend.position = "none"),
          ncol = 1,
          labels = c("A)", "B)", "C)"),
          label_size = labsize,
          align = "v", axis = "l")

ggsave(
  file = "fig3.pdf",
  plot = fig3,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.5,
  units = "cm",
  dpi = 600
)

