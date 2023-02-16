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
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "", y = "c-Myc signal \n(fold change)\n", fill = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) + plot_theme


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
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "", y = "UBF signal \n(fold change)\n", fill = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line.x = element_blank(),
        axis.ticks.x = element_blank()) + plot_theme

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
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "rps6 signal \n(fold change)\n", fill = "") + plot_theme


# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))

fig4 <- plot_grid(cmyc.plot + theme(legend.position = "none"),
                  ubf.plot + theme(legend.position = "none"),
                  rps6.plot + theme(legend.position = "none"),
                  labels = c("A", "B", "C"), label_size = 12,
                  ncol = 1, nrow = 3,
                  align = "v")

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

##############

# Data

ubf.rdy <- readRDS("./data/data-gen/protein/ubf.rdy.RDS")

rna.rdy <- readRDS("./data/data-gen/protein/rna.rdy.RDS")

## Joining the data frames
joined.dat <- ubf.rdy %>%
  right_join(rna.rdy) %>%
  #filter(supplement != "GLUCOSE") %>%
  print()

# Correlation plot
cor.fig <- joined.dat %>%
  ggplot(aes(log(mean.sign), log(mean.rna), color = supplement)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Log-UBF normalised by pool", y = "Log-total RNA per mg muscle tissue", 
       fill = "Supplement") +
  scale_color_discrete(name = "Supplement", labels = c("Glucose", "Placebo")) +
  theme_classic()

plot_grid(cor.fig,
          )


ggsave(
  file = "fig4.pdf",
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

