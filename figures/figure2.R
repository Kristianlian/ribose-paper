### Ribose figure 2 ######
#
# This script creates figure 2, containing log-fold change data of total RNA and rRNA
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot)
#
### Data
#
## Total RNA
emm.tot <- readRDS("./data/data-gen/rna/emm.tot.RDS")

## rRNA
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

# Plots
totrna.plot <- emm.tot %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, SE = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  # print()
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.5, label = "p = 0.499", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2), shape = 21, size = 3) +
  labs(x = "Time-point", y = "Total RNA per mg muscle tissue \n(fold change)\n", fill = "") +
  theme_classic()  
#theme(legend.position = "none")

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

# Cowplot for gathering figures

legend <- get_legend(plot.47 + theme(legend.box.margin = margin(0, 0, 0,12)))


fig2 <- plot_grid(totrna.plot + theme(legend.position = "none"),
                  plot.47 + theme(legend.position = "none"),
                  plot.18 + theme(legend.position = "none"),
                  plot.28 + theme(legend.position = "none"),
                  plot.5.8 + theme(legend.position = "none"),
                  plot.5 + theme(legend.position = "none"),
                  ncol = 2, nrow = 3)

ggsave(
  file = "fig2.pdf",
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






