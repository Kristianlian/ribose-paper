## rRNA figures

# This script produces the figures for total RNA change

# Author: Kristian Lian

# Packages
library(emmeans)
library(tidyverse)

## Data

emm.tot <- readRDS("./data/data-gen/rna/emm.tot.RDS")


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

saveRDS(totrna.plot, "./data/data-gen/rna/totrna.plot.RDS")
