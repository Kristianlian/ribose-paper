## Training intensity change figure

# Author: Kristian Lian
# Project: Ribose

# This script produces a figure plotting fold change training intensity (% of 1RM). 

# Packages
library(tidyverse)

# Data

emm.trainint <- readRDS("./data/data-gen/training/emm.trainint.RDS")

# Emmeans figure 
pos <- position_dodge(width = 0.2) # creates a position dodge

totalint.plot <- emm.trainint %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "change.2", y = 1.29, label = "p = NS", size = 2.5) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Session 2", "change.3" = "Session 3",
                            "change.4" = "Session 4", "change.5" = "Session 5", 
                            "change.6" = "Session 6")) +
  scale_y_continuous(limits = c(1, 1.3), breaks = c(1.05, 1.10, 1.15, 1.20, 1.25, 1.30),
                     expand = expansion(0)) +
  labs(x = "", y = "Training intensity \n(Fold change)\n", fill = "") +
  theme_classic() +
  #theme(plot.background = element_rect(fill = "gray80")) +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

saveRDS(totalint.plot, "./data/data-gen/training/trainint.plot.RDS")

## Annotate code
# Over one time point: annotate("text", x = "change.2", y = 1.3, label = "Between legs: p > 0.05", size = 2.5) +
# Over all time points:
# annotate("text", x = c("change.2", "change.3", "change.4", "change.5", "change.6"),
#y = c(1.13, 1.17, 1.21, 1.24, 1.28), label = "p > 0.05", size = 2.5) +




