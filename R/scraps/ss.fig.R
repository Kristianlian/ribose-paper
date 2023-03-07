#### Sessions score figure

# This script creates the session score figure.

# Packages
library(tidyverse);library(emmeans)

# Data

emm.ss <- readRDS("./data/data-gen/training/emm.ss.RDS")

# Figure
# Plots fold change of estimated marginal means per supplement and saves the figures to be used in the thesis

pos <- position_dodge(width = 0.2) # creates a position dodge

# Session score figure
ss.plot <- emm.ss %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "", "change.2" = "", "change.3" = "",
                            "change.4" = "", "change.5" = "", 
                            "change.post" = "")) +
  labs(x = "", y = "RPE (1-10) \n(Fold-change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(ss.plot, "./data/data-gen/training/ss.plot.RDS")
