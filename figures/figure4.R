### Ribose figure 4 ######
#
# This script produces figure 4, containing the correlation plot between total RNA content and UBF protein
# levels
#
#
# Packages
library(emmeans); library(tidyverse); library(ggpubr)

#
# Data

ubf.rdy <- readRDS("./data/data-gen/protein/ubf.rdy.RDS")

rna.rdy <- readRDS("./data/data-gen/protein/rna.rdy.RDS")

## Joining the data frames
joined.dat <- ubf.rdy %>%
  right_join(rna.rdy) %>%
  #filter(supplement != "GLUCOSE") %>%
  print()

# Correlation plot
joined.dat %>%
  ggplot(aes(mean.sign, mean.rna, color = time)) + geom_point() +
  geom_smooth(method = "lm")


