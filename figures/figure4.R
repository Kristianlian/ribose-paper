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
fig4 <- joined.dat %>%
  ggplot(aes(log(mean.sign), log(mean.rna), color = supplement)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Log-UBF normalised by pool", y = "Log-Total RNA per mg muscle tissue", fill = "") +
  theme_classic()
  
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



