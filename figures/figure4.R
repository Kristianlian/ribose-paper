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

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

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
  labs(x = "Log-UBF normalised by pool", y = "Log-total RNA per mg muscle tissue", 
       fill = "Supplement") +
  scale_color_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  plot_theme
  
ggsave(
  file = "fig4.pdf",
  plot = fig4,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.75,
  units = "cm",
  dpi = 600
)



