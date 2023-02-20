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

textsize <- 8
htextsize <- 9

fig4 <- joined.dat %>%
  ggplot(aes(log(mean.sign), log(mean.rna), color = supplement)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Mean log-UBF \nper normalization factor\n", y = "Mean log-total RNA \nper mg muscle tissue\n", 
       color = "Supplement") +
  scale_color_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = htextsize),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = 6))
  
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



