### Ribose figure 4 ######
#
# This script produces figure 4, containing the correlation plot between total RNA content and UBF protein
# levels
#
#
# Packages


colors <- c("#d7191c",  
            "#fdae61", 
            "#abd9e9",
            "#2c7bb6")





library(emmeans); library(tidyverse); library(ggpubr)

#
# Data

ubf.rdy <- readRDS("./data/data-gen/protein/ubf.rdy.RDS")

rna.rdy <- readRDS("./data/data-gen/protein/rna.rdy.RDS")

joined.dat <- ubf.rdy %>%
  right_join(rna.rdy) %>%
  #filter(supplement != "GLUCOSE") %>%
  ungroup() %>%
  mutate(sd.ubf = (mean.sign - mean(mean.sign, na.rm = TRUE ))/sd(mean.sign, na.rm = TRUE)) %>%
  print()

saveRDS(joined.dat, "./data/data-gen/rna.prot.RDS")

## Model

library(nlme)

m <- lme(mean.rna ~ sd.ubf + time , 
         random = list(subject = ~ 1), 
         data = joined.dat, 
         na.action = na.omit)

summary(m)
intervals(m)


plot(m)



## Extract x min and xmax from observed data 
min_max <- joined.dat %>%
  group_by(time) %>%
  summarise(min = min(sd.ubf, na.rm = TRUE), 
            max = max(sd.ubf, na.rm = TRUE))


# Correlation plot

textsize <- 8
htextsize <- 9

fig4 <- joined.dat %>%
  ggplot(aes(sd.ubf, mean.rna, fill = time)) + 
  geom_point(shape = 24) +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_fill_manual(values = colors[c(2,3)]) +
  annotate("segment", 
           x = filter(min_max, time == "pre")$min, 
           xend = filter(min_max, time == "pre")$max, 
           y = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "pre")$min, 
           yend = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "pre")$max) +
  
  
  
  annotate("segment", 
           x = filter(min_max, time == "post")$min, 
           xend = filter(min_max, time == "post")$max, 
           y = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "post")$min + coef(summary(m))[3,1] , 
           yend = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "post")$max + coef(summary(m))[3,1]) +
  
  
  
  
  
  labs(x = "UBF per normalization factor<br>(SD units)", y = "Total RNA (ng &times; mg<sup>-1</sup>)", 
       fill = "Time") +

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_markdown(size = htextsize),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = 6),
        axis.title.y = element_markdown(size = htextsize))
  
ggsave(
  file = "fig4.pdf",
  plot = fig4,
  device = "pdf",
  path = "./figures",
  width = 7.7,
  height = 23*0.25,
  units = "cm",
  dpi = 600
)



