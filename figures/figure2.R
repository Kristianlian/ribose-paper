### Ribose figure 2 in ejap format ######
#
# This script creates figure 2, containing log-fold change data of total RNA and rRNA
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot); library(ggtext)

#
### Data
#
## Total RNA
emm.tot <- readRDS("./data/data-gen/rna/emm.tot.RDS")

## rRNA
# 18S
emm.18 <- readRDS("./data/data-gen/rna/emm.18.RDS") %>%
  mutate(target = "18S rRNA",
         time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0,
          target = "18S rRNA") %>%
  print()

# 28S
emm.28 <- readRDS("./data/data-gen/rna/emm.28.RDS")  %>%
  mutate(target = "28S rRNA",
         time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0,
          target = "28S rRNA") %>%
  print()

# 5.8S
emm.5.8 <- readRDS("./data/data-gen/rna/emm.5.8.RDS") %>%
  mutate(target = "5.8S rRNA",
         time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0,
          target = "5.8S rRNA") %>%
  print()

# 5S
emm.5 <- readRDS("./data/data-gen/rna/emm.5.RDS")  %>%
  mutate(target = "5S rRNA",
         time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0,
          target = "5S rRNA") %>%
  print()

# 47S
emm.47 <- readRDS("./data/data-gen/rna/emm.47.RDS") %>%
  mutate(target = "47S rRNA",
         time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0,
          target = "47S rRNA") %>%
  print() 

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "white", colour = NA),
                    axis.line = element_line(colour = "black", linewidth = 0.4))

# Colours

colors <- c("black","white")
                     
# Default textsizes
labsize <- 8
textsize <- 6
htextsize <- 7
atext <- 2
labsize <- 8

# Total RNA plot
totrna.plot <- emm.tot %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, SE = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2), lty = 2) +
  geom_point(position = position_dodge(width = 0.2), shape = 21, size = 2) +
  scale_fill_manual(values = colors[c(1,2)],
                    labels = c("Glucose", "Placebo")) +
  labs(x = "Time-point", y = "Total RNA (ng &times; mg<sup>-1</sup> fold change)", fill = "Supplement") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_markdown(size = htextsize),
        axis.text.x = element_text(size = textsize, color = "black"),
        axis.text.y = element_text(size = textsize, color = "black"),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = textsize),
        legend.key.size = unit(0.3, "cm")) +
  plot_theme


# rRNA plot

rrna.fig <- bind_rows(emm.47, emm.18, emm.28, emm.5.8, emm.5) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post")),
         target = factor(target, levels = c("47S rRNA", "18S rRNA", "28S rRNA", "5.8S rRNA", "5S rRNA"),
                         labels = c("47S", "18S", "28S", "5.8S", "5S"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3), lty = 2) +
  geom_point(shape = 21, size = 1.5, position = position_dodge(width = 0.3)) +
  scale_fill_manual(values = colors[c(1,2)]) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "Time", y = "AU fold change", fill = "Supplement") +
  facet_wrap(~target, nrow = 1) +
  theme(axis.title = element_text(size = htextsize),
        axis.text = element_text(size = textsize, color = "black"),
        legend.title = element_text(size = textsize),
        legend.text = element_text(size = textsize),
        strip.text = element_text(hjust = 0, size = htextsize),
        strip.background = element_rect(fill = "white")) + 
  theme(axis.title.x = element_blank()) +
  plot_theme 


fig2 <- plot_grid(plot_grid(NULL,
                            totrna.plot,
                            nrow = 1,
                            rel_widths = c(0.05, 1)),
                  
                  rrna.fig + theme(legend.position = "none"),
                  nrow = 2,
                  rel_heights = c(1.5, 1),
                  labels = c("a", "b"),
                  label_size = labsize)


ggsave(
  file = "fig2.pdf",
  plot = fig2,
  device = "pdf",
  path = "./figures",
  width = 84,
  height = 234*0.33,
  units = "mm",
  dpi = 1200
)
