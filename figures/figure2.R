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

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

# Default textsizes
labsize <- 8
textsize <- 6
htextsize <- 7
atext <- 2


# Plots
totrna.plot <- emm.tot %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, SE = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  # print()
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.5, label = "p = 0.499", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  geom_point(position = position_dodge(width = 0.2), shape = 21, size = 2) +
  labs(x = "Time-point", y = "Total RNA per mg muscle tissue \n(fold change)\n", fill = "") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
       axis.title.y = element_text(size = htextsize),
       axis.text.x = element_text(size = textsize),
       axis.text.y = element_text(size = textsize)) +
  plot_theme
  
#theme(legend.position = "none")


# 47S
plot.47 <- emm.47 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 3.5, label = "p = 0.502", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time-point", y = "47S pre-rRNA \n(fold change)\n", fill = "") +
  #theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = htextsize),
        axis.text.x = element_text(size = textsize),
        axis.text.y = element_text(size = textsize)) + plot_theme

# 18S
plot.18 <- emm.18 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.585", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "", y = "18S rRNA \n(fold change)\n", fill = "") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = htextsize),
        axis.text.x = element_text(size = textsize),
        axis.text.y = element_text(size = textsize)) + plot_theme

# 28S
plot.28 <- emm.28%>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.740", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "", y = "28S rRNA \n(fold change)\n", fill = "") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_text(size = htextsize),
        axis.text.x = element_text(size = textsize),
        axis.text.y = element_text(size = textsize)) + plot_theme

# 5.8S
plot.5.8 <- emm.5.8 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 2.1, label = "p = 0.935", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "5.8S rRNA \n(fold change)\n", fill = "") +
  theme_classic()+
  theme(axis.title.y = element_text(size = htextsize),
        axis.title.x = element_text(size = htextsize),
        axis.text.x = element_text(size = textsize),
        axis.text.y = element_text(size = textsize)) + plot_theme

# 5S
plot.5 <- emm.5 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.9, label = "p = 0.790", size = atext) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "5S rRNA \n(fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.title.y = element_text(size = htextsize),
        axis.title.x = element_text(size = htextsize),
        axis.text.x = element_text(size = textsize),
        axis.text.y = element_text(size = textsize),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = 5)) + plot_theme




# Cowplot for gathering figures

row1.2 <- plot_grid(totrna.plot + theme(legend.position = "none"),
          plot.47 + theme(legend.position = "none"),
          plot.18 + theme(legend.position = "none"),
          plot.28 + theme(legend.position = "none"),
          ncol = 2, 
          nrow = 2,
          labels = c("A)", "B)", "C)", "D)"),
          label_size = labsize,
          align = "v", axis = "l")

row3 <- plot_grid(plot.5.8+ theme(legend.position = "none"),
                  plot.5,
                  ncol = 2, rel_widths = c(1, 1.4), labels = c("E)", "F)"),
                  label_size = labsize)

fig2 <- plot_grid(row1.2,
          row3,
          nrow = 2,
          rel_heights = c(1.75, 1))


ggsave(
  file = "fig2.pdf",
  plot = fig2,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.75,
  units = "cm",
  dpi = 600
)





