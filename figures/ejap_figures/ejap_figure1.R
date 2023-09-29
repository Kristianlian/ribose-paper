### Ribose figure 1 in ejap format######
#
# Study design, strength (1RM, Humac), c-peptide and blood glucose
#
#
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot); library(grid); library(gridExtra); library(readxl); 
library(ggtext)
#
## Data

# Study design
d.dat <- read_excel("./data/design.dat.xlsx", na = "NA")

# C-peptide
cpep.change <- readRDS("./data/data-gen/glucose/insulin.figchange.RDS") # Untransformed data

# Blood glucose
glu.change <- readRDS("./data/data-gen/glucose/gluc.change.RDS") # Untransformed data

# Humac data
str.change <- readRDS("./data/data-gen/humac/str.change.RDS")
str.emm <- readRDS("./data/data-gen/humac/emm.str.RDS")

# Training volume data
vol.lemm <- readRDS("./data/data-gen/training/vol.lemm.RDS")


### Plots
pos <- position_dodge(width = 10)

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "white", colour = NA),
                    axis.line = element_line(colour = "black", linewidth = 0.4))

# Colours
# Colours

colors <- c("black","white")
                     


## C-peptide

cpep.fig <- cpep.change %>%
  data.frame() %>%
  add_row(supplement = "PLACEBO", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "GLUCOSE", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  ggplot(aes(time.c, emmean, group = supplement, fill = supplement)) +
  annotate("text", x = c(120, 150), y = c(975, 950), label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                width = 0,
                position = position_dodge(width = 10)) +
  geom_line(position = position_dodge(width = 10), lty = 2) +
  geom_point(shape = 21, position = position_dodge(width = 10), size = 2) +
  scale_fill_manual(values = colors[c(1,2)]) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 90, 120, 150, 270),
                     expand = expansion(0), labels = c("change.1" = "-120 min", "change.90" = "-30 min", "change.120" = "0 min", 
                                                       "change.150" = "30 min", "change.270" = "120 min")) +
  labs(x = "", y = "Plasma c-peptide levels <br>(pmol &times; L<sup>-1</sup> change)", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_markdown(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 4),
        legend.key = element_rect(fill = "white"),
        axis.text.y = element_text(size = 6)) +
  plot_theme



## Blood glucose

glu.fig <- glu.change %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  ggplot(aes(time.c, emmean, group = supplement, fill = supplement)) +
  annotate("text", x = c(120, 135, 150), y = c(2.4, 2, 2.04), label = "*") +
  annotate("text", x = c(270), y = c(0.23), label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                width = 0,
                position = pos) +
  geom_line(position = pos, lty = 2) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = colors[c(1,2)]) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 45, 90, 120, 135, 150, 270),
                     expand = expansion(0), labels = c("0" = "-120 min", "45" = "-90 min", "90" = "-30 min", "120" = "0 min", "135" = "15 min",
                                                       "150" = "30 min", "270" = "120 min")) +
  labs(x = "", y = "Plasma glucose levels <br>(mmol &times; L<sup>-1</sup> change)", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_markdown(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme



## Gathered strenght figure

str.emm2 <- str.emm %>%
  add_row(supplement = "placebo", time = "0", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "0", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  mutate(timeh = if_else(time == "0",
                         "0",
                         if_else(time == "change.2",
                                 "8",
                                 if_else(time == "change.3",
                                         "16",
                                         if_else(time == "change.4",
                                                 "20",
                                                 if_else(time == "change.5",
                                                         "21",
                                                         if_else(time == "change.6",
                                                                 "22",
                                                                 if_else(time == "change.7", 
                                                                         "23.5", time)))))))) %>%
  mutate(#timeh = factor(timeh, levels = c("0", "72", "192", "264", "266", "268", "291")),
    timeh = as.numeric(timeh)) %>%
  mutate(status = if_else(time %in% c("0", "change.2", "change.3", "change.4"),
                          "rest",
                          if_else(time %in% c("change.5", "change.6", "change.7"),
                                  "acute", time))) 

str.fig2 <- str.emm2 %>%
  data.frame() %>%
  ggplot(aes(timeh, emmean, fill = supplement)) +
  annotate("text", x = 20, y = -0.04, label = "*") +
  
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  
  geom_line(lty = 2, position = position_dodge(width = 0.5)) +
  
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.5)) +
  
  
  scale_fill_manual(values = colors[c(1,2)],
                    labels = c("Glucose", "Placebo")) +
  scale_x_continuous(limits = c(-1,25), breaks = c(0, 8, 16, 20, 21, 22, 23.5),
                     expand = expansion(0),
                     labels = c("0" = "Baseline", "8" = "Post 2RT", "16" = "Post 4RT", "21" = "Post 5RT",
                                "22.7" = "30min post 6RT", "23.4" = "2h post 6RT",
                                "24.4" = "23h post 6RT")) +
  labs(x = "Time", y = "Strength index change", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 6),
        axis.title = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  plot_theme

#str.emm2 %>%
#mutate(status = factor(status, levels = c("rest", "acute"),
#                       labels = c("Performance", 
#                                  "Recovery"))) %>%
#  ggplot(aes(timeh, emmean, group = supplement, fill = supplement)) +
#  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
#                width = 0.1,
#                position = position_dodge(width = 0.3)) +
#  geom_line(position = position_dodge(width = 0.3)) +
#  geom_point(shape = 21, size = 1.5, position = position_dodge(width = 0.3)) +
#  facet_grid(~status, space = "free", scale = "free") +
#  scale_fill_manual(values = colors[c(1,4)]) +
#  scale_x_continuous(limits = c(0,45), breaks = c(0, 8, 16, 20, 20.5, 22, 45),
#                                          labels = c("0" = "Baseline", "8" = "Post 2RT", "16" = "Post 4RT", "20" = "Post 5RT",
#                                                     "20.5" = "30min post 6RT", "22" = "2h post 6RT",
#                                                     "45" = "23h post 6RT")) +
# # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
#  labs(x = "", y = "", fill = "Supplement") 

# theme(axis.title = element_text(size = htextsize),
#       axis.text = element_text(size = textsize),
#       legend.title = element_text(size = htextsize),
#       legend.text = element_text(size = textsize),
#       strip.text = element_text(hjust = 0, size = htextsize),
#       strip.background = element_rect(fill = "white")) + 
# theme(axis.title.x = element_blank()) +
#plot_theme 


## Study design

biopsy_glyph <- "\U0023" # U2303 works
blood_glyph <-  "\U2020"
strength_glyph <- "\U2021"

line_size <- 0.2
htextsize <- 2.5
textsize <- 2

d.fig <- d.dat %>%
  ggplot(aes(time, tp)) +
  scale_y_continuous(limits = c(0,7), breaks = c(1, 2, 3, 4, 5, 6, 7), expand = c(0,0)) +
  scale_x_continuous(limits = c(-3,21), breaks = seq(1:20), expand = c(0,0),
                     labels = c("1" = "-7", "2" = "-6", "3" = "-5", "4" = "-4",
                                "5" = "-3", "6" = "-2", "7" = "-1", "8" = "1",
                                "9" = "2", "10" = "3", "11" = "4", "12" = "5", "13" = "6",
                                "14" = "7", "15" = "8", "16" = "9", "17" = "10",
                                "18" = "11", "19" = "12", "20" = "13")) +
  
  # Time periods (familiarisation/internvetion)
  annotate("rect", xmin = -3, xmax = 21, ymin = 0, ymax = 7, alpha = 2, color = "white", fill = "white") +
  annotate("text", x = 2, y = 5.6, label = "Familiarization (7 days)", size = htextsize) +
  annotate("text", x = 11, y = 5.6, label = "Unilateral RT + Dietary intervention (12 days)", size = htextsize) +
  
  ## Signs
  # Biopsy
  annotate("text", x = -1.7, y = 4.5, label = "Muscle biopsies", vjust = 0, size = textsize) +
  annotate("text", x = -0.2, y = 4.45, label = rep(biopsy_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = c(8, 18), y = rep(3.6),
           label = rep(biopsy_glyph, 2), color = "black", vjust = 0, size = textsize) +
  annotate("text", x = c(9, 19), y = rep(1.6),
           label = rep(biopsy_glyph, 2), color = "black", vjust = 0, size = textsize) +
  
  # Strength
  annotate("text", x = -2, y = 4, label = "Strength test", vjust = 0, size = textsize) +
  annotate("text", x = -0.8, y = 4, label = rep(strength_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = c(1, 3, 7), y = rep(2.5),
           label = rep(strength_glyph, 3), vjust = 0, size = textsize) +
  annotate("text", x = c(11, 15, 18, 18.2, 18.5, 19), y = rep(4),
           label = rep(strength_glyph, 6), color = "black", vjust = 0, size = textsize) +
  annotate("text", x = c(12, 16, 19, 19.2, 19.5, 20), y = rep(1.2),
           label = rep(strength_glyph, 6), color = "black", vjust = 0, size = textsize) +
  
  
  # Blood
  annotate("text", x = -2.5, y = 3.5, label = "Blood", vjust = 0, size = textsize) +
  annotate("text", x = -1.75, y = 3.5, label = rep(blood_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = 8, y = rep(2.5),
           label = rep(blood_glyph, 3), vjust = 0, size = textsize) +
  annotate("text", x = 18.2, y = rep(3.6),
           label = rep(blood_glyph, 1), color = "black", vjust = 0, size = textsize) +
  annotate("text", x = 19.2, y = rep(1.6),
           label = rep(blood_glyph, 1), color = "black", vjust = 0, size = textsize) +
  
  ## Randomization
  
  # Inclusion
  annotate(geom = "label", x = -1, y = 2.5, label = c("Inclusion (n=16)"), fill = "white", size = textsize) +
  
  # Randomization
  annotate(geom = "label", x = 5, y = 2.5, label = c("Randomization"), fill = "white", size = textsize) +
  
  # Pointers
  annotate("segment", x = 6.3, xend = 7.3, y = 2.5, yend = 4.3, linewidth = line_size) +
  annotate("segment", x = 6.3, xend = 8.3, y = 2.5, yend = 0.7, linewidth = line_size) +
  
  ## Randomized unilateral RT
  # Glucose
  # Day 1
  annotate(geom = "text", x = 8.2, y = 4.5, label = c("Leg 1 RT +\nGLU (n=8)\n"), color = "black", angle = 0, size = textsize) +
  # Day 3
  annotate(geom = "text", x = 10, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "black", angle = 0, size = textsize) +
  # Day 5
  annotate(geom = "text", x = 12, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "black", angle = 0, size = textsize) +
  # Day 7
  annotate(geom = "text", x = 14, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "black", angle = 0, size = textsize) +
  # Day 9
  annotate(geom = "text", x = 16, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "black", angle = 0, size = textsize) +
  # Day 11
  annotate(geom = "text", x = 18, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "black", angle = 0, size = textsize) +
  
  # Placebo
  # Day 2
  annotate(geom = "text", x = 9.2, y = 0.5, label = c("Leg 2 RT +\nPLA (n=8)\n"), color = "black", angle = 0, size = textsize) +
  # Day 4
  annotate(geom = "text", x = 11, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "black", angle = 0, size = textsize) +
  # Day 6
  annotate(geom = "text", x = 13, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "black", angle = 0, size = textsize) +
  # Day 8
  annotate(geom = "text", x = 15, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "black", angle = 0, size = textsize) +
  # Day 10
  annotate(geom = "text", x = 17, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "black", angle = 0, size = textsize) +
  # Day 12
  annotate(geom = "text", x = 19, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "black", angle = 0, size = textsize) +
  
  theme_classic()+
  
  labs(x = "Days", y = "") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        plot.background = element_rect(color = "white",
                                             linewidth = 0.50,
                                             fill = "white"))



# Joined figure 1



fig1 <- plot_grid(d.fig,
                  NULL,
                  plot_grid(glu.fig + theme(legend.position = "none"),
                            cpep.fig + theme(legend.position = "none"),
                            ncol = 2),
                  plot_grid(str.fig2),
                  ncol = 1,
                  rel_heights = c(1.5, 0.3, 1.5, 1.5)) +
  draw_plot_label(label = c("a", "b", "c", "d"),
                  x = c(0.02, 0.02, 0.53, 0.02),
                  y = c(0.98, 0.63, 0.63, 0.3),
                  hjust = .5, vjust = .5, size = 7) 



ggsave(
  file = "ejapfig1.pdf",
  plot = fig1,
  device = "pdf",
  path = "./figures/ejap_figures",
  width = 7.7*2,
  height = 23*0.75,
  units = "cm",
  dpi = 600
)





