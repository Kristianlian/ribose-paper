### Ribose figure 1 ######
#
# Study design, strength (1RM, Humac), c-peptide and blood glucose
#
#
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot); library(grid); library(gridExtra); library(readxl); 
library(elementalist)
#
## Data

# Study design
d.dat <- read_excel("./data/design.dat.xlsx", na = "NA")

# C-peptide
cpep.change <- readRDS("./data/data-gen/glucose/insulin.figchange.RDS") # Untransformed data
cpep.emm <- readRDS("./data/data-gen/glucose/cpep.emm.RDS") # Unstransformed emmeans
cpep.lchange <- readRDS("./data/data-gen/glucose/insulin.change.RDS") # Log-transformed data
cpep.lemm <- readRDS("./data/data-gen/glucose/cpep.lemm.RDS") # Log-transformed emmeans

# Blood glucose
glu.change <- readRDS("./data/data-gen/glucose/gluc.change.RDS") # Untransformed data
glu.emm <- readRDS("./data/data-gen/glucose/gluc.emm.RDS") # Untransformed emmeans
glu.lchange <- readRDS("./data/data-gen/glucose/glu.change.RDS") # Log-transformed data
glu.lemm <- readRDS("./data/data-gen/glucose/gluc.logemm.RDS") # Log-transformed emmeans

# Humac pre->post 5 session
isom.lchange <- readRDS("./data/data-gen/humac/isom.lchange.RDS") # Log-transformed data
lemm.isom <- readRDS("./data/data-gen/humac/emm.isom.RDS") # Log-transformed emmans
isok60.lchange <- readRDS("./data/data-gen/humac/isok60.lchange.RDS") # Log-transformed data
lemm.60 <- readRDS("./data/data-gen/humac/emm.60.RDS") # Log-transformed emmeans
isok240.lchange <- readRDS("./data/data-gen/humac/isok240.lchange.RDS") # Log-transformed data
lemm.240 <- readRDS("./data/data-gen/humac/emm.240.RDS") # Log-transformed emmeans

# Humac pre->post sessions 6
isomac.lchange <- readRDS("./data/data-gen/humac/isom.lchange.ac.RDS") # Log-transformed
lemm.isomac <- readRDS("./data/data-gen/humac/emm.ac.isom.RDS")
iso60ac.lchange <- readRDS("./data/data-gen/humac/iso60.lchange.ac.RDS") # Log-transformed
lemm.60ac <- readRDS("./data/data-gen/humac/emm.ac.60.RDS")
iso240ac.lchange <- readRDS("./data/data-gen/humac/iso240.lchange.ac.RDS") # Log-transformed
lemm.240ac <- readRDS("./data/data-gen/humac/emm.ac.240.RDS")

### Plots
pos <- position_dodge(width = 0.2)

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))


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
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "", y = "C-peptide levels \n(pmol/L change)\n", fill = "Supplement") +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 90, 120, 150, 270),
                     expand = expansion(0), labels = c("change.1" = "-120", "change.90" = "-30", "change.120" = "0", 
                                                       "change.150" = "30", "change.270" = "120")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 4),
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
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  labs(x = "", y = "Plasma glucose levels \n(mmol/L change)\n", fill = "Supplement") +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 45, 90, 120, 135, 150, 270),
                     expand = expansion(0), labels = c("0" = "-120", "45" = "-90", "90" = "-30", "120" = "0", "135" = "15",
                                                       "150" = "30", "270" = "120")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

## Humac pre->post 5 sessions

# Isometric

isom.mfig <- lemm.isom %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 2", "change.3" = "Test 3",
                            "change.4" = "Post")) +
  labs(x = "", y = "Isometric peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

# Isokinetic 60 d/s

mfig.60 <- lemm.60 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  annotate("text", x = c("change.4"), y = c(1.02), label = "*") +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 2", "change.3" = "Test 3",
                            "change.4" = "Post")) +
  labs(x = "", y = "Iso 60 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

# Isokinetic 240

mfig.240 <- lemm.240 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 2", "change.3" = "Test 3",
                            "change.4" = "Post")) +
  labs(x = "", y = "Iso 240 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

## Humac pre->post 6th session

# Isometric

isom.acfig <- lemm.isomac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min", "change.3" = "2hrs",
                            "change.4" = "23hrs")) +
  labs(x = "", y = "Isometric peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

# Isokinetic 60 d/s

acfig.60 <- lemm.60ac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min", "change.3" = "2hrs",
                            "change.4" = "23hrs")) +
  labs(x = "Time", y = "Iso 60 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

# Isokinetic 240

acfig.240 <- lemm.240ac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  scale_fill_manual(values = c("glucose" = "red", "placebo" = "royalblue")) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min", "change.3" = "2hrs",
                            "change.4" = "23hrs")) +
  labs(x = "", y = "Iso 240 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.title.y = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  plot_theme

## Study design

biopsy_glyph <- "\U2193"
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
  annotate("rect", xmin = -3, xmax = 21, ymin = 0, ymax = 7, alpha = 2, color = "lightblue", fill = "lightblue") +
  annotate("text", x = 2, y = 5.6, label = "Familiarization (7 days)", size = htextsize) +
  annotate("text", x = 11, y = 5.6, label = "Unilateral RT + Dietary intervention (12 days)", size = htextsize) +
  
  ## Signs
  # Biopsy
  annotate("text", x = -1.7, y = 4.5, label = "Muscle biopsies", vjust = 0, size = textsize) +
  annotate("text", x = -0.4, y = 4.45, label = rep(biopsy_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = c(8, 18), y = rep(3.6),
           label = rep(biopsy_glyph, 2), color = "red", vjust = 0, size = textsize) +
  annotate("text", x = c(9, 19), y = rep(1.6),
           label = rep(biopsy_glyph, 2), color = "royalblue", vjust = 0, size = textsize) +
  
  # Strength
  annotate("text", x = -2, y = 4, label = "Strength test", vjust = 0, size = textsize) +
  annotate("text", x = -0.8, y = 4, label = rep(strength_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = c(1, 3, 7), y = rep(2.5),
           label = rep(strength_glyph, 3), vjust = 0, size = textsize) +
  annotate("text", x = c(11, 15, 18, 18.2, 18.5, 19), y = rep(4),
           label = rep(strength_glyph, 6), color = "red", vjust = 0, size = textsize) +
  annotate("text", x = c(12, 16, 19, 19.2, 19.5, 20), y = rep(1.2),
           label = rep(strength_glyph, 6), color = "royalblue", vjust = 0, size = textsize) +
  
  
  # Blood
  annotate("text", x = -2.5, y = 3.5, label = "Blood", vjust = 0, size = textsize) +
  annotate("text", x = -1.75, y = 3.5, label = rep(blood_glyph, 1), vjust = 0, size = textsize) +
  annotate("text", x = 8, y = rep(2.5),
           label = rep(blood_glyph, 3), vjust = 0, size = textsize) +
  annotate("text", x = 18.2, y = rep(3.6),
           label = rep(blood_glyph, 1), color = "red", vjust = 0, size = textsize) +
  annotate("text", x = 19.2, y = rep(1.6),
           label = rep(blood_glyph, 1), color = "royalblue", vjust = 0, size = textsize) +
  
  ## Randomization
  
  # Inclusion
  annotate(geom = "label", x = -1, y = 2.5, label = c("Inclusion (n=16)"), fill = "lightblue", size = textsize) +
  
  # Randomization
  annotate(geom = "label", x = 5, y = 2.5, label = c("Randomization"), fill = "lightblue", size = textsize) +
  
  # Pointers
  annotate("segment", x = 6.3, xend = 7.3, y = 2.5, yend = 4.3, size = line_size) +
  annotate("segment", x = 6.3, xend = 8.3, y = 2.5, yend = 0.7, size = line_size) +
  
  ## Randomized unilateral RT
  # Glucose
  # Day 1
  annotate(geom = "text", x = 8.2, y = 4.5, label = c("Leg 1 RT +\nGLU (n=8)\n"), color = "red", angle = 0, size = textsize) +
  # Day 3
  annotate(geom = "text", x = 10, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "red", angle = 0, size = textsize) +
  # Day 5
  annotate(geom = "text", x = 12, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "red", angle = 0, size = textsize) +
  # Day 7
  annotate(geom = "text", x = 14, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "red", angle = 0, size = textsize) +
  # Day 9
  annotate(geom = "text", x = 16, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "red", angle = 0, size = textsize) +
  # Day 11
  annotate(geom = "text", x = 18, y = 4.5, label = c("Leg 1 \nRT+GLU\n"), color = "red", angle = 0, size = textsize) +
  
  # Placebo
  # Day 2
  annotate(geom = "text", x = 9.2, y = 0.5, label = c("Leg 2 RT +\nPLA (n=8)\n"), color = "royalblue", angle = 0, size = textsize) +
  # Day 4
  annotate(geom = "text", x = 11, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "royalblue", angle = 0, size = textsize) +
  # Day 6
  annotate(geom = "text", x = 13, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "royalblue", angle = 0, size = textsize) +
  # Day 8
  annotate(geom = "text", x = 15, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "royalblue", angle = 0, size = textsize) +
  # Day 10
  annotate(geom = "text", x = 17, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "royalblue", angle = 0, size = textsize) +
  # Day 12
  annotate(geom = "text", x = 19, y = 0.5, label = c("Leg 2 \nRT+PLA\n"), color = "royalblue", angle = 0, size = textsize) +
  
  theme_classic()+
  
  labs(x = "Days", y = "") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        plot.background = element_rect_round(color = "lightblue",
                                             size = 0.50,
                                             radius = unit(0.50, "cm"),
                                             fill = "lightblue"))



# Joined figure 1


fig1 <- plot_grid(d.fig,
          plot_grid(glu.fig + theme(legend.position = "none"),
                    cpep.fig,
                    ncol = 2, rel_widths = c(0.9,1.25)),
          plot_grid(isom.mfig + theme(legend.position = "none"),
                    mfig.60 + theme(legend.position = "none"), 
                    mfig.240 + theme(legend.position = "none"),
                    isom.acfig + theme(legend.position = "none"),
                    acfig.60 + theme(legend.position = "none"), 
                    acfig.240 + theme(legend.position = "none"), ncol = 3, nrow = 2),
          ncol = 1,
          rel_heights = c(1.5,1,2)) +
  draw_plot_label(label = c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)", "I)"),
                  x = c(0.02, 0.035, 0.45, 0.035, 0.375, 0.71, 0.035, 0.375, 0.71),
                  y = c(0.98, 0.66, 0.66, 0.45, 0.45, 0.45, 0.23, 0.23, 0.23),
                  hjust = .5, vjust = .5, size = 7) 



ggsave(
  file = "fig1.pdf",
  plot = fig1,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.75,
  units = "cm",
  dpi = 600
)





