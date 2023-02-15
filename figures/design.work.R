####### Study design fig work

# Packages
library(tidyverse)
library(readxl)


# Data
d.dat <- read_excel("./data/design.dat.xlsx", na = "NA") %>%
  print()

biopsy_glyph <- "\U2193"
blood_glyph <-  "\U2020"
strength_glyph <- "\U2021"


d.dat %>%
  ggplot(aes(time, tp)) +
  
  
  scale_y_continuous(limits = c(0,10), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  
  scale_x_continuous(limits = c(-3,21), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                   11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 
                     labels = c("-3" = "", "-2" = "", "-1" = "", "0" = "",
                                "1" = "-7", "2" = "-6", "3" = "-5", "4" = "-4",
                                "5" = "-3", "6" = "-2", "7" = "-1", "8" = "1",
                                "9" = "2", "10" = "3", "11" = "4", "12" = "5", "13" = "6",
                                "14" = "7", "15" = "8", "16" = "9", "17" = "10",
                                "18" = "11", "19" = "12", "20" = "13")) +
  
  # Time periods (familiarisation/internvetion)
  annotate("rect", xmin = -3, xmax = 21, ymin = 0, ymax = 8.5, alpha = 2, color = "lightblue", fill = "lightblue") +
  annotate("text", x = 2, y = 8, label = "Familiarization (7 days)") +
  annotate("text", x = 11, y = 8, label = "Unilateral RT + Dietary intervention (12 days)") +
  
  ## Signs
  # Biopsy
  annotate("text", x = -1.8, y = 7.5, label = "Muscle biopsies", size = 3) +
  annotate("text", x = -0.4, y = 7.45, label = rep(biopsy_glyph, 1), vjust = 0) +
  annotate("text", x = c(8, 18), y = rep(4.3),
           label = rep(biopsy_glyph, 2), color = "blue", vjust = 0) +
  annotate("text", x = c(9, 19), y = rep(4.3),
           label = rep(biopsy_glyph, 2), color = "red", vjust = 0) +
  
  # Strength
  annotate("text", x = -2, y = 7, label = "Strength test", size = 3) +
  annotate("text", x = -0.8, y = 6.95, label = rep(strength_glyph, 1), vjust = 0) +
  annotate("text", x = c(1, 3, 7), y = rep(4),
           label = rep(strength_glyph, 3), vjust = 0) +
  annotate("text", x = c(11, 15, 18, 18.8), y = rep(4),
           label = rep(strength_glyph, 4), color = "blue", vjust = 0) +
  annotate("text", x = c(12, 16, 19.2, 20), y = rep(4),
           label = rep(strength_glyph, 4), color = "red", vjust = 0) +
  

  # Blood
  annotate("text", x = -2.45, y = 6.5, label = "Blood", size = 3) +
  annotate("text", x = -1.65, y = 6.45, label = rep(blood_glyph, 1), vjust = 0) +
  annotate("text", x = 8, y = rep(3.7),
           label = rep(blood_glyph, 3), vjust = 0) +
  annotate("text", x = 18, y = rep(3.7),
           label = rep(blood_glyph, 1), color = "blue", vjust = 0) +
  annotate("text", x = 19, y = rep(3.7),
           label = rep(blood_glyph, 1), color = "red", vjust = 0) +
  
  ## Randomisation
  
  # Inclusion
  annotate("segment", x = -0.8, xend = -0.3, y = 3, yend = 3) +
  annotate("segment", x = -0.8, xend = -0.3, y = 5, yend = 5) +
  annotate("segment", x = -0.8, xend = -0.8, y = 3, yend = 5) +
  annotate("segment", x = -0.3, xend = -0.3, y = 3, yend = 5) +
  annotate(geom = "text", x = -0.6, y = 4, label = c("Inclusion (n=16)"), angle = 90) +
  
  # Randomiser
  annotate("segment", x = 4.7, xend = 5.4, y = 3, yend = 3) +
  annotate("segment", x = 4.7, xend = 5.4, y = 5, yend = 5) +
  annotate("segment", x = 4.7, xend = 4.7, y = 3, yend = 5) +
  annotate("segment", x = 5.4, xend = 5.4, y = 3, yend = 5) +
  annotate(geom = "text", x = 5, y = 4, label = c("Randomization"), angle = 90) +
  
  # Pointers
  annotate("segment", x = 5.4, xend = 7.5, y = 4, yend = 6.5) +
  annotate("segment", x = 5.4, xend = 7.5, y = 4, yend = 1.5) +
  
  # Glucose
  annotate("segment", x = 7.5, xend = 8.5, y = 5.7, yend = 5.7) +
  annotate("segment", x = 7.5, xend = 8.5, y = 7.3, yend = 7.3) +
  annotate("segment", x = 7.5, xend = 7.5, y = 5.7, yend = 7.3) +
  annotate("segment", x = 8.5, xend = 8.5, y = 5.7, yend = 7.3) +
  annotate(geom = "text", x = 8.2, y = 6.5, label = c("Leg 1 RT +\nGLU (n=8)\n"), color = "blue", angle = 90) +
  
  # Placebo
  annotate("segment", x = 7.5, xend = 8.5, y = 0.7, yend = 0.7) +
  annotate("segment", x = 7.5, xend = 8.5, y = 2.3, yend = 2.3) +
  annotate("segment", x = 7.5, xend = 7.5, y = 0.7, yend = 2.3) +
  annotate("segment", x = 8.5, xend = 8.5, y = 0.7, yend = 2.3) +
  annotate(geom = "text", x = 8.2, y = 1.5, label = c("Leg 1 RT +\nPLA (n=8)\n"), color = "blue", angle = 90) +
  
  # Training (start with GLU)
  # Day 2
  annotate(geom = "text", x = 9, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  # Day 3
  annotate(geom = "text", x = 10, y = 6.5, label = c("Leg 1 RT+GLU"), color = "blue", angle = 90) +
  # Day 4
  annotate(geom = "text", x = 11, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  # Day 5
  annotate(geom = "text", x = 12, y = 6.5, label = c("Leg 1 RT+GLU"), color = "blue", angle = 90) +
  # Day 6
  annotate(geom = "text", x = 13, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  # Day 7
  annotate(geom = "text", x = 14, y = 6.5, label = c("Leg 1 RT+GLU"), color = "blue", angle = 90) +
  # Day 8
  annotate(geom = "text", x = 15, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  # Day 9
  annotate(geom = "text", x = 16, y = 6.5, label = c("Leg 1 RT+GLU"), color = "blue", angle = 90) +
  # Day 10
  annotate(geom = "text", x = 17, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  # Day 11
  annotate(geom = "text", x = 18, y = 6.5, label = c("Leg 1 RT+GLU"), color = "blue", angle = 90) +
  # Day 12
  annotate(geom = "text", x = 19, y = 6.5, label = c("Leg 2 RT+PLA"), color = "red", angle = 90) +
  
  ## Training (start with PLA)
  # Day 2
  annotate(geom = "text", x = 9, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  # Day 3
  annotate(geom = "text", x = 10, y = 1.5, label = c("Leg 1 RT+PLA"), color = "blue", angle = 90) +
  # Day 4
  annotate(geom = "text", x = 11, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  # Day 5
  annotate(geom = "text", x = 12, y = 1.5, label = c("Leg 1 RT+PLA"), color = "blue", angle = 90) +
  # Day 6
  annotate(geom = "text", x = 13, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  # Day 7
  annotate(geom = "text", x = 14, y = 1.5, label = c("Leg 1 RT+PLA"), color = "blue", angle = 90) +
  # Day 8
  annotate(geom = "text", x = 15, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  # Day 9
  annotate(geom = "text", x = 16, y = 1.5, label = c("Leg 1 RT+PLA"), color = "blue", angle = 90) +
  # Day 10
  annotate(geom = "text", x = 17, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  # Day 11
  annotate(geom = "text", x = 18, y = 1.5, label = c("Leg 1 RT+PLA"), color = "blue", angle = 90) +
  # Day 12
  annotate(geom = "text", x = 19, y = 1.5, label = c("Leg 2 RT+GLU"), color = "red", angle = 90) +
  
  theme_classic() +
  labs(x = "Days", y = "") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
  
  
  
  
  
 
 coord_flip()
