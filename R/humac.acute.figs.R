#### Humac "acute" data figures

## Author: Kristian Lian/DH
## Project: Ribose

# Purpose: This script creates the figures from the humac.main.analysis, plotting change from pre to post the 6th session per supplement.

# Packages
library(tidyverse);library(emmeans)

# Data

emm.ac.isom <- readRDS("./data/data-gen/humac/emm.ac.isom.RDS")

emm.ac.60 <- readRDS("./data/data-gen/humac/emm.ac.60.RDS")

emm.ac.240 <- readRDS("./data/data-gen/humac/emm.ac.240.RDS")

## Figures

pos <- position_dodge(width = 0.2)


# Isom

isom.plot <- emm.ac.isom %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  #annotate("text", x = "change.1", y = 25, label = "p = NS", size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min post", "change.3" = "2hr post",
                            "change.4" = "23hr post")) +
  labs(x = "", y = "Isometric \n(Nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

saveRDS(isom.plot, "./data/data-gen/humac/isom.ac.plot.RDS")

# Isok 60
isok60.plot <- emm.ac.60 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min post", "change.3" = "2hr post",
                            "change.4" = "23hr post")) +
  labs(x = "", y = "Isokinetic 60 \n(Nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
#theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(isok60.plot, "./data/data-gen/humac/isok60.ac.plot.RDS")

# Isok 240
isok240.plot <- emm.ac.240 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min", "change.3" = "2hr",
                            "change.4" = "23hr")) +
  labs(x = "", y = "Isokinetic 240 \n(Nm change)\n", fill = "Supplement") +
  theme_classic() 
  # theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 saveRDS(isok240.plot, "./data/data-gen/humac/isok240.ac.plot.RDS")
