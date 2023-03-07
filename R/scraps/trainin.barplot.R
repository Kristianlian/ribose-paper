## Training intensity barplot

# Author: Kristian Lian
# Project: Ribose

# This script produces a barplot of training intensity (% of 1RM). 

# Packages
library(tidyverse)

# Data

mean.joined <- readRDS("./data/data-gen/training/mean.joined.RDS")


## Absolute data - summarising mean and SD for barplot
# Mean and SD are calculated from "absolute" numbers of training intensity, creating a basis for a barplot. 

tot.barplot <- ggplot(mean.joined, aes(fill = supplement, y = mean.prm, x = time)) +
  annotate("text", x = c("session2", "session3", "session4", "session5", "session6"),
           y = c(82, 86, 90, 92, 94), label = "†") +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes( ymin=mean.prm-sd.prm, ymax=mean.prm+sd.prm),
                width = 0.2,
                position = position_dodge(width = 1)) +
  labs(x = "", y = "Training intensity \n(%1RM)\n", fill = "") +
  scale_x_discrete(labels=c("baseline" = "Baseline", "session2" = "Session 2", "session3" = "Session 3",
                            "session4" = "Session 4", "session5" = "Session 5", 
                            "session6" = "Session 6")) +
  scale_y_continuous(limits = c(0, 100), breaks = c(0, 10, 20, 30, 40, 50, 60,
                                                    70, 80, 90, 100),
                     expand = expansion(0)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7, angle = 45, hjust = 1))

saveRDS(tot.barplot, "./data/data-gen/training/tot.barplot.RDS")
