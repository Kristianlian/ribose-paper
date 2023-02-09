#### Session score - mean summarising + barplot

# This script summarises mean and SD of sessions score, and creates a barplot with absolute values based on the RPE 1-10
# sessions score given 15 min post RT.

## Packages

library(tidyverse);library(lme4);library(broom);library(emmeans)

## Data

ss.dat <- readRDS("./data/data-gen/training/ss.dat.RDS")

# Mean and SD are calculated from absolute (kg) total volume, creating a basis for a barplot. 

ss.exp <- ss.dat %>%
  select(subject, supplement, time, session.score) %>%
  group_by(supplement, time) %>%
  summarise(mean.ss = mean(session.score),
            sd.ss = sd(session.score)) %>%
  mutate(stat = paste0(round(mean.ss, 1), " (", round(sd.ss, 1), ")")) %>%
  select(supplement, time, stat) %>%
  pivot_wider(names_from = time,
              values_from = stat) %>%
  print()

saveRDS(ss.exp, "./data/data-gen/training/ss.abs.RDS")

ss.barplot <- ggplot(ss.exp, aes(fill = supplement, y = mean.ss, x = time)) +
  geom_bar(position = "dodge", stat = "identity") +
  #geom_errorbar(aes(x = time, ymin = mean.prm - sd.prm, ymax = mean.prm + sd.prm),
  #             width = 0.2)
  labs(x = "", y = "RPE \n(1-10)\n", fill = "Supplement") +
  scale_x_discrete(labels=c("baseline" = "", "session2" = "", "session3" = "",
                            "session4" = "", "session5" = "", 
                            "post" = "")) +
  scale_y_continuous(limits = c(0, 10), breaks = c(0, 1, 2, 3, 4, 5, 6,
                                                   7, 8, 9, 10),
                     expand = expansion(0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

saveRDS(ss.barplot, "./data/data-gen/training/ss.barplot.RDS")



