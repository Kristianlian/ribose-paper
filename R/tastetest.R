###### Taste test
#
#
## Author: KL
#
#
## Purpose:


## Packages
library(readxl); library(tidyverse)

## Data

dat <- read_excel("./data/tests/ribose_tastetest.xlsx")

score.dat <- read_excel("./data/tests/ribose_tastescore.xlsx")


mean.dat <- dat %>%
  select(subject, content, glass, points, guess) %>%
  group_by(content) %>%
  summarise(mean = mean(points),
            sd = sd(points)) %>%
  print()

mean.score <- score.dat %>%
  select(subject, score) %>%
  summarise(mean = mean(score),
            sd = sd(score)) %>%
  print()

# One point was awarded per correct answer, with a maximum of 4 points per participant. On average, the participants had a score of 2 points (2.08 ± 1.24) out 
# of 4, making the 
