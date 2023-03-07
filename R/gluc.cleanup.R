### Cleanup of glucose measurement data
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script cleans up raw glucose data, and saves the RDS "gluc.clean.RDS"
#
#
## Comments: 
# Participant 101, 102 and 103 did not get baseline samples at T3 and T4, therefore,
# baseline values from T1 was used as baseline for T3 and T4
#
#
## Packages

library(readxl); library(tidyverse)

## Data

gluc.dat <- read_excel("./data/glucose/fingerstick.xlsx", na = "NA")

## Data handling

gluc.dat$sample_time <- as.character(gluc.dat$sample_time)

gluc.clean <- gluc.dat %>%
  mutate(time = if_else(sample_time == "0",
                        "baseline",
                        if_else(sample_time == "45",
                                "min45",
                                if_else(sample_time == "90",
                                        "min90",
                                        if_else(sample_time == "120",
                                                "min120",
                                                if_else(sample_time == "135",
                                                        "min135",
                                                        if_else(sample_time == "150",
                                                                "min150",
                                                                if_else(sample_time == "270",
                                                                        "min270", sample_time))))))),
         time = factor(time, levels = c("baseline", "min45", "min90", "min120", "min135", "min150", "min270"))) %>%
  mutate(glu = as.numeric(glu),
         lak = as.numeric(lak)) 

saveRDS(gluc.clean, "./data/data-gen/glucose/gluc.clean.RDS")


