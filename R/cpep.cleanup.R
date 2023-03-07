### Cleanup of c-peptide data
#
#
## Author: KL
#
#
## Project: Ribose
#
#
## Purpose: This script cleans up raw c-peptide data, and saves the RDS "cpep.clean.RDS"
#
#
## Packages
library(readxl); library(tidyverse)

## Data

ins.dat <- read_excel("./data/glucose/ribose.insulin.xlsx", na = "NA")


code <- read_excel("data/code_key.xlsx", na = "NA") %>%
  select(-leg) %>%
  print()


## Data handling

ins.dat$sample.time <- as.character(ins.dat$sample.time)

ins.dat2 <- ins.dat %>%
  inner_join(code) %>%
  mutate(sample.time = as.character(sample.time)) %>%
  mutate(time = if_else(sample.time == "0",
                        "pre",
                        if_else(sample.time == "0.1",
                                "baseline",
                                if_else(sample.time == "45",
                                        "min90",
                                        if_else(sample.time == "90",
                                                "min120",
                                                if_else(sample.time == "120",
                                                        "min150",
                                                        if_else(sample.time == "270",
                                                                "min270", sample.time))))))) %>%
  mutate(time = factor(time, levels = c("pre", "baseline", "min90", "min120", "min150", "min270")),
         c.pep = as.numeric(c.pep)) %>%
  print()

saveRDS(ins.dat2, "./data/data-gen/glucose/cpep.clean.RDS")



