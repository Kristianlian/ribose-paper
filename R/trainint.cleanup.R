## Training intensity cleanup

# This script tidies up the data for analysis of training intensity (load lifted in % of 1RM) for later analysis

# Packages
library(readxl)

# Data
tot.load.raw <- read_excel("./data/training/ribose_training.xlsx", na = "NA") %>%
  select(subject, timepoint, supplement, exercise, load)


# ## Handling the data by creating a new factor called time from timepoint. This factor combines any observation at T1 and T2 to baseline, etc. 
# The code also sorts the order of the factor time, from baseline to session 6, using time = factor(time, levels c()), and sets placebo to be compared to 
# glucose via supplement = factor(supplement, levels = c()).

# Total 
tot.load <- tot.load.raw %>%
  mutate(time = if_else(timepoint %in% c("T1", "T2"),
                        "baseline",
                        if_else(timepoint %in% c("D3", "D4"),
                                "session2",
                                if_else(timepoint %in% c("D5", "D6"),
                                        "session3",
                                        if_else(timepoint %in% c("D7", "D8"),
                                                "session4",
                                                if_else(timepoint %in% c("D9", "D10"),
                                                        "session5",
                                                        if_else(timepoint %in% c("T3", "T4"),
                                                                "session6", timepoint))))))) %>%
  mutate(time = factor(time, levels = c("baseline", "session1", "session2", "session3", "session4", "session5", "session6")),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) #%>%
  #print()

saveRDS(tot.load, "./data/data-gen/training/tot.load.RDS")




