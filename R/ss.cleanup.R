#### Sessions score cleanup

# This script tidies up the RPE (session score, i.e. RPE 1-10 15min post session) data from all sessions.

## Packages
library(readxl)

## Handling the data by creating a new factor called time from timepoint. This factor combines any observation at T1 and T2 to baseline, etc. 
# The code also sorts the order of the factor time, from baseline to session 6, using time = factor(time, levels c()), and sets placebo to be compared to 
# glucose via supplement = factor(supplement, levels = c()).

ss.dat <- read_excel("./data/training/ribose_rpe.xlsx", na = "NA") %>%
  select(subject, timepoint, session.score, supplement) %>%
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
                                                                "post", timepoint))))))) %>%
  mutate(time = factor(time, levels = c("baseline", "session1", "session2", "session3", "session4", "session5", "post")),
         supplement = factor(supplement, levels = c("placebo", "glucose"))) 

saveRDS(ss.dat, "./data/data-gen/training/ss.dat.RDS")
