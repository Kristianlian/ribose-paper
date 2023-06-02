##-------------------------------------
## RNA-cleanup.R
##
## Title: RNA cleanup
## Purpose: Filter away bad RNA estimates based on RNA/weight relationship
## Author: DH/KL
##
##
##
##-------------------------------------
## Notes:
# Filter outliers by model log(rna) ~ muscle weight.
# Set threshold for filtering by adjusting threshold for standardized
# residuals. A sensible threshold is 3 sd's from the modeled relationship
# between RNA and muscle weight, taking training status into account.
#
#
#
#
#
## ------------------------------------


library(tidyverse); library(readxl); library(nlme)

# A data frame with technical outliers

outlier_tech <- data.frame(subject = 
                             c(101,
                               102,
                               102,
                               109,
                               109,
                               112,
                               112,
                               113,
                               113), 
                           
                           time = 
                             c("T3",
                               "T3",
                               "T4",
                               "T1",
                               "T2",
                               "T1",
                               "T2",
                               "T2",
                               "T2"),       
                           rep = c("2",
                                   "1",
                                   "1",
                                   "1",
                                   "2",
                                   "1",
                                   "2",
                                   "1",
                                   "2"), 
                           technical = "out")

## Load data
dat <- read_excel("./data/rna/RNA_raw.xlsx") %>%
  pivot_longer(names_to = "variable", 
               values_to = "value", 
               cols = RATIO_R260280_1:RNA_conc_4) %>%
  separate(variable, into = c("Type", "subtype", "measurement")) %>%
  mutate(Type = if_else(Type == "RNA", Type, subtype)) %>%
  select(-subtype) %>%
  pivot_wider(names_from = Type, 
              values_from = value) %>%
  mutate(RNA_tot = RNA * 2 * eluate) %>%
  separate(time_rep, into = c("time", "rep"), sep = "rna") %>%
  mutate(trained = if_else(time %in% c("T3", "T4"), "trained", "untrained"), 
         biopsy = paste0(subject, time)) %>%
  left_join(outlier_tech) %>%
  mutate(technical = if_else(is.na(technical), "in", technical)) %>%
  print()

# Plot RNA vs. weight
dat %>%
  ggplot(aes(log(weight), log(RNA_tot), color = trained)) + geom_point() + geom_smooth(method = "lm")


# Model RNA to weight relationship using a linear model.
# training status is expected to contribute to total RNA concentrations.

m <- lm(log(RNA_tot) ~ log(weight) + trained,
        data = dat)


summary(m)

# Calculate residuals
# These are raw residuals, they are standardized below.
dat$resid <- resid(m)

# Plot the effect of outlier detection
dat %>%
  mutate(resid = resid/sd(resid), 
         outlier = if_else(abs(resid) > 3, "out", "in")) %>%
  ggplot(aes(weight, log(RNA_tot), color = outlier)) +
  geom_point() +
  geom_smooth(method = "lm", aes(color = NULL))

# Check the effect of outlier detection on included samples
dat %>%
  mutate(resid = resid/sd(resid), 
         outlier = if_else(abs(resid) > 3, "out", "in")) %>%
  filter(outlier == "in") %>%
  group_by(subject) %>%
  summarise(n = n()/32)

# Save the complete data set

dat_complete <- dat %>%
  mutate(resid = resid/sd(resid), 
         outlier = if_else(abs(resid) > 3, "out", "in")) %>%
  print()


saveRDS(dat_complete, "./data/data-gen/rna/rna_complete.RDS")

