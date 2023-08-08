##### UBF-Total RNA correlation analysis

## Author: Kristian Lian

# Packages: 

library(tidyverse);library(ggpubr)

# Data 
west.dat <- readRDS("./data/data-gen/protein/prot.datc.RDS") # Quantified and cleaned western data,
# (outliers marked) normalised by pool

rna.dat <- readRDS("./data/data-gen/rna/rna.dat.RDS") # Quantified and cleaned (outliers marked), 
# total RNA data, normalised by wet muscle weight


## Prepare data frames for joining

ubf.rdy <- west.dat %>%  # One data frame without outliers
  select(subject, time, outlier, supplement, target, norm.sign) %>%
  filter(target == "ubf",
         outlier == "in") %>%
  select(-target) %>%
  group_by(subject, time, supplement) %>%
  summarise(mean.sign = mean(norm.sign, na.rm = TRUE)) 


rna.rdy <- rna.dat %>%
  select(subject, time, supplement, RNA.weight) %>%
  
  mutate(subject = as.character(subject)) %>%
  group_by(subject, time, supplement) %>%
  summarise(mean.rna = mean(RNA.weight, na.rm = TRUE)) 

## Correlation analysis

joined.dat <- ubf.rdy %>%
  right_join(rna.rdy) %>%
  #filter(supplement != "GLUCOSE") %>%
  
  mutate(sd.ubf = (mean.sign - mean(mean.sign, na.rm = TRUE )/sd(mean.sign, na.rm = TRUE))) 


library(lme4)

corr.mod <- lmerTest::lmer(mean.rna ~ mean.sign + time + (1|subject), 
                           data = joined.dat, 
                           na.action = na.omit)

corr.sum <- summary(corr.mod)
corr.res <- plot(corr.mod)

corr.lmod <- lmerTest::lmer(log(mean.rna) ~ log(mean.sign) + time + (1|subject), 
                            
                            data = joined.dat, 
                            na.action = na.omit)


corr.lsum <- summary(corr.lmod)
corr.lres <- plot(corr.lmod)

corrcoef <- coef(summary(corr.mod))
corrp <- sprintf("%.3f", coef(summary(corr.lmod))[2,5])
  
  #round(corrcoef[2,5], 3)
saveRDS(corrp, "./data/data-gen/protein/corrp.RDS")

## Percentage increase in RNA per 1 SD uinit increase in UBF using log-model to show relative increase
corr_incr <- round(100 * (exp(coef(summary(corr.lmod))[2,1])-1), 0)
saveRDS(corr_incr, "./data/data-gen/protein/corr_incr.RDS")


########################### OLD CODE BELOW ###################################



## Data 
#west.dat <- readRDS("./data/data-gen/protein/prot.datc.RDS") # Quantified and cleaned western data,
#                                                               # (outliers marked) normalised by pool
#
#rna.dat <- readRDS("./data/data-gen/rna/rna.dat.RDS") # Quantified and cleaned (outliers marked), 
#                                                      # total RNA data, normalised by wet muscle weight
#
#
### Prepare data frames for joining
#
#ubf.rdy <- west.dat %>%  # One data frame without outliers
#  select(subject, time, outlier, supplement, target, norm.sign, tpl) %>%
#  filter(target == "ubf",
#         outlier == "in") %>%
#  select(-target) %>%
#  mutate(norm.sign = norm.sign / tpl) %>%
#  #print()
#  group_by(subject, time, supplement) %>%
#  summarise(mean.sign = mean(norm.sign, na.rm = TRUE)) %>%
#  print()
#
#saveRDS(ubf.rdy, "./data/data-gen/protein/ubf.rdy.RDS")
#
#rna.rdy <- rna.dat %>%
#  select(subject, time, supplement, RNA.weight) %>%
#  mutate(subject = as.character(subject)) %>%
#  group_by(subject, time, supplement) %>%
#  summarise(mean.rna = mean(RNA.weight, na.rm = TRUE)) %>%
#  print()
#
#saveRDS(rna.rdy, "./data/data-gen/protein/rna.rdy.RDS")
#
### Correlation analysis
#
#joined.dat <- ubf.rdy %>%
#  right_join(rna.rdy) %>%
#  #filter(supplement != "GLUCOSE") %>%
#  
#  mutate(sd.ubf = (mean.sign - mean(mean.sign, na.rm = TRUE )/sd(mean.sign, na.rm = TRUE))) %>%
#  print()
#
#
#library(nlme)
#
#m <- lme(mean.rna ~ mean.sign + time , 
#         random = list(subject = ~ 1), 
#          data = joined.dat, 
#         na.action = na.omit)
#
#saveRDS(m, "./data/data-gen/protein/correlation.model.RDS")
#
#summary(m)
#
#coef(summary(m))
#
#plot(m)
#
#
#
#joined.dat %>%
#  ggplot(aes(mean.sign, mean.rna, color = time)) + geom_point() +
#  geom_smooth(method = "lm")
#
#
######################### c-Myc below ######################
#
### Prepare data frames for joining
#
#cmyc.rdy <- west.dat %>%  # One data frame without outliers
#  select(subject, time, outlier, supplement, target, norm.sign, tpl) %>%
#  filter(target == "cmyc",
#         outlier == "in") %>%
#  select(-target) %>%
#  mutate(norm.sign = norm.sign / tpl) %>%
#  #print()
#  group_by(subject, time, supplement) %>%
#  summarise(mean.sign = mean(norm.sign, na.rm = TRUE)) %>%
#  print()
#
#
#rna.rdy <- rna.dat %>%
#  select(subject, time, outlier, supplement, RNA.weight) %>%
#  filter(outlier == "in") %>%
#  mutate(subject = as.character(subject)) %>%
#  group_by(subject, time, supplement) %>%
#  summarise(mean.rna = mean(RNA.weight, na.rm = TRUE)) %>%
#  print()
#
#
### Correlation analysis
#
#joined.dat <- cmyc.rdy %>%
#  right_join(rna.rdy) %>%
#  #filter(supplement != "GLUCOSE") %>%
#  
#  mutate(sd.cmyc = (mean.sign - mean(mean.sign, na.rm = TRUE )/sd(mean.sign, na.rm = TRUE))) %>%
#  print()
#
#
#library(nlme)
#
#m <- lme(mean.rna ~ mean.sign + time , 
#         random = list(subject = ~ 1), 
#         data = joined.dat, 
#         na.action = na.omit)
#
#summary(m)
#
#coef(summary(m))
#
#plot(m)
#
#
#
#joined.dat %>%
#  ggplot(aes(mean.sign, mean.rna, color = time)) + geom_point() +
#  geom_smooth(method = "lm")
#
#
######################### rpS6 below ######################
#
### Prepare data frames for joining
#
#s6.rdy <- west.dat %>%  # One data frame without outliers
#  select(subject, time, outlier, supplement, target, norm.sign, tpl) %>%
#  filter(target == "rps6",
#         outlier == "in") %>%
#  select(-target) %>%
#  mutate(norm.sign = norm.sign / tpl) %>%
#  #print()
#  group_by(subject, time, supplement) %>%
#  summarise(mean.sign = mean(norm.sign, na.rm = TRUE)) %>%
#  print()
#
#
#rna.rdy <- rna.dat %>%
#  select(subject, time, outlier, supplement, RNA.weight) %>%
#  filter(outlier == "in") %>%
#  mutate(subject = as.character(subject)) %>%
#  group_by(subject, time, supplement) %>%
#  summarise(mean.rna = mean(RNA.weight, na.rm = TRUE)) %>%
#  print()
#
#
### Correlation analysis
#
#joined.dat <- s6.rdy %>%
#  right_join(rna.rdy) %>%
#  #filter(supplement != "GLUCOSE") %>%
#  
#  mutate(sd.rps6 = (mean.sign - mean(mean.sign, na.rm = TRUE )/sd(mean.sign, na.rm = TRUE))) %>%
#  print()
#
#
#library(nlme)
#
#m <- lme(mean.rna ~ mean.sign + time , 
#         random = list(subject = ~ 1), 
#         data = joined.dat, 
#         na.action = na.omit)
#
#summary(m)
#
#coef(summary(m))
#
#plot(m)
#
#
#
#joined.dat %>%
#  ggplot(aes(mean.sign, mean.rna, color = time)) + geom_point() +
#  geom_smooth(method = "lm")





