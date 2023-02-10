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
  summarise(mean.sign = mean(norm.sign, na.rm = TRUE)) %>%
  print()

rna.rdy <- rna.dat %>%
  select(subject, time, outlier, supplement, RNA.weight) %>%
  filter(outlier == "in") %>%
  mutate(subject = as.character(subject)) %>%
  group_by(subject, time, supplement) %>%
  summarise(mean.rna = mean(RNA.weight, na.rm = TRUE)) %>%
  print()

## Correlation analysis

joined.dat <- ubf.rdy %>%
  right_join(rna.rdy) %>%
  #filter(supplement != "GLUCOSE") %>%
  print()


## pre-post values correlation
ggscatter(joined.dat, x = "mean.sign", y = "mean.rna", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "UBF", ylab = "Total RNA")
sprintf("%.5f", 3.7e-06)


stp.ubf <- shapiro.test(joined.dat$mean.sign) 
sprintf("%.5f", 1.667e-05)
shapiro.test(joined.dat$mean.rna) 

ggqqplot(joined.dat$mean.sign, ylab = "mean.sign")
ggqqplot(joined.dat$mean.rna, ylab = "mean.rna")

correlation <- cor.test(joined.dat$mean.sign, joined.dat$mean.rna, 
                method = "pearson")
correlation

corr.p <- sprintf("%.5f", 3.694e-05)
corr.p

