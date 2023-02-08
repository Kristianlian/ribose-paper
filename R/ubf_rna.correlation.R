##### UBF-Total RNA correlation analysis

## Author: Kristian Lian

# Packages: 

library(tidyverse);library(ggpubr)

# Data 

west.dat <- readRDS("./data/data-gen/protein/prot.dat.RDS")
west.clean <- readRDS("./data/data-gen/protein/protein_complete.RDS")

rna.dat <- readRDS("./data/data-gen/rna/rna_complete.RDS")

# Ready RNA data
rna.ratio <- read_excel("./data/rna/RNA.raw.xlsx", na = "NA")

code <- read_excel("./data/code_key.xlsx")

# 260/280 Ratio for estimation of sample purity
mean.rat <- rna.ratio %>%
  select(sample, RATIO_R260280_1, RATIO_R260280_2, RATIO_R260280_3, RATIO_R260280_4) %>%
  pivot_longer(names_to = "rep",
               values_to = "ratio",
               cols = RATIO_R260280_1:RATIO_R260280_4) %>%
  group_by(sample) %>%
  #print()
  mutate(mean.ratio = mean(ratio),
         sd.ratio = sd(ratio)) %>%
  print()

# Sort baseline data (T1 and T2) from the two legs into pre, and post biopsy data (T3 and T4) into post, calculates total RNA per wet muscle weight.

rna.dat2 <- rna.dat %>%
  inner_join(code) %>%
  mutate(time = if_else(time %in% c("T1", "T2"), "pre", "post"), 
         time = factor(time, levels = c("pre", "post"))) %>%
  # filter(outlier == "in") %>%
  group_by(subject, time, rep, outlier, technical, biopsy, supplement) %>%
  summarise(weight = mean(weight), 
            RNA = mean(RNA_tot)) %>%
  ungroup() %>%
  mutate(weight.mc = weight/mean(weight)) %>%
  mutate(RNA.weight = RNA / weight) %>%
  print()

# Illustration of individual subjects change in total RNA

rna.red <- rna.dat2 %>%
  filter(technical == "in") %>%
  group_by(subject, time,  supplement) %>%
  summarise(weight = mean(weight), 
            RNA = mean(RNA)) %>%
  mutate(RNA = RNA/weight) %>%
  select(subject, time, supplement, RNA) %>%
  mutate(subject = as.character(subject)) %>%
  print()

## Ready western dat

ubf.red <- west.dat %>%  # One data frame without removing outliers
  select(subject, time, supplement, target, norm.sign) %>%
  filter(target == "ubf") %>%
  select(-target) %>%
  print()

ubf.clean <- west.clean %>%  # One data frame removing outliers
  select(subject, time, supplement, target, norm.sign) %>%
  filter(target == "ubf") %>%
  select(-target) %>%
  print()

## Correlation analysis

joined.dat <- ubf.red %>%
  right_join(rna.red) %>%
  #filter(supplement != "PLACEBO") %>%
  print()

joined.clean <- ubf.clean %>%
  right_join(rna.red) %>%
  print()

## pre-post values correlation
ggscatter(joined.clean, x = "RNA", y = "norm.sign", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total RNA", ylab = "UBF signal")


shapiro.test(joined.clean$norm.sign) 
shapiro.test(joined.clean$RNA) # Does not appear normally distributed?

ggqqplot(joined.clean$norm.sign, ylab = "norm.sign")
ggqqplot(joined.clean$norm.sign, ylab = "RNA")

res <- cor.test(joined.clean$norm.sign, joined.clean$RNA, 
                method = "pearson")
res




