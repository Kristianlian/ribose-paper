### Analysis of total RNA data
#
#
## Author: KL/DH
#
#
## Project: Ribose
#
#
## Purpose: This script analyses the cleaned up total RNA data by change-score
# comparisons
#
#
## Asscociated scripts: cpep.cleanup.R
#
#
## Packages
library(tidyverse); library(readxl);library(nlme);library(lme4);library(emmeans)

## Data 

rna_dat <- readRDS("./data/data-gen/rna/rna_complete.RDS")

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

rna_dat2 <- rna_dat %>%
  inner_join(code) %>%
  mutate(time = if_else(time %in% c("T1", "T2"), "pre", "post"), 
         time = factor(time, levels = c("pre", "post")), 
         supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
  
## Filter away technical outliers; these are outliers based on known technical artifacts
  # filters away outliers; these are filtered due to large deviance from RNA to muscle weights
 filter(technical == "in", 
        outlier == "in") %>%
  group_by(subject, time, rep, supplement) %>%
  summarise(weight = mean(weight), 
            RNA = mean(RNA_tot)) %>%
  ungroup() %>%
  mutate(weight.mc = (weight - mean(weight)) / sd(weight)) %>%
  mutate(RNA.weight = RNA / weight, 
         ln.RNA.weight = log(RNA.weight)) %>%
  print()

## Save for plotting/stats in UBF-RNA correlation
saveRDS(rna_dat2, "./data/data-gen/rna/rna.dat.RDS")


rna_dat2 %>%
  ggplot(aes(weight, RNA)) + geom_point()



rna_dat2 %>%
  group_by(subject, supplement, time, rep) %>%
  summarise(RNA.weight = mean(RNA.weight)) %>%
  ggplot(aes(time, RNA.weight, group = paste(subject, supplement, rep), color = supplement)) + geom_line()



### Modelling 

# Using the buildmer package we can automate the process of selecting random effects structures in 
# general cases. Here, the most complex model is used as template and reduced to 



library(buildmer)


max_mod <- buildmer(ln.RNA.weight ~ time * supplement + 
          
            (time * supplement|subject),
            
            
            
           data = data.frame(rna_dat2), 
           
           buildmerControl=buildmerControl(direction='order',
                                           args=list(control=lmerControl(optimizer='bobyqa')), 
                                           include = ~ time * supplement +  (1|subject), 
                                           
                                           ddf = "Satterthwaite"))


## Check assumptions of the model 
# mod_assume(max_mod@model)


## Get within condition confidence intervals
models_final <- max_mod@model
summary(models_final)

fold_change <- confint(pairs(emmeans(models_final, specs = ~ time | supplement), reverse = TRUE)) %>%
  data.frame() 

## Get between condition confidence intervals on change

coefs <- cbind(coef(summary(models_final)), confint(models_final, parm = "beta_")) %>%
  data.frame() %>%
  mutate(coef = row.names(.), 
         target = "totalrna") %>%
  data.frame(row.names = NULL) %>%
  dplyr::select(target, coef, estimate = Estimate, se = Std..Error, 
                df, tval = t.value, pval = Pr...t.., lower = X2.5.., upper = X97.5..)



## Save results

saveRDS(list(model = models_final, 
             fold_change = fold_change, 
             coefs = coefs), "./data/data-gen/rna/tot_rna_coefs.RDS")




# Saved for reference

#  # Illustration of individual subjects change in total RNA
#  
#  rna_dat2 %>%
#    filter(technical == "in") %>%
#    group_by(subject, time,  supplement) %>%
#    summarise(weight = mean(weight), 
#              RNA = mean(RNA)) %>%
#    mutate(RNA = RNA/weight) %>%
#    
#    ggplot(aes(time, RNA, group=paste(subject, supplement), 
#               color = supplement)) + 
#    scale_y_continuous(limits = c(400, 1600)) +
#    geom_line() +
#    geom_point()
#  
#  saveRDS(rna_dat2, "./data/data-gen/rna/rna.dat.RDS")
#  
#  
#  # Log-fold change score calculation: We are interested in if the change in GLUCOSE is different from change in 
#  # PLACEBO. 
#  
#  change_dat <- rna_dat2 %>%
#    #  filter(technical == "in", 
#    #        outlier == "in") %>%
#    dplyr::select(subject:rep, supplement, RNA.weight) %>%
#    
#  
#    
#    group_by(subject, time, supplement) %>%
#    summarise(RNA.weight = mean(RNA.weight, na.rm = TRUE)) %>%
#    
#    pivot_wider(names_from = time, 
#                values_from = RNA.weight) %>%
#    ungroup() %>%
#    mutate(change = log(post) - log(pre), 
#           pre = pre - mean(pre, na.rm = TRUE), 
#           supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE")), 
#           leg = paste0(subject,supplement))
#  
#  saveRDS(change_dat, "./data/data-gen/rna/totrna.change.RDS")
#  
#  
#  ### 
#  
#  
#  
#  ## Linear mixed effects model 
#  # The data set is fitted in an ANCOVA model, explaining change based on the condition (supplement), 
#  # accounting for differential baseline values and
#  # multiple sampling of the same subjects. The effect of condition will answer the question.
#  # Adding the (1|subject/leg) fully takes care of duplicate measures in leg.
#  
#  m1 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
#                       data = change_dat)
#  
#  
#  summary(m1)
#  
#  m1x <- lme(change ~ pre + supplement, 
#            random = list(subject = ~ 1), 
#  
#            data = change_dat)
#  
#  
#  VarCorr(lm1)
#  VarCorr(m1)
#  
#  saveRDS(m1, "./data/data-gen/rna/totalrna.model.RDS")
#  
#  plot(m1)
#  
#  summary(m1)
#  summary(lm1)
#  ## Fold-change estimated means
#  # Gets estimated means from the model, these are average increase at pre = 0 (the average pre value).
#  # These are log-fold change values (changeble with the mutate function), reverse transformed with exp() for illustration
#  
#  emm.tot <- confint(emmeans(m1, specs = ~"supplement")) %>%
#    data.frame()
#  
#  saveRDS(emm.tot, "./data/data-gen/rna/emm.tot.RDS")
#  
#  
#  