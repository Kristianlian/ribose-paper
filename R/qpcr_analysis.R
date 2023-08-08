### Analysis of qpcr data
#
#
## Author: DH/KL
#
#
## Project: Ribose
#
#
## Purpose: This script analyses the cleaned up qpcr data by a maximal mixed-model
#
#
## Asscociated scripts: qpcr.import.R
#
#
## Packages

library(tidyverse)
library(readxl)
library(lmerTest)
library(buildmer)


# Load data

qpcr_data <- readRDS("./data/data-gen/rna/qpcr-data.RDS")


samples <- read_excel("./data/rna/RNA.raw.xlsx") %>%
  dplyr::select(subject, time_rep, sample, weight) %>%
  print()

code_key <- read_excel("./data/code_key.xlsx") %>%
  dplyr::select(subject, supplement, time) %>%
  print()


# Calculate normalization factor based on lambda

nf <- qpcr_data %>%
  mutate(sample = as.numeric(sample)) %>% 
  inner_join(samples) %>%
  separate(time_rep, into = c("time","rep"), sep = "rna") %>%
  mutate(rep = paste0("cdna", rep)) %>%
  inner_join(code_key) %>% 
  mutate(time = if_else(time %in% c("T1", "T2"), "Pre", "Post"), 
         time = factor(time, levels = c("Pre", "Post")), 
         expr = eff ^ -cq) %>%
  
  filter(target %in% c("Lambda F2R2", "Lambda F3R3"), 
         cq < 35) %>%
  separate(target, into = c("target", "nf_primer")) %>%
  
  mutate(nf.w = expr * weight, 
         nf.w = nf.w / max(nf.w)) %>%
  
  group_by(sample, subject, time, rep, supplement) %>%
  summarise(nf.w = mean(nf.w)) %>%
  print()
  

qpcr_data2 <- qpcr_data %>%
  mutate(sample = as.numeric(sample)) %>% 
  inner_join(samples) %>%
  inner_join(nf) %>%
  separate(time_rep, into = c("time","rep"), sep = "rna") %>%
  mutate(rep = paste0("cdna", rep)) %>%
  inner_join(code_key) %>% 
  mutate(time = if_else(time %in% c("T1", "T2"), "Pre", "Post"), 
         time = factor(time, levels = c("Pre", "Post")), 
         expr = eff ^ -cq, 
         nf.expr = log(expr / nf.w)) %>%
  filter(!(target %in% c("Lambda F2R2", "Lambda F3R3"))) %>% 
  mutate(target = gsub("rRNA", "", target),
         target = gsub("  ", " ", target),
         target = paste0("trg_", target)) %>%
  separate(target, into = c("target", "primer"), sep = " ") %>%
  print()


### Save raw data 
saveRDS(qpcr_data2, "./data/data-gen/rna/qpcr_data2.RDS")



qpcr_data2 %>%
  filter(target == target[1]) %>%
  
  ggplot(aes(time, nf.expr, color = supplement)) + geom_point() + 
  facet_wrap(~ subject)




### Modelling


targets <- distinct(qpcr_data2, target) %>%
  filter(target %in% c("trg_18s", "trg_28s", "trg_47s", "trg_5.8s", "trg_5s")) %>%
  pull(target)




coefs <- list()
fold_change <- list()
models_final <- list()



for(i in 1:length(targets)) {
  
  ## Using buildmer to get the maximal model
  max_model <- buildmer(nf.expr ~ time * supplement + 
                  (time*supplement|subject),
                
                data = filter(qpcr_data2, target == targets[i]), 
                buildmerControl=buildmerControl(direction='order',
                                                args=list(control=lmerControl(optimizer='bobyqa')), 
                                                include = ~ time * supplement +  (1|subject), 
                                                
                                                ddf = "Satterthwaite"))
  
  
  
  
  
  ## Get within condition confidence intervals
  models_final[[i]] <- max_model@model
  
  ## Convert to lmerTest model by extracting from the buildmer object
  model <- max_model@model
  

  names(models_final)[i] <- targets[i]
  

  fold_change[[i]] <- confint(pairs(emmeans(model, specs = ~ time | supplement), reverse = TRUE)) %>%
    data.frame() %>%
    mutate(target = targets[i])
  
  ## Get between condition confidence intervals on change

  coefs[[i]] <- cbind(coef(summary(model)), confint(model, parm = "beta_")) %>%
    data.frame() %>%
    mutate(coef = row.names(.), 
           target = targets[i]) %>%
    data.frame(row.names = NULL) %>%
    dplyr::select(target, coef, estimate = Estimate, se = Std..Error, 
                  df, tval = t.value, pval = Pr...t.., lower = X2.5.., upper = X97.5..)

 

} 



rrna_analysis_results <- list(rrna_models = models_final, 
     coefs = bind_rows(coefs),
     fold_change = bind_rows(fold_change))



saveRDS(rrna_analysis_results, "./data/data-gen/rna/rrna_analysis_results.RDS")


### Function for inline reporting of total RNA and rRNA data


stat_fun_rna <- function(model) {
  
  coefs <- coef(summary(model))
  ci <- confint(model)
  meandiff <- sprintf("%.1f", 100 * (exp(coefs[4,1]) -1))
  cilower <- sprintf("%.1f",100 * (exp(ci[6,1]) -1))
  ciupper <- sprintf("%.1f",100 * (exp(ci[6,2]) -1))
  pval <- sprintf("%.3f",coefs[4,5])
  
  
  stats <- paste0(meandiff, "%, [", cilower, ", ", ciupper,"], *p* = ", pval)
  
  stats
}

# Retrieve models from rds

rrna_analysis_results <- readRDS("./data/data-gen/rna/rrna_analysis_results.RDS")


# Get estimated means from the model, these are average baseline-corrected log-fold differences 
# at post.

# Applying the stat_fun_rna function to models
stat_rrna <- lapply(rrna_analysis_results$rrna_models, 
                    stat_fun_rna)

saveRDS(stat_rrna, "./data/data-gen/rna/stat_rrna.RDS")



### Old code from here ....

# 
# 
# 
# qdat <- qpcr_data2 %>%
#   mutate(target = gsub("rRNA", "", target),
#          target = gsub("  ", " ", target),
#          target = paste0("trg_", target)) %>%
#   separate(target, into = c("target", "primer"), sep = " ") %>%
#   
#   # Create a weight-normalized variable
#   mutate(nf.expr = log(expr / nf.w), 
#          nf.w = scale(nf.w),
#          # Technical random effect
#          technical = paste(subject, time, supplement, rep, sep = "_"),
#          biological = paste0("S", sample)) %>%
#   filter(!(target %in% c("trg_MHC1", "trg_MHC2A", "trg_MHC2X"))) %>%
# 
#   
#   print()
# 
# saveRDS(qdat, "./data/data-gen/rna/qclean.RDS")
# 
# ## Change data
# rrna18 <- qdat %>%
#   filter(target == "trg_18s") %>%
#   print()
# 
# rrna28 <- qdat %>%
#   filter(target == "trg_28s") %>%
#   print()
# 
# rrna5.8 <- qdat %>%
#   filter(target == "trg_5.8s") %>%
#   print()
# 
# rrna5 <- qdat %>%
#   filter(target == "trg_5s") %>%
#   print()
# 
# rrna47 <- qdat %>%
#   filter(target == "trg_47s") %>%
#   print()
# 
# 
# # Change scores
# change.18 <- rrna18 %>%
#   dplyr::select(subject, time, rep, nf.expr, supplement) %>%
#   group_by(subject, time, supplement) %>%
#   summarise(nf.expr = mean(nf.expr, na.rm = TRUE)) %>%
#   pivot_wider(names_from = time,
#               values_from = nf.expr) %>%
#   ungroup() %>%
#   # print()
#   mutate(change = Post-Pre,
#          pre = Pre - mean(Pre, na.rm = TRUE),
#          supplement = factor(supplement, levels = c("PLACEBO", "GLUCOSE"))) %>%
#   print()
# 
# # Create model: 
# # Needs to have an intercept per participant (mixed model)
# # Control for pre values.
# 
# m1 <- lmerTest::lmer(change ~ pre + supplement + (1|subject), 
#                      data = change.18)
# 
# plot(m1)
# 
# summary(m1)
# 
# 
# 
# 
# 