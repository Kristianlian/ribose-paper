##### Western analysis
#
#
## Purpose: The purpose of this script is to analyse the cleaned up western data
#
# Author: KL/DH
#
# Packages
library(tidyverse); library(emmeans); library(lme4); library(buildmer)

#
# Data 
# from the script: western_import.R
prot.dat <- readRDS("./data/data-gen/protein/prot.dat.RDS") %>%
  mutate(sample.id = paste0(gel.sorted, sample.id)) %>%
  print()


# Western data is analyzed as "maximal" random effects models  
#



targets <- prot.dat %>%
  ungroup() %>% 
  distinct(target) %>%
  pull(target)




coefs <- list()
fold_change <- list()
models_final <- list()



for(i in 1:length(targets)) {
  
  dat_target <- prot.dat %>%
    filter(target == targets[i]) %>%
    mutate(ln.norm.sign = log(norm.sign))
  
  
  ## Using buildmer to get the maximal model
  max_model <- buildmer(ln.norm.sign ~ time * supplement + 
                          (time*supplement|subject),
                        
                        data = dat_target, 
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



prot_models <- list(prot_models = models_final, 
                              coefs = bind_rows(coefs),
                              fold_change = bind_rows(fold_change))

saveRDS(prot_models, "./data/data-gen/protein/protein_lmer_models.RDS")


# Data and models


prot_models <- readRDS("./data/data-gen/protein/protein_lmer_models.RDS")

model <- prot_models$prot_models$cmyc


## extract estimated change
west_estimate <- function(model, p = FALSE) {
  
  # Return absolute values 
  
  
  if(isFALSE(p)){
    
    est <- round(100 * (exp(coef(summary(model))[4,1])-1),0)
    
    return(est)
  }
  
  if(p){
    
    pval <- sprintf("%.3f", coef(summary(model))[4,5])
    
    return(pval)
  }
  
  
}

stat_west <- lapply(prot_models$prot_models, west_estimate)

saveRDS(stat_west, "./data/data-gen/protein/stat_west.RDS")


p_west <- unlist(lapply(prot_models$prot_models, west_estimate, p = TRUE))

saveRDS(p_west, "./data/data-gen/protein/p_west.RDS")






# Western blot data analyzed as change scores -- 
# This accounts for regression towards the mean in change-calculations
# and a fully nested model can be fitted to account for the duplicate 
# measures on the leg level.
# Note that it is unclear how to deal with pairing of post/diff values and the covariate pre as
# these are not linked. Aggregating throws away information that could be used as above in a 
# mixed model with random slopes. The solution below is therefore kept here only for reference. 

# Prepare the data
#  prot.change <- prot.dat %>%
#    dplyr::select(gel, subject, leg, time, target, supplement, norm.sign) %>%
#    
#    group_by(subject, leg, time, target, supplement) %>%
#    summarise(norm.sign = mean(norm.sign)) %>%
#    
#   
#    pivot_wider(names_from = time, values_from = norm.sign) %>%
#    
#    mutate(change = log(post) - log(pre)) %>%
#  
#    ungroup() %>%
#    
#    print()
#  
#  
#  
#  ## Switching to lme to be able to account for heteroscedasticity
#  
#  # C-myc
#  cmyc.mod <- lme(change ~ pre + supplement, 
#                  random = list(subject = ~ 1),
#             #    weights = varExp(), # varExp can "normalize" residuals if 
#                 control=list(msMaxIter=2000,
#                              opt = "nloptwrap",msVerbose=TRUE), 
#                 method = "REML", 
#                             data = filter(prot.change, target == "cmyc"))
#  
#  
#  m <- lme4::lmer(change ~ scale(pre) + supplement + (1|subject) + (1|gel), 
#             data = filter(prot.change, target == "cmyc"))
#  
#  
#  summary(cmyc.mod)
#  plot(cmyc.mod)
#  
#  cmyc.mod2 <- lme(change ~ scale(pre) + supplement, 
#                  random = list(subject = ~ 1),
#                #  weights = varExp(),
#                  control = lmeControl(maxIter = 2000, 
#                                       opt = "optim", method = "BFGS"),
#                  data = filter(prot.change, target == "cmyc"))
#  # rps6
#  rps6.mod <- lme(change ~ scale(pre) + supplement, 
#                  random = list(subject = ~ 1),
#                  weights = varExp(), # varExp can "normalize" residuals if 
#                  control = lmeControl(maxIter = 2000, 
#                                       opt = "optim", method = "BFGS"),
#                  data = filter(prot.change, target == "rps6"))
#  
#  rps6.mod2 <- lme(change ~ scale(pre) + supplement, 
#                   random = list(subject = ~ 1),
#                   #  weights = varExp(),
#                   control = lmeControl(maxIter = 2000, 
#                                        opt = "optim", method = "BFGS"),
#                   data = filter(prot.change, target == "rps6"))
#  # ubf
#  ubf.mod <- lme(change ~ scale(pre) + supplement, 
#                  random = list(subject = ~ 1),
#                  weights = varExp(), # varExp can "normalize" residuals if 
#                  control = lmeControl(maxIter = 2000, 
#                                       opt = "optim", method = "BFGS"),
#                  data = filter(prot.change, target == "ubf"))
#  
#  ubf.mod2 <- lme(change ~ scale(pre) + supplement, 
#                   random = list(subject = ~ 1),
#                   #  weights = varExp(),
#                   control = lmeControl(maxIter = 2000, 
#                                        opt = "optim", method = "BFGS"),
#                   data = filter(prot.change, target == "ubf"))
#  
#  
#  ## Test for including varExp in weights
#  anova(cmyc.mod, cmyc.mod2) # No evidence for better fit with varExp
#  
#  gridExtra::grid.arrange(plot(cmyc.mod, main = "No weights"), plot(cmyc.mod2, main = "varExp(form = fitted(.)"), 
#                          ncol = 2)
#  
#  anova(rps6.mod, rps6.mod2)
#  anova(ubf.mod, ubf.mod2)
#  
#  # Both rps6 and ubf looks ok
#  gridExtra::grid.arrange(plot(rps6.mod2, main = "RPS6"), plot(ubf.mod2, main = "UBF"), 
#                          ncol = 2)
#  
#  plot(rps6.mod2)
#  summary(rps6.mod)
#  summary(ubf.mod2)
#  ## Save models 
#  
#  prot_models <- list(cmyc = cmyc.mod2, 
#                      ubf = ubf.mod2, 
#                      rps6 = rps6.mod)
#  
#  saveRDS(prot_models, "./data/data-gen/protein/protein_change_models.RDS")
#  
#  
#    
#  
#  ### Modelling target signal per supplement 
#  # These models try to explain the normalised signal by time, supplement and time+supplement (?). The random = list command creates an individual 
#  # intercept for the included (e.g., subject, gel.sample). 
#  
#  ## c-Myc model
#  
#  # Untransformed
#  cmyc.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
#                  random = list(subject = ~ 1, leg = ~ 1),
#                  
#                  weights = varExp(),
#                  control = lmeControl(maxIter = 500),
#                  
#                  data = filter(prot.dat, target == "cmyc"))
#  
#  summary(cmyc.mod)
#  plot(cmyc.mod, resid(., type = "p") ~ fitted(.))
#  
#  
#  # Log-transformed
#  cmyc.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
#                   random = list(subject = ~ 1, leg = ~ 1),
#                   data = filter(prot.dat, target == "cmyc"))
#  
#  summary(cmyc.lmod)
#  plot(cmyc.lmod, resid(., type = "p") ~ fitted(.))
#  
#  
#  
#  
#  ## UBF model
#  # Untransformed
#  ubf.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
#                 random = list(subject = ~ 1),
#                 data = filter(prot.dat, target == "ubf"))
#  
#  
#  summary(ubf.mod)
#  plot(ubf.mod, resid(., type = "p") ~ fitted(.))
#  
#  # Log-transformed
#  ubf.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
#                  random = list(subject = ~ 1),
#                  data = filter(prot.dat, target == "ubf"))
#  
#  saveRDS(ubf.lmod, ("./data/data-gen/protein/protein.model.RDS"))
#  
#  summary(ubf.lmod)
#  plot(ubf.lmod, resid(., type = "p") ~ fitted(.))
#  
#  
#  # RPS6 model
#  # Untransformed
#  rps6.mod <- lme(norm.sign ~ time + supplement + time:supplement, 
#                   random = list(subject = ~ 1),
#                   data = filter(prot.dat, target == "rps6"))
#  
#  summary(rps6.mod)
#  
#  plot(rps6.mod, resid(., type = "p") ~ fitted(.))
#  
#  
#  # Log-transformed
#  rps6.lmod <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
#            random = list(subject = ~ 1),
#            data = filter(prot.dat, target == "rps6"))
#  
#  summary(rps6.lmod)
#  
#  plot(rps6.lmod, resid(., type = "p") ~ fitted(.))
#  
#  
#  ### Getting emmeans from the models
#  
#  ## c-Myc emmeans
#  # Untransformed
#  cmyc.emm <- emmeans(cmyc.mod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  
#  cmyc.emm %>%  
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  
#  saveRDS(cmyc.emm, "./data/data-gen/protein/cmyc.emm.RDS")
#  
#  # Log-transformed
#  cmyc.lemm <- emmeans(cmyc.lmod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  
#  cmyc.lemm %>%  
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  
#  saveRDS(cmyc.lemm, "./data/data-gen/protein/cmyc.lemm.RDS")
#  
#  
#  ## UBF emmeans
#  # Untransformed
#  ubf.emm <- emmeans(ubf.mod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  ubf.emm %>%
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  saveRDS(ubf.emm, "./data/data-gen/protein/ubf.emm.RDS")
#  
#  # Log-transformed
#  ubf.lemm <- emmeans(ubf.lmod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  ubf.lemm %>%
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  saveRDS(ubf.lemm, "./data/data-gen/protein/ubf.lemm.RDS")
#  
#  
#  ## RPS6
#  # Untransformed
#  rps6.emm <- emmeans(rps6.mod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  rps6.emm %>%
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  saveRDS(rps6.emm, "./data/data-gen/protein/rps6.emm.RDS")
#  
#  # Log-transformed
#  rps6.lemm <- emmeans(rps6.lmod, specs = ~ time|supplement) %>%
#    data.frame()
#  
#  rps6.lemm %>%
#    mutate(time = factor(time, levels = c("pre", "post"))) %>%
#    ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#    geom_line() + 
#    geom_point(shape= 21) 
#  
#  saveRDS(rps6.lemm, "./data/data-gen/protein/rps6.lemm.RDS")
#  
#  
#  
#  

