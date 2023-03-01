###### Western analysis outlier check

# Author: KL

# Packages
library(tidyverse); library(nlme); library(emmeans)

# Data

prot.dat <- readRDS("./data/data-gen/protein/prot.dat.RDS") # from the script: western.import.R


## Visualising individual changes with ggplot
prot.dat %>%
  #filter(gel.sorted == "A") %>%
  #filter(gel.sorted == "B") %>%
  ggplot(aes(time, log(norm.sign), group=paste(subject, supplement),
             color = supplement)) +
  geom_line() +
  geom_point() +
  facet_wrap(~target)


### Plotting target signal on duplicate A and B

prot.dat %>%
  
  dplyr::select(gel.sorted, sample.id, target, norm.sign) %>%
  pivot_wider(names_from = gel.sorted, values_from = norm.sign) %>%
  ggplot(aes(log(A), log(B))) + geom_point() + facet_wrap(~target)

### Modelling target signal per supplement 
# These models try to explain the normalised signal by time, supplement and time+supplement (?). The random = list command creates an individual 
# intercept for the included (e.g., subject, gel.sample). 

# c-Myc model

m0 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = prot.dat)

# Calculate residuals
# Thjese are raw residuals, they are standardized below.
prot.dat$resid <- resid(m0)


## Outlier check
prot.dat %>%
  mutate(resid = resid/sd(resid), 
         outlier = if_else(resid < -2, "out", "in")) %>%
  filter(outlier == "in") %>%
  group_by(subject) %>%
  summarise(n = n()/32)

clean.dat <- prot.dat %>%
  mutate(resid = resid/sd(resid), 
         outlier = if_else(resid < -2, "out", "in")) %>%
  #filter(outlier != "out") %>%
  print()

saveRDS(clean.dat, "./data/data-gen/protein/prot.datc.RDS")

## Visualising individual changes with ggplot
clean.dat %>%
  #filter(gel.sorted == "A") %>%
  #filter(gel.sorted == "B") %>%
  filter(target == "ubf") %>%
  ggplot(aes(time, log(norm.sign), group=paste(gel.sorted, subject, supplement),
             color = supplement)) +
  geom_line() +
  geom_point() +
  facet_wrap(~target)


### Plotting target signal on duplicate A and B

clean.dat %>%
  
  dplyr::select(gel.sorted, sample.id, target, norm.sign) %>%
  pivot_wider(names_from = gel.sorted, values_from = norm.sign) %>%
  ggplot(aes(log(A), log(B))) + geom_point() + facet_wrap(~target)

################################
## Modelling
# c-Myc model

m0 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(clean.dat, target == "cmyc"))

plot(m0)
summary(m0)

# UBF model

m1 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(clean.dat, target == "ubf"))
plot(m1)
summary(m1)

# rpS6 model

m2 <- lme(log(norm.sign) ~ time + supplement + time:supplement, 
          random = list(subject = ~ 1),
          data = filter(clean.dat, target == "rps6"))
plot(m2)
summary(m2)


### Getting emmeans from the models

# c-Myc emmeans

cmyc.emm <- emmeans(m0, specs = ~ time|supplement) %>%
  data.frame()

#cmyc.emm2 <- emmeans(m0.2, specs = ~ time|supplement) %>%
#data.frame()

cmyc.emm %>%  
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

#cmyc.emm2 %>%  
# mutate(time = factor(time, levels = c("pre", "post"))) %>%
#ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
#geom_line() + 
#geom_point(shape= 21) 

#saveRDS(cmyc.emm, "./data/data-gen/protein/cmyc.emm.RDS")


# UBF emmeans

ubf.emm <- emmeans(m1, specs = ~ time|supplement) %>%
  data.frame()

ubf.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

#saveRDS(ubf.emm, "./data/data-gen/protein/ubf.emm.RDS")


# RPS6

rps6.emm <- emmeans(m2, specs = ~ time|supplement) %>%
  data.frame()

rps6.emm %>%
  mutate(time = factor(time, levels = c("pre", "post"))) %>%
  ggplot(aes(time, emmean, fill = supplement, group = supplement)) + 
  geom_line() + 
  geom_point(shape= 21) 

#saveRDS(rps6.emm, "./data/data-gen/protein/rps6.emm.RDS")
