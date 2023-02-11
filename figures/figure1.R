### Ribose figure 1 ######
#
# Study design, strength (1RM, Humac), c-peptide and blood glucose
#
#
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot)
#
## Data

# C-peptide
cpep.change <- readRDS("./data/data-gen/glucose/insulin.figchange.RDS") # Untransformed data
cpep.emm <- readRDS("./data/data-gen/glucose/cpep.emm.RDS") # Unstransformed emmeans
cpep.lchange <- readRDS("./data/data-gen/glucose/insulin.change.RDS") # Log-transformed data
cpep.lemm <- readRDS("./data/data-gen/glucose/cpep.lemm.RDS") # Log-transformed emmeans

# Blood glucose
glu.change <- readRDS("./data/data-gen/glucose/gluc.change.RDS") # Untransformed data
glu.emm <- readRDS("./data/data-gen/glucose/gluc.emm.RDS") # Untransformed emmeans
glu.lchange <- readRDS("./data/data-gen/glucose/glu.change.RDS") # Log-transformed data
glu.lemm <- readRDS("./data/data-gen/glucose/gluc.logemm.RDS") # Log-transformed emmeans

# Humac pre->post 5 session
isom.lchange <- readRDS("./data/data-gen/humac/isom.lchange.RDS") # Log-transformed data
lemm.isom <- readRDS("./data/data-gen/humac/emm.isom.RDS") # Log-transformed emmans
isok60.lchange <- readRDS("./data/data-gen/humac/isok60.lchange.RDS") # Log-transformed data
lemm.60 <- readRDS("./data/data-gen/humac/emm.60.RDS") # Log-transformed emmeans
isok240.lchange <- readRDS("./data/data-gen/humac/isok240.lchange.RDS") # Log-transformed data
lemm.240 <- readRDS("./data/data-gen/humac/emm.240.RDS") # Log-transformed emmeans

# Humac pre->post sessions 6
isomac.lchange <- readRDS("./data/data-gen/humac/isom.lchange.ac.RDS") # Log-transformed
lemm.isomac <- readRDS("./data/data-gen/humac/emm.ac.isom.RDS")
iso60ac.lchange <- readRDS("./data/data-gen/humac/iso60.lchange.ac.RDS") # Log-transformed
lemm.60ac <- readRDS("./data/data-gen/humac/emm.ac.60.RDS")
iso240ac.lchange <- readRDS("./data/data-gen/humac/iso240.lchange.ac.RDS") # Log-transformed
lemm.240ac <- readRDS("./data/data-gen/humac/emm.ac.240.RDS")

### Plots
pos <- position_dodge(width = 0.2)

## C-peptide
cpep.fig <- cpep.change %>%
  data.frame() %>%
  add_row(supplement = "PLACEBO", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "GLUCOSE", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  #mutate(time = factor(time, levels = c("change.45", "change.90", "change.120", "change.135", "change.150", "change.270"))) %>%
  #mutate(time = as.numeric(time)) %>%
  ggplot(aes(time.c, emmean, group = supplement, fill = supplement)) +
  annotate("text", x = c(120, 150), y = c(1000, 1000), label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  labs(x = "", y = "C-peptide levels \n(pmol/L change)\n", fill = "") +
  theme_classic() +
  # theme(plot.background = element_rect(fill = "gray80")) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 90, 120, 150, 270),
                     expand = expansion(0), labels = c("change.1" = "-120", "change.90" = "-30", "change.120" = "0", 
                                                       "change.150" = "30", "change.270" = "120")) #+
  #scale_y_continuous(limits = c(0,1000)) +
 # theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Blood glucose
glu.fig <- glu.change %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before = 2) %>%
  mutate(time.c = gsub("change.", "", time), 
         time.c = as.numeric(time.c)) %>%
  #mutate(time = factor(time, levels = c("change.45", "change.90", "change.120", "change.135", "change.150", "change.270"))) %>%
  #mutate(time = as.numeric(time)) %>%
  ggplot(aes(time.c, emmean, group = supplement, fill = supplement)) +
  annotate("text", x = c(120, 135, 150), y = c(2.5, 2.1, 2.1), label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                size = 0.5,
                position = pos) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 2) +
  #scale_x_discrete(labels=c("change.45" = "45", "change.90" = "90",
  #                         "change.120" = "120", "change.135" = "135", 
  #                        "change.150" = "150", "change.270" = "270")) +
  labs(x = "", y = "Plasma glucose levels \n(mmol/L change)\n", fill = "") +
  theme_classic() +
  # theme(plot.background = element_rect(fill = "gray80")) +
  scale_x_continuous(limits = c(0, 300), breaks = c(0, 45, 90, 120, 135, 150, 270),
                     expand = expansion(0), labels = c("0" = "-120", "45" = "-90", "90" = "-30", "120" = "0", "135" = "15",
                                                       "150" = "30", "270" = "120")) #+
 # theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Humac pre->post 5 sessions

# Isometric

isom.mfig <- lemm.isom %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Isometric peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Isokinetic 60 d/s

mfig.60 <- lemm.60 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Iso 60 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Isokinetic 240

mfig.240 <- lemm.240 %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "Time", y = "Iso 240 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

## Humac pre->post 6th session

# Isometric

isom.acfig <- lemm.isomac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Isometric peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Isokinetic 60 d/s

acfig.60 <- lemm.60ac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "", y = "Iso 60 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))

# Isokinetic 240

acfig.240 <- lemm.240ac %>%
  data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) + # exp() reverse-transformes log-data, so we get fold change in nm
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "Test 1", "change.3" = "Test 2",
                            "change.4" = "Test 3")) +
  labs(x = "Time", y = "Iso 240 d/s peak torque \n(nm fold change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.text.x = element_text(size=8))


# Joined figure 1

legend <- get_legend(acfig.240 + theme(legend.box.margin = margin(0, 0, 0,12)))


fig1 <- plot_grid(cpep.fig + theme(legend.position = "none"),
                  glu.fig + theme(legend.position = "none"),
                  isom.mfig + theme(legend.position = "none"),
                  isom.acfig + theme(legend.position = "none"),
                  mfig.60 + theme(legend.position = "none"),
                  acfig.60 + theme(legend.position = "none"),
                  mfig.240 + theme(legend.position = "none"),
                  acfig.240 + theme(legend.position = "none"),
                  ncol = 2, nrow = 4)

ggsave(
  file = "fig1.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "./figures",
  scale = 1,
  width = 6,
  height = 12,
  units = c("in", "cm", "mm", "px"),
  dpi = 600,
  limitsize = TRUE,
  bg = NULL
)





