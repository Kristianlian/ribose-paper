

library(tidyverse)

str.emm <- readRDS("./data/data-gen/humac/emm.str.RDS")

str.emm2 <- str.emm %>% 
  filter(time %in% c("change.2", "change.3", "change.4")) %>%
  mutate(tp = if_else(time %in% c("change.2", "change.3", "change.4"),
                                  "strength", time),
         timep = if_else(time == "change.2",
                         "Post RT2",
                         if_else(time == "change.3",
                                 "Post RT4",
                                 if_else(time == "change.4",
                                         "Post RT5", time)))) %>%
  add_row(supplement = "placebo", timep = "Baseline", 
          tp = "strength", emmean = 0, SE = 0, 
          df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", timep = "Baseline", 
          tp = "strength", emmean = 0, SE = 0, 
          df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  mutate(timep = factor(timep, levels = c("Baseline", "Post RT2", "Post RT4", "Post RT5"))) %>%
  select(-time)

base.astr <- str.emm2 %>%
  filter(timep == "Post RT5") %>%
  mutate(tp = if_else(tp == "strength",
                      "recov", tp),
         timep = if_else(timep == "Post RT5",
                         "baseline", timep)) %>%
  print()

astr.emm <- readRDS("./data/data-gen/humac/emm.astr.RDS")

astr.emm2 <- astr.emm %>% 
  mutate(tp = if_else(time %in% c("change.2", "change.3", "change.4"),
                      "recov", time),
         timep = if_else(time == "change.2",
                         "30min post RT6",
                         if_else(time == "change.3",
                                 "2hrs post RT6",
                                 if_else(time == "change.4",
                                         "23hrs post RT6", time)))) %>%
  full_join(base.astr) %>%
  mutate(timep = factor(timep, levels = c("baseline", "30min post RT6", "2hrs post RT6", "23hrs post RT6"))) %>%
  select(-time)



### Joining
#
#str.joined <- str.emm2 %>%
#  full_join(astr.emm2) %>%
#  select(supplement, timep, tp, emmean, SE, df, lower.CL, upper.CL) %>%
#  print()


## Fig settings

# Plots
pos <- position_dodge(width = .2)

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

# Colours

colors <- c("#d7191c",
                     "#fdae61",
                     "#abd9e9",
                     "#2c7bb6")


# Fig prepp

#str.joined2 <- str.joined %>%
#  mutate(tp = factor(tp, levels = c("strength", "recov")),
#         timep = factor(timep, levels = c("Baseline", "Post RT2", "Post RT4", "Post RT5",
#                                          "baseline", "30min post RT6", "2hrs post RT6", "23hrs post RT6"))) %>%
#  print()
  

# Fig 

str.facet <- bind_rows(str.emm2, astr.emm2) %>%
  mutate(tp = factor(tp, levels = c("strength", "recov"))) %>%
  ggplot(aes(timep, emmean, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(lty = 2) +
  geom_point(shape = 21, size = 2) +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 6)) +
  facet_wrap(~tp, nrow = 1)
  
  
  print()
  
str.fig2 <- str.emm2 %>%
  data.frame() %>%
  ggplot(aes(timeh, emmean, fill = supplement)) +
  annotate("text", x = 20, y = -0.04, label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(lty = 2) +
  geom_point(shape = 21, size = 2) +
  scale_fill_manual(values = colors[c(1,4)],
                    labels = c("Glucose", "Placebo")) +
  scale_x_continuous(limits = c(0,25), breaks = c(0, 8, 16, 20, 21, 22, 23.5),
                     expand = expansion(0),
                     labels = c("0" = "Baseline", "8" = "Post 2RT", "16" = "Post 4RT", "21" = "Post 5RT",
                                "22.7" = "30min post 6RT", "23.4" = "2h post 6RT",
                                "24.4" = "23h post 6RT")) +
  labs(x = "Time", y = "Strength index change", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 6),
        axis.title = element_text(size = 7),
        axis.title.x = element_blank(),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key = element_rect(fill = "white"),
        legend.key.size = unit(0.3, "cm")) +
  plot_theme






