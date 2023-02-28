


library(readxl); library(tidyverse)

str.emm <- readRDS("./data/data-gen/humac/emm.str.RDS") %>%
  add_row(supplement = "placebo", time = "0", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "0", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  mutate(timeh = if_else(time == "0",
                         "0",
                         if_else(time == "change.2",
                                 "8",
                                 if_else(time == "change.3",
                                         "16",
                                         if_else(time == "change.4",
                                                 "21",
                                                 if_else(time == "change.5",
                                                         "22.7",
                                                         if_else(time == "change.6",
                                                                 "23.4",
                                                                 if_else(time == "change.7", 
                                                                         "24.4", time)))))))) %>%
  mutate(#timeh = factor(timeh, levels = c("0", "72", "192", "264", "266", "268", "291")),
         timeh = as.numeric(timeh)) %>%
  print()

### Plots
pos <- position_dodge(width = 0.2)

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

# Colours
# Colours

colors <- c("#d7191c",
                     "#fdae61",
                     "#abd9e9",
                     "#2c7bb6")

str.fig <- str.emm %>%
  data.frame() %>%
  ggplot(aes(timeh, emmean, fill = supplement)) +
  #annotate("text", x = "change.4", y = -0.04, label = "*") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line() +
  geom_point(shape = 21, size = 2) +
  scale_fill_manual(values = colors[c(1,4)]) +
  scale_x_continuous(limits = c(0,25), breaks = c(0, 8, 16, 21, 22.7, 23.4, 24.4),
                     expand = expansion(0),
                     labels = c("0" = "Baseline", "8" = "Post 2RT", "16" = "Post 4RT", "21" = "Post 5RT",
                                "22.7" = "30min post 6RT", "23.4" = "2h post 6RT",
                                "24.4" = "23h post 6RT")) +
  labs(x = "Time", y = "Strength index fold change", fill = "Supplement") +
  theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, size = 6),
        axis.title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.key = element_rect(fill = "white")) +
  plot_theme

  
  


