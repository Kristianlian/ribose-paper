### Ribose figure 2 ######
#
# This script creates figure 2, containing log-fold change data of total RNA and rRNA
#
# Packages 
library(tidyverse); library(emmeans); library(cowplot); library(ggtext)
   


rrna_analysis_results <- readRDS("./data/data-gen/rna/rrna_analysis_results.RDS")


rrna_foldchange <- rrna_analysis_results$fold_change %>%
  mutate(target = gsub("trg_", "", target), 
         target = paste0(toupper(target), " rRNA"), 
         time = "post") %>%
  add_row(time = "pre", supplement = "PLACEBO", estimate = 0, lower.CL = 0, upper.CL = 0, 
          target = c("18S rRNA", 
                       "28S rRNA" ,
                       "47S rRNA" ,
                       "5.8S rRNA",
                       "5S rRNA", 
                     "18S rRNA", 
                     "28S rRNA" ,
                     "47S rRNA" ,
                     "5.8S rRNA",
                     "5S rRNA")) %>%
  add_row(time = "pre", supplement = "GLUCOSE", estimate = 0, lower.CL = 0, upper.CL = 0, 
          target = c("18S rRNA", 
                     "28S rRNA" ,
                     "47S rRNA" ,
                     "5.8S rRNA",
                     "5S rRNA", 
                     "18S rRNA", 
                     "28S rRNA" ,
                     "47S rRNA" ,
                     "5.8S rRNA",
                     "5S rRNA")) %>%
  
  mutate(estimate = (exp(estimate)), 
         lower.CL =  (exp(lower.CL)),
         upper.CL =  (exp(upper.CL))) %>%
  
  
  print()
  
  

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

# Default textsizes
labsize <- 8
textsize <- 6
htextsize <- 7
atext <- 2
labsize <- 8

# Total RNA plot



tot_rna_results <- readRDS("./data/data-gen/rna/tot_rna_coefs.RDS")


totrna.plot <- tot_rna_results$fold_change %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", estimate = 0, SE = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(estimate), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2), lty = 2) +
  geom_point(position = position_dodge(width = 0.2), shape = 21, size = 2) +
  scale_fill_manual(values = colors[c(1,4)],
                    labels = c("Glucose", "Placebo")) +
  labs(x = "Time-point", y = "Total RNA (ng &times; mg<sup>-1</sup> fold change)", fill = "Supplement") +
  theme_classic() +
  theme(axis.title.x = element_blank(), 
       axis.title.y = element_markdown(size = htextsize),
       axis.text.x = element_text(size = textsize),
       axis.text.y = element_text(size = textsize),
       legend.title = element_text(size = htextsize),
       legend.text = element_text(size = textsize),
       legend.key.size = unit(0.3, "cm")) +
  plot_theme
  

# rRNA plot

rrna.fig <- rrna_foldchange %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post")),
         target = factor(target, levels = c("47S rRNA", "18S rRNA", "28S rRNA", "5.8S rRNA", "5S rRNA"),
                         labels = c("47S", "18S", "28S", "5.8S", "5S"))) %>%
  ggplot(aes(time, estimate, group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0,
                position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3), lty = 2) +
  geom_point(shape = 21, size = 1.5, position = position_dodge(width = 0.3)) +
  scale_fill_manual(values = colors[c(1,4)]) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "Time", y = "AU fold change", fill = "Supplement") +
  facet_wrap(~target, nrow = 1) +
  theme(axis.title = element_text(size = htextsize),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = textsize),
        legend.text = element_text(size = textsize),
        strip.text = element_text(hjust = 0, size = htextsize),
        strip.background = element_rect(fill = "white")) + 
  theme(axis.title.x = element_blank()) +
  plot_theme 
  

fig2 <- plot_grid(plot_grid(NULL,
                            totrna.plot,
                            nrow = 1,
                            rel_widths = c(0.05, 1)),
            
          rrna.fig + theme(legend.position = "none"),
          nrow = 2,
          rel_heights = c(1.5, 1),
          labels = c("A)", "B)"),
          label_size = labsize)


ggsave(
  file = "fig2.pdf",
  plot = fig2,
  device = "pdf",
  path = "./figures",
  width = 7.7,
  height = 23*0.33,
  units = "cm",
  dpi = 600
)





