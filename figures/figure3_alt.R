### Ribose figure 3 ######
#
# This script produces the figures for cmyc, ubf and rps6
#
#
# Packages
library(emmeans); library(tidyverse); library(cowplot); library(magick); 

library(nlme); library(ggtext)
#
# Data


prot_models <- readRDS("./data/data-gen/protein/protein_lmer_models.RDS")


prot_models <- prot_models$prot_models


cmyc <- readRDS("./data/data-gen/protein/cmyc.lemm.RDS")

ubf <- readRDS("./data/data-gen/protein/ubf.lemm.RDS")

rps6 <- readRDS("./data/data-gen/protein/rps6.lemm.RDS")

joined.dat <- readRDS("./data/data-gen/rna.prot.RDS")

# Images

g3 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/g3_115_annotated.png", scale = 0.9)
g6 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/g6_115_unannotated.png", scale = 0.9)

ubf.x2.img <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/ubf_x2_115.png", scale = 0.8)
cmyc.x2.img <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/cmyc_x2_115.png", scale = 0.8)
rps6.x2.img <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/rps6_x2_115.png", scale = 0.8)

# Colours

colors <- c("#d7191c",
            "#fdae61",
            "#abd9e9",
            "#2c7bb6")
            
# Textsizes

labsize <- 8
textsize <- 6
htextsize <- 7
legendti <- 6
legendtex <- 5.25
atext <- 2
keysize <- 2

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    #plot.background = element_rect(fill = "lightblue", color = NA),
                    axis.line = element_line(colour = "black"))

## cmyc fig

comp_plot <- function(model, method = "Wald") {
  
  ## Plot settings 
  
  labsize <- 8
  textsize <- 6
  htextsize <- 7
  legendti <- 6
  legendtex <- 5.25
  atext <- 2
  keysize <- 2
  

  ## Get group changes
  grp_change <- confint(pairs(emmeans(model, specs = ~ time + supplement), reverse = TRUE), adjust = "none") %>%
    data.frame() %>%
    filter(contrast %in% c("post PLACEBO - pre PLACEBO", 
                           "post GLUCOSE - pre GLUCOSE")) %>%
    mutate(estimate = exp(estimate), 
           lower.CL = exp(lower.CL), 
           upper.CL = exp(upper.CL), 
           supplement = gsub("post ", "", contrast), 
           supplement = gsub(" - pre.*", "", supplement))
  
  ## Plot raw fold changes
  raw_fold <- model@frame %>%
    pivot_wider(names_from = time, values_from = 'ln.norm.sign', values_fn = mean) %>%
  mutate(change = post - pre) %>%
    group_by(subject, supplement) %>%
    summarise(change = exp(mean(change))) 
  
  
  ## Get group diff
  ci <- confint.merMod(model, method = method)
  
  grp_diff <- data.frame(supplement = "DIFF", 
                         estimate = exp(coef(summary(model))["timepost:supplementGLUCOSE", 1]),
                         upper = exp(ci["timepost:supplementGLUCOSE",2]), 
                         lower = exp(ci["timepost:supplementGLUCOSE",1]))
  
  

    
  
  # Get max in raw data
  
  max_raw <- raw_fold %>%
    ungroup() %>%
    summarise(m = max(change)) %>%
    pull(m)
  
  
  # Set 0 for diff calculation scale
  
  
  
  zero_diff <- grp_change %>%
    filter(supplement == "PLACEBO") %>%
    pull(estimate)
  
  ## Settings for second scale
  
  y0 <- zero_diff
  # negative 50% of zero_diff
  y_1 <- y0 *  0.5
  # positive 50% increase from zero_diff
  y1 <- y0 * 1.5 
  
  x_sc <- 3.4
  x_scin <- 3.45
  
## Colors  
 colors <- c("#d7191c",
  "#fdae61",
  "#abd9e9",
  "#2c7bb6")
  
  
  
  
PLOT <- grp_change %>%
    ggplot(aes(x = supplement, y = estimate, fill = supplement)) + 
    
  
  ## Raw data points
  geom_point(data = raw_fold, 
             aes(x = supplement, 
                 y = change, 
                 fill = supplement), 
             shape = 21, 
             alpha = 0.3,
             
             position = position_jitter(width = 0.05)) +
  
  
  
  
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                  width = 0, 
                  linewidth = 0.4) +
    geom_point(shape = 21, size = 2) +
    
  ## Scales
  
  scale_fill_manual(values = c(colors[1], colors[2], colors[3])) +
  
  
    scale_x_discrete(breaks = c("PLACEBO", "GLUCOSE", "DIFF"), 
                     limits = factor(c("PLACEBO", "GLUCOSE", "DIFF")),
                     expand = c(0, 1.2),
                     labels = c("Placebo", "Glucose", "Difference")) + 
    
    ## Setting up the second scale
    geom_segment(y = zero_diff, yend = zero_diff, 
                 x = x_sc, xend = x_sc - 0.75, 
                 lty = 2, 
                 linewidth = 0.2) +
    
    geom_segment(y = c(y_1, y_1), 
                 yend = c(y1, y_1), 
                 x = c(x_sc, x_sc), 
                 xend = c(x_sc, x_scin), 
                 lty = 1, 
                 linewidth = 0.2) +
    annotate("segment", 
             y = c(y1, y1, y0), 
                 yend = c(y1, y1, y0), 
                 x = c(x_sc, x_sc, x_sc), 
                 xend = c(x_sc, x_scin, x_scin), 
                 lty = 1, 
             linewidth = 0.2) +
    
    
    
    ## Error bars on difference
      geom_errorbar(data = grp_diff, 
                  aes(x = supplement, y = zero_diff * estimate, 
                      ymin = lower * zero_diff, 
                      ymax = upper * zero_diff), 
                  width = 0, 
                  linewidth = 0.4) +
    
    
    geom_point(data = grp_diff, 
               aes(x = supplement, 
                   y = zero_diff * estimate), 
               size = 1.5,
               shape = 21, 
               fill = colors[1]) +
    

    annotate("text", 
             x = x_scin + 0.25, 
              y = c(zero_diff,  zero_diff * 1.5, zero_diff * 0.5),
              label = c(0, 0.5, -0.5), 
             size = 2) +
    #
  
  
  annotation_custom(grid::textGrob(label = "Fold-difference",
                                   x = unit(0.97, "npc"), y = unit(0.5, "npc"),
                                   rot = -90,
                                   gp = grid::gpar(cex = 0.5))) +
  
    

  
  labs(y = "Fold-change (95% CI)") +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA),
        #plot.background = element_rect(fill = "lightblue", color = NA),
        axis.line = element_line(colour = "black"), 
        legend.position = "none", 
        axis.title = element_text(size = htextsize),
        axis.title.x = element_blank(),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = legendti),
        legend.text = element_text(size = legendtex),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.key.size = unit(0.3, "cm"))
  
  
    
    PLOT
    
  
  
} 




rps6.plot <- comp_plot(prot_models$rps6) +
  
  labs(subtitle = "RPS6") +
  
  scale_y_continuous(limits = c(0, 3), expand = c(0,0)) +
  
  theme(axis.title.y = element_blank(), 
        plot.subtitle = element_text(size = 8))


cmyc.plot <- comp_plot(prot_models$cmyc)  +
  
  labs(subtitle = "c-Myc") +
  scale_y_continuous(limits = c(0, 12), expand = c(0,0)) +
  
  theme(
        plot.subtitle = element_text(size = 8))
 
ubf.plot <- comp_plot(prot_models$ubf) +
  
  labs(subtitle = "UBF") +
  
  scale_y_continuous(limits = c(0, 5), expand = c(0,0)) +
  
  theme(axis.title.y = element_blank(), 
        plot.subtitle = element_text(size = 8))




# UBF/Total RNA correlation plot

## Model

library(nlme)

m <- lme(mean.rna ~ sd.ubf + time , 
         random = list(subject = ~ 1), 
         data = joined.dat, 
         na.action = na.omit)

summary(m)
intervals(m)


plot(m)



## Extract x min and xmax from observed data 
min_max <- joined.dat %>%
  group_by(time) %>%
  summarise(min = min(sd.ubf, na.rm = TRUE), 
            max = max(sd.ubf, na.rm = TRUE))


# Correlation plot

textsize <- 8
htextsize <- 9

corr.fig <- joined.dat %>%
  ggplot(aes(sd.ubf, mean.rna, fill = time)) + 
  geom_point(shape = 24, size = 1) +
  # geom_smooth(method = "lm", se = FALSE) +
  scale_fill_manual(values = colors[c(2,3)],
                    labels = c("Baseline", "Post")) +
  annotate("segment", 
           x = filter(min_max, time == "pre")$min, 
           xend = filter(min_max, time == "pre")$max, 
           y = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "pre")$min, 
           yend = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "pre")$max) +
  
  
  
  annotate("segment", 
           x = filter(min_max, time == "post")$min, 
           xend = filter(min_max, time == "post")$max, 
           y = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "post")$min + coef(summary(m))[3,1] , 
           yend = coef(summary(m))[1,1] + coef(summary(m))[2,1] * filter(min_max, time == "post")$max + coef(summary(m))[3,1]) +
  
  
  
  
  
  labs(x = "UBF per normalization factor<br>(SD units)", y = "Total RNA (ng &times; mg<sup>-1</sup>)", 
       fill = "Time") +
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "lightblue", colour = NA),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_markdown(size = 7),
        axis.text = element_text(size = 7),
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key = element_rect(fill = "white"),
        axis.title.y = element_markdown(size = 7),
        legend.key.size = unit(0.3, "cm"))

# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))


## Individual protein figs



rps6.fig <- plot_grid(rps6.plot + theme(plot.margin = unit(c(0.5, 0.2, -0.5, 0.2), "cm")),
                      rps6.x2.img,
                      ncol = 1,
                      rel_heights = c(1, 1)) +
  
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "37", "25", "37", "25", 
                            "Duplicate 1", "Duplicate 2"),
                  x = c(.155, .23, .305, .38,
                        .62, .695, .77, .845, 
                        .07, .07, .54, .54, 
                        .26, .73),
                  y = c(.31, .31, .31, .31,
                        .31, .31, .31, .31, 
                        .28, .22, .28, .22,
                        .37, .37),
                  lineheight = 0.75,
                  hjust = .5, vjust = .5, size = 4) 





cmyc.fig <- plot_grid(cmyc.plot + theme(plot.margin = unit(c(0.5, 0.2, -0.5, 0.2), "cm")),
                      cmyc.x2.img,
                      ncol = 1,
                      rel_heights = c(1, 1))  +
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "75", "50", "75", "50", 
                            "Duplicate 1", "Duplicate 2"),
                  x = c(.155, .23, .305, .38,
                        .62, .695, .77, .845, 
                        .07, .07, .54, .54, 
                        .26, .73),
                  y = c(.31, .31, .31, .31,
                        .31, .31, .31, .31, 
                        .28, .22, .28, .22,
                        .37, .37),
                  lineheight = 0.75,
                  hjust = .5, vjust = .5, size = 4) 


ubf.fig <- plot_grid(ubf.plot + theme(plot.margin = unit(c(0.5, 0.2, -0.5, 0.2), "cm")),
                
                      ubf.x2.img,
                      ncol = 1,
                     rel_heights = c(1, 1))  +
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "100", "75", "100", "75", "Duplicate 1", "Duplicate 2"),
                  x = c(.155, .23, .305, .38,
                        .62, .695, .77, .845, 
                        .07, .07, .54, .54, 
                        .26, .73),
                  y = c(.31, .31, .31, .31,
                        .31, .31, .31, .31, 
                        .28, .22, .28, .22,
                        .37, .37),
                  lineheight = 0.75,
                  hjust = .5, vjust = .5, size = 4) 


prot.fig <- plot_grid(
          cmyc.fig,
          ubf.fig,
          rps6.fig,
          nrow = 1) 


# Total Protein stain images

tot.fig <- plot_grid(NULL,
                     g3,
                     NULL,
          g6,
          NULL,
          ncol = 5, 
          rel_widths = c(0.4,1,0.1,1,0.65)) +
  draw_plot_label(label = c("Duplicate 1", "250 kDa", "150", "100", "75", "50", "37", "25", "20", "15", 
                            "Duplicate 2"), #"250", "150", "100", "75", "50", "37", "25", "20", "15"),
                                    x = c(.37, .15, .15, .15, .15, .15, .15, .15, .15, .15, 
                                          .70), #.683, .683, .683, .69, .69, .69, .69, .69, .69),
                                    y = c(.98,.93, .885, .833, .775, .653, .53, .332, .25, .072,
                                          .98),#.93, .885, .833, .775, .653, .535, .332, .25, .072),
                                    hjust = 1, vjust = .5, size = 6)
                  

## Gathered fig

#fig3 <- plot_grid(plot_grid(corr.fig,
#                            NULL,
#                            tot.fig,
#                            NULL,
#                            ncol = 4,
#                            rel_widths = c(1.7,0, 1.5, 0.1)),
#                  NULL,
#                  plot_grid(NULL,
#                            prot.fig,
#                            ncol = 2,
#                            rel_widths = c(0, 1)),
#                  nrow = 3,
#                  rel_heights = c(.85, .05, 1.2))  + 
#  draw_plot_label(label = c("A)", "B)", "C)"),
#                  x = c(.015, .5, .015),
#                  y = c(.97, .97, .57),
#                  hjust = .5, vjust = .5, size = 5) 

fig3 <- plot_grid(plot_grid(NULL,
                    prot.fig,
                    ncol = 2,
                    rel_widths = c(.01, .999)),
          NULL,
          plot_grid(tot.fig,
              
                    corr.fig,
                    NULL,
                    ncol = 3,
                    rel_widths = c(1.7,2, 0.1)),
          nrow = 3,
          rel_heights = c(1, .01, 1))  + 
  draw_plot_label(label = c("A)", "B)", "C)"),
                  x = c(.015, .015, .475),
                  y = c(.97, .5, .5),
                  hjust = .5, vjust = .5, size = 10) 
          

ggsave(
  file = "fig3.pdf",
  plot = fig3,
  device = "pdf",
  path = "./figures",
  width = 7.7*2,
  height = 23*0.5,
  units = "cm",
  dpi = 600
)

