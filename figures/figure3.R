### Ribose figure 3 ######
#
# This script produces the figures for cmyc, ubf and rps6
#
#
# Packages
library(emmeans); library(tidyverse); library(cowplot); library(magick); 
library(ggpubr); library(nlme); library(ggtext)
#
# Data

cmyc <- readRDS("./data/data-gen/protein/cmyc.change.RDS")

ubf <- readRDS("./data/data-gen/protein/ubf.change.RDS")

rps6 <- readRDS("./data/data-gen/protein/rps6.change.RDS")

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
legendti <- 5
legendtex <- 4.25
atext <- 2
keysize <- 2

# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    #plot.background = element_rect(fill = "lightblue", color = NA),
                    axis.line = element_line(colour = "black"))

## cmyc fig

cmyc.plot <- cmyc %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = colors[c(1,4)]) +
  scale_y_continuous(breaks = c(1,2,3,4),
                   labels = c("1.0", "2.0", "3.0", "4.0"))+
  labs(x = "Time", y = "c-Myc AU \n(fold change)\n", fill = "Supplement") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = htextsize),
        axis.text = element_text(size = textsize)) +
  plot_theme
        

# UBF plot


ubf.plot <- ubf %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 3, label = "*", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = colors[c(1,4)]) +
  labs(x = "Time", y = "UBF AU \n(fold change)\n", fill = "") + 
  theme(axis.title.x = element_blank(),
    axis.title = element_text(size = htextsize),
      axis.text = element_text(size = textsize)) +
  plot_theme


# rpS6 plot


rps6.plot <- rps6 %>%
  mutate(time = "post") %>%
  add_row(supplement = c("PLACEBO", "GLUCOSE"), time = "pre", emmean = 0, lower.CL = 0, upper.CL = 0) %>%
  mutate(time = factor(time, levels = c("pre", "post"), labels = c("Baseline", "Post"))) %>%
  ggplot(aes(time, exp(emmean), group = supplement, fill = supplement)) +
  annotate("text", x = "Post", y = 1.87, label = "*", size = 3) +
  geom_errorbar(aes(ymin = exp(lower.CL), ymax = exp(upper.CL)),
                width = 0.1,
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(shape = 21, size = 2, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = colors[c(1,4)]) +
  labs(x = "Time", y = "rpS6 AU \n(fold change)\n", fill = "Supplement") +
  theme(axis.title = element_text(size = htextsize),
        axis.title.x = element_blank(),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = legendti),
        legend.text = element_text(size = legendtex),
        legend.key = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white")) +
  plot_theme

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
  scale_fill_manual(values = colors[c(2,3)]) +
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
        legend.text = element_text(size = 5),
        legend.key = element_rect(fill = "white"),
        axis.title.y = element_markdown(size = 7))

# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))


## Individual protein figs

rps6.fig <- plot_grid(rps6.plot + theme(legend.position = "none"),
                      NULL,
                      NULL,
                      NULL,
          rps6.x2.img,
          ncol = 5,
          rel_widths = c(.75,
                         .15, 
                         .05, 
                         .0, 
                         1.5)) +
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "kDa 37", "25", "37", "25", 
                            "Duplicate 1", "Duplicate 2"),
                  x = c(.4825, .529, .576, .625,
                        .77, .818, .864, .907, 
                        .428, .44, .7285, .7285, 
                        .555, .84),
                  y = c(.75, .75, .75, .75,
                        .75, .75, .75, .75, 
                        .68, .315, .68, .315,
                        .98, .98),
                  hjust = .5, vjust = .5, size = 5) 


cmyc.fig <- plot_grid(cmyc.plot + theme(legend.position = "none"),
                      NULL,
                      legend,
                      NULL,
                      cmyc.x2.img,
                      ncol = 5,
                      rel_widths = c(.75,
                                     .1, 
                                     .0, 
                                     .1, 
                                     1.5)) +
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "kDa 75", "50", "75", "50", "Duplicate 1", "Duplicate 2"),
                  x = c(.4825, .529, .576, .625,
                        .77, .818, .864, .907, 
                        .428, .44, .7285, .7285, 
                        .555, .84),
                  y = c(.75, .75, .75, .75,
                        .75, .75, .75, .75, 
                        .68, .315, .68, .315,
                        .98, .98),
                  hjust = .5, vjust = .5, size = 5) 


ubf.fig <- plot_grid(ubf.plot + theme(legend.position = "none"),
                      NULL,
                      NULL,
                      NULL,
                      ubf.x2.img,
                      ncol = 5,
                      rel_widths = c(.75,
                                     .15, 
                                     .05, 
                                     .0, 
                                     1.5))+
  draw_plot_label(label = c("PLA \npost\n", "GLU \npre\n", "GLU \npost\n", "PLA \npre\n",
                            "PLA \npre\n", "GLU \npost\n", "GLU \npre\n", "PLA \npost\n",
                            "kDa 100", "75", "100", "75", "Duplicate 1", "Duplicate 2"),
                  x = c(.4825, .529, .576, .625,
                        .77, .818, .864, .907, 
                        .428, .44, .7285, .7285, 
                        .555, .84),
                  y = c(.75, .75, .75, .75,
                        .75, .75, .75, .75, 
                        .68, .315, .68, .315,
                        .95, .95),
                  hjust = .5, vjust = .5, size = 5) 


prot.fig <- plot_grid(ubf.fig,
          cmyc.fig,
          rps6.fig,
          nrow = 3) 


# Total Protein stain images

tot.fig <- plot_grid(NULL,
                     g3,
                     NULL,
          g6,
          NULL,
          ncol = 5, 
          rel_widths = c(0.4,1,0.1,1,0.65)) +
  draw_plot_label(label = c("Gel 3", "kDa 250", "150", "100", "75", "50", "37", "25", "20", "15", 
                            "Gel 6"), #"250", "150", "100", "75", "50", "37", "25", "20", "15"),
                                    x = c(.29, .11, .143, .143, .15, .15, .15, .15, .15, .15, 
                                          .64), #.683, .683, .683, .69, .69, .69, .69, .69, .69),
                                    y = c(.97,.93, .885, .833, .775, .653, .53, .332, .25, .072,
                                          .97),#.93, .885, .833, .775, .653, .535, .332, .25, .072),
                                    hjust = .5, vjust = .5, size = 6)
                  

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
                    NULL,
                    corr.fig,
                    NULL,
                    ncol = 4,
                    rel_widths = c(1.7,0,2, 0.1)),
          nrow = 3,
          rel_heights = c(1.1, .05, .9))  + 
  draw_plot_label(label = c("A)", "B)", "C)"),
                  x = c(.015, .015, .475),
                  y = c(.99, .43, .43),
                  hjust = .5, vjust = .5, size = 5) 
          

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

