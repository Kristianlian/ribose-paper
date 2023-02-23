### Ribose figure 3 ######
#
# This script produces the figures for cmyc, ubf and rps6
#
#
# Packages
library(emmeans)
library(tidyverse)
library(cowplot)
#
# Data

cmyc <- readRDS("./data/data-gen/protein/cmyc.change.RDS")

ubf <- readRDS("./data/data-gen/protein/ubf.change.RDS")

rps6 <- readRDS("./data/data-gen/protein/rps6.change.RDS")

# Images

tot.img <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/tot_gel2_annotated.png", scale = 0.9)

rps6.img1 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/rps6_3_115_annotated.png", scale = 0.9)
rps6.img2 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/rps6_6_115_annotated.png", scale = 0.9)

cmyc.img1 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/cmyc_3_115_annotated.png", scale = 0.9)
cmyc.img2 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/cmyc_6_115_annotated.png", scale = 0.9)

ubf.img1 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/ubf_3_115_annotated.png", scale = 0.9)
ubf.img2 <- cowplot::ggdraw() + cowplot::draw_image("./figures/archive/ubf_6_115_annotated.png", scale = 0.9)


# Designing the plot theme

plot_theme <- theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "lightblue", colour = NA),
                    axis.line = element_line(colour = "black"))

labsize <- 8
textsize <- 6
htextsize <- 7
atext <- 2
keysize <- 2


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
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "c-Myc signal \n(fold change)\n", fill = "Supplement") + plot_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = htextsize),
        axis.text = element_text(size = textsize))
        

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
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "UBF signal \n(fold change)\n", fill = "") + plot_theme +
  theme(axis.title = element_text(size = htextsize),
      axis.text = element_text(size = textsize))


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
  geom_point(shape = 21, size = 3, position = position_dodge(width = 0.2)) +
  scale_fill_manual(values = c("GLUCOSE" = "red", "PLACEBO" = "royalblue")) +
  labs(x = "Time", y = "rps6 signal \n(fold change)\n", fill = "Supplement") + plot_theme +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = htextsize),
        axis.text = element_text(size = textsize),
        legend.title = element_text(size = htextsize),
        legend.text = element_text(size = 5),
        legend.key = element_rect(fill = "white"))


# Cowplot for gathering figures

legend <- get_legend(rps6.plot + theme(legend.box.margin = margin(0, 0, 0,12)))


## Individual protein figs
rps6.fig <- plot_grid(rps6.plot + theme(legend.position = "none"),
                      NULL,
          plot_grid(NULL,
                    rps6.img1,
                    rps6.img2,
                    nrow = 3,
                    rel_heights = c(1,2,2)),
          ncol = 3,
          rel_widths = c(1.5,0,2))



cmyc.fig <- plot_grid(cmyc.plot + theme(legend.position = "none"),
                      NULL,
                      plot_grid(NULL,
                                cmyc.img1,
                                cmyc.img2,
                                nrow = 3,
                                rel_heights = c(1,2,2)),
                      ncol = 3,
                      rel_widths = c(1.5,0,2))



ubf.fig <- plot_grid(ubf.plot + theme(legend.position = "none"),
                      NULL,
                      plot_grid(NULL,
                                ubf.img1,
                                ubf.img2,
                                nrow = 3,
                                rel_heights = c(1,2,2)),
                      ncol = 3,
                      rel_widths = c(1.5,0,2))
## Gathered fig
prot.fig <- plot_grid(ubf.fig,
          cmyc.fig,
          rps6.fig,
          nrow = 3)

#tot.fig <- plot_grid(legend,
#                     tot.img,
#                     ncol = 2,
#          rel_widths = c(1, 2/2))

tot.fig <- plot_grid(NULL,
          plot_grid(legend,
                    tot.img,
                    ncol = 2,
                    rel_widths = c(0.1, 1)),
          ncol = 2,
          rel_widths = c(0.2,2),
          axis = "l")



fig3 <- plot_grid(tot.fig,
                  prot.fig,
                  nrow = 2,
                  rel_heights = c(1, 2),
                  rel_widths = c(0.2, 1))

          

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

