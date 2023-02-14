####### Study design fig work

# Packages
library(tidyverse)
library(readxl)


# Data
d.dat <- read_excel("./data/design.dat.xlsx", na = "NA") %>%
  print()

d.dat %>%
  ggplot(aes(time, tp)) +
  
  #annotate("segment", x = 1, xend = 7.5, y = 10.5, yend = 10.5,
   #        colour = "black") #+
  #annotate("segment", x = 1, xend = 20, y = 1, yend = 1,
   #        colour = "black") #+
  
  #annotate("segment", x = 7.5, xend = 20, y = 10.5, yend = 10.5,
   #        colour = "black") #+
  
  annotate("rect", xmin = 7.5, xmax = 21, ymin = 0, ymax = 8.5, alpha = 2, color = "lightblue", fill = "lightblue") +
  
  annotate("rect", xmin = 0, xmax = 7.5, ymin = 0, ymax = 8.5, alpha = 2, color = "grey", fill = "grey") +
  
  annotate("text", x = 4, y = 8, label = "Familiarization") +
  
  annotate("text", x = 13, y = 8, label = "RT + Dietary intervention") +
  
  annotate("rect", xmin = 6.6, xmax = 9.52, ymin = 0, ymax = 7.5, aplha = 2, color = "black", fill = "white") +
  
  annotate("text", x = 8, y = 7.1, label = "Pre test") +
  
  annotate("rect", xmin = 17.43, xmax = 20.4, ymin = 0, ymax = 7.5, aplha = 2, color = "black", fill = "white") +
  
  annotate("text", x = 19, y = 7.1, label = "Post test") +
  
  annotate("label", x = 7, y = 6.5, label = "DXA", color = "white", fill = "grey", size = 3) +
  
  annotate("label", x = c(8, 18, 19), y = 5, label = "Blood", color = "white", fill = "red", size = 3) +
  
  annotate("label", x = c(8, 9, 18, 19), y = 3.5, label = "Biopsy", color = "white", fill = "red", size = 3) +
  
  annotate("label", x = c(1, 3, 7, 11, 12, 15, 16, 18, 19, 20), y = 2, label = "Test", color = "white", fill = "blue",
           size = 3) +
  
  annotate("label", x = c(8, 10, 12, 14, 16, 18), y = 0.5, label = "RTL1", color = "white", fill = "black", size = 3) +
  
  annotate("label", x = c(9, 11, 13, 15, 17, 19), y = 0.5, label = "RTL2", color = "black", fill = "white", size = 3) +
  
  scale_y_continuous(limits = c(0,10), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  
  scale_x_continuous(limits = c(0,21), breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20), 
                     labels = c("1" = "-7", "2" = "-6", "3" = "-5", "4" = "-4",
                                "5" = "-3", "6" = "-2", "7" = "-1", "8" = "1",
                                "9" = "2", "10" = "3", "11" = "4", "12" = "5", "13" = "6",
                                "14" = "7", "15" = "8", "16" = "9", "17" = "10",
                                "18" = "11", "19" = "12", "20" = "13")) +
  labs(x = "Days", y = "", fill = "") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())
  
  
  

data.frame() %>%
  add_row(supplement = "placebo", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =1) %>%
  add_row(supplement = "glucose", time = "change.1", emmean = 0, SE = 0, df = 0, lower.CL = 0, upper.CL = 0, .before =2) %>%
  ggplot(aes(time, emmean, group = supplement, fill = supplement)) +
  #annotate("text", x = "change.1", y = 25, label = "p = NS", size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pos,
                width = 0.2) +
  geom_line(position = pos) +
  geom_point(shape = 21, position = pos, size = 3) +
  scale_x_discrete(labels=c("change.1" = "Baseline", "change.2" = "30min post", "change.3" = "2hr post",
                            "change.4" = "23hr post")) +
  labs(x = "", y = "Isometric \n(Nm change)\n", fill = "Supplement") +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),


######################################## OLD ###########################################
  scale_x_discrete(labels=c("d-7" = "-7", "d-6" = "-6", "d-5" = "-5", "d-4" = "-4",
                            "d-3" = "-3", "d-2" = "-2", "d-1" = "-1", "d1" = "1",
                            "d2" = "2", "d3" = "3", "d4" = "4", "d5" = "5", "d6" = "6",
                            "d7" = "7", "d8" = "8", "d9" = "9", "d10" = "10",
                            "d11" = "11", "d12" = "12", "d13" = "13")) 
  
  
  

  


  
