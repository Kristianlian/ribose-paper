### Abbreviations table

## Packages
library(readxl); library(tidyverse); library(flextable)

## Data
abbreviations.dat <- read_excel("./data/abbreviations.xlsx")

tab.dat <- abbreviations.dat


## Flextable
abb.tab <- tab.dat %>%
  flextable() %>%
  set_caption(caption = "Abbreviations",
              align_with_table = FALSE) %>%
  set_table_properties(layout = "autofit")

abb.tab <- delete_part(x = abb.tab, part = "header")
  



abb.tab <- border(abb.tab, 
                     i = NULL,
                     j = NULL,
                     border = NULL,
                     border.top = fp_border_default(color = "black"),
                     border.bottom = fp_border_default(color = "black"),
                     border.left = fp_border_default(color = "black"),
                     border.right = fp_border_default(color = "black"),
                     part = "all") 

abb.tab
