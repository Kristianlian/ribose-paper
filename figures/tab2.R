### Primer sequence table

## Packages
library(readxl); library(tidyverse); library(flextable)

## Data
primer.dat <- read_excel("./data/primer.seq2.xlsx")

tab.dat <- primer.dat %>%
  select(Gene, fr, CT.mean, E)


## Flextable
primer.tab <- tab.dat %>%
  flextable() %>%
  compose(i = 1, j = c(1, 2, 3, 4), part = "header",
          value = c(as_paragraph("Gene"),
                    as_paragraph("Sequence (forward - reverse)"),
                    as_paragraph("Ct mean (SD)"),
                    as_paragraph("E"))) %>%
  set_table_properties(layout = "autofit")



primer.tab <- border(primer.tab, 
                     i = NULL,
                     j = NULL,
                     border = NULL,
                     border.top = fp_border_default(color = "black"),
                     border.bottom = fp_border_default(color = "black"),
                     border.left = fp_border_default(color = "black"),
                     border.right = fp_border_default(color = "black"),
                     part = "all") 

primer.tab