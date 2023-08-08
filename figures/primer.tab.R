### Primer sequence table

## Packages
library(readxl); library(tidyverse); library(flextable)

## Data
primer.dat <- read_excel("./data/primer.seq2.xlsx")

tab.dat <- primer.dat %>%
  select(Gene, fr, CT.mean, E) %>%
  print()



## Flextable
tab.dat %>%
  flextable() %>%
  compose(i = 1, j = c(1, 2, 3, 4), part = "header",
          value = c(as_paragraph("Gene"),
                    as_paragraph("Sequence (forward - reverse)"),
                    as_paragraph("CT mean (SD)"),
                    as_paragraph("E"))) %>%
  #add_header_row(values = c(" ", " ", " ", "Primer sequences"), 
               #  colwidths = c(1, 1, 1, 1)) %>%
  set_caption("Table 2. Primer Sequences") %>%
  set_table_properties(layout = "autofit")

