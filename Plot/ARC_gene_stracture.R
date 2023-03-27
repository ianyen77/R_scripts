library(gggenes)
library(tidyverse)
library(ggplot2)
library(openxlsx)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/VF/VF_ARG_gggenes.xlsx",sheet=1)
ggplot(data, aes(xmin = start, xmax = end, y = molecule, fill = gene, forward = direction)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()