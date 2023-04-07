library("openxlsx")
library("gggenes")
library("tidyverse")
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/MGE/ARC MGE.xlsx",sheet=4)
ggplot(data, aes(xmin = start, xmax = end, y = molecule, fill = Subtype,label=gene,forward = direction)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()

