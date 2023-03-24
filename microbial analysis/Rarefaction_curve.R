library(tidyverse)
library(openxlsx)
library(vegan)

#importing the file and parsing the file correctly
# Replace the kraken_final name to the actual filename.
Data=read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/kraken2_mapout(raw).xlsx",sheet=1,colNames = T,rowNames = T)
Data_t=as.data.frame(t(Data))
#count the number of species
S <- specnumber(Data_t)
raremax <-min(rowSums(Data_t))
#Rarefaction of the samples
Srare <- rarefy(Data_t, raremax)
Srare
#plotting the rarefaction curves
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0, 1)
pdf("Rarefaction_curve.pdf")
rarecurve(Data_t, step =10000, sample = raremax, col = "blue", cex = 0.4)
dev.off()
