library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(circlize)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/SARG_V3.2/SARGv3.2_ARC_analysis.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Chloroflexi"]<-"Other Phyla"
data_contig$contig_phyla[data_contig$contig_phyla == "Cyanobacteria"]<-"Other Phyla"
data_contig$contig_phyla[data_contig$contig_phyla == "Terrabacteria group"]<-"Other Phyla"
data_contig$Type[data_contig$Type == "fosmidomycin"]<-"Others"
data_contig$Type[data_contig$Type == "kasugamycin"]<-"Others"
data_contig$Type[data_contig$Type == "sulfonamide"]<-"Others"
data_contig$Type[data_contig$Type == "macrolide-lincosamide-streptogramin"]<-"Others"
data_contig$Type[data_contig$Type == "quinolone"]<-"Others"
data_contig$Type[data_contig$Type == "fosfomycin"]<-"Others"
data_contig$Type[data_contig$Type == "unclassified"]<-"Others"
data_contig$Type[data_contig$Type == "streptothricin"]<-"Others"
data_contig$Type[data_contig$Type == "novobiocin"]<-"Others"
data_contig$Type[data_contig$Type == "mupirocin"]<-"Others"
data_contig$Type<-str_to_title(data_contig$Type)
data_contig$Type[data_contig$Type == "Beta-Lactam"]<-"Beta-lactam"
#If you want to draw data without raw ,the color is 13
#data_contig<-data_contig[-grep("T1", data_contig$Sample),]

data_stat<-data_contig%>%
  group_by(Type,contig_phyla)%>%
  count(Type)
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/489)

#sunset,BuPu,agsunset都很水
#https://blog.r-project.org/2019/04/01/hcl-based-color-palettes-in-grdevices/
cols <- hcl.colors(18, "sunset")
chordDiagram(data_stat, annotationTrack = "grid", preAllocateTracks = 1,big.gap =10,transparency = 0.55,grid.col = cols)
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE,cex=0.8,adj = c(0, 0.5))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.length = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)

#col<-c( aminoglycoside="#8DD3C7" ,bacitracin="#FFFFB3",multidrug="#BEBADA",`beta-lactam`="#80B1D3" ,Others="#FB8072",rifamycin="#B8CAE6",tetracycline="#FDB462",vancomycin="#FCCDE5",Actinobacteria="#BEBADA", Proteobacteria="#BEBADA",Unclassified="#BEBADA",Firmicutes="#BEBADA",Planctomycetes="#BEBADA",Verrucomicrobia="#BEBADA",`Terrabacteria group`="#BEBADA")
#RColorBrewer::display.brewer.all()
#display.brewer.pal(n=12,name="Set3")
#brewer.pal(n=12,name="Set3")
