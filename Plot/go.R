library(tidyverse)
library(ontologyIndex)
library(pheatmap)
library(RColorBrewer)
library(openxlsx)
library(stats)
humann3_go <- read.xlsx("C:/Users/USER/Desktop/GO_humann.xlsx",colNames = T,sheet=1)
# Rename column
colnames(humann3_go) <- c("Gene",
                          "T1-W-1","T1-W-2","T1-W-3",
                          "T2-W-1","T2-W-2","T2-W-3",
                          "T3-W-1","T3-W-2","T3-W-3",
                          "T4-W-1","T4-W-2","T4-W-3",
                          "T5-W-1","T5-W-2","T5-W-3")
# Filter out taxonomy row
go <- humann3_go %>% 
  filter(!(grepl(pattern = "\\|", x = Gene)))
write.xlsx(go,"C:/Users/USER/Desktop/humann_analysis/go_adjust.xlsx")
# Transform to gather format
gather_go <- gather(go, key = "sample", value = "cpm", 2:16)
# Calculate z-score 
gather_go <- gather_go %>% group_by(Gene) %>% 
  mutate(z_score = (cpm-mean(cpm))/sd(cpm))
# adding group annonation
gather_go$sample_type <- gather_go$sample
sample_name<-c("T1-W-1","T1-W-2","T1-W-3",
               "T2-W-1","T2-W-2","T2-W-3",
               "T3-W-1","T3-W-2","T3-W-3",
               "T4-W-1","T4-W-2","T4-W-3",
               "T5-W-1","T5-W-2","T5-W-3")
sample_type<-rep(c("Raw","Finished","Upsteram","Midstream","Downstream"),each=3)
for(i in seq_along(sample_name)){
  gather_go[["sample_type"]][gather_go[["sample_type"]]== sample_name[i]] <-    sample_type[i]
  }

# Reorder gene by dominant gene in ARP
#go_zscore <- go_zscore %>% group_by(Gene) %>% mutate(ARP_mean_z = (`T5-W-1`+`T5-W-2`+`T5-W-3`/3))
#go_zscore <- go_zscore %>% arrange(desc(ARP_mean_z))
#go_zscore <- go_zscore %>% ungroup() %>% select(!(ARP_mean_z))

# Remove unnecessary column
go_zscore <- gather_go %>% ungroup() %>% select(Gene, sample, z_score)
# Transform to spread format
go_zscore <- spread(go_zscore, key = "sample", value = "z_score")
# Import GO database & select gene
GO_db <- get_OBO("C:/Users/USER/Desktop/go-basic.obo")

#GO_db$name[["GO:0000747"]] # View GO name
#get_term_property(ontology=GO_db , property="ancestors", 
                 # term="GO:0006950", as_names=TRUE) # View GO ancestors
#get_term_property(ontology=GO_db , property="children", 
                 # term="GO:0006950", as_names=TRUE) # View GO children
##### Choose one GO for analysis ------------------------------------------------------

HGT_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0009292")] # Horizontal gene transfer
Spore_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0043934")] # Sporulation
stress_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0006950")]# Stress
chemical_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0042221")]# chemical
ar_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0046677")]# antibiotic
starvation_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0031667")]# starvation
agg_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0098743")]#cell aggregation
detox_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0098754")]#detoxification
regulation_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0065007")]#regulation
secretion_subGene <- GO_db$id[grep(GO_db$ancestors, pattern="GO:0046903")]#secretion
ox_stress_subGene<- GO_db$id[grep(GO_db$ancestors, pattern="GO:0006979")]#oxidative stress
redox_state_subGene<- GO_db$id[grep(GO_db$ancestors, pattern="GO:0051775")]#redox state
abotic_stimulus_subGene<- GO_db$id[grep(GO_db$ancestors, pattern="GO:0009628")]#abiotic stimulus

ar_subGene<-stress_subGene
# Fiter select gene from GO database
tmp <- go_zscore %>% filter(grepl(pattern = ar_subGene[1], x = Gene))
for (i in 2:length(ar_subGene)){
  tmp2 <- go_zscore %>% filter(grepl(pattern = ar_subGene[i], x = Gene))
  tmp <- rbind(tmp,tmp2)
}
stress_go <- tmp
# Transform to matrix 
stress_go_mat <- as.matrix(stress_go[,-1])
row.names(stress_go_mat) <- stress_go$Gene
# Edit row.name 
row.names(stress_go_mat) <- gsub('.\\[BP\\]','',row.names(stress_go_mat))
# Annotation row
annotation_row = data.frame(Sample = factor(rep(c("Raw","Finished","Upsteram","Midstream","Downstream"),each=3)))
rownames(annotation_row) = colnames(stress_go_mat)
# Color
display.brewer.all()
brewer.pal(7, "Set3")
ann_colors = list(Sample = c(Raw= "#FB8072", Finished = "#80B1D3", Upsteram = "#B3DE69",Midstream ="#BEBADA",Downstream ="#8DD3C7" ))
# Plot
p <-pheatmap(stress_go_mat, cluster_cols = FALSE, 
             clustering_distance_rows = "euclidean",
             annotation_col = annotation_row, annotation_colors = ann_colors,
             fontsize = 10, fontsize_row = 6, fontsize_col = 7,
             cellwidth = 7, cellheight = 6, bg = "transparent")

#transformation plot
pt <-pheatmap(t(stress_go_mat), cluster_rows = FALSE, 
              clustering_distance_rows = "euclidean",
              annotation_row = annotation_row, annotation_colors = ann_colors,
              fontsize = 10, fontsize_row = 6, fontsize_col = 7,
              cellwidth = 7, cellheight = 6, bg = "transparent")
