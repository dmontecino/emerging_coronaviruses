

# Collapsible tree with the full data 

library(collapsibleTree)
library(htmlwidgets)
library(stringr)

gb.all=readRDS( "Data/COV_data_Feb_12_2022.RDS")

gb.all[which(gb.all$class=="Bivalvia"),]$host="Chaerephon plicatus"
gb.all[which(gb.all$class=="Bivalvia"),]$species="Chaerephon plicatus"
gb.all[which(gb.all$class=="Bivalvia"),]$genus="Chaerephon"
gb.all[which(gb.all$class=="Bivalvia"),]$family="Molossidae"
gb.all[which(gb.all$class=="Bivalvia"),]$order="Chiroptera"
gb.all[which(gb.all$class=="Bivalvia"),]$class="Mammalia"


gb.all$CoV=NA

gb.all[is.na(gb.all$species),]$species="Unknown"

gb=unique(gb.all[, c("CoV", "CoV_genus", "subgenus", "class", "order", "family", "species")])

gb[is.na(gb$CoV_genus),]$CoV_genus="Unknown"

gb.not.unknown=gb[gb$species!="Unknown",]


gb.not.unknown[is.na(gb.not.unknown$subgenus), ]$subgenus="Unknown"

gb.not.unknown$subgenus<-str_to_title(gb.not.unknown$subgenus)


cov.genera.hosts.tree <- collapsibleTree( gb.not.unknown, 
                                          colnames(gb.not.unknown), root = "Coronaviridae")

cov.genera.hosts.tree

saveWidget(cov.genera.hosts.tree, file="CoV_dendrogram_interactive.html")
