

# Collapsible tree with the full data 

library(collapsibleTree)
library(htmlwidgets)
library(stringr)

dat=readRDS("Data/Cleaned_dataframe/coronavirus_host_data_Feb_12_2022.RDS")


dat$CoV=NA

dat[is.na(dat$species),]$species="Unknown"

dat=unique(dat[, c("CoV", "CoV_genus", "subgenus", "class", "order", "family", "species")])

dat[is.na(dat$CoV_genus),]$CoV_genus="Unknown"

dat=dat[dat$species!="Unknown",]


dat[is.na(dat$subgenus), ]$subgenus="Unknown"

dat$subgenus<-str_to_title(dat$subgenus)


cov.genera.hosts.tree <- collapsibleTree( dat, 
                                          colnames(dat), root = "Coronaviridae")

cov.genera.hosts.tree

# dir.create("CoV_hosts_full")

saveWidget(cov.genera.hosts.tree, file="CoV_hosts_full/CoV_hosts_full_interactive.html", selfcontained = T)
