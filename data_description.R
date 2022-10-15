library(dplyr)
library(tidyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(kableExtra)

#open data created using the "genbank_data_creation.R" script
gb.all=readRDS("Data/COV_data_Feb_12_2022.RDS")

#adding a column to create the figure
gb.all$CoV=NA

# replacing NA values in the species column
gb.all[is.na(gb.all$species),]$species="Unknown"



# editing error in genbank based on the paper
gb.all[which(gb.all$class=="Bivalvia"),]$host="Chaerephon plicatus"
gb.all[which(gb.all$class=="Bivalvia"),]$species="Chaerephon plicatus"
gb.all[which(gb.all$class=="Bivalvia"),]$genus="Chaerephon"
gb.all[which(gb.all$class=="Bivalvia"),]$family="Molossidae"
gb.all[which(gb.all$class=="Bivalvia"),]$order="Chiroptera"
gb.all[which(gb.all$class=="Bivalvia"),]$class="Mammalia"

# subset the data to summarize
gb=unique(gb.all[, c("CoV", "CoV_genus", "subgenus", "class", "order", "family", "species")])

#number of host species

gb%>%filter(species!="Unknown")%>%pull(species) %>% n_distinct()

#number of host species per class

gb%>%distinct(class, species)%>%filter(species!="Unknown")%>%dplyr::count(class)

#number of orders 

gb%>%filter(species!="Unknown")%>%pull(order) %>% n_distinct()

# number of families

gb%>%filter(species!="Unknown")%>%pull(family) %>% n_distinct()
  
  # Comparing the number of hosts versus the total number of species per order
  
  cov.hosts.per.order=
  
    #species in data per order
  gb.all%>%
    filter(species!="Unknown")%>%
    distinct(order, species)%>%
    dplyr::count( order)%>%
    dplyr::rename(order=order,species=n)%>%#%>%dplyr::select(species)%>%sum()
    arrange(order, -species)%>%
    filter(species>10)
  
  # 
  
  cov.hosts.per.order$order<-tolower(cov.hosts.per.order$order)
  
  aves<-read.csv("Data/number of species by class and order Aves IUCN.csv")  
  mammals<-read.csv("Data/number of species by class and order Mammals IUCN.csv")  
  
  #names(aves)
  aves$Name<-tolower(aves$Name)
  mammals$Name<-tolower(mammals$Name)
  
aves.mammals=
  rbind(
  aves%>%
    filter(Name%in%c("anseriformes", "charadriiformes", "passeriformes"))%>%select(Name, Total),
  mammals%>%
    filter(Name%in%c("cetartiodactyla", "carnivora", "chiroptera", "rodentia"))%>%select(Name, Total))
  

aves.mammals$Name[aves.mammals$Name=="cetartiodactyla"]<-"artiodactyla"
aves.mammals<-aves.mammals%>%dplyr::rename(order=Name, total=Total)
aves.mammals$total<-c(177, 384, 6659, 296, 333, 1332, 2375)

prop.hosts.pos.versus.all.hosts.per.order=
left_join(cov.hosts.per.order, aves.mammals, by = "order")%>%
  dplyr::mutate(prop=round(species/total,4)*100)


# number of reports per species

gb.all %>% dplyr::count(species) %>% arrange(-n)

