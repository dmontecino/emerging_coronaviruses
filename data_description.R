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
  
# #number of orders per class
# 
# gb%>%filter(species!="Unknown")%>%distinct(class, order)%>%dplyr::count(class)

# number of families

gb%>%filter(species!="Unknown")%>%pull(family) %>% n_distinct()

# #number of families per class
# 
# gb%>%filter(species!="Unknown")%>%distinct(class, order, family)%>%dplyr::count(class)
# 
# 
# 
# #number of host species per order
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(class, order, species)%>%
#   dplyr::count(class, order)%>%
#   dplyr::rename(order=order,species=n)%>%#%>%dplyr::select(species)%>%sum()
#   arrange(class, -species)
# 
# 
# 
# #number of host species per family class 
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(class, order, family, species)%>%
#   dplyr::count(class, order, family)%>%
#   dplyr::rename(class=class, order=order,species=n)%>%#%>%dplyr::select(species)%>%sum()
#   arrange(class, -species) %>%kbl()
# 
# 
# # number of unique host-Cov genus relatioships
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, species)%>%nrow()
# 
# #number of hosts per genera
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, species)%>%
#   dplyr::count(CoV_genus)#%>%select(n)%>%sum()
#   
# 
# 
# # #number of families per CoV genera
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, family)%>%
#   dplyr::count(CoV_genus)
# 
# 
# # #number of orders per CoV genera
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, order)%>%
#   dplyr::count(CoV_genus)
# 
# 
# # hosts per Cov genus per  class
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, class, species)%>%
#   dplyr::count(class, CoV_genus)%>%
#   arrange(CoV_genus)
# 
# 
# # mammal families and Gamma and Delta CoV
# 
# gb.all%>%
#   filter(!is.na(family))%>%
#   distinct(CoV_genus, class, family)%>%
#   filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Mammalia")
# 
# 
# # mammal species and Gamma and Delta CoV
# # 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, class, species)%>%
#   filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Mammalia")
# 
# # papers associated
# gb.all%>%
#   filter(species!="Unknown")%>%
#   filter(CoV_genus%in%c("Gammacoronavirus") & family=="Monodontidae")
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   filter(CoV_genus%in%c("Gammacoronavirus") & family=="Delphinidae")
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   filter(CoV_genus%in%c("Gammacoronavirus") & family=="Camelidae")
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   filter(CoV_genus%in%c("Deltacoronavirus") & family=="Suidae")
# 
# 
# #number of CoV genues per host (Aves)  
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, class, order, family, species)%>%
#   filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Aves")%>%
#   dplyr::count(class, order, family, species)%>%
#   arrange(-n)%>%
#   filter(n>1)%>%dplyr::count(order)%>%arrange(-n)
# 
# 
# #order with species that have more tha one  CoV genues reported
# 
# gb.all%>%
#   filter(species!="Unknown")%>%
#   distinct(CoV_genus, class, order, family, species)%>%
#   filter(CoV_genus%in%c("Alphacoronavirus", "Betacoronavirus") & class=="Mammalia")%>%
#   dplyr::count(class, order, family, species)%>%
#   arrange(-n)%>%
#   filter(n>1)%>%dplyr::count(order)%>%arrange(-n)
# 
# 
# 
# # records with betacov 
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   nrow()
# 
# # beta cov records with betacov dubgenera info
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus" & !is.na(subgenus))%>%
#   nrow()
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   dplyr::count(subgenus)
# 
# # order of hosts per CoV subgenus
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, family, species)%>%
#   dplyr::count(subgenus, class, order)%>%
#   arrange(subgenus, -n)
# 
# #number of bat species per subgenus
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, species)%>%
#   filter(order=="Chiroptera")%>%
#   dplyr::count(subgenus)
#   
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, species)%>%
#   filter(order=="Chiroptera" & subgenus%in%c("Embeco", "Nobeco", "Hibeco"))%>%
#   distinct(species)
# 
# 
# 
# # host species per order per CoV subgenus
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, species)%>%
#   dplyr::count(subgenus, class, order)%>%
#   arrange(subgenus, order, family, -n)
# 
# # family of hosts per CoV subgenus
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, family, species)%>%
#   dplyr::count(subgenus, class, order, family)%>%
#   arrange(subgenus, order, family, -n)
# 
# gb.all%>%
#   filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#   distinct(subgenus, class, order, family, species)%>%
#   filter(order=="Chiroptera" & subgenus=="Sarbeco")%>%
#   dplyr::count(family)
# 
# 
# # families with more than oe subgenus
# 
#   gb.all%>%
#     filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#     distinct(family, subgenus)%>%
#     group_by(family, subgenus)%>% 
#     dplyr::summarise(n=n(), .groups = 'drop')%>%
#     spread(subgenus, n, fill=0)%>%
#     dplyr::mutate(total=rowSums(.[2:6]))%>%
#     arrange(-total)%>%select(family)
#   
#   
#   gb.all%>%
#     filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
#     distinct(species, subgenus)%>%
#     group_by(species, subgenus)%>% 
#     dplyr::summarise(n=n(), .groups = 'drop')%>%
#     spread(subgenus, n, fill=0)%>%
#     mutate(total=rowSums(.[2:6]))%>%
#     arrange(-total)%>%
#     filter(total>1)
#   
#   
# # family of bats sourcing sarbecovirus  
#   
#   gb.all%>%
#     filter(species!="Unknown" & subgenus=="Sarbeco" & order =="Chiroptera")%>%
#     distinct(order, family, species)%>%
#     dplyr::count(family)
# 
#   # family of bats sourcing merbecovirus  
#   
#   gb.all%>%
#     filter(species!="Unknown" & subgenus=="Merbeco" & order =="Chiroptera")%>%
#     distinct(order, family, species)%>%
#     dplyr::count(family)  
#   
#   # carnivore families with sarbecoviruses
#   
#   gb.all%>%
#     filter(species!="Unknown" & subgenus=="Sarbeco" & order =="Carnivora")%>%
#     distinct(order, family, species)%>%
#     dplyr::count(family)  
#   
  
  
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

