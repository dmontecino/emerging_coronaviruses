library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(stringr)
# library(kableExtra)


gb_cleaned=readRDS( "Data/Cleaned_dataframe/coronavirus_host_data_Feb_12_2022.RDS")

gb_cleaned[is.na(gb_cleaned$species),]$species<-"Unknown"



# General results


#total number of records 

# nrow(gb_cleaned) #26555

# nrow(gb_cleaned[gb_cleaned$species!="Unknown",]) #24434


# gb.all %>% 
# sort(table(gb.all[gb.all$species!="Unknown",]$species, useNA = "always"))


# taxonomiic host classes

host_taxa=gb_cleaned %>% distinct(class, order, family, species)

host_taxa %>%
  filter(species!="Unknown") %>%
  distinct(class)


#number of host species

host_taxa %>% 
  filter(species!="Unknown") %>%
  select(species) %>% 
  arrange(species) %>% 
  nrow()

# 401  


#number of host species per class

host_taxa %>% 
  dplyr::filter(species!="Unknown") %>% 
  dplyr::count(class)


#number of orders 

host_taxa %>%
  dplyr::filter(species!="Unknown") %>% 
  distinct(order) %>% 
  nrow()



#number of orders per class

host_taxa %>% 
  dplyr::filter(species!="Unknown") %>% 
  distinct(class, order) %>% 
  dplyr::count(class)


# number of families

host_taxa %>%
  dplyr::filter(species!="Unknown") %>% 
  dplyr::distinct(family) %>% 
  nrow()


#number of families per class

host_taxa %>% 
  dplyr::filter(species!="Unknown") %>% 
  distinct(class, family) %>% 
  dplyr::count(class)


#number of host species per order

host_taxa%>%
  filter(species!="Unknown") %>%
  distinct(class, order, species) %>% 
  dplyr::count(order) %>% 
  arrange(n) 


#number of host species per family 

host_taxa%>%
  filter(species!="Unknown") %>%
  distinct(class, order, family, species) %>%
  dplyr::count(class, order, family) %>%
  dplyr::rename(class=class, order=order,num_species=n) %>% #%>%dplyr::select(species)%>%sum()
  arrange(class, -num_species) 





# Coronavirus-host data

host_CoV=gb_cleaned %>% distinct(CoV_genus, subgenus, class, order, family, species)




# number of unique host-Cov genus relatioships

host_CoV%>%
  filter(species!="Unknown") %>%
  distinct(CoV_genus, species) %>%
  nrow() # 581


#number of hosts species per CoV genera

host_CoV%>%
  filter(species!="Unknown") %>%
  distinct(CoV_genus, species) %>%
  dplyr::count(CoV_genus)#%>%select(n)%>%sum()


# #number of families per CoV genera

host_CoV%>%
  filter(species!="Unknown") %>%
  distinct(CoV_genus, class, order, family) %>%
  dplyr::count(CoV_genus)


# #number of orders per CoV genera

host_CoV%>%
  filter(species!="Unknown")%>%
  distinct(CoV_genus, class, order) %>%
  dplyr::count(CoV_genus)


# hosts per Cov genus per  class

host_CoV%>%
  filter(species!="Unknown")%>%
  distinct(CoV_genus, class, order, family, species) %>%
  dplyr::count(class, CoV_genus) %>%
  arrange(CoV_genus)


# mammalia families and Gamma and Delta CoV

host_CoV%>%
  filter(!is.na(family))%>%
  distinct(CoV_genus, class, family)%>%
  filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Mammalia")


# mammalia species and Gamma and Delta CoV

host_CoV%>%
  filter(species!="Unknown")%>%
  distinct(CoV_genus, class, species)%>%
  filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Mammalia")



#number of CoV genera per host (Aves)  

host_CoV%>%
  filter(species!="Unknown")%>%
  filter(CoV_genus%in%c("Gammacoronavirus", "Deltacoronavirus") & class=="Aves")%>%
  distinct(CoV_genus, class, order, family, species)%>%
  dplyr::count(class, order, family, species)%>%
  arrange(-n) #%>%



#order with number of corresponding species that have more than one CoV genera reported

host_CoV%>%
  filter(species!="Unknown")%>%
  distinct(CoV_genus, class, order, family, species)%>%
  filter(class=="Mammalia")%>%
  dplyr::count(class, order, family, species)%>%
  arrange(-n)%>%
  filter(n>1)%>%
  dplyr::count(order)%>%
  arrange(-n)




# beta cov records with betacov subgenera info
host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus" & !is.na(subgenus))%>%
  nrow() #110

#number of host species of betacoronavirus subgenera
host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus") %>%
  dplyr::count(subgenus)


# number of hosts species per class per order per CoV subgenera

host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(subgenus, class, order, species)%>%
  dplyr::count(subgenus, class, order)%>%
  arrange(subgenus, -n)


#number of bat species per subgenera
host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(subgenus, class, order, species)%>%
  filter(order=="Chiroptera")%>%
  dplyr::count(subgenus)


# host species per class, order and family per CoV subgenera

host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(subgenus, class, order, family, species)%>%
  dplyr::count(subgenus, class, order, family)%>%
  arrange(subgenus, order, family, -n)


# number of host species per taxonomic family with Sarbeco BetaCoV subgenus detected

host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(subgenus, class, order, family, species)%>%
  filter(order=="Chiroptera" & subgenus=="Sarbeco")%>%
  dplyr::count(family)




#taxonomic families with more than one Betacoronavirus subgenus and the
#number of subgenera

host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(family, subgenus)%>%
  filter(!is.na(subgenus)) %>% 
  dplyr::count(family) %>% 
  arrange(-n)



# species with more than one betaCoV subgenus

host_CoV%>%
  filter(species!="Unknown" & CoV_genus=="Betacoronavirus")%>%
  distinct(species, subgenus)%>%
  filter(!is.na(subgenus)) %>% 
  dplyr::count(species) %>% 
  filter(n>1) %>% 
  select(species)






# bat species per family with sarbecocovirus detected

host_CoV%>%
  filter(species!="Unknown" & subgenus=="Sarbeco" & order =="Chiroptera")%>%
  distinct(order, family, species)%>%
  dplyr::count(family)

# bat species per family with merbecovirus detected

host_CoV%>%
  filter(species!="Unknown" & subgenus=="Merbeco" & order =="Chiroptera")%>%
  distinct(order, family, species)%>%
  dplyr::count(family)  

# carnivore species per families with sarbecoviruses detected

host_CoV%>%
  filter(species!="Unknown" & subgenus=="Sarbeco" & order =="Carnivora")%>%
  distinct(order, family, species)%>%
  dplyr::count(family)  

# species of other families with sarbecoviruses detected

host_CoV%>%
  filter(species!="Unknown" & subgenus=="Sarbeco" & !(order%in%c("Carnivora", "Chiroptera")))%>%
  distinct(order, family, species)%>%
  dplyr::count(family)  










# comparing the number of hosts versus the total number of species per order

cov.hosts.per.order=
  
  #species in data per order
  host_CoV%>%
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
      filter(Name%in%c("anseriformes", "charadriiformes"))%>%select(Name, Total),
    mammals%>%
      filter(Name%in%c("cetartiodactyla", "carnivora", "chiroptera", "rodentia"))%>%select(Name, Total))


aves.mammals$Name[aves.mammals$Name=="cetartiodactyla"]<-"artiodactyla"
aves.mammals<-aves.mammals%>%dplyr::rename(order=Name, total=Total)
aves.mammals$total<-c(177, 384, 296, 333, 1332, 2375)

prop.hosts.pos.versus.all.hosts.per.order=
  left_join(cov.hosts.per.order,
            aves.mammals, 
            by = "order") %>%
  dplyr::mutate(prop=round(species/total,4)*100)

prop.hosts.pos.versus.all.hosts.per.order
