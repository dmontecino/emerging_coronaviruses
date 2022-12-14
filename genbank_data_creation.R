library(stringr)
library(stringi)
library(rentrez)
library(readxl)
library(parallel)
library(tidyr)
library(dplyr)
library(myTAI)
library(purrr)
library(plyr)

#Genbank search test
#((((deltacoronavirus) NOT human) NOT homo) NOT sapiens) 
#((((gammacoronavirus) NOT human) NOT homo) NOT sapiens) 
#((((betacoronavirus) NOT human) NOT homo) NOT sapiens) 
#((((alphacoronavirus) NOT human) NOT homo) NOT sapiens) 
#((((coronavirus) NOT human) NOT homo) NOT sapiens)   

# load data
#remember to download the files in here 
#https://drive.google.com/drive/u/0/folders/1Ty-mPdDAKPGYe2M-SYzAJwEQByE433qq 
#and place them in the "Data" folder of your cloned repository

file.paths=grep("genbank_exported_from_Geneious_Prime", list.files("Data/"), value = T)

all.sequence.data=mclapply(file.paths, function(x) read.csv(paste0("Data/", x)), mc.cores = 6)

# if the sequence column is present take it out
all.sequence.data=mclapply(all.sequence.data, function(x) if("Sequence" %in% colnames(x)) x%>%dplyr::select(-Sequence) else x, mc.cores = 6)


#removing all homo sapiens host

all.sequence.data= mclapply(all.sequence.data, function(x) x%>%filter(host!="Homo sapiens"), mc.cores = 6)

all.sequence.data= mclapply(all.sequence.data, function(x) x%>%filter(!grepl("Human|human|huamn", host)), mc.cores = 6)

all.sequence.data= mclapply(all.sequence.data, function(x) x%>%filter(!grepl("Homo|sapiens", host)), mc.cores = 6)

# all.sequence.data= mclapply(all.sequence.data, function(x) x%>%filter(host!="sapiens"), mc.cores = 6)

#removing observations where there is a lab host
all.sequence.data=mclapply(all.sequence.data, function(x) if("lab_host"%in%colnames(x)) x%>%filter(lab_host=="") else x, mc.cores = 6)


# select the columns of interest
all.sequence.data=mclapply(all.sequence.data, function(x) x%>%select(Name, Accession, Common.Name,
                                                                     Description,note, Molecule.Type, Organism,
                                                                     Taxonomy, collection_date, Created.Date, country, host, ), mc.cores = 6)



dat=do.call(rbind, all.sequence.data)

dat=unique(dat)



# remove rows that do not have the "coronaviridae" in the taxonomy

# dat2=dat%>%filter(grepl("coronavirus", Taxonomy, ignore.case = T))
dat=dat%>%filter(grepl("coronaviridae", Taxonomy, ignore.case = T))


# Check the host species
sort(unique(dat$host))


# Vero 6 is removed
dat=dat%>%filter(host!="Vero E6")


# remove the extra words in the host species

# test=unlist(sapply(dat$host, function(x) if(length(x)>0) strsplit(x, split = ";")[[1]] else x), use.names = F)

# all(sapply(test, length, USE.NAMES = F)==1)


dat$host[which(grepl(";", dat$host))]=sapply(strsplit(dat$host[which(grepl(";", dat$host))], split = ";"), "[[", 1)


#remove the content in parenthesis

dat$host[which(grepl("\\(", dat$host))]=sapply(strsplit(dat$host[which(grepl("\\(", dat$host))], split = "\\("), "[[", 1)

# convert anything with "dog" to canis familiaris

dogs=unique(dat$host[which(grepl("dog|Dog", dat$host))])[-2] # number 2 is the racoon dog
dat$host[dat$host%in%dogs]="Canis lupus familiaris"

# anything with chicken, broiler, hen, layer, poultry is Gallus gallus

chicken=unique(dat$host[which(grepl("chicken|broiler|layer|poultry|Layer|chick|chikcen|Chicken|Broiler", dat$host))])
dat$host[dat$host%in%chicken]="Gallus gallus"


# anything with Gallus gallus (e,g. Gallus gallus domesticus is Gallus gallus

chicken=unique(dat$host[which(grepl("Gallus gallus|Gallus domesticus", dat$host))])
dat$host[dat$host%in%chicken]="Gallus gallus"


# anuthing ith Equus caballus, horse or Horse is Equus caballus

horse=unique(dat$host[which(grepl("Equus caballus|horse|Horse", dat$host))])
dat$host[dat$host%in%horse]="Equus caballus"

# anything with pig, piglet, swine, porcine is Sus scrofa

pig=unique(dat$host[which(grepl("Pig|pig|piglet|Swine|swine|Porcine|porcine|boar|scrofa", dat$host))])
dat=dat[dat$host!="large pig roundworm",]
dat$host[dat$host%in%pig[-6]]="Sus scrofa"

# dat$host[which(grepl("Sus scrofa|Sus_scrofa", dat$host))]="Sus scrofa"

# anything with calf, cow is Bos taurus

cow=unique(dat$host[which(grepl("calf|cow|Bos taurus|Bos Taurus", dat$host))])
dat$host[dat$host%in%cow]="Bos taurus"

# anything with domestic cat is Felis catus

cat=unique(dat$host[which(grepl("domestic cat|catus", dat$host))])[-5] # 5 is not a cat
dat$host[dat$host%in%cat]="Felis catus"

# anything with donkey is Equus asinus
donkey=unique(dat$host[which(grepl("donkey|Donkey", dat$host))])
dat$host[dat$host%in%donkey]="Equus asinus"

# anything with Mus musculus is Mus musculus
mouse=unique(dat$host[which(grepl("Mus musculus|Mus_musculus|mouse", dat$host))])
dat$host[dat$host%in%mouse]="Mus musculus"

# anything not provided or unknwon is Unknown

unknown=unique(dat$host[which(grepl("not provided|unknown", dat$host))])
dat$host[dat$host%in%unknown]="Unknown"

# anything with bat is Chiroptera

# unique(dat$host[which(grepl("Bat|bat", dat$host))])
chiroptera=unique(dat$host[which(grepl("Bat|bat", dat$host))])
dat=dat[dat$host!="liquid chorioallantoic of embryonated egg with 18 days of incubation",]
dat$host[dat$host%in%chiroptera]="Chiroptera"

#anything with birds bird avian is Aves
avian=unique(dat$host[which(grepl("Avain|avian|bird|Birds", dat$host))])
dat$host[dat$host%in%avian]="Aves"

#anything with camels is the same species
camel=unique(dat$host[which(grepl("camel|Camel|Dromedary", dat$host))])
dat$host[dat$host%in%camel]="Camelus dromedarius"


# laboratory work removed
dat=dat[dat$Description!="Structure of SARS-CoV-2 replication-transcription complex bound to nsp13 helicase - nsp13(2)-RTC",]

# dat=dat[!(grepl(pattern = "Severe acute respiratory syndrome coronavirus 2 isolate ", dat$Description)),]
temp=dat[grepl(pattern = "Severe acute respiratory syndrome coronavirus 2 isolate ", dat$Description),]
# unique(temp$host)

# all hosts only first word capitalized

dat$host=str_to_sentence(dat$host)

# replace spp with sp.
dat$host=gsub(pattern = " spp", replacement = " sp.", fixed = T, dat$host)

#replace ".sp" with nothing
dat$host=gsub(pattern = " sp.", replacement = " ", fixed = T, dat$host)

#replace .cf with nothing 

dat$host=gsub(pattern = " cf. ", replacement = " ", fixed = T, dat$host)


# if the host has a space at the end, then delete the space
dat$host=sapply(dat$host, function(x) 
  if(stri_count(x,regex="\\S+")>1) word(x, start=1, end=stri_count(x,regex="\\S+")) 
  else gsub(" ", replacement = "", x = x), USE.NAMES = F)



dat[dat$host=="Cattle egret",]$host="Bubulcus ibis"
dat[dat$host=="Watusi cattle",]$host="Bos taurus"
dat[dat$host=="Cattle",]$host="Bos taurus"
dat[dat$host=="Asiatic lion",]$host="Panthera leo persica"
dat[dat$host=="Rat",]$host="Rattus ratus"
dat[dat$host=="" & dat$Organism=="Tylonycteris bat coronavirus HKU4", ]$host="Tylonycteris sp"
dat[dat$host=="" & dat$Organism=="Pipistrellus bat coronavirus HKU5", ]$host="Pipistrellus sp"
dat[dat$host=="" & dat$Organism=="Bat SARS coronavirus HKU3", ]$host="Chiroptera"
dat[dat$host=="Aselliicus stoliczkanus",]$host="Aselliscus stoliczkanus"
dat[dat$host=="Bottlenose dolphin",]$host="Tursiops aduncus"
dat[dat$host=="Hipposideroa pratti",]$host="Hipposideros pratti"
dat[dat$host=="Pipistrellus sp",]$host="Pipistrellus"
dat[dat$host=="Pipistrellys abramus",]$host="Pipistrellus abramus"
dat[dat$host=="Rhinolphus affinis",]$host="Rhinolophus affinis"
dat[dat$host=="Tylonycteris sp",]$host="Tylonycteris"
dat[dat$host=="Quail", ]$host="Coturnix sp."
dat[dat$host=="Alpaca",]$host="Vicugna pacos"
dat$host[grepl("Ferret", dat$host)]="Mustela putorius"
dat$host[grepl("Antelope", dat$host)]="Bovidae sp"
dat$host[grepl("Bovidae", dat$host)]="Bovidae sp"
dat$host[grepl("Bean goose", dat$host)]="Anseriformes"
dat$host[grepl("Black-headed gull", dat$host)]="Chroicocephalus ridibundus"
dat$host[grepl("Brentgoose", dat$host)]="Branta bernicla"
dat$host[grepl("Buffalo", dat$host)]="Bovidae sp"
dat$host[grepl("Familiaris", dat$host)]="Canis familiaris"
dat$host[grepl("Canine", dat$host)]="Canidae sp"
dat$host[grepl("Canis lupus famaliaris", dat$host)]="Canis lupus familiaris"
dat$host[grepl("Coot", dat$host)]="Fulica sp"
dat$host[grepl("Common pheasant", dat$host)]="Phasianus colchicus"
dat$host[grepl("Common gull", dat$host)]="Larus canus"
dat$host[grepl("Common snipe", dat$host)]="Gallinago gallinago"
dat$host[grepl("Common tern", dat$host)]="Sterna hirundo"
dat$host[grepl("Common teal", dat$host)]="Anas crecca"
dat$host[grepl("Common starling", dat$host)]="Sturnus vulgaris"
dat$host[grepl("Chiroptera", dat$host)]="Chiroptera"
dat$host[grepl("Cormoran", dat$host)]="Phalacrocoracidae"
dat[dat$host=="Wisent",]$host="Bison bonasus"
dat[dat$host=="Himalayan tahr",]$host="Hemitragus jemlahicus"
dat[dat$host=="A.stoliczkanus",]$host="Aselliscus stoliczkanus"
dat[dat$host=="Sitatunga",]$host="Tragelaphus spekii"
dat[dat$host=="Nyala",]$host="Tragelaphus angasii"
dat[dat$host=="Columbia livia",]$host="Columba livia"
dat[dat$host=="Yak",]$host="Bos grunniens"
dat[dat$host=="Wigeon",]$host="Mareca"
dat[dat$host=="Goat",]$host="Capra aegagrus"
dat[dat$host=="Teal",]$host="Anas sp"
dat[dat$host=="Western sandpiper",]$host="Calidris mauri"
dat[dat$host=="Waterbuck",]$host="Kobus ellipsiprymnus"
dat[dat$host=="Turkey",]$host="Meleagris gallopavo"
dat[dat$host=="Swan",]$host="Cygnus cygnus"
dat[dat$host=="Snow goose",]$host="Anser caerulescens"
dat[dat$host=="Sambar deer",]$host="Rusa unicolor"
dat[dat$host=="White-eye",]$host="Zosteropidae"
dat[dat$host=="White-tailed deer",]$host="Odocoileus virginianus"
dat[dat$host=="White-rumped munia",]$host="Lonchura striata"
dat[dat$host=="Tapir",]$host="Tapiridae"
dat[dat$host== "Magpie-robin", ]$host="Copsychus saularis"
dat[dat$host== "Night-heron", ]$host="Nycticorax nycticorax"
dat[dat$host== "Red-necked avocet", ]$host="Recurvirostra novaehollandiae"
dat[dat$host== "Red-whiskered bulbul", ]$host="Pycnonotus jocosus"
dat[dat$host== "Sow", ]$host="Sus scrofa"
dat[dat$host== "Sparrow", ]$host="Passer"
dat[dat$host== "Tufted duck", ]$host="Aythya fuligula"
dat[dat$host== "Spotted hyena", ]$host="Crocuta crocuta"
dat[dat$host== "Sunda pangolin", ]$host="Manis javanica"
dat[dat$host== "Pintail", ]$host="Anas acuta"
dat[dat$host== "Rock sandpiper", ]$host="Calidris ptilocnemis"
dat[dat$host=="Scotphilus kuhli large intestine",]$host="Scotophilus kuhli"
dat[dat$host=="Pheasant",]$host="Phasianus colchicus"
dat[dat$host=="Peafowl",]$host="Phasianidae"
dat[dat$host=="Pangolin",]$host="Pholidota"
dat[dat$host=="Phoca vitulina richardsii",]$host="Pholidota"
dat[dat$host=="Palm civet", ]$host="Paradoxurus hermaphroditus"
dat[dat$host=="Palm civet cats", ]$host="Paradoxurus hermaphroditus"
dat[dat$host=="Civet", ]$host="Paradoxurus hermaphroditus"
dat[dat$host=="Pecari tajacu", ]$host="Dicotyles tajacu"
dat[dat$host=="Rodent", ]$host="Rodentia"
dat[dat$host=="Mute swan", ]$host="Cygnus olor"
dat[dat$host== "Mink", ]$host="Mustelidae"
dat[dat$host== "Miniopterus .", ]$host="Miniopterus"
dat[dat$host=="Mallard duck", ]$host="Anas platyrhynchos"
dat[dat$host=="Leopard cat", ]$host="Prionailurus bengalensis"
dat[dat$host=="House sparrow", ]$host="Passer domesticus"
dat[dat$host=="Hering gull", ]$host="Larus argentatus"
dat[dat$host=="Gull", ]$host="Larus"
dat[dat$host=="Guinea fowl", ]$host="Numididae"
dat[dat$host=="Goose", ]$host="Anatidae"
dat[dat$host=="Feline", ]$host="Felidae"
dat[dat$host=="Fox", ]$host="Canidae"
dat[dat$host=="Fulica sp", ]$host="Fulica"
dat[dat$host=="Giant panda", ]$host="Ailuropoda melanoleuca"
dat[dat$host== "Glaucous-winged gull", ]$host="Larus glaucescens"
dat[dat$host== "Glaucus-gull", ]$host="Larus hyperboreus"
dat[dat$host=="Giraffe", ]$host="Giraffa camelopardalis"
dat[dat$host=="Grey-backed thrush", ]$host="Turdus hortulorum"
dat[dat$host=="Greylag goose", ]$host="Anser anser"
dat[dat$host=="H.larvatus", ]$host="Hipposideros larvatus"
dat[dat$host=="H.pomona", ]$host="Hipposideros pomona"
dat[dat$host=="Ozimops ", ]$host="Ozimops"
dat[dat$host=="Dromedary", ]$host="Camelus dromedarius"
dat[dat$host=="Duck", ]$host="Anatidae"
dat[dat$host=="Coturnix sp." , ]$host="Coturnix"
dat[dat$host=="Dicotyles tajacu" , ]$host="Pecari tajacu"
dat[dat$host=="Cynopterus brachyotis large intestine", ]$host="Cynopterus brachyoti"
dat[dat$host=="Common magpie" , ]$host="Pica pica"
dat[dat$host=="Chiropteron", ]$host="Chiroptera"
dat[dat$host=="Chinese pangolin", ]$host="Manis pentadactyla"
dat[dat$host=="Chinese bulbul", ]$host="Pycnonotus sinensis"
dat[dat$host=="Cavia_porcellus", ]$host="Cavia porcellus"
dat[dat$host=="Catus", ]$host="Felis catus"
dat[dat$host=="Canis familiaris" , ]$host="Canis lupus familiaris"
dat[dat$host=="Canidae sp"  , ]$host="Canidae"
dat[dat$host=="Bovidae sp", ]$host="Bovidae"
dat[dat$host=="Apodemus .", ]$host="Apodemus"
dat[dat$host=="Blue-winged teal" , ]$host= "Spatula discors"  
dat[dat$host=="Anas sp", ]$host="Anas"
dat[dat$host=="Brent-goose", ]$host="Branta bernicla"         
dat[dat$host=="Chaerephon plicata", ]$host="Chaerephon plicatus"  
dat[dat$host=="Cynopterus brachyoti", ]$host="Cynopterus brachyotis"  
dat[dat$host=="Eptescus serotinus", ]$host="Eptesicus serotinus"  
dat[dat$host=="Nannopterum brasilianus", ]$host="Phalacrocorax brasilianus"
dat[dat$host=="Neoromicia capensis", ]$host="Laephotis capensis"
dat[dat$host=="Rattus ratus",]$host="Rattus rattus"
dat[dat$host=="Rattus ",]$host="Rattus"
dat[dat$host=="Rhinilophus ferrumequinum",]$host="Rhinolophus ferrumequinum"
dat[dat$host=="Rhinolophus smithersi",]$host="Rhinolophus smithersi"
dat[dat$host=="Vespadelus baverstocki",]$host="Vespadelus baverstocki"
dat=dat[dat$host!="Breeder",]
dat[dat$host=="Bovine",]$host="Bovidae"
dat[dat$host=="Cat",]$host="Felis catus"
dat[dat$host=="Chichen",]$host="Gallus gallus"
dat[dat$host=="Common moorhen",]$host="Gallinula chloropus"
dat[dat$host=="Laying hens",]$host="Gallus gallus"
dat[dat$host=="Pigeon",]$host="Columbidae"
dat[dat$host=="Raccoon dog",]$host="Nyctereutes procyonoides"
dat[dat$host=="Scotophilus kuhli",]$host="Scotophilus kuhlii"
dat[dat$host=="Rabbit",]$host="Oryctolagus cuniculus"
dat[dat$host=="Hipposidero melanopogon",]$host="Hipposideros melanopogon"
dat[dat$host=="Eptesicus serotinus horikawai",]$host="Eptesicus serotinus"
dat[dat$host=="Myotis daubentoni petax",]$host="Myotis petax" 

# anything with water, liquid, is removed

dat=dat%>%filter(!grepl("water|liquid|Environment sample|Wastewater", host))


# rows with unknown host are removed
dat=dat[dat$host!="Unknown",]

# rows with the word vaccine or Patent or Immunogenic in the description are removed

dat=dat[!(grepl("vaccine|Vaccine|VACCINE|Patent|PATENT|Immunogenic|IMMUNOGENIC|Chain|CHAIN|UNVERIFIED|ATTENUATED|Crystal|Remdesivir|PARAMYXOVIRIDAE", x = dat$Description)),]

dat=dat[!(grepl("Biological Material|REAGENTS AND METHODS FOR DETECTING SEVERE ACUTE RESPIRATORY SYNDROME CORONAVIRUS", x = dat$Description)),]

dat=dat[!(grepl("MULTI-ALLELIC MOLECULAR DETECTION OF SARS-ASSOCIATED CORONAVIRUS", x = dat$Description)),]

dat=dat[!(grepl("ENZYMATIC DNA MOLECULES", x = dat$Description)),]

dat=dat[!(grepl("ANTISENSE ANTIVIRAL COMPOUND AND METHOD FOR TREATING SSRNA VIRAL INFECTION", x = dat$Description)),]

dat=dat[!(grepl("A Preventive Or Therapeutic Agent For Feline Infectious Peritonitis Virus", x = dat$Description)),]

dat=dat[!(grepl("MULTI-ALLELIC MOLECULAR DETECTION OF SARS-ASSOCIATED CORONAVIRUS", x = dat$Description)),]

dat=dat[!(grepl("RESEQUENCING PATHOGEN MICROARRAY", x = dat$Description)),]

dat=dat[!(grepl("Nucleic acid sequence comprising the RNA packaging signal of group 1 coronavirus and the applications thereof", x = dat$Description)),]

dat=dat[!(grepl("Means and methods for distinguishing FECV and FIPV", x = dat$Description)),]

dat=dat[!(grepl("COMPUTATIONAL METHOD FOR IDETIFYING ADHESIN", x = dat$Description)),]

dat=dat[!(grepl("A sensitive and specific test to detect SARS coronavirus", x = dat$Description)),]

dat=dat[!(grepl("A COMPUTER BASED VERSATILE METHOD FOR IDENTIFYING PROTEIN CODING DNA SEQUENCES USEFUL AS DRUG TARGETS", x = dat$Description)),]

dat=dat[!(grepl("IMAGEABLE ANIMAL MODEL OF SARS INFECTION", x = dat$Description)),]

dat=dat[!(grepl("Compositions and Methods for Determining the Presence of SARS Coronavirus in a Sample", x = dat$Description)),]

dat=dat[!(grepl("MODIFIED SMALL INTERFERING RNA MOLECULES AND METHODS OF USE", x = dat$Description)),]

dat=dat[!(grepl("RNAI THERAPEUTIC FOR RESPIRATORY VIRUS INFECTION", x = dat$Description)),]

dat=dat[!(grepl("Method for preparing sample used for amplification of nucleic acids", x = dat$Description)),]

dat=dat[!(grepl("A method for producing a virus protease", x = dat$Description)),]

dat=dat[!(grepl("Oligonucleotide analogs having cationic intersubunit linkages", x = dat$Description)),]

dat=dat[!(grepl("Modified Microbial Nucleic Acid", x = dat$Description)),]

dat=dat[!(grepl("ANTIBODIES TO SARS CORONAVIRUS", x = dat$Description)),]

dat=dat[!(grepl("MODIFIED SMALL INTERFERING RNA MOLECULES AND METHODS OF USE", x = dat$Description)),]

dat=dat[!(grepl("S2=glycoprotein [porcine hemagglutinating encephalomyelitis virus HEV, 67N, Genomic, 645 nt]", x = dat$Description)),]

dat=dat[!(grepl("XXX", x = dat$Description)),]

dat=dat[!(grepl("cDNA encoding antigenic peptide of IBV", x = dat$Description)),]

dat=dat[!(grepl("passage", x = dat$Description)),]

dat=dat[!(grepl("passage", x = dat$note)),]

dat=dat[!(grepl("Vero", x = dat$Description)),]

dat=dat[!(grepl("Vero", x = dat$note)),]

# remove the SARS CoV in mus musculus of the united states and those with unkown host
# Check the chiropterasn wihth SARS in the US as well. 
# print(dat[grepl("SARS coronavirus", dat$Description),], max = 3000)

dat=dat[!(grepl("SARS coronavirus", x = dat$Description) & dat$country=="USA: Nashville, TN" & dat$host==""),]

dat=dat[dat$host!="Usa: louisiana",]

# rows with human in the description and unknown host are removed. 


dat=dat[!(grepl("Human|human", x = dat$Description) & dat$host==""),]

dat=dat[dat$host!="",]

#  Delete observations with unknown host

# nrow(dat) # 26527






####################################
####### Adding host taxonomy #######
####################################

# ---- run lines 426 to 442 to generate the host_species_order.RDS object
# Sys.setenv(ENTREZ_KEY='your_key')
#
# 
hosts=sort(unique(dat$host))
# 
# order=vector(mode = "list", length = length(hosts))
# 
# for(i in 1:length(hosts)){ #nrow(dat)up to  5487
#   
#   order[[i]]=
#     
#     taxonomy( organism = hosts[i],
#               db       = "ncbi",
#               output   = "classification")#}
#   
#   cat(paste0(i, " "))}
#
# saveRDS(order,"Data/host_species_order.RDS")

order.temp<-readRDS("Data/host_species_order.RDS")

taxonomy= lapply(order.temp, function(x) x[x$rank%in%c("class", "order", "family", "genus"),])

# hosts[which(sapply(order.temp, nrow)==1)]
#"Egretta picata"   
taxonomy[[which(sapply(order.temp, nrow)==1)[1]]]=data.frame(
  name=c("Aves", "Pelecaniformes", "Ardeidae", "Egretta"),
  rank=c("class", "order", "family", "genus"),
  id=NA)

#Hipposideros melanopogon"
taxonomy[[which(sapply(order.temp, nrow)==1)[2]]]=data.frame(
  name=c("Mammalia", "Chiroptera", "Hipposideridae", "Hipposideros"),
  rank=c("class", "order", "family", "genus"),
  id=NA)


#Ozimops
taxonomy[[which(sapply(order.temp, nrow)==1)[3]]]=data.frame(
  name=c("Mammalia", "Chiroptera", "Molossidae", "Ozimops"),
  rank=c("class", "order", "family", "genus"),
  id=NA)

#Pipistrellus inexspectatu
taxonomy[[which(sapply(order.temp, nrow)==1)[4]]]=data.frame(
  name=c("Mammalia", "Chiroptera", "Molossidae", "Pipistrellus"),
  rank=c("class", "order", "family", "genus"),
  id=NA)

#Rhinolophus smithers
taxonomy[[which(sapply(order.temp, nrow)==1)[5]]]=data.frame(
  name=c("Mammalia", "Chiroptera", "Rhinolophidae", "Rhinolophus"),
  rank=c("class", "order", "family", "genus"),
  id=NA)

#Vespadelus baverstocki
taxonomy[[which(sapply(order.temp, nrow)==1)[6]]]=data.frame(
  name=c("Mammalia", "Chiroptera", "Vespertilionidae", "Vespadelus"),
  rank=c("class", "order", "family", "genus"),
  id=NA)



for(i in 1:length(taxonomy)){
  
  taxonomy[[i]]<-pivot_wider(taxonomy[[i]]%>%select(-id), names_from = rank, values_from = name)}

taxonomy<-rbind.fill(taxonomy)

taxonomy$host=hosts

# join the taxonomy to the data

dat=left_join(dat, taxonomy%>%select(class, host), by="host" )
dat=left_join(dat, taxonomy%>%select(class, order, host), by=c("class", "host"))
dat=left_join(dat, taxonomy%>%select(class, order, family, host), by=c("class","order","host"))
dat=left_join(dat, taxonomy%>%select(class, order, family, genus, host), c("class","order", "family", "host"))

dat$species=dat$host

dat$species[is.na(dat$order)]=NA
dat$species[is.na(dat$family)]=NA
dat$species[is.na(dat$genus)]=NA


dat[dat$species%in%unique(dat[which(lengths(strsplit(dat$species, " "))==1),]$host), ]$species=NA






################################################
####### Adding CoV genus and subgenus ##########
################################################


# table(dat$CoV_genus, useNA = "always")

temp=strsplit(dat$Taxonomy, ";")

alpha.index=which(sapply(temp, function(x) any(grepl(pattern = "Alpha|alpha", x))))
beta.index=which(sapply(temp, function(x) any(grepl(pattern = "Beta|beta", x))))
gamma.index=which(sapply(temp, function(x) any(grepl(pattern = "Gamma|gamma", x))))
delta.index=which(sapply(temp, function(x) any(grepl(pattern = "Delta|delta", x))))

# identical(sort(c(alpha.index, beta.index, gamma.index, delta.index)),c(1:nrow(dat)))
# 
# length(c(alpha.index, beta.index, gamma.index, delta.index))==nrow(dat)

dat$CoV_genus<-NA

dat$CoV_genus[alpha.index]="Alphacoronavirus"
dat$CoV_genus[beta.index]="Betacoronavirus"
dat$CoV_genus[gamma.index]="Gammacoronavirus"
dat$CoV_genus[delta.index]="Deltacoronavirus"

#rmove Brazil results that are argued as wrong in other papers
dat=dat[!(dat$CoV_genus=="Betacoronavirus" & dat$class=="Aves"),]


# Adding subgenera for betaconronaviruses
dat$subgenus=NA

dat[grep("Sarbeco|sarbeco", dat$Taxonomy, ignore.case = T),]$subgenus="Sarbeco"
dat[grep("Merbeco|merbeco", dat$Taxonomy, ignore.case = T),]$subgenus="Merbeco"
dat[grep("Embeco|embeco", dat$Taxonomy, ignore.case = T),]$subgenus="Embeco"
dat[grep("Nobeco|nobeco", dat$Taxonomy, ignore.case = T),]$subgenus="Nobeco"
dat[grep("Hibeco|hibeco", dat$Taxonomy, ignore.case = T),]$subgenus="Hibeco"

# nrow(dat[dat$CoV_genus!="Betacoronavirus" & !is.na(dat$subgenus),])==0

# table(dat$CoV_genus, dat$subgenus,  useNA = "always")

dat<-dat%>%distinct()

dat<-dat[!is.na(dat$Name), ]

#nrow(dat) # 26437





########################################
####### Getting the PUBMED.id ##########
########################################

# ---- run lines 575 to 614 to generate the pubids_2022.RDS object

# names=unique(dat$Name)
# 
# PUBMED.id=c()
# 
# for(i in seq(names)){
# 
#   tryCatch({
#   if(grepl("PUBMED", entrez_fetch(db="nuccore", id =names[i], rettype = "gb"))){
# 
#     PUBMED.id[i]=strsplit(strsplit(entrez_fetch(db="nuccore", id =names[i], rettype = "gb"), "PUBMED")[[1]][2], "\n  ")[[1]][1]}else{
# 
#         PUBMED.id[i]="No PUBMED id"}
# 
#     #data is already available in the folder
#   #saveRDS(PUBMED.id, "Data/pubids_2022.RDS")
# 
#   cat(paste0(i, " "))}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# 
# }
# 
# 
# 
# 
# 
# PUBMED.id=gsub(pattern = "   ", "", x = PUBMED.id)
# 
# PUBMED.id=sapply(strsplit(PUBMED.id, "\n"), function(x)x[[1]])
# 
# PUBMED.id=data.frame(Name=names, pubid=PUBMED.id)
# 
# PUBMED.id<-left_join(PUBMED.id, dat%>%select(Name, Accession)%>%distinct, by="Name")
# 
# PUBMED.id[is.na(PUBMED.id$pubid),]$pubid=c(17533554, 17533554, 27178127, "No PUBMED id",
#                                            30209269, 21500655, "No PUBMED id", "No PUBMED id",
#                                            20556502, 20556502, "No PUBMED id", 7710354,
#                                            7710354, 7710354, 24613433, NA, 32475435)
# 
# PUBMED.id=PUBMED.id[-22356,]
#
#saveRDS(PUBMED.id, Data/pubids_2022.RDS")

PUBMED.id<-readRDS("Data/pubids_2022.RDS")

#join the pubmed id data 

dat=left_join(dat, PUBMED.id%>%dplyr::select(Name, pubid)%>%distinct(), by = c("Name"))



#####################################################
####### Adding the publication information ##########
#####################################################

# ---- run lines 630 to 682 to generate the pubinfo_2022.RDS object

# unique.pubmed.id=unique(dat$pubid)
# unique.pubmed.id=unique.pubmed.id[unique.pubmed.id!="No PUBMED id"]
# 
# 
# publication.info.all=vector(mode = "list", length(unique.pubmed.id))
# 
# for(i in seq(unique.pubmed.id)){
# 
#   tryCatch({
#   if(unique.pubmed.id[i]!="No PUBMED id" & !(is.na(unique.pubmed.id[i]))){
# 
#     multi_summs <- entrez_summary(db="pubmed", id=unique.pubmed.id[i])
# 
# 
#     date_and_cite <- extract_from_esummary(multi_summs, c("uid",
#                                                           "pubdate",
#                                                           "title",
#                                                           "authors",
#                                                           "volume",
#                                                           "fulljournalname",
#                                                           "pages"))
# 
# 
#     publication.info=data.frame(t(date_and_cite))
# 
#     publication.info.all[[i]]=publication.info}
# 
#   saveRDS(publication.info.all, "Data/pubinfo_2022.RDS")
# 
# 
#   cat(paste0(i, " "))
# }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
# }
# 
# 
# # for 25779817 it is actually 25889235
# multi_summs <- entrez_summary(db="pubmed", id=25889235)
# 
# 
# date_and_cite <- extract_from_esummary(multi_summs, c("uid",
#                                                       "pubdate",
#                                                       "title",
#                                                       "authors",
#                                                       "volume",
#                                                       "fulljournalname",
#                                                       "pages"))
# 
# 
# publication.info=data.frame(t(date_and_cite))
# 
# publication.info.all[[which(unique.pubmed.id=="25779817")]]=publication.info
#
# saveRDS(publication.info.all, "Data/pubinfo_2022.RDS")




publication.info.all<-readRDS("Data/pubinfo_2022.RDS")

publication.info.all=rbind.fill(publication.info.all)%>%select(uid, authors,pubdate,title,fulljournalname,volume,pages)



publication.info.all$uid<-unlist(publication.info.all$uid)
publication.info.all$pubdate<-c(unlist(publication.info.all$pubdate))

publication.info.all$title[sapply(publication.info.all$title, is.null)] <- NA
publication.info.all$title<-unlist(publication.info.all$title, recursive = F)

publication.info.all$fulljournalname[sapply(publication.info.all$fulljournalname, is.null)] <- NA
publication.info.all$fulljournalname<-unlist(publication.info.all$fulljournalname)

publication.info.all$volume[sapply(publication.info.all$volume, is.null)] <- NA
publication.info.all$volume<-unlist(publication.info.all$volume)

publication.info.all$pages[sapply(publication.info.all$pages, is.null)] <- NA
publication.info.all$pages<-unlist(publication.info.all$pages)

names(publication.info.all)<-c("pubid", "authors","pubdate", "title", "fulljournalname", "volume", "pages")

#head(publication.info.all)

publication.info.all$pubdate<-sapply(strsplit(publication.info.all$pubdate, split = " "), "[[", 1)

#join the publication information data

dat=left_join(dat, publication.info.all, by = c("pubid"))

# nrow(dat) # 26437





# remove other data

dat=dat[!(grepl("passage|culture|experiment", x = dat$Description, ignore.case = T)),]

dat=dat[!(grepl("passage|vaccine|culture|experiment", x = dat$title, ignore.case = T)),]

# nrow(dat) #26154


# saveRDS(dat, "Data/COV_data_Feb_12_2022.RDS")



