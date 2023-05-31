library(tidyverse)
library(myTAI)
library(plyr)


gb_dat_2<-readRDS( "Data/Original_data_dataframe/all_data_nucleotide_ready_to_be_cleaned.RDS")


gb_dat_2=gb_dat_2%>%
  filter(grepl("coronaviridae|alphacoronavirus|betacoronavirus|deltacoronavirus|gammacoronavirus|coronavirus", 
               taxonomy, ignore.case = T))

#removing non-orthocoronavirinae subfamilies of the coronaviridae family
gb_dat_2=gb_dat_2[!grepl("Letovirinae|Pitovirinae", gb_dat_2$taxonomy, ignore.case = T),]

#removing all homo sapiens host
gb_dat_2<-gb_dat_2[!grepl("huamn|Homosapeins|homo|sapiens|human", gb_dat_2$host, ignore.case = T ),]

# removing all Vero E6 host
gb_dat_2<-gb_dat_2[!grepl("vero|E6", gb_dat_2$host, ignore.case = T ),]

gb_dat_2=gb_dat_2%>%filter(host!="liquid chorioallantoic of embryonated egg with 18 days of incubation")

#removing extra spaces
gb_dat_2$host=gsub("\\s*\\(.*?\\)", "", gb_dat_2$host)

#gb_dat_2$host=gsub(".*calf|cow|Bos taurus|Bos Taurus.*", "Bos taurus", gb_dat_2$host)
gb_dat_2$host=gsub(".*\\b(calf|cow|Bos taurus|Bos Taurus|beef|dairy)\\b.*", "Bos taurus", gb_dat_2$host)

# anything with chicken, broiler, hen, layer, poultry is Gallus gallus
#gb_dat_2$host=gsub(".*chicken|broiler|layer|poultry|Layer|chick|chikcen|Chicken|Broiler|Gallus domesticus|Gallus gallus.*", "Gallus gallus", gb_dat_2$host)
gb_dat_2$host=gsub(".*\\b(chickens|chicken|broiler|layer|poultry|Layer|chick|chikcen|Chicken|Broiler|broilers|layers|Layers|Gallus|Gallus domesticus|Gallus gallus|breeder|broiler)\\b.*", "Gallus gallus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Equus caballus|horse|Horse)\\b.*", "Equus caballus", gb_dat_2$host)

# anything with pig, piglet, swine, porcine is Sus scrofa
gb_dat_2$host=gsub(".*\\b(Pig|pig|piglet|Swine|swine|Porcine|porcine|boar|scrofa|Sus_scrofa)\\b.*", "Sus scrofa", gb_dat_2$host)

# gb_dat_2$host=gsub("dog|Dog", "Canis lupus familiaris", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(domestic cat|catus)\\b.*", "Felis catus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(donkey|Donkey)\\b.*", "Equus asinus", gb_dat_2$host)

# anything with Mus musculus is Mus musculus
gb_dat_2$host=gsub(".*\\b(Mus musculus|Mus_musculus|mouse)\\b.*", "Mus musculus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(not provided|unknown|Unknown)\\b.*", "Unknown", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Triaenops afer)\\b.*", "Triaenops afer", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Triaenops menamena)\\b.*", "Triaenops menamena", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Rousettus madagascariensis)\\b.*", "Rousettus madagascariensis", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Rhinolophus rhodesiae)\\b.*", "Rhinolophus rhodesiae", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Rhinolophus lobatus)\\b.*", "Rhinolophus lobatus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Rhinolophus ferrumequinum)\\b.*", "Rhinolophus ferrumequinum", gb_dat_2$host, ignore.case = T)

gb_dat_2$host=gsub(".*\\b(Nycteris thebaica)\\b.*", "Nycteris thebaica", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Pipistrellus cf. hesperidus)\\b.*", "Pipistrellus hesperidus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Mormopterus jugularis)\\b.*", "Mormopterus jugularis", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Mops midas)\\b.*", "Mops midas", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Mops condylurus)\\b.*", "Mops condylurus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Miniopterus mossambicus)\\b.*", "Miniopterus mossambicus", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Hipposideros caffer)\\b.*", "Hipposideros caffer", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Canis lupus familiaris)\\b.*", "Canis lupus familiaris", gb_dat_2$host)

gb_dat_2$host=gsub(".*\\b(Artibeus planirostris)\\b.*", "Artibeus planirostris", gb_dat_2$host)

# gb_dat_2$host=gsub(".*\\b(bat\\b.*", "Chiroptera", gb_dat_2$host, ignore.case = T)
gb_dat_2$host=gsub(" sp\\.|spp\\.", "", gb_dat_2$host, ignore.case = T)

#anything with birds bird avian is Aves
gb_dat_2$host=gsub(".*\\b(Avain|avian|bird|Birds)\\b.*", "Aves", gb_dat_2$host)

#anything with camels is the same species
gb_dat_2$host=gsub(".*\\b(camel|Camel|Dromedary)\\b.*", "Camelus dromedarius", gb_dat_2$host)

gb_dat_2[gb_dat_2$host=="cattle egret",]$host="Bubulcus ibis"
gb_dat_2[gb_dat_2$host=="Watusi cattle",]$host="Bos taurus"
gb_dat_2[gb_dat_2$host=="cattle",]$host="Bos taurus"
gb_dat_2[gb_dat_2$host=="Asiatic Lion",]$host="Panthera leo persica"
gb_dat_2[gb_dat_2$host=="rat",]$host="Rattus ratus"
gb_dat_2[gb_dat_2$host=="A.stoliczkanus",]$host="Aselliscus stoliczkanus" 
gb_dat_2[gb_dat_2$host=="Aselliicus stoliczkanus",]$host="Aselliscus stoliczkanus" 
gb_dat_2[gb_dat_2$host=="bottlenose dolphin",]$host="Tursiops aduncus"
gb_dat_2[gb_dat_2$host=="Hipposideroa pratti",]$host="Hipposideros pratti"
gb_dat_2[gb_dat_2$host=="Pipistrellys abramus",]$host="Pipistrellus abramus"
gb_dat_2[gb_dat_2$host=="Rhinolphus affinis",]$host="Rhinolophus affinis"
gb_dat_2[gb_dat_2$host=="Rhinilophus ferrumequinum",]$host="Rhinolophus ferrumequinum"
gb_dat_2[gb_dat_2$host%in%c("Quail", "quail"), ]$host="Coturnix"
gb_dat_2[gb_dat_2$host=="alpaca",]$host="Vicugna pacos"
gb_dat_2$host[grepl("Ferret", gb_dat_2$host ,ignore.case = T)]="Mustela putorius"
gb_dat_2$host[grepl("antelope", gb_dat_2$host, ignore.case = T)]="Bovidae"
gb_dat_2$host[grepl("Bean goose", gb_dat_2$host)]="Anseriformes"
gb_dat_2$host[grepl("Black-headed gull", gb_dat_2$host)]="Chroicocephalus ridibundus"
gb_dat_2$host[grepl("Brentgoose", gb_dat_2$host)]="Branta bernicla"
gb_dat_2$host[grepl("Buffalo", gb_dat_2$host)]="Bovidae sp"
gb_dat_2$host[grepl("Familiaris", gb_dat_2$host)]="Canis lupus familiaris"
gb_dat_2$host[grepl("Canine", gb_dat_2$host)]="Canidae sp"
gb_dat_2$host[grepl("Canis lupus famaliaris", gb_dat_2$host)]="Canis lupus familiaris"
gb_dat_2$host[grepl("coot", gb_dat_2$host)]="Fulica sp"
gb_dat_2$host[grepl("common pheasant", gb_dat_2$host)]="Phasianus colchicus"
gb_dat_2$host[grepl("common gull", gb_dat_2$host)]="Larus canus"
gb_dat_2$host[grepl("common snipe", gb_dat_2$host)]="Gallinago gallinago"
gb_dat_2$host[grepl("common tern", gb_dat_2$host)]="Sterna hirundo"
gb_dat_2$host[grepl("common teal", gb_dat_2$host)]="Anas crecca"
gb_dat_2$host[grepl("common starling", gb_dat_2$host)]="Sturnus vulgaris"
gb_dat_2$host[grepl("Chiroptera", gb_dat_2$host)]="Chiroptera"
gb_dat_2$host[grepl("Cormoran", gb_dat_2$host)]="Phalacrocoracidae"
gb_dat_2[gb_dat_2$host=="wisent",]$host="Bison bonasus"
gb_dat_2[gb_dat_2$host=="Himalayan tahr",]$host="Hemitragus jemlahicus"
gb_dat_2[gb_dat_2$host=="sitatunga",]$host="Tragelaphus spekii"
gb_dat_2[gb_dat_2$host=="nyala",]$host="Tragelaphus angasii"
gb_dat_2$host[grepl("columbia livia", gb_dat_2$host, ignore.case = T)]="Columba livia"
gb_dat_2[gb_dat_2$host=="yak",]$host="Bos grunniens"
gb_dat_2[gb_dat_2$host=="wigeon",]$host="Mareca"
gb_dat_2[gb_dat_2$host=="goat",]$host="Capra aegagrus"
gb_dat_2[gb_dat_2$host=="teal",]$host="Anas sp"
gb_dat_2[gb_dat_2$host=="western sandpiper",]$host="Calidris mauri"
gb_dat_2[gb_dat_2$host=="waterbuck",]$host="Kobus ellipsiprymnus"
gb_dat_2[gb_dat_2$host=="turkey",]$host="Meleagris gallopavo"
gb_dat_2[gb_dat_2$host=="swan",]$host="Cygnus cygnus"
gb_dat_2[gb_dat_2$host=="snow goose",]$host="Anser caerulescens"
gb_dat_2[gb_dat_2$host=="sambar deer",]$host="Rusa unicolor"
gb_dat_2[gb_dat_2$host=="white-eye",]$host="Zosteropidae"
gb_dat_2[gb_dat_2$host=="white-tailed deer",]$host="Odocoileus virginianus"
gb_dat_2[gb_dat_2$host=="white-rumped munia",]$host="Lonchura striata"
gb_dat_2[gb_dat_2$host=="tapir",]$host="Tapiridae"
gb_dat_2[gb_dat_2$host== "magpie-robin", ]$host="Copsychus saularis"
gb_dat_2[gb_dat_2$host== "night-heron", ]$host="Nycticorax nycticorax"
gb_dat_2[gb_dat_2$host== "Red-necked Avocet", ]$host="Recurvirostra novaehollandiae"
gb_dat_2[gb_dat_2$host== "red-whiskered bulbul", ]$host="Pycnonotus jocosus"
gb_dat_2[gb_dat_2$host== "sow", ]$host="Sus scrofa"
gb_dat_2[gb_dat_2$host== "sparrow", ]$host="Passer"
gb_dat_2[gb_dat_2$host== "tufted duck", ]$host="Aythya fuligula"
gb_dat_2[gb_dat_2$host== "spotted hyena", ]$host="Crocuta crocuta"
gb_dat_2[gb_dat_2$host== "Sunda pangolin", ]$host="Manis javanica"
gb_dat_2[gb_dat_2$host== "pintail", ]$host="Anas acuta"
gb_dat_2[gb_dat_2$host== "rock sandpiper", ]$host="Calidris ptilocnemis"
gb_dat_2[gb_dat_2$host=="Scotphilus kuhli large intestine",]$host="Scotophilus kuhli"
gb_dat_2[gb_dat_2$host=="Pheasant",]$host="Phasianus colchicus"
gb_dat_2[gb_dat_2$host=="peafowl",]$host="Phasianidae"
gb_dat_2[gb_dat_2$host=="pangolin",]$host="Pholidota"
gb_dat_2[gb_dat_2$host=="Phoca vitulina richardsii",]$host="Phoca vitulina"
gb_dat_2[gb_dat_2$host=="palm civet", ]$host="Paradoxurus hermaphroditus"
gb_dat_2[gb_dat_2$host=="palm civet cats", ]$host="Paradoxurus hermaphroditus"
gb_dat_2[gb_dat_2$host=="civet", ]$host="Paradoxurus hermaphroditus"
gb_dat_2[gb_dat_2$host=="Pecari tajacu", ]$host="Dicotyles tajacu"
gb_dat_2[gb_dat_2$host=="rodent", ]$host="Rodentia"
gb_dat_2[gb_dat_2$host=="Rodent", ]$host="Rodentia"
gb_dat_2[gb_dat_2$host=="mute swan", ]$host="Cygnus olor"
gb_dat_2[gb_dat_2$host== "mink", ]$host="Mustelidae"
gb_dat_2[gb_dat_2$host== "Mink", ]$host="Mustelidae"
gb_dat_2[gb_dat_2$host== "Miniopterus", ]$host="Miniopterus"
gb_dat_2[gb_dat_2$host=="mallard duck", ]$host="Anas platyrhynchos"
gb_dat_2[gb_dat_2$host=="leopard cat", ]$host="Prionailurus bengalensis"
gb_dat_2[gb_dat_2$host=="house sparrow", ]$host="Passer domesticus"
gb_dat_2[gb_dat_2$host=="hering gull", ]$host="Larus argentatus"
gb_dat_2[gb_dat_2$host=="gull", ]$host="Larus"
gb_dat_2[gb_dat_2$host=="guinea fowl", ]$host="Numididae"
gb_dat_2[gb_dat_2$host=="goose", ]$host="Anatidae"
gb_dat_2[gb_dat_2$host=="feline", ]$host="Felidae"
gb_dat_2[gb_dat_2$host=="fox", ]$host="Canidae"
gb_dat_2[gb_dat_2$host=="giant panda", ]$host="Ailuropoda melanoleuca"
gb_dat_2[gb_dat_2$host== "glaucous-winged gull", ]$host="Larus glaucescens"
gb_dat_2[gb_dat_2$host== "glaucus-gull", ]$host="Larus hyperboreus"
gb_dat_2[gb_dat_2$host=="giraffe", ]$host="Giraffa camelopardalis"
gb_dat_2[gb_dat_2$host=="grey-backed thrush", ]$host="Turdus hortulorum"
gb_dat_2[gb_dat_2$host=="greylag goose", ]$host="Anser anser"
gb_dat_2[gb_dat_2$host=="H.larvatus", ]$host="Hipposideros larvatus"
gb_dat_2[gb_dat_2$host=="H.pomona", ]$host="Hipposideros pomona"
gb_dat_2[gb_dat_2$host=="Ozimops DP-2019", ]$host="Ozimops"
gb_dat_2[gb_dat_2$host=="dromedary", ]$host="Camelus dromedarius"
gb_dat_2[gb_dat_2$host=="duck", ]$host="Anatidae"
gb_dat_2[gb_dat_2$host=="Dicotyles tajacu", ]$host="Pecari tajacu"
gb_dat_2[gb_dat_2$host=="Cynopterus brachyotis large intestine", ]$host="Cynopterus brachyoti"
gb_dat_2[gb_dat_2$host=="common magpie" , ]$host="Pica pica"
gb_dat_2[gb_dat_2$host=="Chiropteron", ]$host="Chiroptera"
gb_dat_2[gb_dat_2$host=="Chinese pangolin", ]$host="Manis pentadactyla"
gb_dat_2[gb_dat_2$host=="Chinese bulbul", ]$host="Pycnonotus sinensis"
gb_dat_2[gb_dat_2$host=="Cavia_porcellus", ]$host="Cavia porcellus"
gb_dat_2[gb_dat_2$host=="Catus", ]$host="Felis catus"
gb_dat_2[gb_dat_2$host=="Canis familiaris" , ]$host="Canis lupus familiaris"
gb_dat_2[gb_dat_2$host=="Canidae sp"  , ]$host="Canidae"
gb_dat_2[gb_dat_2$host=="Apodemus ", ]$host="Apodemus"
gb_dat_2[gb_dat_2$host=="Anas sp", ]$host="Anas"
gb_dat_2[gb_dat_2$host=="brent-goose", ]$host="Branta bernicla"         
gb_dat_2[gb_dat_2$host=="Chaerephon plicata", ]$host="Chaerephon plicatus"  
gb_dat_2[gb_dat_2$host=="Cynopterus brachyoti", ]$host="Cynopterus brachyotis"  
gb_dat_2[gb_dat_2$host=="Eptescus serotinus", ]$host="Eptesicus serotinus"  
gb_dat_2[gb_dat_2$host=="Nannopterum brasilianus", ]$host="Phalacrocorax brasilianus"
gb_dat_2[gb_dat_2$host=="Neoromicia capensis", ]$host="Laephotis capensis"
gb_dat_2[gb_dat_2$host=="Rattus ratus",]$host="Rattus rattus"
gb_dat_2[gb_dat_2$host=="Rattus",]$host="Rattus"
gb_dat_2[gb_dat_2$host=="Rattus R3",]$host="Rattus"
gb_dat_2[gb_dat_2$host=="bovine",]$host="Bovidae"
gb_dat_2[gb_dat_2$host=="cat",]$host="Felis catus"
gb_dat_2[gb_dat_2$host=="Chichen",]$host="Gallus gallus"
gb_dat_2[gb_dat_2$host=="common moorhen",]$host="Gallinula chloropus"
gb_dat_2[gb_dat_2$host=="Laying hens",]$host="Gallus gallus"
gb_dat_2[gb_dat_2$host=="pigeon",]$host="Columbidae"
gb_dat_2[gb_dat_2$host=="raccoon dog",]$host="Nyctereutes procyonoides"
gb_dat_2[gb_dat_2$host=="Scotophilus kuhli",]$host="Scotophilus kuhlii"
gb_dat_2[gb_dat_2$host=="rabbit",]$host="Oryctolagus cuniculus"
gb_dat_2[gb_dat_2$host=="Hipposidero melanopogon",]$host="Hipposideros melanopogon"
gb_dat_2[gb_dat_2$host=="Hipposideros cf. ruber",]$host="Hipposideros ruber"
gb_dat_2[gb_dat_2$host=="Eptesicus serotinus horikawai",]$host="Eptesicus serotinus"
gb_dat_2[gb_dat_2$host=="Myotis daubentoni petax",]$host="Myotis petax" 
gb_dat_2[gb_dat_2$host=="buffalo",]$host="Bovidae" 
gb_dat_2[gb_dat_2$host=="shorebird",]$host="Aves" 
gb_dat_2[gb_dat_2$host=="Shelter Dog",]$host="Canis lupus familiaris" 
gb_dat_2[gb_dat_2$host=="Scotophilus cf. heathii",]$host="Scotophilus heathii" 
gb_dat_2[gb_dat_2$host=="Rhinolophus; female; FMNH 228951",]$host="Rhinolophus" 
gb_dat_2[gb_dat_2$host=="Prionailurus bengalensis euptilurus",]$host="Prionailurus bengalensis" 
gb_dat_2[gb_dat_2$host=="Tadarida brasiliensis bat",]$host="Tadarida brasiliensis" 
gb_dat_2[gb_dat_2$host=="Oryctolagus cuniculus; breed: New Zealand rabbit",]$host="Oryctolagus cuniculus" 
gb_dat_2[gb_dat_2$host=="Neoromicia cf. capensis",]$host="Neoromicia capensis" 
gb_dat_2[gb_dat_2$host=="Mustela putorius furo",]$host="Mustela putorius furo" 
gb_dat_2[gb_dat_2$host=="Mormopterus BBvV-2008; female",]$host="Mormopterus" 
gb_dat_2[gb_dat_2$host=="Mops cf. condylurus",]$host="Mops condylurus" 
gb_dat_2[gb_dat_2$host=="Miniopterus fuliginosus, feces",]$host="Miniopterus fuliginosus" 
gb_dat_2[gb_dat_2$host=="Miniopterus fuliginosus, intestine",]$host="Miniopterus fuliginosus" 
gb_dat_2[gb_dat_2$host=="Miniopterus cf. natalensis",]$host="Miniopterus natalensis" 
gb_dat_2[gb_dat_2$host=="Miniopterus ",]$host="Miniopterus" 
gb_dat_2[gb_dat_2$host=="Miniopterus schreibersi",]$host="Miniopterus schreibersii" 
gb_dat_2[gb_dat_2$host=="microbat",]$host="Chiroptera" 
gb_dat_2[gb_dat_2$host=="cormorant",]$host="Phalacrocoracidae"
gb_dat_2[gb_dat_2$host=="canine",]$host="Canidae"
gb_dat_2[gb_dat_2$host=="Gorilla gorilla gorilla",]$host="Gorilla gorilla"
gb_dat_2[gb_dat_2$host=="Feline",]$host="Felidae"
gb_dat_2[gb_dat_2$host=="Hipposideros cf ruber",]$host="Hipposideros ruber"
gb_dat_2[gb_dat_2$host=="horseshoe bat",]$host="Rhinolophidae"
gb_dat_2[gb_dat_2$host=="Scotomannes kuhlii",]$host="Scotophilus kuhlii"
gb_dat_2[gb_dat_2$host=="Chaerephon; female",]$host="Chaerephon"
gb_dat_2[gb_dat_2$host=="Dremomys rufigenis fuscus",]$host="Dremomys rufigenis"
gb_dat_2[gb_dat_2$host=="Chaerephon pusillus; female",]$host="Chaerephon pusillus"
gb_dat_2[gb_dat_2$host=="black-headed gull",]$host="Chroicocephalus ridibundus"
gb_dat_2[gb_dat_2$host=="bean goose",]$host="Anser fabalis"
gb_dat_2[gb_dat_2$host=="Mormopterus BBvV-2008; male",]$host="Mormopterus"
gb_dat_2[gb_dat_2$host=="Mustela putorius furo",]$host="Mustela putorius"
gb_dat_2[gb_dat_2$host=="birds",]$host="Aves"
gb_dat_2[gb_dat_2$host=="camelus dromedarius",]$host="Camelus dromedarius"
gb_dat_2[gb_dat_2$host=="Chaerephon pumila",]$host="Chaerephon pumilus"
gb_dat_2[gb_dat_2$host=="Myotis daubentoni",]$host="Myotis daubentonii"
gb_dat_2[gb_dat_2$host=="Rhinolophus pearsoni",]$host="Rhinolophus pearsonii"
gb_dat_2[gb_dat_2$host=="Rattus r3 YH-2020",]$host="Rattus"
gb_dat_2[gb_dat_2$host=="Rousettus leschenaulti",]$host="Rousettus leschenaultii"
gb_dat_2[gb_dat_2$host=="Hamster",]$host="Cricetidae"
gb_dat_2$host[gb_dat_2$host=="Neoromicia capensis"]<-"Laephotis capensis"
gb_dat_2$host[gb_dat_2$host=="Montifringilla taczanowskii"]<-"Montifringilla"
gb_dat_2$host[gb_dat_2$host=="Fulica sp"]<-"Fulica"
gb_dat_2$host[gb_dat_2$host=="Anas clypeata"]<-"Spatula clypeata"
gb_dat_2$host[gb_dat_2$host=="Rupornis magnirostris"]<-"Rupornis"


gb_dat_2$host=gsub(".*\\b(dog|Dog|kennelled)\\b.*", "Canis lupus familiaris", gb_dat_2$host)
gb_dat_2$host=gsub(".*\\b(bat|Bat)\\b.*", "Chiroptera", gb_dat_2$host)

gb_dat_2<-gb_dat_2 %>% filter(!host%in%c("Wastewater sample","USA: Louisiana"))

gb_dat_2<-gb_dat_2[!grepl("water|liquid|Environment sample|Wastewater", gb_dat_2$host, ignore.case = T),]

# rows with unknown host are removed
gb_dat_2=gb_dat_2[gb_dat_2$host!="Unknown",]


# sort(unique(gb_dat_2$host))


title <- gb_dat_2$title
words <- unlist(strsplit(title, "\\s+"))  # Split titles into words
unique_words <- sort(unique(tolower(words)))

# unique(unlist(lapply(
# c("enzyme-linked",
# "eggs",
# "elisa",
# "alternative",
# "alimentary",
# "adaptive",
# "replicase",
# "real-time",
# "protease",
# "laboratory",
# "invertebrate",
# "inoculation",
# "influenza",
# "infection",
# "hospital",
# "hens",
# "water",
# "vivo",
# "virus-neutralizing",
# "vero",
# "vaccinated",
# "torovirus",
# "rotavirus"), function(x) grep(x, title, value = T, ignore.case = T))))
# 

# rows with speific words in title are removed
gb_dat_2=gb_dat_2[!grepl("culture-derived | experimental | elisa | porcine rotavirus | vitro | vero | vaccin| torovirus | knockout | antigen| bacteriophage |Tomatidine |Replicase | inoculation |Propagation |Attenuation |Serological |semi-nested | gammaherpesvirus | invertebrate | bocavirus | bastroviruses",
                         gb_dat_2$title, ignore.case=T),]


description <- gb_dat_2$description
words <- unlist(strsplit(description, "\\s+"))  # Split titles into words
unique_words <- sort(unique(words))
unique_words<-unique_words[!grepl("\\d", unique_words)] # remove words that are numbers because they are not informative


gb_dat_2=gb_dat_2[!grepl("attenuated|Passage|vaccine", 
                         x = gb_dat_2$description, ignore.case=T),]


note <- gb_dat_2$note
words <- unlist(strsplit(note, "\\s+"))  # Split titles into words
unique_words <- sort(unique(words))
unique_words<-unique_words[!grepl("\\d", unique_words)] # remove words that are numbers because they are not informative



gb_dat_2=gb_dat_2[!grepl("vaccine|Vero|culture|plaque|passage|frameshift|cell|attenuated|egg|antigen|antibod|passage|propagated", 
                         x = gb_dat_2$note, ignore.case=T),]

#nrow(gb_dat_2) #26945

# sort(unique(gb_dat_2$host))



######################################
######### GET THE TAXONOMY ###########
######################################

#provide your entrez key 

# your_entrez_key<-"your_key"
# 
Sys.setenv(ENTREZ_KEY=your_entrez_key)

hosts=sort(unique(gb_dat_2$host))

order=vector(mode = "list", length = length(hosts))

for(i in 1:length(hosts)){

  order[[i]]=

    taxonomy( organism = hosts[i],
              db       = "ncbi",
              output   = "classification")#}

  cat(paste0(i, " "))}

# saveRDS(order, "Data/host_species_order.RDS")
# order<-readRDS( "Data/host_species_order.RDS")


taxonomy= lapply(order, function(x) x[x$rank%in%c("class", "order", "family", "genus"),])



#hosts[which(sapply(taxonomy, class)=="logical")]
# 
# [1] "Amazona virdigenalis"       "Egretta picata"             "Hipposideros melanopogon"  
# [4] "Ozimops"                    "Pipistrellus inexspectatus" "Rhinolophus smithersi"     
# [7] "Rupornis"                   "Vespadelus baverstocki"    


taxonomy[[which(hosts=="Amazona virdigenalis")]]=
  data.frame(
    name=c("Aves", "Psittaciformes", "Psittacidae", "Amazona"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Egretta picata")]]= 
  data.frame(
    name=c("Aves", "Pelecaniformes", "Ardeidae", "Egretta"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Hipposideros melanopogon")]]= 
  data.frame(
    name=c("Mammalia", "Chiroptera", "Hipossideridae", "Hipposideros"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Ozimops")]]= 
  data.frame(
    name=c("Mammalia", "Chiroptera", "Molossidae", "Ozimops"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Pipistrellus inexspectatus")]]= 
  data.frame(
    name=c("Mammalia", "Chiroptera", "Molossidae", "Pipistrellus"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Rhinolophus smithersi")]]= 
  data.frame(
    name=c("Mammalia", "Chiroptera", "Rhinolophidae", "Rhinolophus"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Rupornis")]]= 
  data.frame(
    name=c("Aves", "Accipitriformes", "Accipitridae", "Rupornis"),
    rank=c("class", "order", "family", "genus"),
    id=NA)

taxonomy[[which(hosts=="Vespadelus baverstocki")]]= 
  data.frame(
    name=c("Mammalia", "Chiroptera", "Vespertilionidae", "Vespadelus"),
    rank=c("class", "order", "family", "genus"),
    id=NA)






for(i in 1:length(taxonomy)){
  
  taxonomy[[i]]<-pivot_wider(taxonomy[[i]]%>%select(-id), names_from = rank, values_from = name)}

taxonomy<-rbind.fill(taxonomy)

taxonomy$host=hosts


gb_dat_2=left_join(gb_dat_2, taxonomy%>%select(class, host), by="host")
gb_dat_2=left_join(gb_dat_2, taxonomy%>%select(class, order, host), by=c("class", "host"))
gb_dat_2=left_join(gb_dat_2, taxonomy%>%select(class, order, family, host), by=c("class","order","host"))
gb_dat_2=left_join(gb_dat_2, taxonomy%>%select(class, order, family, genus, host), c("class","order", "family", "host"))

gb_dat_2$species=gb_dat_2$host


gb_dat_2$species[is.na(gb_dat_2$order)]=NA
gb_dat_2$species[is.na(gb_dat_2$family)]=NA
gb_dat_2$species[is.na(gb_dat_2$genus)]=NA

#worods per species. Those with one word have the genus only or other
words <- strsplit(gb_dat_2$species, "\\s+")

# Count the number of words
num_words <- sapply(words, length)
#unique(num_words) # no zeros

#index single word
temp.index=num_words<2

gb_dat_2[temp.index,]$species<-NA
#unique(gb_dat_2[temp.index,]$species)


# unique(gb_dat_2$class)
#"Mammalia"     "Aves"         "Bivalvia"     "Lepidosauria" "Insecta"      "Clitellata"  

#check Bivalvia

# following the poper's title, it is not Cristaria plicata but Chaerephon plicatus
gb_dat_2[gb_dat_2$class%in%c("Bivalvia"),]$species<-"Chaerephon plicatus"
gb_dat_2[gb_dat_2$class%in%c("Bivalvia"),]$genus<-"Chaerephon"
gb_dat_2[gb_dat_2$class%in%c("Bivalvia"),]$family<-"Molossidae"
gb_dat_2[gb_dat_2$class%in%c("Bivalvia"),]$order<-"Chiroptera"
gb_dat_2[gb_dat_2$class%in%c("Bivalvia"),]$class<-"Mammalia"


#Non-invasive surveys of mammalian viruses using environmental DNA. So remove the leeches
gb_dat_2<-gb_dat_2[!gb_dat_2$class%in%c("Clitellata"),]


#"Insecta" classes and Lepidosauria remove for now

gb_dat_2<-gb_dat_2[!gb_dat_2$class%in%c("Lepidosauria"),]
gb_dat_2<-gb_dat_2[!gb_dat_2$class%in%c("Insecta"),]


################################################
####### Adding CoV genus and subgenus ##########
################################################

alpha.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("alphacoronavirus", gb_dat_2[,x], ignore.case = T))))
beta.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("betacoronavirus", gb_dat_2[,x], ignore.case = T))))
gamma.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("gammacoronavirus", gb_dat_2[,x], ignore.case = T))))
delta.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("deltacoronavirus", gb_dat_2[,x], ignore.case = T))))

alpha.index<-sort(c(alpha.index, grep("Swine acute diarrhea syndrome related coronavirus", gb_dat_2$organism, ignore.case = T)))
beta.index<-sort(c(beta.index, grep("SARS|severe acute respiratory", gb_dat_2$organism, ignore.case = T)))

# unique(gb_dat_2[-sort(c(alpha.index, beta.index, gamma.index, delta.index)), "organism"])

gb_dat_2$CoV_genus<-NA

gb_dat_2$CoV_genus[alpha.index]="Alphacoronavirus"
gb_dat_2$CoV_genus[beta.index]="Betacoronavirus"
gb_dat_2$CoV_genus[gamma.index]="Gammacoronavirus"
gb_dat_2$CoV_genus[delta.index]="Deltacoronavirus"


#unique(gb_dat_2$class)

#table(gb_dat_2$CoV_genus, gb_dat_2$class) #Aves with Beta

#removing Brazilian results with betacoronavirus in birds. These results have been contested.
gb_dat_2<-gb_dat_2 %>% filter(!(class=="Aves" & CoV_genus=="Betacoronavirus"))


gb_dat_2$subgenus=NA

sarbeco.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("sarbeco", gb_dat_2[,x], ignore.case = T))))
merbeco.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("merbeco", gb_dat_2[,x], ignore.case = T))))
embeco.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("embeco", gb_dat_2[,x], ignore.case = T))))
nobeco.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("nobeco", gb_dat_2[,x], ignore.case = T))))
hibeco.index<-unique(unlist(lapply(c("taxonomy", "organism"), function(x) grep("hibeco", gb_dat_2[,x], ignore.case = T))))

gb_dat_2[sarbeco.index,]$subgenus="Sarbeco"
gb_dat_2[merbeco.index,]$subgenus="Merbeco"
gb_dat_2[embeco.index,]$subgenus="Embeco"
gb_dat_2[nobeco.index,]$subgenus="Nobeco"
gb_dat_2[hibeco.index,]$subgenus="Hibeco"

#table(gb_dat_2$CoV_genus, gb_dat_2$subgenus,  useNA = "always")
#table(gb_dat_2$CoV_genus, gb_dat_2$class,  useNA = "always")
# unique(gb_dat_2[which(gb_dat_2$class=="Mammalia" & gb_dat_2$CoV_genus=="Deltacoronavirus"),]$host)
# gb_dat_2[gb_dat_2$host=="Marmota himalayana",]
# gb_dat_2[gb_dat_2$class=="Mammalia" & gb_dat_2$CoV_genus=="Deltacoronavirus" & is.na(gb_dat_2$host),]
# unique(gb_dat_2[which(gb_dat_2$class=="Mammalia" & gb_dat_2$CoV_genus=="Gammacoronavirus"),]$host)



# nrow(gb_dat_2) #26555

# dir.create("Data/Cleaned_dataframe")
saveRDS(gb_dat_2, "Data/Cleaned_dataframe/coronavirus_host_data_Feb_12_2022.RDS")



