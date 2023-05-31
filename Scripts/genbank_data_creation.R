library(xml2)
library(xmltools)
library(lubridate)
library(tidyverse)
library(parallel)
library(purrr)

queries<-
  list(delta.query='((deltacoronavirus) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])',
       gamma.query='((gammacoronavirus) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])',
       beta.query='((betacoronavirus) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])',
       alpha.query='((alphacoronavirus) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])',
       corona.query='((coronavirus) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])',
       corona2.query='((coronaviridae) NOT homo sapiens) AND ("2000/01/01"[PDAT] : "2022/02/12"[PDAT])')


# Define query parameters
db <- "nucleotide"
retmax <- 99000

# Construct URL for the query
url <- 
  map(queries, function(query)
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
           "esearch.fcgi?db=", db,
           "&term=", URLencode(query),
           "&retmax=", retmax))

# Get the XML content of the search results
xml <- map(url, function(url) read_xml(url))

# xml_structure(xml[[1]])
#xml_view_tree(xml[[1]])

# Extract the list of matching IDs from the search results
ids <- mclapply(xml, function(xml) xml_text(xml_find_all(xml, "//IdList/Id")), mc.cores = 5)

#unique ids from all queries
ids<-unique(unlist(ids, use.names = F))

# Construct URL for retrieving the records
urls <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
              "efetch.fcgi?db=", db,
              # "&id=", paste(ids, collapse = ","),
              "&id=", ids,
              "&rettype=xml")



#list to contain the xml record of each id. This is going to take a while
xml<-vector(mode="list", length = length(urls))


for(x in seq_along(urls)){

    
  xml[[x]] <- read_xml(urls[x]) %>% 
              xml_serialize(NULL) # if not serialized, the list with xml's is not
                                  # properly saved.
  
  cat(paste(x, ", ", sep = "" ))}


#split list in chunks of 5000 object to save each object as a single list less than 25 mb
xml <- split(xml, ceiling(seq_along(xml) / 100))


#save the data in chunks to upload it to the project in github

dir.create("Data/Original_data")

mclapply(seq_along(xml), function(x)
  saveRDS(xml[[x]],
          paste0("Data/Original_data/all_data_nucleotide_in_xml_", x, ".RDS")), mc.cores = 6)


gc()


#read the data into one single list
xml<-mclapply(list.files(path = "Data/Original_data", pattern = "all_data_nucleotide_in_xml_"), 
                           function(x) readRDS(paste0("Data/Original_data/", x)), mc.cores = 6)

      
xml<-lapply(unlist(xml, recursive = F), xml_unserialize)



# Create a dataset from the data in genbank 

gb_dat<-mclapply(seq_along(xml), function(x)

  data.frame(
    
    name= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_locus"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_locus"))),
    
    accession = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_primary-accession"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_primary-accession"))),
    
    common.name=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_source"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_source"))),
    
    description=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_definition"))==0,
      NA, xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_definition"))),
    
    note=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), 
                          ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='note']/GBQualifier_value"))==0,
      NA, 
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='note']/GBQualifier_value"))),
    
    molecule.type=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_moltype"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_moltype"))),
    
    organism= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_organism"))==0, NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_organism"))),
    
    taxonomy= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_taxonomy"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_taxonomy"))),
    
    collection.date =ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), 
                          ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='collection_date']/GBQualifier_value"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='collection_date']/GBQualifier_value"))),
    
    created.date = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_create-date"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_create-date"))),
    
    country=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='country']/GBQualifier_value"))==0,
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='country']/GBQualifier_value"))),
    
    host = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='host']/GBQualifier_value"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), 
                            ".//GBSeq_feature-table/GBFeature/GBFeature_quals/GBQualifier[GBQualifier_name='host']/GBQualifier_value"))),
    
    pubmed.id=ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_pubmed"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_pubmed"))),
    
    title= ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_title"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_title"))),
    
    fulljournalname = ifelse(
      length(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_journal"))==0, 
      NA,
      xml_text(xml_find_all(xml[[x]] %>% xml_child(), ".//GBSeq_references/GBReference[1]/GBReference_journal")))), 
  
  mc.cores=6)

gc()



#dataframe
gb_dat<-do.call(rbind , gb_dat)

#modify the date
gb_dat$created.date <- dmy(gb_dat$created.date)

gc()

dir.create("Data/Original_data_dataframe")

#save the data
saveRDS(gb_dat, "Data/Original_data_dataframe/all_data_nucleotide_ready_to_be_cleaned.RDS")

rm(xml); gc()

