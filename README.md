# Wildlife Health and Disease in Conservation
# Chapter 25: Emerging Coronaviruses: A One Health Harbinger
CK Johnson, JME Freeman,
T Smiley-Evans, Diego Montecino-Latorre,
MM. Uhart

Project to get coronvarirus data from the nucleotide database (https://www.ncbi.nlm.nih.gov/nucleotide/), create a dataset, clean the coronavirus hosts reported, assess the data per host taxon and coronavirus taxon, and create figure 25.2 in the book.

The data aims to provide potential hosts of the Coronaviridae/Orthocoronavirinae viral family/sub-family. I have excluded Letovirinae and Pitovirinae; however, I left other observations that were classified up to the "coronaviridae" family that may have unreported sequences associated with these subfamilies (unlikely given the remaining hosts are not "fishes").
The data does not include betacoronaviruses reported in birds from Brazil because these results have been disputed (PMID: 26250156).
The data does not include an Orthocoronavirus detected in a Culex mosquito.
The data does not include a reptile in China (PMID: 29618816, accession MG600026. Check supplementary materials. It seems to be part of a multispecies pool).
Several reports have not been per-reviewed or published and must be taken cautiously. For example, an alpaca with a Gammacoronavirus detection.
Betacoronavirus sub-genera are the only sub-genera provided in the dataset. However, several alpha, gamma, and deltacoronaviruses are classified to the subgenus level in the original data.

To construct the original dataset, run the "genbank_data_creation.R" script. Queries used to get the data from the nucleotide database are at the beginning of this script and they request data created until February 12th, 2022. Downloading the data can take a while. The resulting original data is stored in the "Data/Original_data" folder. This script converts the original data in xml format to a dataframe (located in "Data/Original_data_dataframe"). 

To reproduce the dataset with cleaned host and assess each change made in the original dataset and filters applied run the "data_host_cleaning.R" script. The starting dataframe is located in "Data/Original_data_dataframe". You need a ENTREZ_KEY to runn this script (line 348). The resulting host-cleaned betacoronavirus cleaned dataset is in the "Data/Cleaned_dataframe" folder and it is named "coronavirus_host_data_Feb_12_2022.RDS"

To get the descriptive quantities regarding hosts and Orthocoronavirus, run the "data_description.R" script.

To reproduce the figure 25.2, run the "script_main_figure.R" script.

To visualize the coronavirus - species relationship to the species level, run the "host_coronavirus_widget.R" script and download the folders and file in the folder "Cov_hosts"full", and  open the html file "CoV_dendrogram_interactive.html" 
