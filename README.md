# emerging_coronaviruses

1. Clone this repository

2. Download the datasets provided here
https://drive.google.com/drive/folders/1Ty-mPdDAKPGYe2M-SYzAJwEQByE433qq?usp=sharing and save them in the your Data folder of the cloned repository. These files have the raw data from Genbank, opened in Geneious Prime, and saved as .csv  

The remaining datasets are already available in the Data folder. You could open them directly or you could build them running the "genbak_data_creation.R" script. For this last case you will also need to insert a NCBI token in line 425. 

To reproduce the quantities provided in the chapter, run the script "data_description.R".

To reproduce the figure 25.2, run the script "script_main_figure.R"

To visualize the coronavirus - species relationship to the species level, run the script "host_coronavirus_widget.R" and then open the html file "CoV_dendrogram_interactive.html"
