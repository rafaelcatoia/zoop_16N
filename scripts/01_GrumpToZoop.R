################################################################
## From entire GRUMP, select only the ASVs that are Zooplanktons
## For only some specific cruises (P16N P16S)
################################################################

library(dplyr) ; library(ggplot2) ; library(tidyr)

### this is the first file Youbin shared with us. In this dataset we just have the Relative Abundance (of all ASVs and not only the zooplanktons)
dfZoopWider = data.table::fread('/Users/rafaelcatoia/MyDrive/20_UCSC/Capstone/00_Data/Pacific Ocean/20230628_P16NS_All_Zooplankton_ASVs_PG_SG1_V1.csv')

length(dfZoopWider$ASV_ID %>% unique()) # 3586


### Creating vectors with specific type of variables that we are looking for.
idGeo_vec = c("SampleID","Cruise","Station","Latitude","Longitude","Depth","Longhurst_Short","Longhurst_Long")
biotic_vec = c("Plas_Domain","Domain","Kingdom" ,"Phylum","Class",
               "Order","Family","Genus","Species","ASV_name","Raw.Sequence.Counts")
abiotic = c("Temperature","Salinity","Oxygen","Silicate","NO2","NO3","NOx","NH3","PO4")


### Loading, filtering and selecting variables from the entire GRUMP
dfGrumpLonger = data.table::fread('/Users/rafaelcatoia/Desktop/repos/Capstone/ZoopData/grump_asv_long.csv') %>% 
  filter(Cruise %in% c('P16N','P16S')) %>% #Selecting only the Cruises that we want
  filter(OTU_ID %in% dfZoopWider$ASV_ID) %>% #Selecting only the OTUs that Youbin provided us at the begining of the study
  select(all_of(c(idGeo_vec,biotic_vec,abiotic)))


length(dfGrumpLonger$ASV_name %>% unique()) # 3422 some ASVs (or OTUs) are not present n GRUMp.
############ I'll Assume I have filtered the data in the right way!

data.table::fwrite(dfGrumpLonger,file = 'data/grump_Zoop_16NS_longer.csv')