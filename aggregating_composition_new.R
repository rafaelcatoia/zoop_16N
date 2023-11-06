###### from the raw data, this funciton creates an aggregated one. 
###### fillZeros = 'Fill': Add fillZerosValue to every 0
###### fillZeros = 'Add':  Add fillZerosValue to every part of the composition.
###### fillZeros = Nothing: No filling 
# aggregating_level = 'Species'
#dFrame <- dat_tax
#metadata = metadat

aggregating_compositions <- function(
  dFrame,
  metadata=FALSE,
  prefix = 'P16',
  fillZeros = 'Nothing',
  fillZerosValue = 1e-09,
  aggregating_level = 'ASV',
  remove_AllZeros=T,
  PresentAtLeast=1,
  RemoveMoment='Before'
){
  
  
  ## First lets just keep the ASV that are present in at least 2 samples.
  if(PresentAtLeast>1 & RemoveMoment=='Before'){
  
  df_zeros <- data.frame(
    ASV_Id = dFrame$ASV_ID,
    KeyAsv = 1:length(dFrame$ASV_ID),
    PresentInNSamples = 195 - apply(dFrame %>% select(starts_with("P16")),1,function(x){sum(x==0)})
    )
  
  #filtering only the ASVs that appear in at list #PresentAtLeast samples
  ASVs_to_Use = df_zeros %>% filter(PresentInNSamples>=PresentAtLeast) %>% 
    select(ASV_Id) %>% pull()
  
  ## Aggregating
  dFrame_out <- dFrame %>% filter(ASV_ID%in%ASVs_to_Use) %>% 
    dplyr::select(all_of(aggregating_level),starts_with("P16")) %>% 
    group_by(across(all_of(aggregating_level))) %>% 
    summarise_all(list(sum)) %>% transpose_df_composition()
  }else{
    
    ## Aggregating
    dFrame_out <- dFrame %>% 
      dplyr::select(all_of(aggregating_level),starts_with("P16")) %>% 
      group_by(across(all_of(aggregating_level))) %>% 
      summarise_all(list(sum)) %>% transpose_df_composition()
  }
  
  # When we remove after 
   if(RemoveMoment=='After'){
     
     df_zeros <- data.frame(
       AggId = colnames(dFrame_out)[-1],
       KeyAsv = 1:length(colnames(dFrame_out)[-1]),
       PresentInNSamples = 195 - apply(dFrame_out[,-1],2,function(x){sum(x==0)})
     )
  
     AggId_to_Use = df_zeros %>% filter(PresentInNSamples>=PresentAtLeast) %>% 
       select(AggId) %>% pull()
     
     dFrame_out = dFrame_out %>% 
       select(Samples,any_of(AggId_to_Use))
     }
  
  
  ## Removing the samples that have 0 composition
  if(remove_AllZeros){
    dFrame_out<-dFrame_out[rowSums(dFrame_out[,-1])!=0,]
  }
  
  ## Filling Zeros - Only the ones that are 0
  if(fillZeros=='Fill'){
    dFrame_out[dFrame_out==0] <- fillZerosValue
  }
  
  
  ## Filling Zeros - Adding to the entire vector
  if(fillZeros=='Add'){
    dFrame_out[,-1] <- dFrame_out[,-1] + fillZerosValue
  }
  
  ## Removing the samples that thave 0 composition
  if(remove_AllZeros){
    dFrame_out<-dFrame_out[rowSums(dFrame_out[,-1])!=0,]
  }
  
  dFrame_out <- bind_cols(
  dFrame_out[,1],
  apply(dFrame_out[,-1],1,function(x){x/sum(x)}) %>% t()
  )
  
  if(!isFALSE(metadata)){
  dFrame_out <- dFrame_out %>% left_join(
    metadata %>% select(Samples,Latitude,Longitude,Depth,
                        LatitudeRegion,Pressure_decibars,Salinity_psu,
                        Temperature_degrees_Celsius)
    )
  }
  
  return(dFrame_out)
}
