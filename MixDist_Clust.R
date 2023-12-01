
## Input list
## input$aggLevel = 'PG'
## input$clustN = 5
## input$AtLeastN = 1
# input$RemoveMoment = "Before"

MixDist_Clust <- function(
    metadat_aux,data_tax_aux,
    alphaGeo,
    depthRank='Y',
    ABS_Latitude=F,
    k_clust,
    ){
  AitDist <- ALR_CLR_distMatrices(data_tax_aux)$dist_Aitchison
  
  if(depthRank=='Y'){
    metadat_aux <- metadat_aux %>%
      select(-Depth) %>% 
      left_join(metadat_aux %>% 
                  select(Samples,Latitude,Depth) %>% 
                  arrange(Samples,Depth) %>% select(-Depth) %>% 
                  group_by(Latitude) %>% mutate(Depth=1:n()))
  }
  
  data_tax_aux <- data_tax_aux %>% left_join(metadat_aux %>% select(Samples,Latitude,Depth))
  
  if(ABS_Latitude){
    GeoDist <- data_tax_aux %>% select(Latitude,Depth) %>% mutate(Latitude=abs(Latitude))%>% dist()  
  }else{
    GeoDist <- data_tax_aux %>% select(Latitude,Depth) %>% dist()  
  }
  
  ## Mixing the distances
  GeoDist = GeoDist/max(GeoDist)
  AitchisonDist <- AitDist/max(AitDist)
  
  FinalDist = alphaGeo*GeoDist+ (1-alphaGeo)*AitchisonDist
  
  output <- data.frame(
    MedoidClust = cluster::pam(x=FinalDist, k = k_clust,stand = FALSE,cluster.only = T),
    WardClust = cutree(hclust(d = FinalDist,method = 'ward.D2'),k = k_clust))
  
return(output)
}
