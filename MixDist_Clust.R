
## Input list
## input$aggLevel = 'PG'
## input$clustN = 5
## input$AtLeastN = 1
# input$RemoveMoment = "Before"

MixDist_Clust <- function(
    metadat_aux,data_tax_aux,
    alphaGeo=0,
    depthRank='Y',
    ABS_Latitude=F,
    scalingLatDepth = F,
    k_clust){
  AitDist <- ALR_CLR_distMatrices(data_tax_aux)$dist_Aitchison
  
  if(depthRank=='Y'){
    metadat_aux <- metadat_aux %>%
      left_join(metadat_aux %>% 
                  select(Samples,Latitude,Depth) %>% 
                  arrange(Depth) %>% mutate(Depth_Calc=1:n()))
  }
  
  data_tax_aux <- metadat_aux %>%
    left_join(metadat_aux %>% select(Samples,Latitude,Depth)) %>% 
    mutate(Depth_Calc = Depth, Latitude_Calc = Latitude)
  
  if(ABS_Latitude){
    data_tax_aux = data_tax_aux %>% mutate(Latitude_Calc=abs(Latitude_Calc))
  }
  
  if(scalingLatDepth){
    data_tax_aux = data_tax_aux %>% 
      mutate(Depth_Calc=scale(Depth_Calc),
             Latitude_Calc=scale(Latitude_Calc))
  }
  
  GeoDist = data_tax_aux %>% select(Latitude_Calc,Depth_Calc) %>% dist()  

  ## Mixing the distances
  GeoDist = GeoDist/max(GeoDist)
  AitchisonDist <- AitDist/max(AitDist)
  
  FinalDist = alphaGeo*GeoDist+ (1-alphaGeo)*AitchisonDist
  
  output <- data.frame(
    MedoidClust = cluster::pam(x=FinalDist, k = k_clust,stand = FALSE,cluster.only = T),
    WardClust = cutree(hclust(d = FinalDist,method = 'ward.D2'),k = k_clust))
  
return(output)
}
