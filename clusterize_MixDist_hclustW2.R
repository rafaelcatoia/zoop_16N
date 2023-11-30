## Function to create the clusters
## data_taxonomy = dat_tax
## vec_aggreg = vec_aggregation
## max_k = maximum number of clusters
## max_at_least = max number of at least present in samples
###### moment = 'Before' means that we will remove the
###### ASVs present in less then max_at_least.
###### moment = 'After' means that we will remove the
###### AGG that are present in less then max_at_least
clusterize_MixDist_hclustW2 <- function(
    data_taxonomy,
    data_metadat,
    vec_aggreg,
    max_k=30,
    max_at_least=1,
    moment='Before',DepthRank=T,propGeo=0.2){
  
  
  if(DepthRank){
    
    data_metadat <- data_metadat %>%
      select(-Depth) %>% 
      left_join(data_metadat %>% 
      select(Samples,Latitude,Depth) %>% 
         arrange(Samples,Depth) %>% select(-Depth) %>% 
         group_by(Latitude) %>% mutate(Depth=1:n()))
  }
  
  list_output <- list()
  #For Each At Least
  
  list_atLeast <- list()
  for(i in 1:max_at_least){
    #For each aggregation level
    list_vecAgg <- list()
    for(j in 1:length(vec_aggregation)){
      
      df_comp <- aggregating_compositions(
        dFrame = data_taxonomy,
        fillZeros = 'Add',
        aggregating_level = vec_aggregation[j],
        PresentAtLeast = i,RemoveMoment = moment
      )
      
        clrObj <- ALR_CLR_distMatrices(df_comp)
        
        df_comp <- df_comp %>% left_join(data_metadat %>% select(Samples,Latitude,Depth))
        
        GeoDist <- df_comp %>% select(Latitude,Depth) %>% dist()
        
        ## Mixing the distances
        GeoDist = GeoDist/max(GeoDist)
        AitchisonDist <- clrObj$dist_Aitchison/max(clrObj$dist_Aitchison)
        
        FinalDist = propGeo*GeoDist+ (1-propGeo)*AitchisonDist
        
        
        hclust_obj <- hclust(d = FinalDist,
               method = 'ward.D2')
        
        dframe_clusters <- as.data.frame(
          matrix(NA,nrow = nrow(clrObj$CLR$x.clr),ncol=max_k)
          )
        
        for(k in 1:max_k){
          dframe_clusters[,k] <- cutree(hclust_obj,k = k)
        }
      
        colnames(dframe_clusters) <- ifelse(1:max_k < 10,
                                            paste('Cluster0',1:max_k,sep=''),
                                            paste('Cluster',1:max_k,sep=''))
        
      list_vecAgg[[j]] <- dframe_clusters
    }
    names(list_vecAgg) <- vec_aggreg
    list_atLeast[[i]]<- list_vecAgg
    rm(list_vecAgg)
  }
  list_output <- list_atLeast
  names(list_output)<- paste('AtLeast',1:max_at_least,sep='')
  return(list_output)
}
