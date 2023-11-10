geoCoherense <- function(
    data_taxonomy,ClustVar
){
  ## Functions that we will use
  sum_dist <- function(x){
    return(sum(dist(x)))
  }
  
  mean_dist <- function(x){
    return(mean(dist(x)))
  }
  
  median_dist <- function(x){
    return(median(dist(x)))
  }
  
  sd_dist <- function(x){
    return(sd(dist(x)))
  }
  
  data_taxonomy$Cluster = data_taxonomy %>% 
    dplyr::select(one_of(ClustVar)) %>% pull()
  
  ### Abiotic Coherense
  array_with_clusters <- 
    split(data_taxonomy %>% select(Cluster,
                                   Pressure_decibars,
                                   Salinity_psu,
                                   Temperature_degrees_Celsius),
          data_taxonomy$Cluster )
  
  df_AbioticsDistance <- data.frame(
    ClustSize = lapply(array_with_clusters,nrow) %>% unlist(),
    SumDist = lapply(array_with_clusters,sum_dist) %>% unlist(),
    MeanDist = lapply(array_with_clusters,mean_dist) %>% unlist(),
    MedianDist = lapply(array_with_clusters,median_dist) %>% unlist(),
    SDDist= lapply(array_with_clusters,sd_dist) %>% unlist()
  )
  
  
  ### Geo Coherence
  array_with_clusters <- 
    split(data_taxonomy %>% select(Cluster,
                                   Latitude,
                                   Depth,
                                   ),
          data_taxonomy$Cluster)
  
  df_LatDephtDistance <- data.frame(
    ClustSize = lapply(array_with_clusters,nrow) %>% unlist(),
    SumDist = lapply(array_with_clusters,sum_dist) %>% unlist(),
    MeanDist = lapply(array_with_clusters,mean_dist) %>% unlist(),
    MedianDist = lapply(array_with_clusters,median_dist) %>% unlist(),
    SDDist= lapply(array_with_clusters,sd_dist) %>% unlist()
  )
  
  output <- list()
  
  output$df_LatDephtDistance <- df_LatDephtDistance
  output$df_AbioticsDistance <- df_AbioticsDistance
  return(output)
}