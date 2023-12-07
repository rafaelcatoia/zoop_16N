geoCoherense <- function(
    data_taxonomy,ClustVar,
    DephtVar='Depth',
    scalingLatDepth_Abiotics = T,
    ABS_Latitude = T
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
  
  data_taxonomy$DephtVar = data_taxonomy %>% 
    dplyr::select(one_of(DephtVar)) %>% pull()
  
  if(scalingLatDepth_Abiotics){
    data_taxonomy %>% mutate(
      Latitude = scale(Latitude),
      DephtVar = scale(DephtVar),
      Pressure_decibars = scale(Pressure_decibars),
      Salinity_psu = scale(Salinity_psu),
      Temperature_degrees_Celsius = scale(Temperature_degrees_Celsius)
    )
    
  }
  
  if(ABS_Latitude){
    data_taxonomy= data_taxonomy %>% mutate(Latitude=abs(Latitude))
  }
  
  
  ### Abiotic Coherense
  array_with_clusters <- 
    split(data_taxonomy %>%
            select(Cluster,
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
    split(data_taxonomy %>%
            select(Cluster,
                   Latitude,
                   DephtVar),
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
