###### ----- Functon to calculate the SumOfSquares 
###### --------------------------------------------
SSq_SSabs <- function(df_aux_,
                cluster_name='Kmedoids',
                medoid_name='Medoid'){
  output_SSq <- list()
  output_SSabs <- list()
  number_Of_Clusters <- df_aux_ %>% select(one_of(cluster_name)) %>% max()
  
  df_aux_$cluster_aux = df_aux_ %>% select(one_of(cluster_name)) %>% pull()
  df_aux_$medoid_aux = df_aux_ %>% select(one_of(medoid_name)) %>% pull()
  
  for(i in 1:number_Of_Clusters){
    
    medoid <- df_aux_ %>% filter(cluster_aux==i, medoid_aux==1) %>% 
      dplyr::select(-cluster_aux,-medoid_aux,-Kmedoids,-Medoid)
    
    #medoid <- t(medoid)[,1]
    
    df_aux_matrix <- df_aux_ %>% filter(cluster_aux==i) %>% 
      dplyr::select(-cluster_aux,-medoid_aux,-Kmedoids,-Medoid) %>%
      as.matrix()

    
    output_SSq[[i]] <- sum((sweep(df_aux_matrix,2,t(medoid),`-`))^2)
    output_SSabs[[i]] <- sum(abs(sweep(df_aux_matrix,2,t(medoid),`-`)))
  }
  
  names(output_SSq) <- paste('Within_',1:number_Of_Clusters,sep='')
  names(output_SSabs) <- paste('Within_',1:number_Of_Clusters,sep='')
  
  output_SSq$SSq_Tot <- sum(unlist(output_SSq))
  output_SSabs$SSabs_Tot <- sum(unlist(output_SSabs))
  
  outlist<-list()
  outlist$SSq <- output_SSq
  outlist$SSabs <- output_SSabs
  
  return(outlist)
}
