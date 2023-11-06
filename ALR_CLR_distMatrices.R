### distance matrices
ALR_CLR_distMatrices <- function(
    dt_compostition
){
  output<-list()
  
  output$ALR <- robCompositions::addLR(
    dt_compostition[,-1] %>% data.frame(),
    ivar =  dt_compostition[,-1] %>% colSums() %>% which.min())
  
  output$CLR <- robCompositions::cenLR(dt_compostition[,-1] %>% data.frame())
  
  #output$ILR <- robCompositions::isomLRp(dt_compostition[,-1] %>% data.frame())
  #
  #output$ALR_dist_euclidean <- dist(output$ALR$x.alr,
  #                           method = 'euclidean')
  
  #output$ALR_dist_manhattan <- dist(output$ALR$x.alr,
  #                           method = 'manhattan')
  #
  #output$ALR_dist_manhattan <- dist(output$ALR$x.alr,
   #                                 method = 'manhattan')
  
  #output$CLR_dist_euclidean <- dist(output$CLR$x.clr,
  #                           method = 'euclidean')
  #
  #output$CLR_dist_manhattan <- dist(output$CLR$x.clr,
  #                           method = 'manhattan')
  #
  #output$CLR_dist_manhattan <- dist(output$CLR$x.clr,
  #                           method = 'manhattan')
  
  output$dist_Aitchison <- robCompositions::aDist(dt_compostition[,-1] %>% data.frame())
  output$dist_Jaccard <- vegan::vegdist(x = dt_compostition[,-1] %>% data.frame() ,method = 'jaccard' )
  output$dist_Chisq <- vegan::vegdist(x = dt_compostition[,-1] %>% data.frame() ,method = 'chisq' )
  
  return(output)
}

