ggLatDepth <- function(
    dataset,
    clusterVar,
    title ='',
    MedoidName='',
    CHull=F,
    OceanLayers=F,
    FacetClusters=F,
    LatitudeRegions=T){
  
  dataset$Cluster = dataset %>% dplyr::select(one_of(clusterVar)) %>% pull()
  
  if(MedoidName!=''){
    dataset$Medoid =  dataset %>% dplyr::select(one_of(MedoidName)) %>% pull()
  }
  
  output <- 
    ggplot(dataset,
           aes(x=Latitude,y=Depth,color=Cluster,label=Cluster))+
    geom_vline(xintercept = c(-60,-45,-5,5,35),col='gray75')+
    #geom_hline(yintercept =0,col='gray75')+
    geom_label(alpha=0.3)+
    scale_y_reverse() +
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  
  if(LatitudeRegions){
    output = output+
      annotate("text", x = -67, y = -25, label = "Southern Ocean",hjust=0.5)+
      annotate("text", x = -52, y = -25, label = "Subantarctic",hjust=0.5)+
      annotate("text", x = -25, y = -25, label = "South Pacific Gyre",hjust=0.5)+
      annotate("text", x = 0, y = -25, label = "Equatorial",hjust=0.5)+
      annotate("text", x = 20, y = -25, label = "North Pacific Gyre",hjust=0.5)+
      annotate("text", x = 45, y = -25, label = "Subarctic",hjust=0.5)
  }
  
  
  if(CHull){
    chull_ <- dataset %>% 
      group_by(Cluster) %>%
      slice(chull(Latitude, Depth))
    output =  output +
      geom_polygon(data = chull_, alpha = 0.2,aes(fill=Cluster))
  }
  
  
  if(MedoidName!=''){
    output <- output +
      geom_label(data = dataset %>% filter(Medoid==1),
                 mapping =aes(x=Latitude,y=Depth,color=Cluster,label=Cluster),
                 label.size = 1.5,alpha=0.6)
  }
  
  if(OceanLayers){
    output = output +
    geom_hline(yintercept = c(0,200,1000),col='gray75')
  }
  
  if(FacetClusters){
    output = output +
      facet_wrap(~Cluster,ncol=3)
  }
  
  return(output)
}
