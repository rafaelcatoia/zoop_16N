ggLatDepth_temp <- function(
    dataset,
    clusterVar,
    title ='',
    IncludeMedoid=F,
    CHull=F){
  output <- list()
  dataset$Cluster = dataset %>% dplyr::select(one_of(clusterVar)) %>% pull()
  
   output$temp <- 
     ggplot(dataset,
            aes(x=Latitude,y=Depth,color=Temperature_degrees_Celsius,label=Cluster))+
     geom_vline(xintercept = c(-60,-45,-5,5,35),col='gray75')+
     #geom_hline(yintercept =0,col='gray75')+
     geom_label(alpha=0.7)+
     scale_y_reverse() +
     theme_minimal()+ 
     annotate("text", x = -67, y = -25, label = "Southern Ocean",hjust=0.5)+
     annotate("text", x = -52, y = -25, label = "Subantarctic",hjust=0.5)+
     annotate("text", x = -25, y = -25, label = "South Pacific Gyre",hjust=0.5)+
     annotate("text", x = 0, y = -25, label = "Equatorial",hjust=0.5)+
     annotate("text", x = 20, y = -25, label = "North Pacific Gyre",hjust=0.5)+
     annotate("text", x = 45, y = -25, label = "Subarctic",hjust=0.5)+
     ggtitle(title)+
     theme(legend.position = 'bottom',
           panel.grid.major = element_blank())+
     scale_color_viridis()
   
   output$Salinity <- ggplot(dataset,
                             aes(x=Latitude,y=Depth,color=Salinity_psu,label=Cluster))+
     geom_vline(xintercept = c(-60,-45,-5,5,35),col='gray75')+
     #geom_hline(yintercept =0,col='gray75')+
     geom_label(alpha=0.7)+
     scale_y_reverse() +
     theme_minimal()+ 
     annotate("text", x = -67, y = -25, label = "Southern Ocean",hjust=0.5)+
     annotate("text", x = -52, y = -25, label = "Subantarctic",hjust=0.5)+
     annotate("text", x = -25, y = -25, label = "South Pacific Gyre",hjust=0.5)+
     annotate("text", x = 0, y = -25, label = "Equatorial",hjust=0.5)+
     annotate("text", x = 20, y = -25, label = "North Pacific Gyre",hjust=0.5)+
     annotate("text", x = 45, y = -25, label = "Subarctic",hjust=0.5)+
     ggtitle(title)+
     theme(legend.position = 'bottom',
           panel.grid.major = element_blank())+
     scale_color_viridis()
   
   output$Pressure <- ggplot(dataset,
                             aes(x=Latitude,y=Depth,color=Pressure_decibars,label=Cluster))+
     geom_vline(xintercept = c(-60,-45,-5,5,35),col='gray75')+
     #geom_hline(yintercept =0,col='gray75')+
     geom_label(alpha=0.7)+
     scale_y_reverse() +
     theme_minimal()+ 
     annotate("text", x = -67, y = -25, label = "Southern Ocean",hjust=0.5)+
     annotate("text", x = -52, y = -25, label = "Subantarctic",hjust=0.5)+
     annotate("text", x = -25, y = -25, label = "South Pacific Gyre",hjust=0.5)+
     annotate("text", x = 0, y = -25, label = "Equatorial",hjust=0.5)+
     annotate("text", x = 20, y = -25, label = "North Pacific Gyre",hjust=0.5)+
     annotate("text", x = 45, y = -25, label = "Subarctic",hjust=0.5)+
     ggtitle(title)+
     theme(legend.position = 'bottom',
           panel.grid.major = element_blank())+
     scale_color_viridis()
  
  
  p1 = ggplot(dataset,
              aes(x=Temperature_degrees_Celsius,y=Pressure_decibars,
                  color=Cluster,label=Cluster))+
    geom_label(alpha=0.7)+
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  p2 = ggplot(dataset,
         aes(x=Temperature_degrees_Celsius,y=Salinity_psu,
             color=Cluster,label=Cluster))+
    geom_label(alpha=0.7)+
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  p3 = ggplot(dataset,
              aes(x=Salinity_psu,y=Pressure_decibars,
                  color=Cluster,label=Cluster))+
    geom_label(alpha=0.7)+
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  if(CHull){
    
    chull_p1 <- dataset %>% 
      group_by(Cluster) %>%
      slice(chull(Temperature_degrees_Celsius,y=Pressure_decibars))
    p1 =  p1 +
      geom_polygon(data = chull_p1, alpha = 0.2,aes(fill=Cluster))
    
    chull_p2 <- dataset %>% 
      group_by(Cluster) %>%
      slice(chull(Temperature_degrees_Celsius,y=Salinity_psu))
    p2 =  p2 +
      geom_polygon(data = chull_p2, alpha = 0.2,aes(fill=Cluster))
    
    chull_p3 <- dataset %>% 
      group_by(Cluster) %>%
      slice(chull(Salinity_psu,y=Pressure_decibars))
    p3 =  p3 +
      geom_polygon(data = chull_p3, alpha = 0.2,aes(fill=Cluster))
  }
  
  if(IncludeMedoid){
    
    p1 = p1 + geom_label(data = dataset %>% filter(Medoid==1),
                         mapping =aes(x=Temperature_degrees_Celsius,y=Pressure_decibars,color=Cluster,label=Cluster),
                         label.size = 1.5,alpha=0.6)
    
    p2 = p2 + geom_label(data = dataset %>% filter(Medoid==1),
                         mapping =aes(x=Temperature_degrees_Celsius,y=Salinity_psu,color=Cluster,label=Cluster),
                         label.size = 1.5,alpha=0.6)
    
    p3 = p3 + geom_label(data = dataset %>% filter(Medoid==1),
                         mapping =aes(x=Salinity_psu,y=Pressure_decibars,color=Cluster,label=Cluster),
                         label.size = 1.5,alpha=0.6)
  }
  
  p1 = p1 + scale_y_reverse()
  p3 = p3 + scale_y_reverse()
  
  output$Temp_Press <- p1
  output$Temp_Salinity <- p2
  output$Salinity_Press <- p3
  output$Scatter = arrangeGrob(p1,p2,p3,ncol=3)
  
  return(output)
}


