
plot_mds2d <- function(df_group,mds_OBJ,ClusterVar,MedoidName=''){
  
  df_group$Cluster = df_group %>% dplyr::select(one_of(ClusterVar)) %>% pull()
  pct_explained = round(100 * mds_OBJ$eig/sum(mds_OBJ$eig),1)
  df_group$MDS1 = mds_OBJ$points[,1]
  df_group$MDS2 = mds_OBJ$points[,2]
  df_group$MDS3 = mds_OBJ$points[,3]
  
  if(MedoidName!=''){
    df_group$Medoid =  df_group %>% dplyr::select(one_of(MedoidName)) %>% pull()
  }
  
  output <- ggplot(
    data = df_group,
    mapping = aes(x=MDS1,y=MDS2,
                  color=Cluster,
                  label=Cluster))+
    geom_label(alpha=0.3) +
    theme_minimal() +
    xlab(paste('MDS1 -',pct_explained[1],'%'))+
    ylab(paste('MDS2 -',pct_explained[2],'%'))+
    theme(legend.position = '') 
  
  if(MedoidName!=''){
      output <- output +
        geom_label(data = df_group %>% filter(Medoid==1),
                   mapping =aes (x=MDS1,y=MDS2,color=Cluster,label=Cluster),
                   label.size = 1.5,alpha=0.5)
    }
  
  return(output)
}
