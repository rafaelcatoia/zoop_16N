plot_mds3d <- function(df_group,ClusterVar,l_=0,r_=0,b_=0,t_=0,pad_=5,mds_OBJ){
  m <- list(
    l=l_,
    r=r_,
    b=b_,
    t=t_,
    pad=pad_
  )
  
  df_group$Cluster = df_group %>% dplyr::select(one_of(ClusterVar)) %>% pull()
  df_group$MDS1 = mds_OBJ$points[,1]
  df_group$MDS2 = mds_OBJ$points[,2]
  df_group$MDS3 = mds_OBJ$points[,3]
  
  output <- plotly::plot_ly(
    x=df_group$MDS1,
    y=df_group$MDS2,
    z=df_group$MDS3,
    type="scatter3d",
    color = as.numeric(df_group$Cluster),
    mode="markers") %>%
    plotly::layout(
      autorange = F, 
      aspectmode = 'manual', 
      scene = list(
        aspectratio = list(x = 4, y = 4, z = 3)
      )
    ) %>% 
    #plotly::layout(margin = m) %>% 
    plotly::hide_colorbar() 
  return(output)
}

