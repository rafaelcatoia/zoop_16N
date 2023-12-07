############ Script to generate images for the presentation ::::::::::::::::::: 
library(tidyverse) ; library(dplyr) ; library(ggplot2)
library(gridExtra) ; library(tidyr) ; library(robCompositions) ; library(viridis)  ; library(plotly)

#### Loading functions 
source("https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/ALR_CLR_distMatrices.R")
source("https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/aggregating_composition_new.R")
source("https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/ggLatDepth.R")
source("https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/ggLatDepth.R")
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/ggLatDepth_temp.R')
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/plot_mds2d.R')
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/plot_mds3d.R')
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/functions.R')
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/clusterize_medoids.R')
source('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/clusterize_hclustW2.R')

metadat = data.table::fread('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/metadat_abiotic.csv') %>% as_tibble()

dat_tax = data.table::fread('https://raw.githubusercontent.com/rafaelcatoia/zoop_16N/main/treated_taxonomy_dat.csv') %>% 
  as_tibble()

dat_tax = dat_tax %>% mutate(
  ASV=ifelse(
    1:nrow(dat_tax)<10,
    paste('ID000',1:nrow(dat_tax),sep=''),
    ifelse(1:nrow(dat_tax)<100,
           paste('ID00',1:nrow(dat_tax),sep=''),
           ifelse(1:nrow(dat_tax)<1000,
                  paste('ID0',1:nrow(dat_tax),sep=''),
                  paste('ID',1:nrow(dat_tax),sep=''))))
  
)

mapview(metadat %>% 
          group_by(Latitude,Longitude),
        xcol = "Longitude",cex = 12,
        ycol = "Latitude", crs = 4269,
        legend = F,
        grid = T)

p1 = ggplot(metadat,aes(x=Latitude,y=Depth))+
  geom_point(alpha=0.9,size=6)+
  scale_y_reverse() +
  theme_minimal(base_size = 20)

ggsave(plot = p1,
       filename = '/Users/rafaelcatoia/MyDrive/20_UCSC/000_Courses/0000_Capstone/Presentation/ggLatDepth_raw.png',
       width = 18,height = 8)

##### Plots for the presentation
vec_aggregation <- c(
  'PG',
  'SG1',
  'Supergroup',
  'Division',
  'Class',
  'Order',
  'Family',
  'Genus',
  'Species',
  'ASV')
source('/Users/rafaelcatoia/Desktop/repos/Capstone/clusterize_MixDistW2.R')
source('/Users/rafaelcatoia/Desktop/repos/Capstone/clusterize_MixDist_kmedoids.R')

## Creating all Clusters
listMedois <- clusterize_MixDist_kmedoids(
  data_taxonomy = dat_tax,
  vec_aggreg = vec_aggregation,
  data_metadat = metadat,
  propGeo = 0,
  max_k = 25,max_at_least = 1)

listW2 <- clusterize_MixDistW2(
  data_taxonomy = dat_tax,
  vec_aggreg = vec_aggregation,
  data_metadat = metadat,
  propGeo = 0,
  max_k = 25, max_at_least = 1)

###### Gif for 
sample_aggLevel <- c('PG','Division','Genus','Species')
desired_k <- c(1,2,3,4,5,10,10,15,15,20,20,25,25)
list_plots <- list()
list_plots_Ward <- list()
list_plots_Medoid <- list()

df_comp <- aggregating_compositions(
  dFrame = dat_tax,
  fillZeros = 'Nothing',
  aggregating_level = vec_aggregation[1]
) %>% select(Samples) %>%
  left_join(metadat %>%
              select(Samples,Latitude,Depth))

for( i in 1:length(sample_aggLevel)){
  
  for(j in 1:length(desired_k)){
    
    ## preparing the dataframe 
    df_comp$ClusterW2 <- factor(listW2[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    df_comp$ClusterMedoid <- factor(listMedois[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    
    list_plots_Ward[[j]] <- ggLatDepth(df_comp,
                                       clusterVar = 'ClusterW2',
                                       baseSize = 24,
                                       labelSize = 24,
                                         title = paste(sample_aggLevel[i],'; k= ',desired_k[j]))
      
    list_plots_Medoid[[j]] <- ggLatDepth(df_comp,
                                         clusterVar = 'ClusterMedoid',
                                         baseSize = 24,
                                         labelSize = 24,
                                         title = paste(sample_aggLevel[i],'; k= ',desired_k[j]))
  }
  
  list_plots[[i]] <- list( Ward = list_plots_Ward,
                           Medoid = list_plots_Medoid
  )
}

names(list_plots) <- sample_aggLevel

list_plots$PG$Ward[[1]]

animation::saveGIF({
  for (i in 1:length(list_plots$Division$Ward)) {
    print(list_plots$Division$Ward[[i]])
  }},movie.name = '/Users/rafaelcatoia/Desktop/repos/Capstone/testward.gif',
  ani.height = 900,
  ani.width = 1600,interval = 1)

animation::saveGIF({
  for (i in 1:length(list_plots$Division$Medoid)) {
    print(list_plots$Division$Medoid[[i]])
  }},movie.name = '/Users/rafaelcatoia/Desktop/repos/Capstone/testmedoid.gif',
  ani.height = 900,
  ani.width = 1600,interval = 1)



##################### -------------
vec_aggregation
sample_aggLevel <- vec_aggregation[-c(1,2,10)]
desired_k <- c(15)
list_plots <- list()
list_plots_Ward <- list()
list_plots_Medoid <- list()

df_comp <- aggregating_compositions(
  dFrame = dat_tax,
  fillZeros = 'Nothing',
  aggregating_level = vec_aggregation[1]
) %>% select(Samples) %>%
  left_join(metadat %>%
              select(Samples,Latitude,Depth))

for( i in 1:length(sample_aggLevel)){
  
  for(j in 1:length(desired_k)){
    
    ## preparing the dataframe 
    df_comp$ClusterW2 <- factor(listW2[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    df_comp$ClusterMedoid <- factor(listMedois[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    
    list_plots_Ward[[j]] <- ggLatDepth(df_comp,
                                       clusterVar = 'ClusterW2',
                                       baseSize = 24,
                                       labelSize = 24,
                                       title = paste('Ward','; k= ',desired_k[j]  ,' ; ', sample_aggLevel[i]))
    
    list_plots_Medoid[[j]] <- ggLatDepth(df_comp,
                                         clusterVar = 'ClusterMedoid',
                                         baseSize = 24,
                                         labelSize = 24,
                                         title = paste('PAM','; k= ',desired_k[j] ,' ; ', sample_aggLevel[i]))
  }
  
  list_plots[[i]] <- list( Ward = list_plots_Ward,
                           Medoid = list_plots_Medoid
  )
}

names(list_plots) <- sample_aggLevel

list_plots[[1]]

animation::saveGIF({
  for (i in 1:length(sample_aggLevel)) {
    print(list_plots[[i]]$Ward)
  }},movie.name = '/Users/rafaelcatoia/Desktop/repos/Capstone/varyingTax_ward.gif',
  ani.height = 900,
  ani.width = 1800,interval = 4)

animation::saveGIF({
  for (i in 1:length(sample_aggLevel)) {
    print(list_plots[[i]]$Medoid)
  }},movie.name = '/Users/rafaelcatoia/Desktop/repos/Capstone/varyingTax_medoid.gif',
  ani.height = 900,
  ani.width = 1800,interval = 4)


####### ------------- Comparing

p1 = df_outputSS %>% 
  mutate(NumberOfClusters = as.integer(gsub('Cluster','',clusters)),
         Dimension = ifelse(dimension=='df_LatDephtDistance','GeoSpatial','Abiotics'),
         agglevel=factor(agglevel,levels=vec_aggregation)) %>%
  filter(atleast=='AtLeastIn1') %>% filter(agglevel!='PG',agglevel!='SG1') %>% 
  filter(Dimension=='GeoSpatial') %>% 
  ggplot(aes(x=NumberOfClusters,y=log(TotalSum),color=method,linetype=method))+
  geom_line(linewidth=1.5,alpha=0.9)+
  facet_wrap(~agglevel,nrow=2)+
  xlab('k')+ylab('log( Sum of Squares ) - Latitude & Depth')+
  theme_minimal(base_size = 20)+
  theme(legend.position = 'bottom')

ggsave(filename = 'SS_raw.png',
       plot = p1,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 18,height = 8)

p2 = df_outputSS %>% 
  mutate(NumberOfClusters = as.integer(gsub('Cluster','',clusters)),
         Dimension = ifelse(dimension=='df_LatDephtDistance','GeoSpatial','Abiotics'),
         agglevel=factor(agglevel,levels=vec_aggregation)) %>%
  filter(atleast=='AtLeastIn1') %>% filter(agglevel!='PG',agglevel!='SG1') %>% 
  filter(Dimension=='GeoSpatial') %>% 
ggplot(aes(x=NumberOfClusters,y=log(TotalSum),color=agglevel))+
  geom_line(linewidth=1.5,alpha=0.9)+
  xlab('k')+ylab('log( Sum of Squares ) - Latitude & Depth')+
  facet_grid(~method)+
  theme_minimal(base_size = 20)+
  theme(legend.position = 'bottom')+
  guides(color = guide_legend(title = "Tax. Granularity")) +
  viridis::scale_color_viridis(discrete = T)


ggsave(filename = 'SS_raw_2.png',
       plot = p2,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 18,height = 9,dpi = 300)


#### ------------ Getting the best plot 


vec_aggregation
sample_aggLevel <- vec_aggregation[8]
desired_k <- c(18)
list_plots <- list()
list_plots_Ward <- list()
list_plots_Medoid <- list()

df_comp <- aggregating_compositions(
  dFrame = dat_tax,
  fillZeros = 'Nothing',
  aggregating_level = vec_aggregation[1]
) %>% select(Samples) %>%
  left_join(metadat %>%
              select(Samples,Latitude,Depth))

for( i in 1:length(sample_aggLevel)){
  
  for(j in 1:length(desired_k)){
    
    ## preparing the dataframe 
    df_comp$ClusterW2 <- factor(listW2[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    df_comp$ClusterMedoid <- factor(listMedois[[1]][[sample_aggLevel[i]]][,desired_k[j]])
    
    list_plots_Ward[[j]] <- ggLatDepth(df_comp,
                                       clusterVar = 'ClusterW2',
                                       baseSize = 24,
                                       labelSize = 24,
                                       title = paste('Ward','; k= ',desired_k[j]  ,' ; ', sample_aggLevel[i]))
    
    list_plots_Medoid[[j]] <- ggLatDepth(df_comp,
                                         clusterVar = 'ClusterMedoid',
                                         baseSize = 24,
                                         labelSize = 24,
                                         title = paste('PAM','; k= ',desired_k[j] ,' ; ', sample_aggLevel[i]))
  }
  
  list_plots[[i]] <- list( Ward = list_plots_Ward,
                           Medoid = list_plots_Medoid
  )
}

names(list_plots) <- sample_aggLevel

list_plots[[1]]


###### -------- Summary 
df_outputSS_ScaledABS_a0=readRDS('df_outputSS_ScaledABS_a0')
df_outputSS_ScaledABS_a0.1=readRDS('df_outputSS_ScaledABS_a0.1')
df_outputSS_ScaledABS_a0.25=readRDS('df_outputSS_ScaledABS_a0.25')

SS_ABS_SCALED <- bind_rows(
  df_outputSS_ScaledABS_a0,
  df_outputSS_ScaledABS_a0.1,
  df_outputSS_ScaledABS_a0.25,
)  %>% mutate(NumberOfClusters = as.integer(gsub('Cluster','',clusters)),
              Dimension = ifelse(dimension=='df_LatDephtDistance','GeoSpatial','Abiotics'),
              agglevel=factor(agglevel,levels=vec_aggregation),
              AlphaGeo = factor(AlphaGeo)) %>%
  filter(Dimension=='GeoSpatial',agglevel!='SG1',
         atleast!='AtLeastIn3') %>% 
  ggplot(aes(x=NumberOfClusters,y=log(TotalSum),
             color=method,linetype=AlphaGeo))+
  geom_line(linewidth=1.2,alpha=0.9)+
  facet_grid(atleast~agglevel)+
  xlab('k')+ylab('log(SS)')+
  theme_minimal(base_size = 24)+
  theme(legend.position = 'bottom')


ggsave(filename = 'SS_scaled_abs.png',
       plot = SS_ABS_SCALED,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 20,height = 10,dpi = 300)

SS_ABS_SCALED_at1<- bind_rows(
  df_outputSS_ScaledABS_a0,
  df_outputSS_ScaledABS_a0.1,
  df_outputSS_ScaledABS_a0.25,
)  %>% mutate(NumberOfClusters = as.integer(gsub('Cluster','',clusters)),
              Dimension = ifelse(dimension=='df_LatDephtDistance','GeoSpatial','Abiotics'),
              agglevel=factor(agglevel,levels=vec_aggregation),
              AlphaGeo = factor(AlphaGeo)) %>%
  filter(Dimension=='GeoSpatial',
         atleast=='AtLeastIn1') %>% 
  ggplot(aes(x=NumberOfClusters,y=log(TotalSum),
             color=method,linetype=AlphaGeo))+
  geom_line(linewidth=1.2,alpha=0.9)+
  facet_wrap(~agglevel,nrow=2)+
  xlab('k')+ylab('log(SS)')+
  theme_minimal(base_size = 24)+
  theme(legend.position = 'bottom')


ggsave(filename = 'SS_scaled_abs_at1.png',
       plot = SS_ABS_SCALED_at1,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 20,height = 10,dpi = 300)

########## ----------------------- Creating the best
selectedData <-
  aggregating_compositions(
    dFrame = dat_tax,
    fillZeros = 'Add',
    RemoveMoment = 'Before',
    aggregating_level = vec_aggregation[9],
    PresentAtLeast = 2
  ) 


clusters_mixdist <- 
  MixDist_Clust(metadat_aux = metadat,
                data_tax_aux = selectedData,
                alphaGeo = 0.1,
                depthRank = 'N',
                ABS_Latitude=T,
                scalingLatDepth =T,
                k_clust = 15)

clusters_mixdist
########## ------------------------ GGLat Depth
df_comp <- aggregating_compositions(
  dFrame = dat_tax,
  fillZeros = 'Nothing',
  aggregating_level = vec_aggregation[1]
) %>% select(Samples) %>%
  left_join(metadat %>%
              select(Samples,Latitude,Depth,Pressure_decibars,
                     Salinity_psu,Temperature_degrees_Celsius)) %>% 
  mutate(ClustWardMix = factor(clusters_mixdist$WardClust))

gglatselected <- ggLatDepth(df_comp,
           clusterVar = 'ClustWardMix',
           baseSize = 24,
           labelSize = 24)

ggsave(filename = 'gglatselected.png',
       plot = gglatselected,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 20,height = 10,dpi = 300)

###### --- Changing the label size
ggLatDepth_temp<-function(
    dataset,
    clusterVar,
    title ='',
    IncludeMedoid=F,
    labelSize=20,
    CHull=F){
  output <- list()
  dataset$Cluster = dataset %>% dplyr::select(one_of(clusterVar)) %>% pull()
  
  output$temp <- 
    ggplot(dataset,
           aes(x=Latitude,y=Depth,color=Temperature_degrees_Celsius,label=Cluster))+
    geom_vline(xintercept = c(-60,-45,-5,5,35),col='gray75')+
    #geom_hline(yintercept =0,col='gray75')+
    geom_label(alpha=0.5,size = labelSize/.pt)+
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
    geom_label(alpha=0.5,size = labelSize/.pt)+
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
    geom_label(alpha=0.7,size = labelSize/.pt)+
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
    geom_label(alpha=0.5,size = labelSize/.pt)+
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  p2 = ggplot(dataset,
              aes(x=Temperature_degrees_Celsius,y=Salinity_psu,
                  color=Cluster,label=Cluster))+
    geom_label(alpha=0.5,size = labelSize/.pt)+
    theme_minimal()+ 
    ggtitle(title)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank())
  
  p3 = ggplot(dataset,
              aes(x=Salinity_psu,y=Pressure_decibars,
                  color=Cluster,label=Cluster))+
    geom_label(alpha=0.5,size = labelSize/.pt)+
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
########## ------------------------ GGLat Depth Temp
gglatselected_abiotic <- ggLatDepth_temp(df_comp,
                                         clusterVar = 'ClustWardMix')



p1_ = gglatselected_abiotic$Temp_Press+
  xlab('Temperature (Celcius)')+ylab('Pressure (decibars)')+
  theme_minimal(base_size = 24) +
  theme(legend.position = 'none',
        panel.grid.major = element_blank())

p2_ = gglatselected_abiotic$Temp_Salinity+
  xlab('Temperature (Celcius)')+ylab('Salinity (psu)')+
  theme_minimal(base_size = 24)+  
  theme(legend.position = 'none',
        panel.grid.major = element_blank())

p3_ = gglatselected_abiotic$Salinity_Press+
  xlab('Pressure (decibars)')+xlab('Salinity (psu)')+
  theme_minimal(base_size = 24)+  
  theme(legend.position = 'none',
        panel.grid.major = element_blank())

ggsave(filename = 'p1_abio.png',
       plot = p1_,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 10,height = 10,dpi = 300)

ggsave(filename = 'p2_abio.png',
       plot = p2_,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 10,height = 10,dpi = 300)

ggsave(filename = 'p3_abio.png',
       plot = p3_,device = 'png',
       path = '/Users/rafaelcatoia/Desktop/repos/Capstone/',
       width = 10,height = 10,dpi = 300)
########## ------------------------ MDS


