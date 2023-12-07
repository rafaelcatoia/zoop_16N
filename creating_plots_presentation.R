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


