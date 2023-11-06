###### --- functions for capstone project

##### --- @date_function ----
#Function for working with numerical/string data
#input = YYYYMMDD 
#output = YYYY-MM-DD
date_function <- function(x){
  #Function for formatting with numerical/string dates to date format
  #input = YYYYMMDD 
  #output = YYYY-MM-DD
  x<-as.character(x)
  Y <- str_sub(x,1,4)
  M <- str_sub(x,5,6)
  D <- str_sub(x,7,8)
  return(as.Date(paste(Y,M,D,sep = "/")))
}
##### -------


##### --- @kable_cust ----
#Function for enhance the visualization of tables/dataframes in rmarkdown
#df_aux = a data frame.
#ndec = number of decimal places to show in the table
#cap = caption of the table
#output = a html table
kable_cust <- function(df_aux,ndec=5,cap=''){
  return(df_aux %>% knitr::kable(digits = ndec,caption = cap) %>%
           kableExtra::kable_styling())
}


##### --- @kable_box ----
#Function for enhance the visualization of tables/dataframes in rmarkdown
#df_aux = a data frame.
#ndec = number of decimal places to show in the table
#cap = caption of the table
#output = a boxed html table, good for working with large datasets
kable_box <- function(df_aux,ndec=5,cap='',h=400,w=100){
  return(
    df_aux %>%
      knitr::kable(digits = ndec,caption = cap) %>%
      kableExtra::kable_styling() %>%
      kableExtra::scroll_box(
        height = paste(h,'px',sep = ''),
        width = paste(w,'%',sep = '')))
}


##### --- @is.0 ----
#counts the number of zeros in a vector
#x = numerical vector
#returns the number of zeros
is.0 <- function(x){
  return(sum(x==0))
}

###### --- @drawmat_precise_ggplot ----


drawmat_precise_ggplot <- function(distmat, colours = c("blue", "red"),
                                   limits = NULL,
                                   xlab = "", ylab = "", title = "",
                                   hcuts = NULL, vcuts = NULL){
  distmat = as.matrix(distmat)
  longData <- reshape2::melt(distmat[nrow(distmat):1,])##, varnames=c('Var1', 'Var2'))
  ## longData<-longData[longData$value!=0,]
  
  p =
    ggplot() +
    geom_raster(aes(x = Var2, y = Var1, fill = value), data = longData)  +
    ## scale_fill_gradient(low="grey90", high="red") +
    ## scale_fill_gradient(low="grey90", high="red") +
    scale_fill_gradientn(colours = colours, guide="colorbar", limits=limits) +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal() + theme(axis.text.x = element_text(size = rel(1), angle = 90, vjust = 0.3),
                            axis.text.y = element_text(size = rel(1)),
                            plot.title = element_text(size = rel(1.5)))
  
  ## Add cuts, if necessary.
  if(!is.null(vcuts)){
    p = p + geom_vline(xintercept = vcuts + 0.5, lwd = 1.5, col = rgb(0,0,0,0.5))
  }
  if(!is.null(hcuts)){
    p = p + geom_hline(yintercept = hcuts + 0.5, lwd = 1.5, col = rgb(0,0,0,0.5))
  }
  return(p)
}

#### Not using the ones below here --------

### Fill the zeros with truncated lognormal distribution

fill_zeros <- function(df_aux,meanLogNorm=1e-08,max_Trunc=1e-07){
  if (any(df_aux == 0)){
    for(i in 1:nrow(df_aux)){
      for(j in 1:ncol(df_aux)){
        if(df_aux[i,j]==0){
          df_aux[i,j] <- EnvStats::rlnormTruncAlt(n = 1,mean = meanLogNorm,min = 0,max = max_Trunc)
        }
      }
    }
  }
  return(df_aux)
}


to_composition_100pct <- function(
    x,meanLogNorm=1e-08,max_Trunc=1e-07,FillZeros=T){
  if(FillZeros==T){
    #for(i in 1:length(x)){
    #  x[x[i]==0]<-EnvStats::rlnormTruncAlt(n = 1,mean = meanLogNorm,min = 0,max = max_Trunc)
    #}
    x<-x+1e-08
  }
  return(x/sum(x))
}

## TODo coment this 
transpose_df_composition <- function(df,col_names=''){
  t_df <- data.table::transpose(df[,-1])
  
  if(col_names==''){
    colnames(t_df) <- df[,1] %>% dplyr::pull()
  }else{
    colnames(t_df) <- col_names
  }
  t_df = dplyr::bind_cols(Samples = colnames(df[,-1]),t_df)
  t_df <- t_df %>% tibble::as_tibble(.)
  return(t_df)
}


######### 
# EDA for Counting Zeros
ZeroPercentage <- function(dataIn, TaxName,Xticks=TRUE){
  out_list <- list()
  
  dataIn <- dataIn %>% select(-Samples) %>%
    pivot_longer(cols = everything(),
                 names_to = 'Groupping',
                 values_to = 'Values') %>%
    group_by(Groupping) %>% 
    summarise(NumberOfZeros=is.0(Values),
              PercentageOfZeros = sum(is.0(Values))/n()) %>% 
    arrange(-PercentageOfZeros)
  
  out_list$table <- dataIn
  
  p1 <- 
    dataIn %>% mutate(Groupping = factor(Groupping,levels = dataIn$Groupping)) %>% 
    ggplot(aes(x=Groupping,y=PercentageOfZeros))+
    geom_bar(stat='identity')+
    xlab(TaxName)
  
  if(!Xticks){
    p1 <- p1 + theme_minimal()+
      theme(axis.ticks.x = element_blank(),
                            axis.text.x = element_blank())
  }
  
  out_list$plot <- p1
  return(out_list)
}

formatDecimal <- function(x, k) format(round(x, k), trim=T, nsmall=k)
