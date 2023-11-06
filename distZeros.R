dist_Zeros <- function(vet_aggreg,data_tax_){
  out <- list()
  for (i in 1:length(vet_aggreg)){
    
    dt_group <-aggregating_compositions(
      dFrame = data_tax_,
      fillZeros = 'Nothing',
      aggregating_level = vet_aggreg[i]
    )
    
    col_names <- colnames(dt_group[,-1])
    vet_Zeros <- apply(dt_group[,-1], 2, is.0)/nrow(dt_group[,-1])
    
    
    
    out[[i]] <- bind_cols(
      Variables = col_names,
      PctZeros = vet_Zeros) %>% 
      arrange(-PctZeros) %>% 
      mutate(
        Variables =
          factor(Variables,
                 levels=names(vet_Zeros[order(-vet_Zeros)]))) %>% 
      ggplot(aes(x=Variables,y=PctZeros))+
      geom_col()+
      theme_minimal()+
      ggtitle(vet_aggreg[i])+
      theme(axis.text.x = element_blank())
    
  }
    names(out)<-vet_aggreg
  return(out)
}
