library(tidyverse)

stats_simdata = function(idx){
  data_path = paste0("data/",idx,'.csv')
  sim_data = read.csv(data_path)
  fs = sim_data$full_sample
  ss = sim_data$sample_split
  bfs = (fs - 1) 
  bss = (ss - 1) 
  return(c(mean(bfs,na.rm=T),mean(bss,na.rm=T),sd(bfs,na.rm = T),sd(bss,na.rm = T)))
}

round_df <- function(x, digits) {
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
make_table <- function(idx){
  r1 = c(stats_simdata(idx[1])[1],stats_simdata(idx[1])[3],
         stats_simdata(idx[1])[2],stats_simdata(idx[1])[4],
         stats_simdata(idx[2])[2],stats_simdata(idx[2])[4])
  r2 = c(stats_simdata(idx[3])[1],stats_simdata(idx[3])[3],
         stats_simdata(idx[3])[2],stats_simdata(idx[3])[4],
         stats_simdata(idx[4])[2],stats_simdata(idx[4])[4])
  df = rbind("XGBoost"=r1,"LASSO"=r2) %>% as.data.frame()
  colnames(df) <- c("f1m","f1se","tm","tse","fm","fse")
  df = round_df(df,3)
  df <- df %>% 
    mutate("Full"=paste0(f1m,"(",f1se,")")) %>% 
    mutate("Twofold"=paste0(tm,"(",tse,")")) %>%
    mutate("Fivefold"=paste0(fm,"(",fse,")")) %>%
    dplyr::select("Full","Twofold","Fivefold") %>% 
    rownames_to_column("Method")
  return(df)
}