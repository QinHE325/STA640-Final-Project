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

make_table2 <- function(idx){
  
  r1 = c(stats_simdata(idx[1])[1],stats_simdata(idx[1])[3],
         stats_simdata(idx[1])[2],stats_simdata(idx[1])[4],
         stats_simdata(idx[2])[1],stats_simdata(idx[2])[3],
         stats_simdata(idx[2])[2],stats_simdata(idx[2])[4],
         stats_simdata(idx[3])[1],stats_simdata(idx[3])[3],
         stats_simdata(idx[3])[2],stats_simdata(idx[3])[4],
         stats_simdata(idx[4])[1],stats_simdata(idx[4])[3],
         stats_simdata(idx[4])[2],stats_simdata(idx[4])[4])
  r2 = c(stats_simdata(idx[5])[1],stats_simdata(idx[5])[3],
         stats_simdata(idx[5])[2],stats_simdata(idx[5])[4],
         stats_simdata(idx[6])[1],stats_simdata(idx[6])[3],
         stats_simdata(idx[6])[2],stats_simdata(idx[6])[4],
         stats_simdata(idx[7])[1],stats_simdata(idx[7])[3],
         stats_simdata(idx[7])[2],stats_simdata(idx[7])[4],
         stats_simdata(idx[8])[1],stats_simdata(idx[8])[3],
         stats_simdata(idx[8])[2],stats_simdata(idx[8])[4])
  df = rbind("XGBoost"=r1,"LASSO"=r2) %>% as.data.frame()
  colnames(df) <- c("fllm","fllse","llm","llse",
                    "flhm","flhse","lhm","lhse",
                    "fhlm","fhlse","hlm","hlse",
                    "fhhm","fhhse","hhm","hhse")
  df = round_df(df,3)
  df <- df %>%
    mutate(fll=paste0(fllm,"(",fllse,")")) %>%
    mutate(flh=paste0(flhm,"(",flhse,")")) %>%
    mutate(fhl=paste0(fhlm,"(",fhlse,")")) %>%
    mutate(fhh=paste0(fhhm,"(",fhhse,")")) %>%
    mutate(ll=paste0(llm,"(",llse,")")) %>%
    mutate(lh=paste0(lhm,"(",lhse,")")) %>%
    mutate(hl=paste0(hlm,"(",hlse,")")) %>%
    mutate(hh=paste0(hhm,"(",hhse,")")) %>%
    dplyr::select(fll,ll,flh,lh,fhl,hl,fhh,hh) %>%
    rownames_to_column(" ")
  return(df)
}