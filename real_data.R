library(tidyverse)
source("simulation_helper.R")

real_data_res <- function(){
  ## 0 Read data
  rhc <- read.csv("rhc.xls",stringsAsFactors = TRUE)
  rhc$treatment = as.numeric(rhc$treatment)
  rhc$dth30 = as.numeric(rhc$dth30)
  rhc <- rhc %>% dplyr::select(-ID)
  rhc <- rhc %>% rename("D"=treatment, "Y"=dth30)
  
  ## model
  rss_xgb2 <- sample_splitting(rhc,ml_method="xgboost")
  rss_xgb5 <- sample_splitting(rhc,k=5,ml_method="xgboost")
  rss_lasso2 <- sample_splitting(rhc,ml_method="lasso")
  rss_lasso5 <- sample_splitting(rhc,k=5,ml_method="lasso")
  rfs_xgb <- full_sample(rhc,ml_method="xgboost")
  rfs_lasso <- full_sample(rhc,ml_method="lasso")
  
  df = data.frame(matrix(c(rss_xgb2,rss_xgb5,rfs_xgb,rss_lasso2,rss_lasso5,rfs_lasso),nrow=2))
  colnames(df) = c("K=2","K=5","full sample")
  df = cbind(method=c("XGBosst","LASSO"),df)
  return(df)
}



