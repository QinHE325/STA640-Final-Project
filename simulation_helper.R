require(caret)
require(ggplot2)
require(ranger)
require(patchwork)
require(e1071)
require(glmnet)
require(xgboost)
require(mlbench)
require(caret)
library(randomForest)
library(tidyverse)

sample_splitting <- function(df,k = 2, ml_method = "xgboost", seed=123){
  set.seed(seed)
  ## partial linear model
  folds <- createFolds(df$Y, k=k)
  ate <- c()
  for (i in 1:k){
    # split into training and prediction sample
    train <- df[-unlist(folds[i]),]
    prediction <- df[unlist(folds[i]),] 
    
    # lasso
    if(ml_method == "lasso"){
      lambdas <- 10^seq(2, -3, by = -.1)
      x = model.matrix(Y ~.-D, train)[,-1]
      d = model.matrix(D ~.-Y, train)[,-1]
      y = train$Y
      D = train$D
      newx_y = model.matrix(Y ~.-D, prediction)[,-1]
      newx_d = model.matrix(D ~.-Y, prediction)[,-1]
      
      lasso_reg_y <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
      fit_y = glmnet(x, y, data = train, lambda= lasso_reg_y$lambda.min , family='gaussian', intercept = F, alpha=1)
      lasso_reg_z <- cv.glmnet(d, D, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
      fit_z = glmnet(d, D, data = train, lambda= lasso_reg_z$lambda.min, family='gaussian', intercept = F, alpha=1)
      
      # predict Y and Z
      py <- predict(fit_y, newx_y)
      pz <- predict(fit_z, newx_d)
      
      # get residuals
      u <- prediction$Y - py
      v <- prediction$D - pz
    }
    
    # xgboost
    if(ml_method == "xgboost"){
      
      dtrain = as.matrix(train)
      
      fit_y <- xgboost(data = as.matrix(train%>%dplyr::select(-D)), 
                       label = train$Y, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "reg:squarederror", verbose = FALSE)
      fit_z <- xgboost(data = as.matrix(train%>%dplyr::select(-Y)),
                       label = train$D, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = FALSE)
      
      # predict Y and Z
      py <- predict(fit_y, as.matrix(prediction%>%dplyr::select(-D)))
      pz <- predict(fit_z, as.matrix(prediction%>%dplyr::select(-Y)))
      
      # get residuals
      u <- prediction$Y - py
      v <- prediction$D - pz
    }
    
    # get the estimate of theta
    m1 <- lm(u~v)
    ate <- c(ate,m1$coefficients[2])
  }
  fit.ite <- mean(ate) # mean ate of K folds
  return(fit.ite) # ite equals ate
}


full_sample <- function(df, ml_method = "xgboost", seed=123){
  set.seed(seed)
  
  # lasso
  if(ml_method == "lasso"){
    lambdas <- 10^seq(2, -3, by = -.1)
    x = model.matrix(Y ~.-D, df)[,-1]
    d = model.matrix(D ~.-Y, df)[,-1]
    y = df$Y
    D = df$D
    newx_y = model.matrix(Y ~.-D, df)[,-1]
    newx_d = model.matrix(D ~.-Y, df)[,-1]
    
    lasso_reg_y <- cv.glmnet(x, y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
    fit_y = glmnet(x, y, data = train, lambda= lasso_reg_y$lambda.min , family='gaussian', intercept = F, alpha=1)
    lasso_reg_z <- cv.glmnet(d, D, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
    fit_z = glmnet(d, D, data = train, lambda= lasso_reg_z$lambda.min, family='gaussian', intercept = F, alpha=1)
    
    # predict Y and Z
    py <- predict(fit_y, newx_y)
    pz <- predict(fit_z, newx_d)
    
    # get residuals
    u <- df$Y - py
    v <- df$D - pz
  }
  
  # xgboost
  if(ml_method == "xgboost"){
    ddf = as.matrix(df)
    
    fit_y <- xgboost(data = as.matrix(df%>%dplyr::select(-D)), 
                     label = df$Y, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "reg:squarederror", verbose = FALSE)
    fit_z <- xgboost(data = as.matrix(df%>%dplyr::select(-Y)), 
                     label = df$D, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic", verbose = FALSE)
    
    # predict Y and Z
    py <- predict(fit_y, as.matrix(df%>%dplyr::select(-D)))
    pz <- predict(fit_z, as.matrix(df%>%dplyr::select(-Y)))
    
    # get residuals
    u <- df$Y - py
    v <- df$D - pz
    
  }
  
  
  
  # get the estimate of theta
  m1 <- lm(u~v)
  return(m1$coefficients[2]) # ite equals ate
}


