require(tidyverse)

generate_data <- function(ncov=5,n=1000,theta0=1, 
                          p = 0.5, mean = 0, variance = 1, 
                          g0_dist = "normal", 
                          outcome_model = "linear",  
                          params = c(1,0.5), seed = 640) {
  set.seed(seed)
  # simulate data
  X = matrix(NA,ncol=ncov,nrow=n)
  for(i in 1:ncov){
    X[,i] <-rnorm(n)
  }
  
  X <- apply(X,2,scale)
  D <- rbinom(n,1,p)
  U <- rnorm(n, mean, variance)
  V <- rnorm(n, mean, variance)
  
  if(g0_dist == "normal"){
    g0 <- rnorm(ncov,params[1],params[2])
  }else if(g0_dist == "uniform"){
    g0 <- runif(ncov,0.5,1.5)
  }
  
  data <- model.matrix(~X+D-1)
  
  if(outcome_model == "linear"){ 
    Y <- as.vector(data%*%c(g0,theta0))+U
  }else{ # model non-linearity
    g1 <- rnorm(ncov,params[1],params[2])
    # add sine elements
    Y <- as.vector(data%*%c(g0,theta0)) + 
      as.vector(sin(data)%*%c(g1,theta0))
  }
  df <- data.frame(cbind(Y,data))
  return(df)
}

