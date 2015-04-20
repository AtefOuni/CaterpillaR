
Lag_X <- function(X,Lags = 12,Var_Name ="X") {
  if(Lags == 0) {
    return(X)
  }
  X_l <- list()
  
  for(k in 1:(Lags+1)) {
    
    X_l[[k]] <- lag(X,k=1-k)
    
  }
  
  X_res <- cbind(X_l[[1]], X_l[[2]])
  
  if(Lags>1) {
    
      for(k in 3:(Lags+1))
      {
        X_res <- cbind(X_res, X_l[[k]])
      }
      
  }
  
  dimnames(X_res)[[2]] <- paste(Var_Name,0:Lags,sep="_")
  
  return(X_res)
  
}



