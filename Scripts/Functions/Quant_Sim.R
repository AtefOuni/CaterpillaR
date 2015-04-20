library(forecast)




Quant_Sim <- function(Model, N_Sim = 1000, Target = 7:9, Quant = c(.05, .1, .25, .5, .75, .90, .95)) {
    
  Somme <- vector()
  
  for(i in 1:N_Sim) {
    Somme[i] <- sum(simulate(object = Model)[Target])
  }
  
  return(quantile(Somme, probs = Quant))
}

# 
# j <- 1
# 
# 
# BC <- Cat[[paste("BHL", j, sep = "_")]][,"BHL_CAT"]
# fit <- ets(BC)
# fit <- auto.arima(BC)
# 
# S <- Cat[[paste("BHL", j, sep = "_")]][,"BHL_CAT"]
# X_0 <- X[[paste("DEP_EQUIP_CAP", j, sep = "_")]]
# 
# X_0.ar <- auto.arima(X_0)
# 
# X_0_f <- forecast(X_0.ar,h=frequency(X_0)*5)$mean
# # X_0 <- ts(c(X_0,X_0_f),start = min(time(X_0)), end = max(time(X_0_f)), frequency = frequency(X_0))
# 
# S <- ts.intersect(S,X_0)[,1]
# X_reg <- ts.intersect(S,X_0)[,-1]
# 
# S.ar <- auto.arima(S,xreg = X_reg)
# 
# Quant_Sim(Model = S.ar)
