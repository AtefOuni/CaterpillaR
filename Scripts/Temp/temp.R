j <- 1

S <- Cat[[paste("BHL", j, sep = "_")]][, "BHL"]
S <- Cat[[paste("BHL", j, sep = "_")]][, "BHL_CAT"]

X_ex <- X[[paste("DEP_EQUIP_CAP", j, sep = "_")]]

S_l <- list()

min_time <- min(time(S))
max_time <- max(time(S))

Lags <- 30

library(forecast)

for(k in 1:(Lags+1))
{
  S_l[[k]] <- lag(X_ex,k=k-1)
  
  if( min( time(S_l[[k]]) ) > min_time) {
    min_time <- min(time(S_l[[k]]))
  } 
  
  if( max(time(S_l[[k]])) < max_time) {
    max_time <- max(time(S_l[[k]]))
  }
}

for(k in 1:(Lags+1))
{
  S_l[[k]] <- window(S_l[[k]], start = min_time, end = max_time)
}

X_reg <- cbind(S_l[[1]], S_l[[2]])

for(k in 3:1:(Lags+1))
{
  X_reg <- cbind(X_reg, S_l[[k]])
}

dimnames(X_reg)[[2]] <- paste("X_lag",0:Lags,sep="_")

S <- window(S, start = min_time, end = max_time)

fit.ar <- auto.arima(x = S,xreg = X_reg)
fit.ar

ts.plot(fitted.Arima(fit.ar), S, col=c(2,1))

#------------------------------

fit <- ets(S)

Somme <- vector()
for(i in 1:10000)
{
  Sim <- simulate(fit.ar,nsim = 9)
  Somme[i] <- sum(Sim[7:9])
}

quantile(Somme)

#------------------------------
Lags <- 10

V1 <- list()
V2 <- list()

for(k in 1:(Lags+1)) {
  V1[[k]] <- lag(X_ex,k=k-1)
  V2[[k]] <- lag(S,k=k-1)
}


V1_l <- cbind(V1[[1]], V1[[2]])
V2_l <- cbind(V2[[1]], V2[[2]])

for(k in 3:1:(Lags+1))
{
  V1_l <- cbind(V1_l, V1[[k]])
  V2_l <- cbind(V2_l, V2[[k]])
}

dimnames(V1_l)[[2]] <- paste("V1_l",0:Lags,sep="_")
dimnames(V2_l)[[2]] <- paste("V2_l",0:Lags,sep="_")

cor(V1_l, V2_l, use = "na.or.complete")

