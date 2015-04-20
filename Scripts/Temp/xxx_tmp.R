data(AirPassengers)

Ap.hw <- HoltWinters(AirPassengers)
plot(forecast(Ap.hw,h=2*12))

Ap.ets <- ets(AirPassengers)
plot(forecast(Ap.ets,h=2*12))

Ap.arima <- auto.arima(AirPassengers)
plot(forecast(Ap.arima,h=2*12))


for(j in names(P))
{
  print(j,dimnames(P[[j]][[2]])) 
  
# rien rien  
}

library(XLConnect)

if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")


for(i in names(Cat)) {
  print(i)
  print(dimnames(Cat[[i]])[[2]])
}


for(i in Classe_Prod) {
  print(i[2])
}

library(forecast)

Xv <- Cat[["BHL_2"]][,"BHL"]
X.fit <- ets(Xv)
X.fit.m <- ets(Xv, model = "MNN")
X.fit.hw.n <- HoltWinters(Xv, beta = FALSE, gamma = FALSE)
X.fit.hw.a <- HoltWinters(Xv, seasonal = "additive")
X.fit.hw.m <- HoltWinters(Xv, seasonal = "multiplicative")
X.vars <-ts.intersect(Xv,cbind(a=X[["DEP_ETAT_1"]],
                               b=X[["DEP_ETAT_CAP_1"]],
                               c=X[["DEP_ETAT_BS_1"]])) 
dimnames(X.vars)[[2]] <- c("Xv","a", "b", "c")
X.fit.ar <- auto.arima(X.vars[,1], trace = TRUE,xreg = X.vars[,-1])
X.fit.ar$coef
ts.plot(X, X.fit$fitted, X.fit.m$fitted, forecast(X.fit.m)$mean,X.fit.hw$fitted,
        lty = 1:5,
        col = 1:5 )

ts.plot(Xv,X.fit.hw.a$fitted[,"xhat"],forecast(X.fit.hw.a)$mean,
        X.fit.hw.m$fitted[,"xhat"],forecast(X.fit.hw.m)$mean,
        lty = 1:5,
        col = 1:5 )

legend("topright", inset = .01,
       c("X", "X.fit$fitted", "X.fit.m$fitted","forecast(X.fit.m)$mean"),
       fill = 1:4)
Uar <- arima(USAccDeaths, order = c(0,1,2), seasonal = list(order=c(0,0,0)))
str(Uar)

# !!! p, q, P, Q, D, d, m   

tmp <- ets(Cat[["BHL_2"]][,"BHL"])
tmp

tmp <- read.csv2(file = "Data//expsmooth_data/visitors.csv", header = TRUE,sep = ",")
tmp <- ts(tmp$visitors,start = c(1985,5),frequency = 12)
tmp <- ets(tmp,model = "MAM")

library(ggplot2)
library(gridExtra)
library(forecast)

Xv <- Cat[["BHL_2"]][,"BHL"]

X.fit <- ets(Xv)
X.fit <- auto.arima(Xv)
Xv.f <- forecast(X.fit)

Xv.df <- data.frame(t = time(Xv),y = Xv)
Xv.df.f <- data.frame(t = time(Xv.f$mean), f_mean = Xv.f$mean,
                      f_lo_80 = Xv.f$lower[,1], f_up_80 = Xv.f$upper[,1],
                      f_lo_95 = Xv.f$lower[,2], f_up_95 = Xv.f$upper[,2])

Xv.pr <- window(Xv,start = 2011)

X.fit.pr <- ets(Xv.pr)
X.fit.pr <- auto.arima(Xv.pr)
Xv.f.pr <- forecast(X.fit.pr)
Xv.df.pr <- data.frame(t = time(Xv.pr),y = Xv.pr)
Xv.df.f.pr <- data.frame(t = time(Xv.f.pr$mean), f_mean = Xv.f.pr$mean,
                      f_lo_80 = Xv.f.pr$lower[,1], f_up_80 = Xv.f.pr$upper[,1],
                      f_lo_95 = Xv.f.pr$lower[,2], f_up_95 = Xv.f.pr$upper[,2])


plot1 <- ggplot(data=Xv.df,aes(x= t , y= Xv))  +
  geom_vline(xintercept = 2011, colour = "white", size = 4) +
  geom_line(colour = "blue") +
  geom_line(data = Xv.df.f.pr, aes(x = t, y = f_mean), colour = "red")+
  geom_ribbon(data = Xv.df.f.pr,aes(y = f_mean, ymin = f_lo_80, ymax = f_up_80), alpha = .2, fill = "red")+
  geom_ribbon(data = Xv.df.f.pr,aes(y = f_mean, ymin = f_lo_95, ymax = f_up_95), alpha = .15, fill = "red")+
  geom_vline(xintercept = 2006:2014, colour = "black", alpha = .3) +
  #   geom_vline(xintercept = 2015, colour = "green") +
  scale_x_continuous(breaks = 2006:2017)+
  scale_y_continuous(breaks = seq(0,100,10))+
  xlab(NULL)+
  ylab(NULL)
  
  

plot2 <- ggplot(data=Xv.df,aes(x= t , y= Xv))  +
  geom_vline(xintercept = 2011, colour = "white", size = 4) +
  geom_rect(xmin=2010, xmax = 2016, ymin = 0, ymax = 100, fill = "green" ,alpha = .005)+
  geom_line(colour = "blue") +
  geom_line(data = Xv.df.f, aes(x = t, y = f_mean), colour = "green")+
  geom_ribbon(data = Xv.df.f,aes(y = f_mean, ymin = f_lo_80, ymax = f_up_80), alpha = .2, fill = "green")+
  geom_ribbon(data = Xv.df.f,aes(y = f_mean, ymin = f_lo_95, ymax = f_up_95), alpha = .15, fill = "green")+
  geom_vline(xintercept = 2006:2014, colour = "black", alpha = .3) +
#   geom_vline(xintercept = 2015, colour = "green") +
  scale_x_continuous(breaks = 2006:2017)+
  scale_y_continuous(breaks = seq(0,100,10))+
  xlab(NULL)+
  ylab(NULL)+ylim(0,100)


grid.arrange(plot1, plot2, nrow=2, ncol=1)
  
  # à voir : http://www.r-bloggers.com/plotting-forecast-objects-in-ggplot-part-2-visualize-observations-fits-and-forecasts/

#   out of sample
# library(forecast)
# fit <- ets(trainingdata)
# fit2 <- ets(testdata, model=fit)
# onestep <- fitted(fit2)

Xv.os <- window(x = Xv, start = time(Xv)[length(Xv)+1-6])
Xv.s <- window(x = Xv, end = time(Xv)[length(Xv)+1-6])

fit <- ets(Xv.s)
fit2 <- ets(Xv.os, model = fit)
accuracy(fit2)

ts.plot(Xv,fit2$fitted,forecast(fit)$mean)

library(ggplot2)
ggplot(data = iris, aes(x = Sepal.Length))+ geom_bar()+annotate("text", label= "azazaz", x = 2, y = 10 , colour = "red")



# j <- 1
# 
# L <- 12
# 
# D <- X[[paste("DEP_EQUIP_CAP", j, sep = "_")]]
# B <- Cat[[paste("BHL", j, sep = "_")]][,"BHL"]
# BC <- Cat[[paste("BHL", j, sep = "_")]][,"BHL_CAT"]
# 
# P <- BC / B
# 
# D_L <- Lag_X(X = D, Lags = L, Var_Name = "D")
# B_L <- Lag_X(X = B, Lags = L, Var_Name = "B")
# BC_L <- Lag_X(X = BC, Lags = L, Var_Name = "BC")
# P_L <- Lag_X(X = P, Lags = L, Var_Name = "P")
# 
# View(cor(B_L,D_L,"na.or.complete"))
# View(cor(BC_L,D_L,"na.or.complete"))
# View(cor(P_L,D_L,"na.or.complete"))
# 
# 
# ggpairs(cbind(S_L,X_L))
# 
# 
# plot(as.numeric(S),as.numeric(X_0))
# abline(lm(as.numeric(S)~as.numeric(X_0)))
# 
# X_0.ar <- auto.arima(X_0)
# 
# X_0_f <- forecast(X_0,h=frequency(X_0)*5)$mean
# # X_0 <- ts(c(X_0,X_0_f),start = min(time(X_0)), end = max(time(X_0_f)), frequency = frequency(X_0))
# 


# X_reg <- X_reg_1



# S <- ts.intersect(S,X_reg)[,1]
# X_reg <- ts.intersect(S,X_reg)[,-1]

# S.ar <- auto.arima(S,xreg = X_reg)
# S.ar 
# ts.plot(S,fitted(S.ar),col=1:2)
# 
# H <- 10
# 
# 
# S_ArimaX <- ts.intersect(S,X_reg)[,1] 
# My_ArimaX <- ts.intersect(S,X_reg)[,-1]
# 
# if(!is.null(ncol(My_ArimaX))){
#   
#   Res.fore <- list()
#   
#   My_ArimaX_fore <- forecast(auto.arima(My_ArimaX[,1]), h = H)$mean
#   
#   for(i in 2:ncol(X_reg)) {
#     
#     Res.fore[[i]] <- forecast(auto.arima(My_ArimaX[,i]), h = H)$mean
#     My_ArimaX_fore <- cbind(My_ArimaX_fore, Res.fore[[i]])
#   }
#   
# } else {
#   
#   My_ArimaX_fore <- forecast(auto.arima(My_ArimaX), h = H)$mean
#   
# }
# 
# My_ArimaX_fore

# fit.Xreg <- auto.arima(My_ArimaX)

# My_ArimaX_fore <- forecast(fit.Xreg, h = H)$mean
# 
# Dummy <- TRUE
# 
# Xmin <- 2006
# Xmax_f <- 2020
# 
# if(Dummy == TRUE) {
#   
#   S_Dummy <- ts(0, start = Xmin, end = Xmax_f,frequency = frequency(S_ArimaX))
#   S_Dummy[time(S_ArimaX)==time(S_ArimaX)[S_ArimaX==max(S_ArimaX)]]=1
#   My_S_Dummy <- ts.intersect(S_Dummy, S_ArimaX)[,1]
#   My_S_Dummy_fore <- ts.intersect(S_Dummy, My_ArimaX_fore)[,1]
#   
#   fit <- auto.arima(S_ArimaX, xreg = cbind(My_ArimaX,My_S_Dummy))
#   fit_fore <- forecast(fit,xreg = cbind(My_ArimaX_fore, My_S_Dummy_fore), h = H)
# } 
# 
# else {
#   
#   fit <- auto.arima(S_ArimaX, xreg = My_ArimaX)
#   fit_fore <- forecast(fit,xreg = My_ArimaX_fore, h = H)
#   
# }

j <- 1

X_reg_1 <- X[[paste("DEP_EQUIP_CAP_",j,sep="")]]
X_reg_2 <- Cat[[paste("BHL_",j,sep="")]][,"BHL"]
X_reg <- cbind(X_reg_1,X_reg_2)

S <- Cat[[paste("BHL_",j,sep="")]][,"BHL_CAT"]
S2 <- Cat[[paste("BHL_",j,sep="")]][,"BHL"]

S <- S/S2*100

Xreg <- Lag_X(X_reg_1,Lags = 12, Var_Name = "D")[,c(4,5,7)]

# M <- Model_and_Plot(S = S, Model = "arima", S_Name = "BHL_")
M <- Model_and_Plot(S = S, Model = "arimax", Xreg = Xreg, S_Name = "BHL_CAT_3", OOS = 4 )
M <- Model_and_Plot(S = S2, Model = "nnetar" ,S_Name = "BHL_CAT_3", OOS = 4 )
M

ggsave(plot = M$Plot, filename = "Plot.jpg")
