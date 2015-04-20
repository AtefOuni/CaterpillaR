load("Data/Data.RData")

require(forecast)


Prod_Men <- Cat[["BHL_MEN"]][,"BHL_CAT"]
Prod_Tri <- Cat[["BHL_TRI"]][,"BHL_CAT"]
Prod_Sem <- Cat[["BHL_SEM"]][,"BHL_CAT"]
Prod_Ann <- Cat[["BHL_ANN"]][,"BHL_CAT"]

# par(mfrow = c(2, 2))

plot(Prod_Men)
plot(Prod_Tri)
plot(Prod_Sem)
plot(Prod_Ann)

acf(Prod)

Prod.ets <- ets(Prod)
Prod.arima <- auto.arima(Prod)
Prod.Hw <- HoltWinters(Prod)

plot(Prod.ets)
plot(Prod.arima)
plot(Prod.Hw)

forecast(Prod.Hw)
plot(forecast(Prod.ets,h = 12*4))
plot(forecast(Prod.ets))
plot(forecast(Prod.Hw,h = 12*4))
forecast(Prod.arima,9)
ts.plot(forecast(Prod.Hw,h=12*4)$mean,
        forecast(Prod.ets,h=12*4)$mean,
        forecast(Prod.arima,h=12*4)$mean,lty=1:3)



