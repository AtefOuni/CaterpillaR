#requirements

require(forecast)
require(ggplot2)


# Constants:

BG_COLOR <- colors()[250]
OOS_COLOR <- colors()[144]
S_COLOR <- colors()[24]
S_POINT_COLOR <- colors()[142]
FORECAST_COLOR <- colors()[374]

#function:

Model_and_Plot <- function(S, S_Name, OOS = 0, FOBS = 0, Model, Horizon_year = 1, Xreg = NULL, Dummy = FALSE) {

Xmin <- min(time(S)) + FOBS/frequency(S)
Xmax <- max(time(S)) - OOS/frequency(S)

Xmax_f <- Xmax + Horizon_year
H <- Horizon_year * frequency(S)

S_0 <- window(S, start = Xmin)
S_0.df <- data.frame(t = time(S_0),y = S_0)

S <- window(S, start = Xmin, end = Xmax)
  
if (Model == "ets") {
  fit <- ets(S)
  
  fit_fore <- forecast(fit, h = H)
  
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_80 = fit_fore$lower[,1], f_up_80 = fit_fore$upper[,1],
                              f_lo_95 = fit_fore$lower[,2], f_up_95 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_95)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_95)
  
  p.fit <- ggplot(data= S_0.df,aes(x= t , y=  y))+
    
            theme_bw() + 
            theme(panel.background = element_rect(fill = BG_COLOR )) 
  if(OOS > 0) {
  
  p.fit <- p.fit + geom_vline(xintercept = c(Xmax+1/frequency(S), max(time(S_0))), colour = OOS_COLOR )
  } 

  p.fit <- p.fit +  geom_line(colour = S_COLOR) +
            geom_point(colour = S_POINT_COLOR, size = 1) +
    
            geom_line(data = S.df.fit, aes(x = t, y = y), colour = FORECAST_COLOR ) +
            geom_point(data = S.df.fit,aes(x = t, y = y), colour = S_POINT_COLOR, size = 1)
    
  p.fit <- p.fit +  
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_80, ymax = f_up_80), alpha = .1, fill = FORECAST_COLOR)+
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_80, ymax = f_up_95), alpha = .3, fill = FORECAST_COLOR)+
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_95, ymax = f_lo_80), alpha = .3, fill = FORECAST_COLOR)
    
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
            scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
            xlab(NULL)+
            ylab(NULL)+
            labs(title = paste(S_Name, fit$method))

}

if (Model == "nnetar") {
  
  fit <- nnetar(S)
  
  fit_fore <- forecast(fit, h = H)
  
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  
  Ymin <- min(S_0.df$y, S.df.fit$y, na.rm = TRUE)
  Ymax <- max(S_0.df$y, S.df.fit$y, na.rm = TRUE)
  
  p.fit <- ggplot(data= S_0.df,aes(x= t , y=  y))+
    
    theme_bw() + 
    theme(panel.background = element_rect(fill = BG_COLOR )) 
  if(OOS > 0) {
    
    p.fit <- p.fit + geom_vline(xintercept = c(Xmax+1/frequency(S), max(time(S_0))), colour = OOS_COLOR )
  } 
  
  p.fit <- p.fit +  geom_line(colour = S_COLOR) +
    geom_point(colour = S_POINT_COLOR, size = 1) +
    
    geom_line(data = S.df.fit, aes(x = t, y = y), colour = FORECAST_COLOR ) +
    geom_point(data = S.df.fit,aes(x = t, y = y), colour = S_POINT_COLOR, size = 1)
  
  
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
    scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
    xlab(NULL)+
    ylab(NULL)+
    labs(title = paste(S_Name, fit$method))
  
}

if (Model == "arima") {
  
  fit <- auto.arima(S)
  
  Arima_Model_t <- paste("ARIMA(",
                         fit$arma[1], "," ,
                         fit$arma[6], ",",
                         fit$arma[2], ")(",
                         fit$arma[3], ",",
                         fit$arma[7], ",",
                         fit$arma[4], ")",
                         fit$arma[5],
                         sep="")
  
  fit_fore <- forecast(fit, h = H)
  
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_80 = fit_fore$lower[,1], f_up_80 = fit_fore$upper[,1],
                              f_lo_95 = fit_fore$lower[,2], f_up_95 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_95)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_95)
  
  p.fit <- ggplot(data= S_0.df,aes(x= t , y=  y))+
    
    theme_bw() + 
    theme(panel.background = element_rect(fill = BG_COLOR )) 
  if(OOS > 0) {
    
    p.fit <- p.fit + geom_vline(xintercept = c(Xmax+1/frequency(S), max(time(S_0))), colour = OOS_COLOR )
  } 
  
  p.fit <- p.fit +  geom_line(colour = S_COLOR) +
    geom_point(colour = S_POINT_COLOR, size = 1) +
    
    geom_line(data = S.df.fit, aes(x = t, y = y), colour = FORECAST_COLOR ) +
    geom_point(data = S.df.fit,aes(x = t, y = y), colour = S_POINT_COLOR, size = 1)
  
  p.fit <- p.fit +  
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_80, ymax = f_up_80), alpha = .1, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_80, ymax = f_up_95), alpha = .3, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_95, ymax = f_lo_80), alpha = .3, fill = FORECAST_COLOR)
  
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
    scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
    xlab(NULL)+
    ylab(NULL)+
    labs(title = paste(S_Name, Arima_Model_t))
  
}

if (Model == "arimax") {
  
  S_ArimaX <- ts.intersect(S,Xreg)[,1] 
  My_ArimaX <- ts.intersect(S,Xreg)[,-1]

  fit.Xreg <- auto.arima(My_ArimaX)
  My_ArimaX_fore <- forecast(fit.Xreg, h = H)$mean
  
  if(Dummy == TRUE) {
  
    S_Dummy <- ts(0, start = Xmin, end = Xmax_f,frequency = frequency(S_ArimaX))
    S_Dummy[time(S_ArimaX)==time(S_ArimaX)[S_ArimaX==max(S_ArimaX)]]=1
    My_S_Dummy <- ts.intersect(S_Dummy, S_ArimaX)[,1]
    My_S_Dummy_fore <- ts.intersect(S_Dummy, My_ArimaX_fore)[,1]
    
    fit <- auto.arima(S_ArimaX, xreg = cbind(My_ArimaX,My_S_Dummy))
    fit_fore <- forecast(fit,xreg = cbind(My_ArimaX_fore, My_S_Dummy_fore), h = H)
  } 
  
  else {
    
    fit <- auto.arima(S_ArimaX, xreg = My_ArimaX)
    fit_fore <- forecast(fit,xreg = My_ArimaX_fore, h = H)
    
  }
  
    
    
  Arima_Model_t <- paste("ARIMA(",
                         fit$arma[1], "," ,
                         fit$arma[6], ",",
                         fit$arma[2], ")(",
                         fit$arma[3], ",",
                         fit$arma[7], ",",
                         fit$arma[4], ")",
                         fit$arma[5],
                         sep="")
  
  
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_80 = fit_fore$lower[,1], f_up_80 = fit_fore$upper[,1],
                              f_lo_95 = fit_fore$lower[,2], f_up_95 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_95)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_95)
  
  p.fit <- ggplot(data= S_0.df,aes(x= t , y=  y))+
    
    theme_bw() + 
    theme(panel.background = element_rect(fill = BG_COLOR )) 
  if(OOS > 0) {
    
    p.fit <- p.fit + geom_vline(xintercept = c(Xmax+1/frequency(S), max(time(S_0))), colour = OOS_COLOR )
  } 
  
  p.fit <- p.fit +  geom_line(colour = S_COLOR) +
    geom_point(colour = S_POINT_COLOR, size = 1) +
    
    geom_line(data = S.df.fit, aes(x = t, y = y), colour = FORECAST_COLOR ) +
    geom_point(data = S.df.fit,aes(x = t, y = y), colour = S_POINT_COLOR, size = 1)
  
  p.fit <- p.fit +  
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_80, ymax = f_up_80), alpha = .1, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_80, ymax = f_up_95), alpha = .3, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_95, ymax = f_lo_80), alpha = .3, fill = FORECAST_COLOR)
  
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
    scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
    xlab(NULL)+
    ylab(NULL)+
    labs(title = paste(S_Name, Arima_Model_t))

}

return(list(Plot = p.fit, Model = fit))

}

  
#end Model_and_Plot