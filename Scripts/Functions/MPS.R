
# Constants:

BG_COLOR <- colors()[250]
OOS_COLOR <- colors()[144]
S_COLOR <- colors()[24]
S_POINT_COLOR <- colors()[142]
FORECAST_COLOR <- colors()[374]

#function:

MPS <- function(S, S_Name ="", OOS = 0, FOBS = 0, Model, Horizon_year = 1, Level = c(.8, .9),
                           Xreg = NULL, Xreg_Name = "",
                           Dummy = FALSE, 
                           Plot = TRUE, Save_Plot = TRUE,Plot_Folder="Plots",
                           Simulate = FALSE, N_Sim = 100000, Target = 7:9,
                           Quant = c(.5, .7, .75, .80, .85, .90, .95)) {
  
require(forecast)
require(ggplot2)
  
Xmin <- min(time(S)) + FOBS/frequency(S)
Xmax <- max(time(S)) - OOS/frequency(S)

Xmax_f <- Xmax + Horizon_year
H <- Horizon_year * frequency(S)

S_0 <- window(S, start = Xmin)
S_0.df <- data.frame(t = time(S_0),y = S_0)


S <- window(S, start = Xmin, end = Xmax)


if (Model == "ets") {
  fit <- ets(S)
  
  fit_fore <- forecast(fit, h = H, level = Level)
  
  if(Simulate == TRUE) {
      
      Somme <- vector()
      
      for(i in 1:N_Sim) {
        Somme[i] <- sum(simulate.ets(object = fit)[Target])
      }
      
      Simulation <- quantile(Somme, probs = Quant, na.rm = TRUE)
  }
  
  if(Plot == TRUE) { 
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_1 = fit_fore$lower[,1], f_up_1 = fit_fore$upper[,1],
                              f_lo_2 = fit_fore$lower[,2], f_up_2 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_2)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_2)
  
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
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_1, ymax = f_up_1), alpha = .1, fill = FORECAST_COLOR)+
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_1, ymax = f_up_2), alpha = .3, fill = FORECAST_COLOR)+
            geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_2, ymax = f_lo_1), alpha = .3, fill = FORECAST_COLOR)
    
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
            scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
            xlab(NULL)+
            ylab(NULL)+
            labs(title = paste(S_Name, fit$method, ifelse(OOS>0, "Validation", "Prévision"),sep=" - "))

            Title  <-  paste(S_Name, fit$method, ifelse(OOS>0, "Validation", "Prévision"),sep=" - ")

  }
}

if (Model == "nnetar") {
  
  fit <- nnetar(S)
  
  fit_fore <- forecast(fit, h = H, level = Level)
  
  if(Plot == TRUE) {
    
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
    labs(title = paste(S_Name, fit$method, ifelse(OOS>0, "Validation", "Prévision"),sep=" - "))
  
    Title  <-  paste(S_Name, fit$method, ifelse(OOS>0, "Validation", "Prévision"),sep=" - ")
  }
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
  
  fit_fore <- forecast(fit, h = H, level = Level)
  
  if(Simulate == TRUE) {
    Somme <- vector()
    
    for(i in 1:N_Sim) {
      Somme[i] <- sum(simulate.Arima(object = fit)[Target])
    }
    
    Simulation <- quantile(Somme, probs = Quant, na.rm = TRUE)
  }
  
  if(Plot == TRUE) {
    
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_1 = fit_fore$lower[,1], f_up_1 = fit_fore$upper[,1],
                              f_lo_2 = fit_fore$lower[,2], f_up_2 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_2)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_2)
  
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
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_1, ymax = f_up_1), alpha = .1, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_1, ymax = f_up_2), alpha = .3, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_2, ymax = f_lo_1), alpha = .3, fill = FORECAST_COLOR)
  
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
    scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
    xlab(NULL)+
    ylab(NULL)+
    labs(title = paste(S_Name, Arima_Model_t, ifelse(OOS>0, "Validation", "Prévision"),sep=" - "))
  
    Title <- paste(S_Name, Arima_Model_t, ifelse(OOS>0, "Validation", "Prévision"),sep=" - ")
  }
}

if (Model == "arimax") {
  
  S_ArimaX <- ts.intersect(S,Xreg)[,1] 
  My_ArimaX <- ts.intersect(S,Xreg)[,-1]
  
  if(!is.null(ncol(My_ArimaX))){
    
    
    My_ArimaX_fore <- forecast(auto.arima(My_ArimaX[,1]), h = H, level = Level)$mean
    
    for(i in 2:ncol(Xreg)) {
      
      Res <- forecast(auto.arima(My_ArimaX[,i]), h = H, level = Level)$mean
      My_ArimaX_fore <- cbind(My_ArimaX_fore, Res)
    }
    
  } else {
    
    My_ArimaX_fore <- forecast(auto.arima(My_ArimaX), h = H, level = Level)$mean
    
  }
  
  if(Dummy == TRUE) {
  
    S_Dummy <- ts(0, start = Xmin, end = Xmax_f,frequency = frequency(S_ArimaX))
    S_Dummy[time(S_ArimaX)==time(S_ArimaX)[S_ArimaX==max(S_ArimaX)]]=1
    My_S_Dummy <- ts.intersect(S_Dummy, S_ArimaX)[,1]
    My_S_Dummy_fore <- ts.intersect(S_Dummy, My_ArimaX_fore)[,1]
    
    fit <- auto.arima(S_ArimaX, xreg = cbind(My_ArimaX,My_S_Dummy))
    fit_fore <- forecast(fit,xreg = cbind(My_ArimaX_fore, My_S_Dummy_fore), h = H, level = Level)
    
    if(Simulate == TRUE) {
      Somme <- vector()
      
      for(i in 1:N_Sim) {
        Somme[i] <- sum(simulate.Arima(object = fit, xreg = cbind(My_ArimaX_fore, My_S_Dummy_fore))[Target])
      }
      
      Simulation <- quantile(Somme, probs = Quant, na.rm = TRUE)
    }
  } 
  
  else {
    
    fit <- auto.arima(S_ArimaX, xreg = My_ArimaX)
    fit_fore <- forecast(fit,xreg = My_ArimaX_fore, h = H, level = Level)
    
    if(Simulate == TRUE) {
      Somme <- vector()
      
      for(i in 1:N_Sim) {
        Somme[i] <- sum(simulate.Arima(object = fit, xreg = My_ArimaX_fore)[Target])
      }
      
      Simulation <- quantile(Somme, probs = Quant, na.rm = TRUE)
    }
  }
  
   
    
  Arima_Model_t <- paste("ARIMAX(",
                         fit$arma[1], "," ,
                         fit$arma[6], ",",
                         fit$arma[2], ")(",
                         fit$arma[3], ",",
                         fit$arma[7], ",",
                         fit$arma[4], ")",
                         fit$arma[5],
                         "  ",
                         Xreg_Name,
                         sep="")
  
  if(Plot == TRUE) {
    
  S.df.fit <- data.frame(t = c(time(fitted(fit)),time(fit_fore$mean)),
                         y = c(fitted(fit),fit_fore$mean))
  
  S.df.fit_fore <- data.frame(t = time(fit_fore$mean),f_mean = fit_fore$mean,
                              f_lo_1 = fit_fore$lower[,1], f_up_1 = fit_fore$upper[,1],
                              f_lo_2 = fit_fore$lower[,2], f_up_2 = fit_fore$upper[,2])
  
  Ymin <- min(S_0.df$y, S.df.fit_fore$f_lo_2, na.rm = TRUE)
  Ymax <- max(S_0.df$y, S.df.fit_fore$f_up_2, na.rm = TRUE)
  
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
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_1, ymax = f_up_1), alpha = .1, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_up_1, ymax = f_up_2), alpha = .3, fill = FORECAST_COLOR)+
    geom_ribbon(data = S.df.fit_fore,aes(y = f_mean, ymin = f_lo_2, ymax = f_lo_1), alpha = .3, fill = FORECAST_COLOR)
  
  p.fit <- p.fit +  scale_x_continuous(breaks = round(Xmin):ceiling(Xmax_f))+
    scale_y_continuous(breaks = round(seq(Ymin, Ymax, by = round(Ymax-Ymin)/12)))+
    
    xlab(NULL)+
    ylab(NULL)+
    labs(title = paste(S_Name, Arima_Model_t, ifelse(OOS>0, "Validation", "Prévision"),sep=" - "))
  
     Title <- paste(S_Name, Arima_Model_t, ifelse(OOS>0, "Validation", "Prévision"),sep=" - ")
  }
}

if(Plot & Save_Plot == TRUE) {
  if(FOBS>0) Title <- paste(Title,"FOBS")
  if(OOS>0) Title <- paste(Title,"OOS")
  ggsave(paste(Plot_Folder, "/__",Title,".pdf",sep=""))
}

if(Model== "nnetar") {
  if(Plot == TRUE) {
    return(list(Plot = p.fit, Model = fit))
  } else {
    return(list(Model = fit))
  }
  
  } else {
    if(Simulate == TRUE) {
      if(Plot == TRUE) {
        return(list(Plot = p.fit, Model = fit, Sim = Simulation))
      } else {
        return(list(Model = fit, Sim = Simulation))
      }
      } else { 
        if(Plot == TRUE) {
        return(list(Plot = p.fit, Model = fit))
        } else {
          return(list( Model = fit))
        }
    }
  }
}

  
#end MPS