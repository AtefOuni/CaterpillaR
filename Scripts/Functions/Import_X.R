
require(XLConnect)

Import_X <- function (Fichier_Donnes) {
  
  X_Raw <- readWorksheetFromFile(Fichier_Donnes, "X")
  
  X_Annuel <- c("DEP_ETAT_PREV", "DEP_ETAT_CAP_PREV", "DEP_ETAT_BS_PREV", "TX_CR", 
                "DEP_EQUIP_PREV", "DEP_EQUIP_CAP_PREV", "DEP_EQUIP_BS_PREV", 
                "CP_EQUIP_PREV", "DEP_EQUIP", "DEP_EQUIP_BS", 
                "TX_CR_INM", "TX_CR_BGC", "Leasing")
  
  X_Trimestriel <- c("TX_CR_GLISS", "TX_CR_VAR", "VA_INM", "TX_CR_INM_GLISS", "TX_CR_INM_VAR", 
                     "VA_BGC", "TX_CR_BGC_GLISS", "TX_CR_BGC_VAR", "DEP_EQUIP_CAP")
  
  X_Mensuel <- c("INV_IMCCV_PREV", "IND_PROD_IMCCV", "Ramadan", "EURO_TND", "USD_TND")
  
  X_Mensuel_Cumul <- c("DEP_ETAT", "DEP_ETAT_CAP", "DEP_ETAT_BS")
  
  X  <-  list()  
  
  for( i in X_Mensuel) {
    X[[paste(i, 1, sep = "_")]] <- na.omit(ts(X_Raw[,i], start = 2006, frequency = 12))
  }
  
  for( i in X_Trimestriel) {
    X[[paste(i, 3, sep = "_")]] <- na.omit(ts(X_Raw[,i], start = 2006, frequency = 12))
    X[[paste(i, 3, sep = "_")]] <- na.omit(window(X[[paste(i, 3, sep = "_")]],frequency = 4))
  }
  
  for( i in X_Annuel) {
    X[[paste(i, 12, sep = "_")]] <- na.omit(ts(X_Raw[,i], start = 2006, frequency = 12))
    X[[paste(i, 12, sep = "_")]] <- na.omit(window(X[[paste(i, 12, sep = "_")]],frequency = 1))
  }
  
  # importation des vairiables à périoddicité mensuelle cumulée
  
  for( i in X_Mensuel_Cumul) {
    
    X[[paste(i, 1, sep = "_")]] <- ts(X_Raw[,i], start = 2006, frequency = 12)
    
    for(k in 0:9) {
      for(j in 12:2) {
        X[[paste(i, 1, sep = "_")]][j+k*12] <- X[[paste(i, 1, sep = "_")]][j+k*12] - 
          X[[paste(i, 1, sep = "_")]][j-1+k*12]
      }
    }
    
    X[[paste(i, 1, sep = "_")]] <- na.omit(X[[paste(i, 1, sep = "_")]])
  }
  
  return(X)
}