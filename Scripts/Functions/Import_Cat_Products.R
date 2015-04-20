
require(XLConnect)

Import_Cat_Products <- function (Fichier_Donnes) {
  
  Table_List <- c("BHL", "HEX", "MG", "MINHEX", "PNEU", "SCOM", 
                  "SSL", "SWL", "TTT", "UCOM", "WHEX", "WL")
  
  Cat  <-  list()
  
  for(i in Table_List) {
    Cat[[paste(i, 1, sep = "_")]] <- ts(data = readWorksheetFromFile(Fichier_Donnes, i),
                                            start = 2006, frequency = 12)
  }
  
  # Yearly, semestrial, quarterly,  series
  
  for(i in Table_List) {
    for(j in c(2, 3, 4, 6, 12)) {
      Cat[[paste(i, j, sep = "_")]] <- aggregate(Cat[[paste(i, 1, sep = "_")]], nfrequency = 12/j)
    }
  }
  
  return(Cat)
}