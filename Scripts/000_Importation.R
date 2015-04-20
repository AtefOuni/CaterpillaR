#importation scripts

require(XLConnect)

source("Scripts/Functions/Import_X.R")
source("Scripts/Functions/Import_Cat_Products.R")
source("Scripts/Functions/Year_Trim_Month_X.R")

# Fichier Data

Fichier_Data <- "Data/Data070315_REV300315.xlsx"

# Labels, inputs

Labels <- readWorksheetFromFile(Fichier_Data, "LABELS")
Inputs <- readWorksheetFromFile(Fichier_Data, "INPUTS")

names(Inputs) <- c("Input", "Val")

# Caterpillar products

Cat <- Import_Cat_Products(Fichier_Donnes = Fichier_Data)

# Exogenous variables

X <- Import_X(Fichier_Donnes = Fichier_Data)

# Year to Month Data

X[["DEP_ETAT_PREV_1"]] <- Year_To_Month_X(Target = X[["DEP_ETAT_PREV_12"]],
                                           Source = X[["DEP_ETAT_1"]])
X[["DEP_EQUIP_CAP_1"]] <- Trim_To_Month_X(Target = X[["DEP_EQUIP_CAP_3"]])
for(j in c(2, 3, 4, 6)) {
  X[[paste("DEP_ETAT_PREV", j, sep = "_")]] <- aggregate(X[["DEP_ETAT_PREV_1"]], nfrequency = 12/j)
}

for(j in c(2, 4, 6, 12)) {
  X[[paste("DEP_EQUIP_CAP", j, sep = "_")]] <- aggregate(X[["DEP_EQUIP_CAP_1"]], nfrequency = 12/j)
}

# Save Data.RData

save(list = c("Cat", "X", "Inputs", "Labels"),file = "Data/Data.RData" )