# Tidydata
data <- read.csv("Data/Raw/Elecciones.csv", header = T, sep = "\t")
new_data <- cbind(data[, 1:2], data[,5:6],data[,9:10],data[,15:16],data[,21:24])
write.csv(new_data,"Data/Tidy/Elecciones_Parcial.csv")

data <- read.csv("Data/Tidy/Elecciones_Parcial.csv",header = T,)[,2:13]
head(data)

# Problema 1 Analisis de componentes principales

