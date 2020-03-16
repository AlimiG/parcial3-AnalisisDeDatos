library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(gridExtra)
library(CCA)
data = read.csv("Data/Tidy/Elecciones_Parcial.csv")[,2:13]
data
# Calculamos las medis y las desviaciones por variables
demo <- data %>%
  select(DEM12,DEM04,DEM92,DEM80)
gop <- data %>%
  select(GOP12,GOP04,GOP92,GOP80)

correl <- matcor(demo,gop)
img.matcor(correl, type = 2)
cc1 <- cc(demo,gop)
barplot(cc1$cor, main = "Canonical correlations for 'cancor()'", col = "gray")
cc1$xcoef
plt.cc(cc1, var.label = TRUE, ind.names = data[,2])
