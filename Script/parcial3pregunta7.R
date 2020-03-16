library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(gridExtra)
library(CCA)
library(tidyverse)
library(psych)
library(corrplot)
library(corrr)
library(stats)
datis = read.csv("Data/Tidy/Elecciones_Parcial.csv")[,2:13]
head(datis)

final <- datis %>%
  select(DEM12,GOP12,DEM04,GOP04,DEM92,GOP92,DEM80,GOP80)

rownames(final) = datis$Estado
head(final)

provMap <- scale(final) #Escalamos los datos para evitar problemas de dimensiones
provMap_d <- get_dist(provMap)#Calculamos la matriz de distancias
fviz_dist(provMap_d, lab_size = 8, gradient = list(low = "white",mid=NULL, high ="red"))

## [1] 38 38
provMap_r <- cmdscale(provMap_d,eig = TRUE,k = 2) #Calculamos dos dimensiones
provMap_r$GOF
## [1] 0.6846313 0.6846313
x <- provMap_r$points[,1]
y <- provMap_r$points[,2]
columnforlabels = dimnames(provMap_r[[1]])[[1]]
plot(x,y, type = 'n',xlab = 'Dimension 1', ylab = 'Dimension 2', main = 'Mapa de Similitudes entre Paises')
columnforlabels = datis$Estado
colorforlabels = datis$REG
paleta = c("Magenta","Green","Red","Blue","Black")
text(x,y, labels = columnforlabels,cex = 0.8, col = paleta[colorforlabels])
legend('topright',legend = levels(colorforlabels),fill = paleta, title = 'Continente',cex = 0.4)
