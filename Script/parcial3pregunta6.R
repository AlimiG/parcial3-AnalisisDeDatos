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

usa <- datis %>%
  select(DEM12,GOP12,DEM04,GOP04,DEM92,GOP92,DEM80,GOP80)
rownames(usa)<- datis$Cod

cor.usa <- cor(usa)
corrplot(cor.usa)

KMO(usa)
det(cor.usa)
bartlett.test(usa)
factanal(usa, factors = 3, rotations = "none")
