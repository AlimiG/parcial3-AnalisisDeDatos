library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(gridExtra)
library(CCA)
library(tidyverse)
votes = read.csv("Data/Tidy/Elecciones_Parcial.csv", header = TRUE)[,2:13]
attach(votes)
addyear12 <- function(demi,repi,dataa){
  testi = repi - demi
  dataa <- dataa %>%
    mutate(Y12 = ifelse( testi >= 10 , "A",ifelse( testi < 10 & testi >2, "B",
                                                      (ifelse( testi < 2 & testi > -2, "C",
                                                               (ifelse(testi < -2 & testi > -10,"D",
                                                                       (ifelse(testi < -10, "E",0))
                                                               )))))))
  return(dataa)

}
addyear04 <- function(demi,repi,dataa){
  testi = repi - demi
  dataa <- dataa %>%
    mutate(Y04 = ifelse( testi >= 10 , "A",ifelse( testi < 10 & testi >2, "B",
                                                   (ifelse( testi < 2 & testi > -2, "C",
                                                            (ifelse(testi < -2 & testi > -10,"D",
                                                                    (ifelse(testi < -10, "E",0))
                                                            )))))))
  return(dataa)
  
}
addyear92 <- function(demi,repi,dataa){
  testi = repi - demi
  dataa <- dataa %>%
    mutate(Y92 = ifelse( testi >= 10 , "A",ifelse( testi < 10 & testi >2, "B",
                                                   (ifelse( testi < 2 & testi > -2, "C",
                                                            (ifelse(testi < -2 & testi > -10,"D",
                                                                    (ifelse(testi < -10, "E",0))
                                                            )))))))
  return(dataa)
  
}
addyear80 <- function(demi,repi,dataa){
  testi = repi - demi
  dataa <- dataa %>%
    mutate(Y80 = ifelse( testi >= 10 , "A",ifelse( testi < 10 & testi >2, "B",
                                                   (ifelse( testi < 2 & testi > -2, "C",
                                                            (ifelse(testi < -2 & testi > -10,"D",
                                                                    (ifelse(testi < -10, "E",0))
                                                            )))))))
  return(dataa)
  
}
datab = addyear80(GOP80,DEM80,votes)
datab = addyear92(GOP92,DEM92,datab)
datab = addyear04(GOP04,DEM04,datab)
datab = addyear12(GOP12,DEM12,datab)
write.csv(datab,"Data/Tidy/problema4.csv")

head(datab)
datac <- datab %>%
  select(REG,Y80,Y92,Y04,Y12)
datac



tablea <- as.table(as.matrix(xtabs(~REG+Y80,datac))+as.matrix(xtabs(~REG+Y92,datac))+as.matrix(xtabs(~REG+Y04,datac))+as.matrix(xtabs(~REG+Y12,datac)))
tablea
tablea <- tablea / sum(tablea)
tablea

library('ca')
rel.ca <- ca(tablea)
plot.ca(rel.ca,mass=c(T,T),col = c('red','blue'))
