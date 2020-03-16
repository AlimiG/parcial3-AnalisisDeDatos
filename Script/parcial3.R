library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(gridExtra)
# Tidydata
data <- read.csv("Data/Raw/Elecciones.csv", header = T, sep = "\t")
new_data <- cbind(data[, 1:2], data[,5:6],data[,9:10],data[,15:16],data[,21:24])
write.csv(new_data,"Data/Tidy/Elecciones_Parcial.csv")

data <- read.csv("Data/Tidy/Elecciones_Parcial.csv",header = T,)[,2:13]
head(data)


## Problema 1 Analisis de componentes principales
### Para los demócratas
demo <- data %>%
  select(Estado,Cod,DEM12,DEM04,DEM92,DEM80,Tend,REG)

pca.demo <- princomp(demo[,3:6],cor = T)
names(pca.demo)
options(digits = 4)
summary(pca.demo)

prop_varianza <- pca.demo$sdev^2 / sum(pca.demo$sdev^2)
ggplot(data = data.frame(prop_varianza, pca.demo = 1:4),
       aes(x = pca.demo, y = prop_varianza)) +
  geom_col(width = 0.3)+
  theme_bw()+labs(x = "Componente Princial",y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)
p <- ggplot(data = data.frame(prop_varianza, pc = 1:4),
            aes(x = pc, y = prop_varianza_acum, group =1))
p + geom_point()+geom_line()+labs(x = "Componente principal", 
                                  y = "Prop. varianza explicada acumulada")+
  geom_label(aes(label = round(prop_varianza_acum,2))) 

biplot(pca.demo)

plot(pca.demo$scores[,1],pca.demo$scores[,2],pch=21,bg = demo$REG)
legend(3,-1,legend = levels(demo$REG), col = c("red","blue","green","black"),lty = 1:2,cex = 0.7)

eigen(cor(demo[,3:6]))$values
mean(eigen(cor(demo[,3:6]))$values)

par(mfrow=c(2,1), bg="azure")
barplot(loadings(pca.demo)[,1],col="blue2",sub="Primera componente")
barplot(loadings(pca.demo)[,2],col="orange",sub="Segunda componente")

## PAra los republicamos
gop <- data %>%
  select(Estado,Cod,GOP12,GOP04,GOP92,GOP80,Tend,REG)

pca.gop <- princomp(gop[,3:6],cor = T)
names(pca.gop)
options(digits = 4)
summary(pca.gop)

prop_varianza <- pca.gop$sdev^2 / sum(pca.gop$sdev^2)
ggplot(data = data.frame(prop_varianza, pca.gop = 1:4),
       aes(x = pca.gop, y = prop_varianza)) +
  geom_col(width = 0.3)+
  theme_bw()+labs(x = "Componente Princial",y = "Prop. de varianza explicada")

prop_varianza_acum <- cumsum(prop_varianza)
p <- ggplot(data = data.frame(prop_varianza, pc = 1:4),
            aes(x = pc, y = prop_varianza_acum, group =1))
p + geom_point()+geom_line()+labs(x = "Componente principal", 
                                  y = "Prop. varianza explicada acumulada")+
  geom_label(aes(label = round(prop_varianza_acum,2))) 

biplot(pca.gop)

plot(pca.gop$scores[,1],pca.gop$scores[,2],pch=21,bg = gop$REG)
legend(-8,1,legend = levels(gop$REG), col = c("red","blue","green","black"),lty = 1:2,cex = 0.7)

eigen(cor(gop[,3:6]))$values
mean(eigen(cor(gop[,3:6]))$values)

par(mfrow=c(2,1), bg="azure")
barplot(loadings(pca.gop)[,1],col="blue2",sub="Primera componente")
barplot(loadings(pca.gop)[,2],col="orange",sub="Segunda componente")

## Método de K-medias

data
numeric.values <- data[,3:10]
rownames(numeric.values) = data$Cod
america.dist <- get_dist(numeric.values, method = "manhattan")
fviz_dist(america.dist, lab_size = 8, gradient = list(low = "white",mid=NULL, high ="red"))
algoritmo="kmeans" #k-medias
p <- c()
for (i in c(2,3,4,5)){
  set.seed(1)
  cuantosClusters=i
  solucionKmeans1 <- eclust(numeric.values,
                            FUNcluster = algoritmo,
                            k = cuantosClusters, #como lo hicimos previamente
                            graph =F, hc_metric = "manhattan")
  p <- cbind(p,solucionKmeans1)
  print(fviz_silhouette(solucionKmeans1) + ggtitle(sprintf("k = %s", i)))
}
p1 <- fviz_cluster(as.list(p[,1]), data = numeric.values) + ggtitle("k = 2")
p2 <- fviz_cluster(p[,2], data = numeric.values) + ggtitle("k = 3")
p3 <- fviz_cluster(p[,3], data = numeric.values) + ggtitle("k = 4")
p4 <- fviz_cluster(p[,4], data = numeric.values) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)


#Pruebas
ncong = NbClust(data=numeric.values, distance = NULL, diss = america.dist, min.nc=2,
                max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(ncong, verbose =T) 

fviz_nbclust(numeric.values, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(numeric.values, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(123)
fviz_nbclust(numeric.values, kmeans, nstart = 4,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

#Jerarquico
# Enhanced hierarchical clustering
res.hc <- eclust(numeric.values, "hclust", k = 2) # compute hclust
fviz_dend(res.hc,rect = TRUE)

res.hc2 <- eclust(numeric.values, "hclust", k = 3) # compute hclust
fviz_dend(res.hc2,rect = TRUE)
