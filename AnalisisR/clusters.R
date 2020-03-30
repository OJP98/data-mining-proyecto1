setwd("./")

library(corrplot) # Para hacer matriz de correlación
library(NbClust) # Para determinar el número de clusters óptimo

falles <- read.csv("../Data/FallecidosLesionados.csv", stringsAsFactors = FALSE)
falles <- falles[(falles$int_o_noint != 9),]

grupoRespuesta <- c()
for (i in 1:nrow(falles)) {
  if (falles[i,"fall_les"] == 1 && falles[i,"int_o_noint"] == 1)
    grupoRespuesta <- c(grupoRespuesta, 3)
  
  else if (falles[i,"fall_les"] == 1 && falles[i,"int_o_noint"] == 2)
    grupoRespuesta <- c(grupoRespuesta, 4)
  
  else if (falles[i,"fall_les"] == 2 && falles[i,"int_o_noint"] == 1)
    grupoRespuesta <- c(grupoRespuesta, 2)
  
  else if (falles[i,"fall_les"] == 2 && falles[i,"int_o_noint"] == 2)
    grupoRespuesta <- c(grupoRespuesta, 1)
}

falles$grupoRespuesta <- grupoRespuesta
rm(grupoRespuesta, i)

View(falles)
#Matriz de correlacion
matriz_cor <- cor(falles[,])
corrplot(matriz_cor)
View(matriz_cor)


#Para determinar la cantidad correcto de clusteres
x<-falles[falles$tipo_veh==4,c(5:7,10,15,19,18)]

wss <- (nrow(x)-1)*sum(apply(x,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(x, i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Se realiza el cluster
library(klaR)
library(cluster) #Para calcular la silueta


siluetas<-c()
historial<-c()
for (i in 2:20) {
  for (variable in combn(names(falles),i,simplify=FALSE)) {
    
    print(variable)
    
    set.seed(20)
    
    x<-falles[falles$tipo_veh==4,variable]
    result <- kmodes(x,3, iter.max = 50, weighted = FALSE)
    silch<-silhouette(result$cluster,dist(x))
    siluetas<-c(siluetas,mean(silch[,3]))
    historial<-c(historial,list(variable))
  }
  
}




x$cluster<-result$cluster

