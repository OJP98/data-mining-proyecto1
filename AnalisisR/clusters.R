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
wss <- (nrow(falles[falles$tipo_veh==4,2:20])-1)*sum(apply(falles[falles$tipo_veh==4,2:20],2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(falles[falles$tipo_veh==4,2:20], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Se realiza el cluster
#K-medias
fallesCluster<-falles[falles$tipo_veh==4,2:20]
km<-kmeans(falles[falles$tipo_veh==4,2:20],3)
fallesCluster$grupo<-km$cluster

silkm<-silhouette(km$cluster,dist(falles[falles$tipo_veh==4,2:20]))
mean(silkm[,3]) #0.81, no es la mejor particiÃ³n pero no estÃ¡ mal

plotcluster(falles[falles$tipo_veh==4,2:20],km$cluster) #grafica la ubicaciÃ³n de los clusters







