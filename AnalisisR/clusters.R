# Universidad del Valle de Guatemala
# Proyecto #1
# Integrantes: Oscar Juárez, José Cifuentes, Luis Esturban
# Fecha: 30/03/2020
setwd("./")

library(corrplot) # Para hacer matriz de correlación
library(NbClust) # Para determinar el número de clusters óptimo

falles <- read.csv("../Data/FallecidosLesionados.csv", stringsAsFactors = FALSE)
falles <- falles[(falles$int_o_noint != 9),]

#Se introduce el grupoRespuesta, el cual es la gravedad del accidente
#1= gravedad leve
#2= gravedad media
#3= gravedad fuerte
#4= gravedad super fuerte
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

#Para determinar la cantidad correcto de clusteres
x<-falles[falles$tipo_veh==4,c("g_hora","g_hora_5","mayor_menor","grupoRespuesta")]

wss <- (nrow(x)-1)*sum(apply(x,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(x, i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

#Se realiza el cluster
library(klaR)
library(cluster) #Para calcular la silueta

#Con esto realizamos todas las posibles combinaciones y guardamos las 
#siluetas para poder encontrar el mejor modelo
siluetas<-c()
historial<-c()
for (i in 3:20) {
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




#Se realiza el cluster con el algoritmo kmodes

variable<-c("g_hora","g_hora_5","mayor_menor","grupoRespuesta")
set.seed(20)
x<-falles[falles$tipo_veh==4,variable]
result <- kmodes(x,3, iter.max = 50, weighted = FALSE)
silch<-silhouette(result$cluster,dist(x))
mean(silch[,3])
x$cluster<-result$cluster

x$cluster==3

#Interpretacion de grupos

#Grupos respecto a g_hora
plot(x[,c("g_hora","cluster")],
     lwd  = 10,
     col  = "chocolate", 
     main="Grupo de cluster respecto a hora"
     )

#Grupos respecto a g_hora_5
plot(x[,c("g_hora_5","cluster")],
     lwd  = 10,
     col  = "chocolate", 
     main="Grupo de cluster respecto a hora"
)

#Grupos respecto a mayor_menor

#Grupo 1
barplot(table(x[x$cluster==1,"mayor_menor"]),
        main = "Cantidad de mayores de edad Grupo 1",
        xlab = "mayor_menor", ylab = "Cantidad de personas",
        col = "royalblue")

#Grupo 2
barplot(table(x[x$cluster==2,"mayor_menor"]),
        main = "Cantidad de mayores de edad Grupo 2",
        xlab = "mayor_menor", ylab = "Cantidad de personas",
        col = "royalblue")

#Grupo 3
barplot(table(x[x$cluster==3,"mayor_menor"]),
        main = "Cantidad de mayores de edad Grupo 3",
        xlab = "mayor_menor", ylab = "Cantidad de personas",
        col = "royalblue")

#Grupos respecto a grupoRespuesta
#Grupo 1
barplot(table(x[x$cluster==1,"grupoRespuesta"]),
        main = "Gravedad del accidente Grupo 1",
        xlab = "Gravedad", ylab = "Cantidad de personas",
        col = "royalblue")

#Grupo 2
barplot(table(x[x$cluster==2,"grupoRespuesta"]),
        main = "Gravedad del accidente Grupo 2",
        xlab = "Gravedad", ylab = "Cantidad de personas",
        col = "royalblue")

#Grupo 3
barplot(table(x[x$cluster==3,"grupoRespuesta"]),
        main = "Gravedad del accidente Grupo 3",
        xlab = "Gravedad", ylab = "Cantidad de personas",
        col = "royalblue")
