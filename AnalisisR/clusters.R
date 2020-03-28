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

# Hacer matriz de correlación para ver que variables aportan
matriz_cor <- cor(falles)
matriz_cor
corrplot(matriz_cor)

# Las variables que aportan, según un rango de correlación
vars <- c("edad_per","tipo_veh","modelo_veh","tipo_eve")

# Acorde a criterio personal
vars <- c("día_ocu","hora_ocu","mes_ocu","día_sem_ocu","edad_per","tipo_veh","modelo_veh","tipo_eve")

# Filtrar la data acorde a las variables seleccionadas
data <- falles[(falles$modelo_veh != 9999), vars]

# Obtener número de clusters según gráfico de codo
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


# Usando otro paquete para saber el mejor número de clusters

##### ATENCIÓN, ESTÁ LIBRERÍA COMO QUE NO FUNCIONA #####
nb <- NbClust(data, distance = "euclidean", min.nc = 2,max.nc = 10, method = "complete", index ="all")


