# Universidad del Valle de Guatemala
# Proyecto #1
# Integrantes: Oscar JuÃ¡rez, JosÃ© Cifuentes, Luis Esturban
# Fecha: 09/05/2020

# Setear directorio de trabajo
setwd("./")

# Importar librerías
library(e1071)
library(caret)
library(corrplot)

# Leer datos del csv
data <- read.csv("./FallecidosLesionados.csv", stringsAsFactors = FALSE)

# Se elimino las filas que no tomaban en cuanta si se interno o no
data2<- data[data$int_o_noint !=9, ]

# Se crea la variable de respuesta dependiendo de la gravedad del accidente
  #Fallecido=1
  #Lesionado=2
  #Internado=1
  #No Internado=2
grupoRespuesta <- c()
for (i in 1:18024) {
  ifelse(data2$fall_les[i] ==1 && data2$int_o_noint[i]==1, grupoRespuesta <- c(grupoRespuesta, "Fuerte"),ifelse( data2$fall_les[i] ==1 && data2$int_o_noint[i]==2, grupoRespuesta <- c(grupoRespuesta, "Letal")  ,ifelse(data2$fall_les[i] ==2 && data2$int_o_noint[i]==1 ,grupoRespuesta <- c(grupoRespuesta, "Medio")  ,grupoRespuesta <- c(grupoRespuesta, "Leve")   ) ) )
}
data2$grupoRespuesta <- grupoRespuesta

# Se asignan los datos que van a ser utilizados para entranar y testear el modelo
set.seed(69)
porcentaje<-0.7
corte <- sample(nrow(data2),nrow(data2)*porcentaje)
train<-data2[corte,]
test<-data2[-corte,]

# Se crea una matriz de correlacion para saber que variables elegir
varNames <- c()
for(name in colnames(data)){
    varNames <- c(varNames, name)
}
matriz_cor <- cor(data2[,varNames])
matriz_cor
corrplot(matriz_cor)

# Se crean las varibles que contienen las columnas que si aportan al modelo
varNames <- c("g_hora","mupio_ocu","depto_ocu","sexo_per","edad_per","g_edad_80ymás","g_edad_60ymás","edad_quinquenales","tipo_veh","tipo_eve","grupoRespuesta")
varNames2 <- c("g_hora","mupio_ocu","depto_ocu","sexo_per","edad_per","g_edad_80ymás","g_edad_60ymás","edad_quinquenales","tipo_veh","tipo_eve")

# Se crea un modelo de Bayes para poder entrenarlo y verificar nuestros resultados
modelo <- naiveBayes(as.factor(grupoRespuesta)~.,data=train[,varNames])
summary(modelo)

# Se crea una prediccion conforme al entrenamiento previamente realizado
predBayes <- predict(modelo, test[,varNames2])
confusionMatrix(predBayes, as.factor(test$grupoRespuesta))

# Se crea Cross validation

# Se asignan los datos que van a ser utilizados para entranar y testear el modelo
#porcentaje<-0.6
#porcentaje2<-0.5
#data3<-data2
#data3$grupoRespuesta <- grupoRespuesta

#corte <- sample(nrow(data3),nrow(data3)*porcentaje)
#train<-data3[corte,]
#data4<-data3[-corte,]
#corte2 <- sample(nrow(data4),nrow(data4)*porcentaje2)
#cross<-data4[corte2,]
#test<-data4[-corte2,]

#modelo<-naiveBayes(as.factor(grupoRespuesta)~.,data=train[,varNames])

#Se realiza la prediccion para cross
#predBayes<-predict(modelo,cross[,varNames2])
#confusionMatrix(predBayes,as.factor(cross$grupoRespuesta))

#ct<-trainControl(method = "cv",train[,varNames2],number = 10814,verboseIter=T)
#modeloCaret<-train(as.factor(grupoRespuesta)~.,data=data3[,varNames2],method="nb",trControl = ct)
#prediccionCaret<-predict(modeloCaret,newdata = cross[,varNames2])
#confusionMatrix(prediccionCaret,as.factor(cross$grupoRespuesta))

#Se realiza la prediccion para test
#predBayes<-predict(modelo,test[,testVarNames])
#confusionMatrix(predBayes,as.factor(cross$grupoRespuesta))

#ct<-trainControl(method = "cv",train[,testVarNames],number=10, verboseIter=T)
#modeloCaret<-train(as.factor(grupoRespuesta)~.,data=data2[,varNames],method="nb",trControl = ct)
#prediccionCaret<-predict(modeloCaret,newdata = test[,varNames2])
#confusionMatrix(prediccionCaret,as.factor(test$grupoRespuesta))

