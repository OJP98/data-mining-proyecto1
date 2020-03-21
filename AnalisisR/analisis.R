# Universidad del Valle de Guatemala
# Proyecto #1
# Integrantes: Oscar Juárez, José Cifuentes, Luis Esturban
# Fecha: 30/03/2020

setwd("./")

HechoTransito<-read.csv("./HechoTransito.csv",stringsAsFactors = FALSE)
importaciones<-read.csv("./importacionesVehiculosSAT.csv",stringsAsFactors = FALSE)
VehiculosInvolucrados<-read.csv("./VehiculosInvolucrados.csv",stringsAsFactors = FALSE)


# d. Graficos exploratorios que le de ideas del estado de los datos.

#Hechos de transito
View(HechoTransito)
#En que años hay mas accidentes?
barplot(table(HechoTransito[,"año_ocu"]),
        main = "Cantidad de accidentes al año",
        xlab = "Años", ylab = "Cantidad de accidentes",
        col = "royalblue")

#Que municipios tiene mas accidentes?
municipiosList<-as.data.frame(head(table(HechoTransito[,"mupio_ocu"]),n=5))
View(municipiosList[order(municipiosList$Freq,decreasing = TRUE),])


#Que tipo de vehiculo tiene mas accidentes?
#En que años hay mas accidentes?
barplot(table(HechoTransito[,"tipo_veh"]),
        main = "Cantidad de accidentes por tipo de vehiculo",
        xlab = "Tipo vehiculo", ylab = "Cantidad de accidentes",
        col = "royalblue")


#En que mes hay mas accidentes
barplot(table(HechoTransito[,"mes_ocu"]),
        main = "Cantidad de accidentes por mes",
        xlab = "Mes", ylab = "Cantidad de accidentes",
        col = "royalblue")

#En que dia de la semana hay mas accidentes
barplot(table(HechoTransito[,"día_sem_ocu"]),
        main = "Cantidad de accidentes por mes",
        xlab = "Mes", ylab = "Cantidad de accidentes",
        col = "royalblue")

