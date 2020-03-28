setwd("./")

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
