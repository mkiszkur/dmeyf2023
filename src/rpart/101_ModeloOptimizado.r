# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(dplyr)


# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo
dapply[, fold := 2]



particionar <- function(data, division, agrupa = "", campo = "fold", start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed)
  
  bloque <- unlist(mapply(function(x, y) {
    rep(y, x)
  }, division, seq(from = start, length.out = length(division))))
  
  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa
  ]
}

predecir <- function(modelo, nm_archivo) {
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    object = modelo,
    newdata = dapply,
    type = "prob"
  )
  
  # prediccion es una matriz con TRES columnas,
  # llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  # cada columna es el vector de probabilidades
  
  # agrego a dapply una columna nueva que es la probabilidad de BAJA+2
  dapply[, prob_baja2 := prediccion[, "BAJA+2"]]
  
  # solo le envio estimulo a los registros
  #  con probabilidad de BAJA+2 mayor  a  1/40
  dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]
  
  # genero el archivo para Kaggle
  # primero creo la carpeta donde va el experimento
  dir.create("./exp/")
  dir.create("./exp/KA2001")
  
  # solo los campos para Kaggle
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = nm_archivo,
         sep = ","
  )
  
}

dtrain1 = copy(dtrain)
particionar(dtrain1, division = c(5, 5), agrupa = "clase_ternaria", seed = 106703)

dtrain2 = copy(dtrain)
particionar(dtrain2, division = c(5, 5), agrupa = "clase_ternaria", seed = 106721)

dtrain3 = copy(dtrain)
particionar(dtrain3, division = c(5, 5), agrupa = "clase_ternaria", seed = 106727)

dtrain4 = copy(dtrain)
particionar(dtrain4, division = c(5, 5), agrupa = "clase_ternaria", seed = 106739)

dtrain5 = copy(dtrain)
particionar(dtrain5, division = c(5, 5), agrupa = "clase_ternaria", seed = 106747)

v_cp = -1
v_minsplit = 1490
v_minbucket = 696
v_maxdepth = 11

#Arbol de 3 niveles
modelo_3_niveles <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = 3
) # profundidad maxima del arbol

predecir (modelo_3_niveles, "./exp/KA2001/K101_000_3.csv")
# ctrx_quarter < 18
# mcuentas_saldo< -1388.2
# mprestamos_personales < 6627.8

# # grafico el arbol
# prp(modelo_3_niveles,
#     extra = 101, digits = -5,
#     branch = 1, type = 4, varlen = 0, faclen = 0
# )


#Con todos
modelo_todos <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain, # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # 
  minsplit = v_minsplit , 
  minbucket = v_minbucket, 
  maxdepth = v_maxdepth
) # profundidad maxima del arbol

# grafico el arbol
prp(modelo_todos,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

predecir (modelo_todos, "./exp/KA2001/K101_000_t.csv")

modelo1 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain1[fold == 1], # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = v_maxdepth
) # profundidad maxima del arbol

# # grafico el arbol
# prp(modelo1,
#     extra = 101, digits = -5,
#     branch = 1, type = 4, varlen = 0, faclen = 0
# )

predecir (modelo1, "./exp/KA2001/K101_001.csv")

modelo2 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain2[fold == 1], # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = v_maxdepth
  ) # profundidad maxima del arbol

predecir (modelo2, "./exp/KA2001/K101_002.csv")

modelo3 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain3[fold == 1], # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = v_maxdepth

) # profundidad maxima del arbol
predecir (modelo3, "./exp/KA2001/K101_003.csv")


modelo4 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain4[fold == 1], # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = v_maxdepth
) # profundidad maxima del arbol
predecir (modelo4, "./exp/KA2001/K101_004.csv")

modelo5 <- rpart(
  formula = "clase_ternaria ~ .",
  data = dtrain5[fold == 1], # los datos donde voy a entrenar
  xval = 0,
  cp = v_cp, # esto significa no limitar la complejidad de los splits
  minsplit = v_minsplit , # minima cantidad de registros para que se haga el split
  minbucket = v_minbucket, # tamaño minimo de una hoja
  maxdepth = v_maxdepth
) # profundidad maxima del arbol
predecir (modelo5, "./exp/KA2001/K101_005.csv")
