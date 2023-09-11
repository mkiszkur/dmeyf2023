###--------------------------------------------------------------------------------------------------------------
# Este script esta basado en z399_BO_Public.r. Lo adapte para poder reproducir   
# los resultados obtenidos con la optimizacion bayesiana
# Se eligio la ejecucion #39.
# Para poder correrlo solo hay que ajustar el working directory
###--------------------------------------------------------------------------------------------------------------


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


#Librerias
require("data.table")
require("rlist")
require("rpart")
require("parallel")


# Defino la  Optimizacion Bayesiana
PARAM <- list()
PARAM$experimento <- "HT3990"




ArbolSimple <- function( data) {


# Iteracion #28 BO Oversampling  
  # modelo <- rpart("clase_binaria ~ . -clase_ternaria",
  #   data = dtrain,
  #   xval = 0,
  #   cp = -1, 
  #   minsplit = 1490 , # minima cantidad de registros para que se haga el split
  #   minbucket = 696, # tamaño minimo de una hoja
  #   maxdepth = 11,
  #   weights = pesos # aqui se hace oversampling
  #   
  # )

  # Iteracion #39 BO Oversampling  
  modelo <- rpart("clase_binaria ~ . -clase_ternaria",
    data = dtrain,
    xval = 0,
    cp = -1, 
    minsplit = 1080 , # minima cantidad de registros para que se haga el split
    minbucket = 507, # tamaño minimo de una hoja
    maxdepth = 12,
    weights = pesos # aqui se hace oversampling
)
  
  # aplico el modelo a los datos de testing
  # aplico el modelo sobre los datos de testing
  # quiero que me devuelva probabilidades
  
  prediccion <- predict(modelo,
    dapply,
    type = "prob"
  )

  # esta es la probabilidad de baja
  prob_baja <- prediccion[, "POS"]

  tablita <- copy( dapply[, list(numero_de_cliente) ] )
  tablita[ , prob := prob_baja ]
  setorder( tablita, -prob )

  # grabo el submit a Kaggle
  tablita[ , Predicted := 0L ]
  tablita[ 1:9793, Predicted := 1L ]

  nom_submit <- paste0("./exp/KA2001/K101_BO_Simple.csv" )
  fwrite( tablita[ , list(numero_de_cliente, Predicted)],
          file= nom_submit,
          sep= "," )

}

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Establezco el Working Directory
setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory

# cargo los datos
dataset <- fread("./datasets/competencia_01.csv")

dataset[foto_mes == 202105, "clase_ternaria"] <- NA


# defino la clase_binaria2
dataset[ , clase_binaria := ifelse( clase_ternaria=="CONTINUA", "NEG", "POS" ) ]

dtrain <- dataset[foto_mes==202103]
dapply <- dataset[foto_mes==202105]

# definicion vector de pesos para oversampling
pesos <- copy( dtrain[, ifelse( clase_ternaria=="CONTINUA",   1.0, 100.0  ) ] )


# creo la carpeta donde va el experimento
#  HT  representa  Hiperparameter Tuning
dir.create("./exp/", showWarnings = FALSE)
dir.create( paste0("./exp/", PARAM$experimento, "/"), 
           showWarnings = FALSE)

ganancia_public <- ArbolSimple( dtrain)