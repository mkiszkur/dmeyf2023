# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/competencia_01.csv")

dtrain <- dataset[foto_mes == 202103] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202105] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables

##Voy a generar variaciones pequenas del modelo reduciendo los datos. Saco el 3%
# Define el porcentaje de datos que deseas retener

cat("# filas de dtrain es:", nrow(dtrain), "\n")

porcentaje_retencion <- 0.97

# Calcula el número de filas que representan el 98% del dataset
num_filas_retencion <- round(nrow(dtrain) * porcentaje_retencion)

# Utiliza la función sample para seleccionar aleatoriamente las filas
filas_seleccionadas <- sample(1:nrow(dtrain), num_filas_retencion)

# Crea un nuevo dataframe con las filas seleccionadas
dtrain <- dtrain[filas_seleccionadas, ]

#Verifico cantidad de filas
cat("# filas de dtrain es:", nrow(dtrain), "\n")



#Volvemos al que mejor me fue
# #Primer Intento
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = 0, # esto significa no limitar la complejidad de los splits
        minsplit = 600 , # minima cantidad de registros para que se haga el split
        minbucket = 1, # tamaño minimo de una hoja
        maxdepth = 14
) # profundidad maxima del arbol


# #Segundo Resultado
# modelo <- rpart(
#   formula = "clase_ternaria ~ .",
#   data = dtrain, # los datos donde voy a entrenar
#   xval = 0,
#   "cp" = -1, # complejidad minima
#   "minsplit" = 400, # minima cant de registros en un nodo para hacer el split
#   "minbucket" = 5, # minima cantidad de registros en una hoja
#   "maxdepth" = 10
# ) # profundidad máxima del arbol

# #Tercer Resultado
# modelo <- rpart(
#   formula = "clase_ternaria ~ .",
#   data = dtrain, # los datos donde voy a entrenar
#   xval = 0,
#   "cp" = -0.5, # complejidad minima
#   "minsplit" = 400, # minima cant de registros en un nodo para hacer el split
#   "minbucket" = 5, # minima cantidad de registros en una hoja
#   "maxdepth" = 10
# ) # profundidad máxima del arbol

# #Cuarto Resultado
# modelo <- rpart(
#   formula = "clase_ternaria ~ .",
#   data = dtrain, # los datos donde voy a entrenar
#   xval = 0,
#   "cp" = 0, # complejidad minima
#   "minsplit" = 400, # minima cant de registros en un nodo para hacer el split
#   "minbucket" = 1, # minima cantidad de registros en una hoja
#   "maxdepth" = 14
# ) # profundidad máxima del arbol

# #Quinto Resultado
# modelo <- rpart(
#   formula = "clase_ternaria ~ .",
#   data = dtrain, # los datos donde voy a entrenar
#   xval = 0,
#   "cp" = 0, # complejidad minima
#   "minsplit" = 200, # minima cant de registros en un nodo para hacer el split
#   "minbucket" = 1, # minima cantidad de registros en una hoja
#   "maxdepth" = 8
# ) # profundidad máxima del arbol


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


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
        file = "./exp/KA2001/K101_001.csv",
        sep = ","
)
