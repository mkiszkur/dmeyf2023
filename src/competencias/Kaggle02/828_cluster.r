# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

#install.packages("randomForest")
library(randomForest)
#install.packages("cluster")
library(cluster)

require("data.table")
require("rlist")

# para que se detenga ante el primer error
# y muestre el stack de funciones invocadas
options(error = function() {
  traceback(20)
  options(error = NULL)
  stop("exiting after script error")
})

# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

PARAM$experimento <- "HT8280"

PARAM$input$dataset <- "./datasets/competencia_02_1000.gz"
#PARAM$input$dataset <- "./datasets/competencia_02_recortado.csv.gz"
#PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"
#PARAM$input$dataset <- "./datasets/_6_meses_competencia_02.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202105)
PARAM$input$validation <- c(202104)
PARAM$input$training <- c(202010, 202011, 202012, 202101, 202102, 202103)

#------------------------------------------------------------------------------

#---- EJEC se utiliza para logear la ejecución:
EJEC <- list()
EJEC$nombre_archivo = "./ejecuciones/log_corridas.csv"
EJEC$fuente = "823_lightgbm_binaria_BO.r"

#definir si es en google cloud o en la maquina local
#EJEC$frenar_corrida = TRUE
EJEC$frenar_corrida = TRUE
EJEC$tipo_ejecicion = 'local'
#EJEC$tipo_ejecicion = 'Google Cloud'

if (EJEC$tipo_ejecicion == 'local') {
  source("~/Documents/dmeyf/dmeyf2023/src/competencias/Kaggle02/Utilities.R")
  #setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory
  setwd("/Users/miguelkiszkurno/Documents/dmeyf") 
  
}else {
  source("/home/miguel_kiszkurno/dmeyf2023/src/competencias/Kaggle02/Utilities.R")
  setwd("~/buckets/b1/") # Establezco el Working Directory
}

EJEC = iniciar_corrida (EJEC)
paste(EJEC$inicio, EJEC$corrida, "Inicio", EJEC$fuente, ",")

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# Obtener los nombres de las columnas
columnas <- colnames(dataset)

# Imprimir los nombres de las columnas
print(columnas)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

#Me quedo unicamente  con los baja+2
dataset_baja_mas_2 = dataset[clase_ternaria == 'BAJA+2', ]


# Contar el número de registros en el dataset filtrado
num_registros <- nrow(dataset_baja_mas_2)

# Imprimir el resultado
cat("El dataset 'dataset_baja_mas_2' tiene", num_registros, "registros.\n")


print(dataset_baja_mas_2[, .(numero_de_cliente, foto_mes)])


#en dataset_filtrado los registros historicos de todos los baja+2
dataset_filtrado <- dataset[numero_de_cliente %in% dataset_baja_mas_2$numero_de_cliente, ]
print(dataset_filtrado[, .(numero_de_cliente, foto_mes)])


#Veo que columnas tienen valores BA en dataset_baja_mas_2
columnas_con_NA <- names(dataset_baja_mas_2)[colSums(is.na(dataset_baja_mas_2)) > 0]
cat("Columnas con NA en dataset_baja_mas_2:\n")
print(columnas_con_NA)

# Recorrer cada columna y reemplazar valores nulos por cero
for (col_name in columnas_con_NA) {
  print(col_name)
  dataset_baja_mas_2[is.na(.SD[[col_name]]), (col_name) := 0]
}

# # Crear un DataFrame con el nombre de la columna y la cantidad de NA en cada columna
# resumen_na <- data.frame(
#   Columna = colnames(dataset_baja_mas_2),
#   NA_Cantidad = colSums(is.na(dataset_baja_mas_2))
# )
# resumen_na <- resumen_na[order(-resumen_na$NA_Cantidad), ]

# # Imprimir el resumen
# resumen_na

rf.fit <- randomForest(x = dataset_baja_mas_2, y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")

rf.cluster = cutree(hclust.rf, k=7)


#iris.pc$rf.clusters <- rf.cluster
#table(rf.cluster, iris$Species)

#A veces quiero probar corridas sin entrenar
if (EJEC$frenar_corrida) {
  # libero espacio
  rm(dataset)
  gc()
  EJEC = finalizar_corrida (EJEC)
  stop("Finalizada por configuracion")
}

# libero espacio
rm(dataset)
gc()


cat("\n\nLa optimizacion Bayesiana ha terminado\n")
EJEC = finalizar_corrida (EJEC)
