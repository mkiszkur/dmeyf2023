# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

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

PARAM$experimento <- "K03FE"

PARAM$input$dataset <- "./datasets/competencia_03_5000.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03_100.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03_1000.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"


#------------------------------------------------------------------------------

#definir si es en google cloud o en la maquina local
EJEC <- list()

#EJEC$tipo_ejecicion = 'local'
EJEC$tipo_ejecicion = 'Google Cloud'

if (EJEC$tipo_ejecicion == 'local') {
  setwd("/Users/miguelkiszkurno/Documents/dmeyf") 
}else {
  setwd("~/buckets/b1/") # Establezco el Working Directory
}


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# Aqui se debe poner la carpeta de la computadora local
#setwd("~/buckets/b1/") # Establezco el Working Directory

# cargo el dataset donde voy a entrenar el modelo
dataset <- fread(PARAM$input$dataset)

# Obtener los nombres de las columnas
columnas <- colnames(dataset)

#columnas sobre las que voy a hacer el feature engineering
columnas_feature_eng <- setdiff(
  columnas,
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)


# Imprimir los nombres de las columnas
print(columnas)

# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

#Me quedo unicamente  con los baja+2
#dataset_baja_mas_2 = dataset[clase_ternaria == 'BAJA+2', ]


# Contar el número de registros en el dataset filtrado
num_registros <- nrow(dataset)

# Imprimir el resultado
cat("El dataset 'dataset_baja_mas_2' tiene", num_registros, "registros.\n")


print(dataset[, .(numero_de_cliente, foto_mes)])

# Ordena el data.table por "numero_de_cliente" y "anho_mes"
dataset <- dataset[order(numero_de_cliente, foto_mes)]

inicio <- Sys.time()


iteracion <- 0
# Itera sobre cada campo en la lista
for (campo in columnas_feature_eng) {
  iteracion <- iteracion + 1
  cat(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " iteracion nro: ", iteracion, "campo: ", campo, "registros.\n")
  
  # Calcula el valor anterior del campo por "numero_de_cliente"
  dataset[, paste0("lag_1_", campo, "_6M") := shift(get(campo), type = "lag"), by = numero_de_cliente]
  dataset[, paste0("lag_2_", campo, "_6M") := shift(get(campo), n = 2L, type = "lag"), by = numero_de_cliente]
  dataset[, paste0("lag_3_", campo, "_6M") := shift(get(campo), n = 3L, type = "lag"), by = numero_de_cliente]
  dataset[, paste0("avg_", campo, "_6M") := frollmean(get(campo), n = 6L, align = "right"), by = numero_de_cliente]
  dataset[, paste0("dlag_1_", campo, "_6M") := get(campo) - get(paste0("lag_1_", campo, "_6M")), by = numero_de_cliente]
  dataset[, paste0("dlag_2_", campo, "_6M") := get(campo) - get(paste0("lag_2_", campo, "_6M")), by = numero_de_cliente]
  dataset[, paste0("dlag_3_", campo, "_6M") := get(campo) - get(paste0("lag_3_", campo, "_6M")), by = numero_de_cliente]
}


# Visualiza las columnas de interés
#result <- dataset[, .(cliente_antiguedad, lag_1_cliente_antiguedad_6M, lag_2_cliente_antiguedad_6M, lag_3_cliente_antiguedad_6M, 
#                      avg_cliente_antiguedad_6M, dlag_1_cliente_antiguedad_6M, dlag_2_cliente_antiguedad_6M, dlag_3_cliente_antiguedad_6M)]


# Muestra el resultado
#print(result)

fwrite(dataset,
       file = "./datasets/competencia_03_NAs_FE.csv.gz",
       sep = "\t"
)


# Imprime el timestamp

cat("Inicio de la ejecución: ", format(inicio, format = "%Y-%m-%d %H:%M:%S"), "\n")
cat("fin de la ejecución: ", format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")


# libero espacio
rm(dataset)
gc()


cat("\n\nEjecucion finalizada\n")
