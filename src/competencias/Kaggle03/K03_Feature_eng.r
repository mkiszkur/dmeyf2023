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

PARAM$experimento <- "K03FE_F6"

PARAM$input$dataset <- "./datasets/competencia_03_5000.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03_1000.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"
PARAM$input$dataset <- "./datasets/competencia_03_100.csv.gz"


#------------------------------------------------------------------------------

#definir si es en google cloud o en la maquina local
EJEC <- list()

EJEC$tipo_ejecicion = 'Google Cloud'
EJEC$tipo_ejecicion = 'local'

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

# diff lag 6
dataset[, paste0(columnas_feature_eng, "_lag1") := lapply(.SD, function(col) (shift(col, type = "lag", n = 1))), by = numero_de_cliente, .SDcols = columnas_feature_eng]
dataset[, paste0(columnas_feature_eng, "_lag2") := lapply(.SD, function(col) (shift(col, type = "lag", n = 2))), by = numero_de_cliente, .SDcols = columnas_feature_eng]
dataset[, paste0(columnas_feature_eng, "_lag3") := lapply(.SD, function(col) (shift(col, type = "lag", n = 3))), by = numero_de_cliente, .SDcols = columnas_feature_eng]
dataset[, paste0(columnas_feature_eng, "_lag6") := lapply(.SD, function(col) (shift(col, type = "lag", n = 6))), by = numero_de_cliente, .SDcols = columnas_feature_eng]

dataset[, paste0(columnas_feature_eng, "_dlag1") := lapply(.SD, function(col) (col - shift(col, type = "lag", n = 1))), by = numero_de_cliente, .SDcols = columnas_feature_eng]
dataset[, paste0(columnas_feature_eng, "_rlag1") := lapply(.SD, function(col) ((col - shift(col, type = "lag", n = 1)))/col), by = numero_de_cliente, .SDcols = columnas_feature_eng]


dataset[, paste0(columnas_feature_eng, "_promedio_lag3") := lapply(.SD, function(col) {
  frollmean(col, n = 3, align = "right", fill = NA)
}), by = numero_de_cliente, .SDcols = columnas_feature_eng]

dataset[, paste0(columnas_feature_eng, "_promedio_lag6") := lapply(.SD, function(col) {
  frollmean(col, n = 6, align = "right", fill = NA)
}), by = numero_de_cliente, .SDcols = columnas_feature_eng]


#Para verificacion y visualizacion
#tst = dataset[, list(numero_de_cliente, foto_mes,
#                     active_quarter, active_quarter_lag1, active_quarter_dlag1, active_quarter_rlag1, active_quarter_lag2, active_quarter_lag3,active_quarter_lag6,active_quarter_promedio_lag6,active_quarter_promedio_lag3,
#                     Visa_mconsumototal, Visa_mconsumototal_lag1, Visa_mconsumototal_dlag1, Visa_mconsumototal_rlag1, Visa_mconsumototal_lag2, Visa_mconsumototal_lag3, Visa_mconsumototal_lag6, Visa_mconsumototal_promedio_lag6,Visa_mconsumototal_promedio_lag3)]
#View(tst)


fwrite(dataset,
       file = "./datasets/competencia_03_NAs_FE_6__dlag_rlag.csv.gz",
       sep = "\t"
)


# Imprime el timestamp

cat("Inicio de la ejecución: ", format(inicio, format = "%Y-%m-%d %H:%M:%S"), "\n")
cat("fin de la ejecución: ", format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), "\n")



# libero espacio
#rm(dataset)
#gc()


cat("\n\nEjecucion finalizada\n")
