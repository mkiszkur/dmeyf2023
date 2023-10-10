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

PARAM$experimento <- "HT8280"

PARAM$input$dataset <- "./datasets/competencia_02_recortado.csv.gz"
#PARAM$input$dataset <- "./datasets/competencia_02.csv.gz"
#PARAM$input$dataset <- "./datasets/_6_meses_competencia_02.csv.gz"

# los meses en los que vamos a entrenar
#  mucha magia emerger de esta eleccion
PARAM$input$testing <- c(202105)
PARAM$input$validation <- c(202104)
PARAM$input$training <- c(202010, 202011, 202012, 202101, 202102, 202103)

# un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$undersampling <- 1.0
PARAM$trainingstrategy$semilla_azar <- 106703 # Aqui poner su  primer  semilla

PARAM$hyperparametertuning$POS_ganancia <- 273000
PARAM$hyperparametertuning$NEG_ganancia <- -7000

# Aqui poner su segunda semilla
PARAM$lgb_semilla <- 106721
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# graba a un archivo los componentes de lista
# para el primer registro, escribe antes los titulos

loguear <- function(
    reg, arch = NA, folder = "./exp/",
    ext = ".txt", verbose = TRUE) {
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) # Escribo los titulos
    {
      linea <- paste0(
        "fecha\t",
        paste(list.names(reg), collapse = "\t"), "\n"
      )

      cat(linea, file = archivo)
    }

  linea <- paste0(
    format(Sys.time(), "%Y%m%d %H%M%S"), "\t", # la fecha y hora
    gsub(", ", "\t", toString(reg)), "\n"
  )

  cat(linea, file = archivo, append = TRUE) # grabo al archivo

  if (verbose) cat(linea) # imprimo por pantalla
}
 
 

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

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

#install.packages("randomForest")
library(randomForest)

#install.packages("cluster")
library(cluster)

rf.fit <- randomForest(x = dataset, y = NULL, ntree = 10000, proximity = TRUE, oob.prox = TRUE)
hclust.rf <- hclust(as.dist(1-rf.fit$proximity), method = "ward.D2")
rf.cluster = cutree(hclust.rf, k=3)
iris.pc$rf.clusters <- rf.cluster
table(rf.cluster, iris$Species)



#A veces quiero probar corridas sin entrenar
if (EJEC$frenar_corrida) {
  EJEC = finalizar_corrida (EJEC)
  stop("mensaje_error")
}



# paso la clase a binaria que tome valores {0,1}  enteros
dataset[, clase01 := ifelse(clase_ternaria == "CONTINUA", 0L, 1L)]



# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
    ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
      ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)



# defino los datos que forman parte de validation
#  no hay undersampling
dataset[, validation := 0L]
dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[validation == 1L, clase01],
  weight = dataset[validation == 1L, 
    ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
      ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

# Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar <- EstimarGanancia_lightgbm # la funcion que voy a maximizar

cat("\n\nLa optimizacion Bayesiana ha terminado\n")
EJEC = finalizar_corrida (EJEC)
