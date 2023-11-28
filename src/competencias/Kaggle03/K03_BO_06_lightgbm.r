# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

#-----------------
#Funciones Auxiliares

escribir_archivo <- function(archivo, tabla, separador = ",") {
  
  fwrite(tabla,
         file = archivo,
         sep = separador
  )
  
  
}  



# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()

#Experimento <- Experimento Colabortivo Prueba 004 (50 semillas)
PARAM$experimento <- "K03_LGBM_006"

PARAM$input$dataset <- "./datasets/competencia_03_NAs_FE_6__dlag_rlag.csv.gz"
#PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"


# meses donde se entrena el modelo
PARAM$input$training <- c(202003, 202004, 202005, 202006, 202007,
                          202008, 202009, 202011, 202012, 
                          202101, 202102, 202103, 202104, 202105, 202106)

PARAM$input$future <- c(202107) # meses donde se aplica el modelo
PARAM$input$kaggle <- c(202109) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 106703

PARAM$finalmodel$optim$num_iterations <- 122
PARAM$finalmodel$optim$learning_rate <- 0.197259002925382
PARAM$finalmodel$optim$feature_fraction <- 0.231056192477788
PARAM$finalmodel$optim$num_leaves <- 975
PARAM$finalmodel$optim$min_data_in_leaf <- 23467
PARAM$finalmodel$optim$neg_bagging_fraction <- 0.570557387490192
  

# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  #neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0

  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0

  extra_trees = TRUE, # Magic Sauce

  seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

#TODO: implementar test de Wilcoxon
PARAM$semillerio <- c(100189, 100193, 100207, 100213, 100237, 
                      100267, 100271, 100279, 100291, 100297)
                      
#, 100313, 100333, 100343, 100357, 100361, 100363, 100379, 100391,  100393, 100403, 100411)

#PARAM$semillerio <- c(100189,100193)
                      

PARAM$tipo_ejecucion = 'Google Cloud'
#PARAM$tipo_ejecucion = 'local'

if (PARAM$tipo_ejecucion == 'local') {
  setwd("/Users/miguelkiszkurno/Documents/dmeyf") 
}else {
  setwd("~/buckets/b1/") # Establezco el Working Directory
}


# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
realidad <- dataset[foto_mes == PARAM$input$future, list(numero_de_cliente, foto_mes, clase_ternaria) ]
realidad[, real := ifelse(clase_ternaria %in% c("CONTINUA", "BAJA+1"), 0L, 1L) ]

campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

# aplico el modelo a los datos sin clase
dapply <- dataset[foto_mes == PARAM$input$future]

dapply_kaggle <- dataset[foto_mes == PARAM$input$kaggle]


#Genero dataset de ceros para acumular las probabilidades
sumarizacion <- dapply[, list(numero_de_cliente, foto_mes)]
sumarizacion[, prob := 0]

sumarizacion_kaggle <- dapply_kaggle[, list(numero_de_cliente, foto_mes)]
sumarizacion_kaggle[, prob := 0]

#if (file.exists(paste0(PARAM$experimento, ".RData"))) {
#  print("------EXISTE ------")
#  load("C:/ ... Your Path ... /all_data.RData")
#}

  

for (s in PARAM$semillerio){
  
  cat(format(Sys.time(), format = "%Y-%m-%d %H:%M:%S"), " - Inicio modelo de la semilla: ", s, "\n")
  
  # reemplazar la semilla con la siguiente semilla
  PARAM$finalmodel$lgb_basicos$seed = s
  
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)

  # genero el modelo
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )

  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  escribir_archivo(paste0 (s, "_impo.txt"), tb_importancia)

  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  prediccion_kaggle <- predict(
    modelo,
    data.matrix(dapply_kaggle[, campos_buenos, with = FALSE])
  )

  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]
  
  sumarizacion[, prob := prob + prediccion]

  # genero la tabla de entrega Kaggle
  tb_entrega_kaggle <- dapply_kaggle[, list(numero_de_cliente, foto_mes)]
  tb_entrega_kaggle[, prob := prediccion_kaggle]
  sumarizacion_kaggle[, prob := prob + prediccion_kaggle]
  
  # grabo las probabilidad del modelo
  escribir_archivo(paste0 (s, "_prediccion.txt"), tb_entrega)
  
  escribir_archivo(paste0 (s, "_prediccion_kaggle.txt"), tb_entrega_kaggle)
  save.image(paste0(PARAM$experimento, ".RData"))
  
}

# grabo las sumas de las probabilidades
escribir_archivo("prediccion_total.txt", sumarizacion)

escribir_archivo("prediccion_total_kaggle.txt", sumarizacion_kaggle)

#Me guardo el verdadero valor (1 si era baja+2, 0 sino)
sumarizacion$real <- realidad$real

# ordeno por probabilidad descendente
setorder(sumarizacion, -prob)

setorder(sumarizacion_kaggle, -prob)

# genero archivos con los "envíos" mejores
cortes <- seq(8000, 15000, by = 500)
ganancias_totales <- data.frame(seq(length(cortes)), cortes)
ganancias_totales$ganancia = 0

for (envios in cortes) {
  
  #Calculo los que tengo que enviar en funcion del corte actual
  sumarizacion[, Predicted := 0L]
  sumarizacion[1:envios, Predicted := 1L]

  #Calculo la ganancia
  ganancias  <- sumarizacion [Predicted == 1]
  ganancias [, gan := ifelse(Predicted == 1 & real == 1, 273000, -7000)]
  ganancias [, gan_acum := cumsum(gan)]
  
  ganancia <- ganancias[.N, gan_acum]
  cat ("ganancia corte", envios, ": ",  ganancia, "\n")
  
  ganancias_totales[which(ganancias_totales$cortes == envios), "ganancia"] <- ganancia
  
  #Escribo el archivo con la ganancia
  escribir_archivo(paste0 (PARAM$experimento, "_", envios, "_ganancias.csv"), ganancias)
  
  
  #Escribo el archivo
  escribir_archivo(paste0 (PARAM$experimento, "_", envios, ".csv"), sumarizacion[, list(numero_de_cliente, Predicted)])
  
  # Ahora kaggle
  
  sumarizacion_kaggle[, Predicted := 0L]
  sumarizacion_kaggle[1:envios, Predicted := 1L]
  escribir_archivo(paste0 (PARAM$experimento, "_", envios, "_kaggle.csv"), sumarizacion_kaggle[, list(numero_de_cliente, Predicted)])
  
  
}

escribir_archivo(paste0 (PARAM$experimento, "_ganancias_totales_envios.csv"), ganancias_totales)


cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")