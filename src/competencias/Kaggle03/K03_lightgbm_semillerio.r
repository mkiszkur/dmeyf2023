# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

inicio <- Sys.time()
cat("Inicio de la ejecución: ", format(inicio, format = "%Y-%m-%d %H:%M:%S"), "\n")

require("data.table")
require("lightgbm")
#install.packages("rjson")
library(rjson)


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "KA03_S"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
#PARAM$input$training <- c(202006, 202007, 202008, 202009, 202010, 202011, 202012, 202101, 202102, 202103, 202104, 202105)
PARAM$input$training <- c(202105)
PARAM$input$future <- c(202107) # meses donde se aplica el modelo

PARAM$finalmodel$semilla <- 102191

# hiperparametros intencionalmente NO optimos
PARAM$finalmodel$optim$num_iterations <- 730
PARAM$finalmodel$optim$learning_rate <- 0.0323601846272594
PARAM$finalmodel$optim$feature_fraction <- 0.909773795582897
PARAM$finalmodel$optim$min_data_in_leaf <- 4637
PARAM$finalmodel$optim$num_leaves <- 667


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir dart, ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar, por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO

  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
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

#PARAM$semillerio <- c(100193, 100207, 100213, 100237, 100267 ,100271, 100279, 100291
#                     ,100297 ,100313 ,100333 ,100343 ,100357 ,100361 ,100363 ,100379
#                     ,100391 ,100393 ,100403 ,100411 ,100417 ,100447 ,100459 ,100469
#                     ,100483 ,100493 ,100501 ,100511 ,100517 ,100519)
PARAM$semillerio <- c(100193,100207)

#PARAM$tipo_ejecucion = 'Google Cloud'
PARAM$tipo_ejecucion = 'local'
PARAM$fecha_hora_inicio = format(inicio, format = "%Y-%m-%d %H:%M:%S")

if (PARAM$tipo_ejecucion == 'local') {
  setwd("/Users/miguelkiszkurno/Documents/dmeyf") 
}else {
  setwd("~/buckets/b1/") # Establezco el Working Directory
}

#--------------------------------------
# creo las carpetas donde van los resultados
dir.create("./exp/", showWarnings = FALSE)

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)


#Guardo los datos que use para la BO en un archivo de parametros
archivo <- paste0("./exp/", PARAM$experimento, "/parametros_inicio.json")

json_data <- toJSON(PARAM)
writeLines(json_data, con = archivo)

cat("Los datos se han guardado en el archivo:", archivo, "\n")


# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)


###### --- Aca empieza con el modelo. tengo que poner el for



sumarizacion <- numeric(nrow(dataset[foto_mes == PARAM$input$future]))

for (s in PARAM$semillerio){
  
  cat(format(inicio, format = "%Y-%m-%d %H:%M:%S"), " - Inicio modelo de la semilla: ", s, "\n")


  #reemplazar la semilla con la siguiente semilla
  PARAM$finalmodel$lgb_basicos$seed = s
  
  
  # genero el modelo
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      PARAM$finalmodel$optim)
  
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo,
  )
  
  #--------------------------------------
  # ahora imprimo la importancia de variables
  tb_importancia <- as.data.table(lgb.importance(modelo))
  archivo_importancia <- paste0 (s, "_impo.txt")
  
  fwrite(tb_importancia,
         file = archivo_importancia,
         sep = "\t"
  )

  # aplico el modelo a los datos sin clase
  dapply <- dataset[foto_mes == PARAM$input$future]
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    data.matrix(dapply[, campos_buenos, with = FALSE])
  )
  
  sumarizacion = sumarizacion + prediccion
  
  # genero la tabla de entrega
  tb_entrega <- dapply[, list(numero_de_cliente, foto_mes)]
  tb_entrega[, prob := prediccion]

    # grabo las probabilidad del modelo
  archivo_prediccion <-  paste0 (s, "_prediccion.txt")
  
  fwrite(tb_entrega,
         file = archivo_prediccion,
         sep = "\t"
  )
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  
  # genero archivos con los "envios" mejores
  # deben subirse "inteligentemente" a Kaggle para no malgastar submits
  # si la palabra inteligentemente no le significa nada aun
  # suba TODOS los archivos a Kaggle
  # espera a la siguiente clase sincronica en donde el tema sera explicado
  
  cortes <- seq(8000, 15000, by = 500)
  for (envios in cortes) {
    tb_entrega[, Predicted := 0L]
    tb_entrega[1:envios, Predicted := 1L]
    
    fwrite(tb_entrega[, list(numero_de_cliente, Predicted)],
           file = paste0(PARAM$experimento, "_",s,"_", envios, ".csv"),
           sep = ","
    )
  }

}

tb_entrega_total <- dapply[, list(numero_de_cliente, foto_mes)]
tb_entrega_total[, prob := sumarizacion]

archivo_prediccion <-  "prediccion_total.txt"

fwrite(tb_entrega_total,
       file = archivo_prediccion,
       sep = "\t"
)



cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")
