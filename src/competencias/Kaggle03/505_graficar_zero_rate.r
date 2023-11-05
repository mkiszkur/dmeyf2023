# Este script genera graficos que muestra que para algunos meses,
#  ciertas variables #  fueron pisadas con CEROS por el sector de
#  IT que genera el DataWarehouse

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_03.csv.gz"
PARAM$experimento <- "CA5050"
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("/Users/miguelkiszkurno/Documents/dmeyf") 

# cargo el dataset
dataset <- fread(PARAM$dataset) # donde entreno


# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))


# ordeno el dataset
setorder(dataset, foto_mes, numero_de_cliente)

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria")
)


#------------------------------------------------------------------------------
# Para cada variable ,
# grafico para cada mes el ratio de ceros que tiene esa variable
# el zeroes_ratio de una variable para un mes dado
# es el cociente entre
#   la cantidad de veces que la variable toma el valor cero ese mes
#   y la cantidad total de registros para ese mes


#aca voy a guardar los fotomes y campos que tienen todos zeros
data_table_consolidada <- data.table(
  campo = character(0),
  foto_mes = integer(0),
  zero_ratio = numeric(0)
)


for (campo in campos_buenos) {
  
  tbl <- dataset[
    ,
    list("zero_ratio" = sum(get(campo) == 0, na.rm = TRUE) / .N),
    foto_mes
  ]

  tbl[, campo := campo]
  
  tbl_filtrado <- tbl[zero_ratio == 1]
  
  data_table_consolidada <- rbind(data_table_consolidada, tbl_filtrado)
  
}
  archivo_zero_ratios <- "./cero_ratios.csv"
  
  
  fwrite(data_table_consolidada,
         file = archivo_zero_ratios,
         sep = ","
  )  

  for (i in 1:nrow(data_table_consolidada)) {
    
    # Obtén los valores del registro actual en data_table_consolidada
    campo_actual <- data_table_consolidada$campo[i]
    foto_mes_actual <- data_table_consolidada$foto_mes[i]
    zero_ratio_actual <- data_table_consolidada$zero_ratio[i]
    
    dataset[foto_mes == foto_mes_actual, (campo_actual) := NA]
    
  }
  
  fwrite(dataset,
         file = "./competencia_03_NAs.csv.gz",
         sep = "\t"
  )
  


for (campo in campos_buenos) {
  
  tbl <- dataset[
    ,
    list("nas_ratio" = sum(is.na(get(campo))) / .N),
    foto_mes
  ]
  
  tbl[, campo := campo]
  
  tbl_filtrado <- tbl[nas_ratio == 1]
  
  data_table_consolidada <- rbind(data_table_consolidada, tbl_filtrado)
  
}  
  archivo_zero_ratios <- "./nas_ratios.csv"
  
  
  fwrite(data_table_consolidada,
         file = archivo_zero_ratios,
         sep = ","
  )  
  
