# Este script genera clustering en bajas y los graficos de las variables

# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")
require("ggplot2")
require("RColorBrewer")
require("progress")

# Parametros del script
PARAM <- list()
PARAM$dataset <- "./datasets/competencia_02.csv.gz"
PARAM$dataset_clusters <- "./exp/RF-CLUST0001/RF-CLUST0001_competencia_02_b2_clusters.csv.gz"
PARAM$plot_output <- "competencia_02_b2_clusters_variables.pdf"
PARAM$experimento <- "RF-CLUST0002"
PARAM$max_lag <- -18
PARAM$undersampling <- 0.1
# FIN Parametros del script

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

# copio si hace falta el dataset

set.seed(106703)

print("Cargando dataset")
# cargo el dataset
setwd("/Users/miguelkiszkurno/Documents/dmeyf") 

dataset <- fread(PARAM$dataset) 
#dataset <- fread("./datasets/historico_fallecidos.csv.gz") 
dataset_clusters <- fread("./datasets/clusters.csv")

setnames(dataset_clusters, old = "Cluster", new = "cluster")


print("Haciendo transformaciones")
dataset[dataset_clusters, on = "numero_de_cliente", cluster := i.cluster]

# clientes unicos
unique_continua_clients <- unique(dataset[is.na(cluster), ], by = "numero_de_cliente")
unique_continua_clients[, azar := runif(nrow(unique_continua_clients)),]

dataset[
  unique_continua_clients,
  on = "numero_de_cliente",
  azar := i.azar
]
dataset[
  is.na(cluster) &
    (azar <= PARAM$undersampling),
  cluster := -1L
]
dataset <- dataset[!is.na(cluster), ]

# ordeno el dataset
dataset <- dataset[order(numero_de_cliente, foto_mes), ]

# obtengo la fecha de ultima foto para armar la columna lag
dataset[, last_seen := max(foto_mes), by = numero_de_cliente]

dataset[, lag :=
          (as.integer(foto_mes / 100) - as.integer(last_seen / 100)) * 12 +
          (as.integer(foto_mes) %% 100) - (as.integer(last_seen) %% 100), ]

dataset <- dataset[lag >= PARAM$max_lag, ]
dataset <- dataset[cluster %in% c(4,5,6,7), ]

# creo la carpeta donde va el experimento
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)
# Establezco el Working Directory DEL EXPERIMENTO
setwd(paste0("./exp/", PARAM$experimento, "/"))

campos_buenos <- setdiff(
  colnames(dataset),
  c("numero_de_cliente", "foto_mes", "clase_ternaria", "cluster")
)

campos_buenos <- c(
  "cliente_vip",
  "mrentabilidad",
  "mcuenta_debitos_automaticos",
  "chomebanking_transacciones",
  "cpagodeservicios",
  "mpagodeservicios",
  "cpagomiscuentas",
  "mplazo_fijo_pesos",
  "mplazo_fijo_dolares",
  "ctransferencias_recibidas",
  "ctransferencias_emitidas",
  "ccheques_emitidos_rechazados",
  "ccheques_emitidos",
  "ccheques_depositados_rechazados",
  "mcheques_emitidos_rechazados",
  "mprestamos_hipotecarios",
  "cprestamos_hipotecarios",
  "cseguro_auto",
  "minversion1_dolares",
  "minversion1_pesos",
  "ccaja_seguridad",
  "mpayroll",
  "mpayroll2",
  "cpayroll2_trx",
  "Master_mconsumosdolares",
  "Master_mpagado",
  "Master_mpagosdolares",
  "Master_delinquency",
  "Master_madelantopesos",
  "mtarjeta_master_descuentos",
  "ctarjeta_master_debitos_automaticos",
  "mttarjeta_master_debitos_automaticos",
  "ctarjeta_master_transacciones",
  "mtarjeta_master_consumo",
  "ctarjeta_visa_transacciones",
  "mtarjeta_visa_consumo",
  "ctarjeta_visa_debitos_automaticos",
  "Visa_mpagominimo",
  "Visa_delinquency",
  "Visa_mconsumospesos",
  "Visa_mpagosdolares",
  "Visa_cconsumos",
  "mttarjeta_visa_debitos_automaticos",
  "cforex",
  "mforex_sell",
  "cforex_buy",
  "mforex_buy",
  "cforex_sell"
)

# Use the RColorBrewer package to choose a palette
len_clusters <- length(unique(dataset$cluster))
dataset[, cluster_name := ifelse(cluster == -1, "CONTINUA", as.character(cluster))]

print("Creando plots")
pb <- progress_bar$new(
  format = "  creando plots [:bar] :percent eta: :eta",
  total = length(campos_buenos), clear = FALSE, width = 60)

options(warn = -1)
print("Cluster sizes")
print(dataset[, .(cluster_name, count = .N), by = cluster_name])

pdf(PARAM$plot_output)
for (campo in campos_buenos) {
  plt <- ggplot(dataset, aes(x = lag, !!sym(campo), group = cluster_name, colour = cluster_name,)) +
    geom_smooth() +
    ggtitle(paste(campo, " by cluster")) +
    scale_fill_brewer(name = "Cluster", palette="Dark2")
  print(plt)
  pb$tick()
}
dev.off()