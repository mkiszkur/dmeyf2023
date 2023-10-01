SEPARADOR = ","

sumar <- function(a, b) {
  return(a + b)
}


finalizar_corrida <- function(EJEC){
  
  EJEC$fin <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  linea <- paste(EJEC$fin, EJEC$corrida, "finalizacion", EJEC$fuente, , EJEC$nombre_maquina, sep = SEPARADOR)
  cat(linea, file = EJEC$archivo_log, "\n")
  
  close(EJEC$archivo_log)
  
  return(EJEC)
}
iniciar_corrida <- function(EJEC){
  
  #Crea el directorio de ejecuciones
  dir.create("./ejecuciones/", showWarnings = FALSE)
  
  system_info <- Sys.info()
  EJEC$nombre_maquina = system_info["nodename"]
  
  #Si no hay archivo de corridas lo crea  
  if (!file.exists(EJEC$nombre_archivo)) {
    # Si el archivo no existe, crearlo y establecer la corrida inicial en 1
    EJEC$corrida <- 1
  }else {
    # Si el archivo existe, busca la última corrida
    lineas <- readLines(EJEC$nombre_archivo)
    ultima_linea <- tail(lineas, 1)
    campos <- strsplit(ultima_linea, SEPARADOR)
    EJEC$corrida <- as.integer(campos[[1]][2]) + 1
    
  }  
  
  EJEC$archivo_log <- file(EJEC$nombre_archivo, open = "a+")
  seek(EJEC$archivo_log, where = -1, origin = "end")
  
  # Obtener la fecha y hora actual
  EJEC$inicio <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Componer la línea a escribir en el archivo
  linea <- paste(EJEC$inicio, EJEC$corrida, "Inicio", EJEC$fuente, EJEC$nombre_maquina, sep = SEPARADOR)
  
  # Escribir la línea en el archivo
  cat(linea, file = EJEC$archivo_log, "\n")
  
  return(EJEC)
}