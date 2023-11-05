# Función para verificar si un número es primo


es_primo <- function(n) {
  if (n <= 1) return(FALSE)
  if (n <= 3) return(TRUE)
  if (n %% 2 == 0 || n %% 3 == 0) return(FALSE)
  i <- 5
  while (i * i <= n) {
    if (n %% i == 0 || n %% (i + 2) == 0) return(FALSE)
    i <- i + 6
  }
  return(TRUE)
}

setwd("/Users/miguelkiszkurno/Documents/dmeyf") 

# Número inicial mayor que 999983
hasta_numero = 999983
desde_numero = 100003

numero = desde_numero

# Lista para almacenar los números primos
numeros_primos <- c()

# Bucle para encontrar los 200 números primos
while (numero < hasta_numero) {
  numero <- numero + 1
  if (es_primo(numero)) {
    numeros_primos <- c(numeros_primos, numero)
    cat(numero, "\n")
  }
}

# Imprimir la lista de números primos
print(numeros_primos)

tb_semillas <- as.data.table(numeros_primos)
archivo_importancia <- "./datasets/semillas.txt"

fwrite(tb_semillas,
       file = archivo_importancia,
       sep = "\t"
)



