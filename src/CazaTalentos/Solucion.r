# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory

#Semillas: 106703, 106721, 106727, 106739, 106747
semilla <- 106703



ejecutar_experimento <- function(cant_jugadoras, cant_tiros, ratio_encestes) {

#  set.seed(semilla)

  e <- rbinom(cant_jugadoras, cant_tiros, ratio_encestes)
  e
  tabla_frecuencia <- table(e)
  tabla_frecuencia
  maximo <- max(e)
  max_menos_uno <- maximo - 1
  max_menos_dos <- maximo - 2
  minimo <- min(e)
  media <- mean(e)
  mediana <- median(e)
  desviacion_estandar <- sd(e)
  cantidad_maximo <- sum(e == maximo)
  cantidad_minimo <- sum(e == minimo)
  cantidad_max_menos_uno <- sum(e == max_menos_uno)
  cantidad_max_menos_dos <- sum(e == max_menos_dos)
  
  
  
  return(c(maximo, minimo, media, mediana, desviacion_estandar, cantidad_maximo, cantidad_minimo, cantidad_max_menos_uno,cantidad_max_menos_dos ))
}

cant_experimemtos = 1000

experimentos <- data.frame(
  id = integer(0), # Agregamos una columna "ID"
  maximo = numeric(0),
  minimo = numeric(0),
  media = numeric(0),
  mediana = numeric(0),
  desviacion_estandar = numeric(0),
  cantidad_maximo = integer(0),
  cantidad_minimo = integer(0),
  cantidad_max_menos_uno = integer(0),
  cantidad_max_menos_dos = integer(0),
  caza_talentos = integer(0)
)


####Cazatalentos 1
caza_talentos = 1
for (i in 1:cant_experimemtos) {

  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(100, 100, 0.685)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))

}

colnames(experimentos) <- c("id", "maximo", "minimo", "media", "mediana", "desviacion_estandar", "cantidad_maximo", "cantidad_minimo", "cantidad_max_menos_uno", "cantidad_max_menos_dos", "caza_talentos")


#df_ct1 <- experimentos[experimentos$maximo == 80  & experimentos$caza_talentos == 1, ]
df_ct1 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 1, ]
df_ct1
print(nrow(df_ct1))


####Cazatalentos 2

caza_talentos = 2

for (i in 1:cant_experimemtos) {
  
  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(200, 100, 0.67)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))
} 

df_ct2 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 2, ]
df_ct2
print(nrow(df_ct2))


####Cazatalentos 3
caza_talentos = 3

for (i in 1:cant_experimemtos) {
  
  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(2, 100, 0.7)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))
} 
####Cazatalentos 4
caza_talentos = 4

for (i in 1:cant_experimemtos) {
  
  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(100, 10, 0.7)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))
} 


####Cazatalentos 5
caza_talentos = 5

for (i in 1:cant_experimemtos) {
  
  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(100, 100, 0.7)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))
} 
####Cazatalentos 6
caza_talentos = 6

for (i in 1:cant_experimemtos) {
  
  #en e el experimento. cant_jugadoras, cant_tiros, ratio_encestes
  e <- ejecutar_experimento(1, 100, 0.7)
  experimentos <- rbind(experimentos, c(i, e,caza_talentos))
} 

df_ct1 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 1, ]
print(nrow(df_ct1))


df_ct2 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 2, ]
print(nrow(df_ct2))


df_ct3 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 3, ]
print(nrow(df_ct3))

#df_ct4 <- experimentos[experimentos$maximo == 9 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 4, ]
df_ct4 <- experimentos[experimentos$maximo == 8 & experimentos$caza_talentos == 4, ]
print(nrow(df_ct4))


df_ct5 <- experimentos[experimentos$maximo == 85 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 5, ]
print(nrow(df_ct5))

df_ct6 <- experimentos[experimentos$maximo == 80 & experimentos$cantidad_maximo == 1 & experimentos$caza_talentos == 6, ]
print(nrow(df_ct6))

df_ct1


