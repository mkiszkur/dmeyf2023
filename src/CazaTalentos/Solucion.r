# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


setwd("/Users/mkiszkurno/Documents/dmeyf/") # Establezco el Working Directory

#Semillas: 106703, 106721, 106727, 106739, 106747
semilla <- 106703

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


ejecutar_experimento <- function(ratio_encestes, cant_jugadoras, cant_tiros) {

  set.seed(semilla)

  jugadoras <- rep(ratio_encestes, cant_jugadoras)

  vaciertos <- mapply(ftirar, jugadoras, cant_tiros) # 10 tiros libres cada jugador  


  #   primera_ganadora <- 0
  
  #   for (i in 1:10000) { # diez mil experimentos
  
  #     vaciertos <- mapply(ftirar, jugadoras, cant_tiros) # 10 tiros libres cada jugador
  
  #     mejor_ronda <- which.max(vaciertos)
  #     if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
  
  
  return(vaciertos)
}

# #Casatalentos 1:
 caza_talentos <- function (proba_mejor, cant_tiros, peloton) {

   set.seed(semilla)

   # defino los jugadores
   mejor <- proba_mejor

   jugadoras <- c(mejor, peloton)

   # veo que tiene el vector
   jugadoras

   # hago que los 100 jugadores tiren 10 veces cada uno
   mapply(ftirar, jugadoras, cant_tiros)

   primera_ganadora <- 0

   for (i in 1:10000) { # diez mil experimentos

     vaciertos <- mapply(ftirar, jugadoras, cant_tiros) # 10 tiros libres cada jugador

     mejor_ronda <- which.max(vaciertos)
     if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
   }


   paste0("Cantidad ganadora posta: ", primera_ganadora)
   paste0("probabilidad: ", primera_ganadora / 10000)

   return (primera_ganadora / 10000)

 }


# CazaTalentos2 <- function (proba_mejor, cant_tiros){

#     set.seed(106703)

#     # defino los jugadores
#     mejor <- proba_mejor
#     peloton <- c(rep(790,6),rep(789,5),rep(788,4),rep(787,3),rep(786,2), rep(785,179)) / 1000

#     jugadoras <- c(mejor, peloton)

#     # veo que tiene el vector
#     jugadoras

#     # hago que los 100 jugadores tiren 10 veces cada uno
#     mapply(ftirar, jugadoras, cant_tiros)

#     primera_ganadora <- 0

#     for (i in 1:10000) { # diez mil experimentos

#     vaciertos <- mapply(ftirar, jugadoras, cant_tiros) # 10 tiros libres cada jugador

#     mejor_ronda <- which.max(vaciertos)
#     if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
#     }


#     paste0 ("Cantidad ganadora posta: ", primera_ganadora)
#     paste0("probabilidad: ", primera_ganadora / 1000)


# }




# #CazaTalentos 1
# c1 = caza_talentos (0.8, 100, c(rep(790:742,2),741) / 1000) # probabilidad: 0.055

# #CazaTalentos 2
# c2 = caza_talentos (0.8, 100, c(rep(790,6),rep(789,5),rep(788,4),rep(787,3),rep(786,2), rep(785,179)) / 1000) # probabilidad: 0.0181

#de cada 100 tiros, cuantos encestes hace c1 y cuantos encestes hace c2

# de cada 100 veces, 5.5 c1 es el mejor de su tanda
# de cada 100 veces, 1.8 c2 es el mejor de su tanda
# de cada 100 veces 95, que no sabes
 
# P (#encestes c1 > #encestes c2 | tiraron 100 veces) > 0.5
   
 
  
# #CazaTalentos 3
#CazaTalentos (0.80, 100, 0.75) # 0.8296

# # #CazaTalentos 4
# CazaTalentos (0.90, 10, rep(800:768,3)) # 0.349

# #CazaTalentos 5
# proba1 <- CazaTalentos (0.85, 10, c(0.84,0.84,0.82,0.81)) # 0.4003

# proba2 <- CazaTalentos (0.69, 10, c(0.74,0.74,0.70,0.75)) # 0.2235

# proba3 <- CazaTalentos (0.70, 10, c(0.76,0.75,0.73,0.74)) # 0.2199

# print (proba1 * (1-proba2) * (1- proba3))

#ORdernar de la siguiente manera:
#Ci < Cj sii prob( #Encestes Ci < #Encestes Cj | 100 tiros cada uno ) > 0.5

#Comparar C1 y C2
#CazaTalentos (0.055, 100, 0.181 ) # 0.4003


experimentos <- data.frame(
  id = integer(0), # Agregamos una columna "ID"
  maximo = numeric(0),
  minimo = numeric(0),
  media = numeric(0),
  mediana = numeric(0),
  desviacion_estandar = numeric(0),
  cantidad_maximo = integer(0),
  cantidad_minimo = integer(0),
  caza_talentos = integer(0)
)


for (i in 1:1000) {

  #en e el experimento. ratio promedio de 0.7, cantidad de jugadoras = 100 y cantidad de tiros 100
  #e <- ejecutar_experimento(0.7, 100, 100)
  e <- rbinom(100, 100, 0.70)
  e
  tabla_frecuencia <- table(e)
  tabla_frecuencia
  maximo <- max(e)
  minimo <- min(e)
  media <- mean(e)
  mediana <- median(e)
  desviacion_estandar <- sd(e)
  cantidad_maximo <- sum(e == maximo)
  cantidad_minimo <- sum(e == minimo)
  experimentos <- rbind(experimentos, c(i, maximo, minimo, media, mediana, desviacion_estandar, cantidad_maximo, cantidad_minimo, 1))


}

colnames(experimentos) <- c("id", "maximo", "minimo", "media", "mediana", "desviacion_estandar", "cantidad_maximo", "cantidad_minimo", "caza_talentos")

experimentos
 
 
 
 
 k=0
 for (i in 1:10000){
   tirada<-rbinom(100, 100, 0.70)
   if(max(tirada)==80){k =k+1}
   
 }  
 print(k/10000)    
 
 
 
 
 k=0
 
 for (i in 1:10000){
   tirada<-rbinom(200, 100, 0.78)
   if(max(tirada)==80){k =k+1}
   if(m < max(tirada)){m = max(tirada)}
 }  
 
 print(k/10000)    
 
 
 
 
 k=0
 for (i in 1:10000){
   tirada<-rbinom(1, 100, 0.832)
   if(max(tirada)==80){k =k+1}
   
 }  
 print(k/10000)    
 
 
 
 
 k=0
 for (i in 1:10000){
   tirada<-rbinom(2, 100, 0.8)
   if(max(tirada)==80){k =k+1}
   
 }  
 print(k/10000)    
 
 
 k=0
 for (i in 1:10000){
   tirada<-rbinom(100, 10, 0.53)
   if(max(tirada) ==9){k =k+1}
   
 }  
 print(k/10000)