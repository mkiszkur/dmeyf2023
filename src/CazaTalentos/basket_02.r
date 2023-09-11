

set.seed(106703)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# defino los jugadores

jugadoras <- c(rep(70, 100) / 1000)


cant_tiros <- 100


# hago que los 100 jugadores tiren 10 veces cada uno
mapply(ftirar, jugadoras, cant_tiros)

primera_ganadora <- 0

for (i in 1:10000) { # diez mil experimentos

  vaciertos <- mapply(ftirar, jugadoras, cant_tiros) # 10 tiros libres cada jugador

  mejor_ronda <- which.max(vaciertos)
  if (mejor_ronda == 1) primera_ganadora <- primera_ganadora + 1
}


paste0 ("Cantidad ganadora posta: ", primera_ganadora)
paste0("probabilidad: ", primera_ganadora / 1000)

