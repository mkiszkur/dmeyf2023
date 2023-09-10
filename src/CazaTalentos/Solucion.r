#Casatalentos 1:


CazaTalentos <- function (proba_mejor, cant_tiros, peloton){

    set.seed(106703)

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


    paste0 ("Cantidad ganadora posta: ", primera_ganadora)
    paste0("probabilidad: ", primera_ganadora / 10000)

    return (primera_ganadora / 10000)

}


CazaTalentos2 <- function (proba_mejor, cant_tiros){

    set.seed(106703)

    # defino los jugadores
    mejor <- proba_mejor
    peloton <- c(rep(790,6),rep(789,5),rep(788,4),rep(787,3),rep(786,2), rep(785,179)) / 1000

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


    paste0 ("Cantidad ganadora posta: ", primera_ganadora)
    paste0("probabilidad: ", primera_ganadora / 1000)


}


# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar <- function(prob, qty) {
  return(sum(runif(qty) < prob))
}


# #CazaTalentos 1
# CazaTalentos (0.8, 100, c(rep(790:742,2),741) / 1000) # 0.055

# #CazaTalentos 2
# CazaTalentos (0.8, 100, c(rep(790,6),rep(789,5),rep(788,4),rep(787,3),rep(786,2), rep(785,179)) / 1000) # 0.181

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
CazaTalentos (0.055, 100, 0.181 ) # 0.4003