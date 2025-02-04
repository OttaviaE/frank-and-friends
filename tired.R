# i'm just so tired, please have mercy (Cit. anyone during the administration of the MMPI)
library(tidyverse)
source("functions-new.R")
rbinom(1000,1, IRT(theta = 3, a = 1, b = 3))

nellie = function(parameters, target, speed = 0.01) {
  if (is.null(target)) {
    stop("I can't work without a target :)")
  }
  theta = target$theta
  all_iifs = item_info(parameters, theta)
  my_target = target
  sel_item = NULL
  token = TRUE
  distances_tif = NULL
  distances_pif = NULL
  original_parameters = parameters
  all_stfs = data.frame(matrix(nrow = 1, ncol = 3))
  colnames(all_stfs) = c("nitems", "items", "pif")
 # browser()
  for (i in 0:nrow(parameters)) {
    if (token == TRUE & i < nrow(parameters)) {
      if (i == 0) {
        my_target$tif_stf = 0
      } else {
        my_target$tif_stf = pif
      }
      my_target$distance = abs(my_target$mean_tif - my_target$tif_stf) 
      
      parameters$temp_theta = my_target[which(my_target$distance == max(my_target$distance)), "theta"]
      parameters$temp_distance = with(parameters, 
                                      abs(b - temp_theta))
      # attenzione eprché facendo così rischio di prendere di nuovo lo stesso item
      # potrei sostituire gli item con na
      index_item = which(parameters$temp_distance == min(parameters$temp_distance, na.rm = T))
      sel_item = c(sel_item, rownames(parameters)[index_item])
      if (i == 0) {
        selected_iifs = data.frame(all_iifs[, index_item])
        colnames(selected_iifs) = rownames(parameters)[index_item]
        pif = selected_iifs[,1]
        mypif = data.frame(nitems = 1, 
                           items = sel_item, 
                           pif = pif)
        all_stfs = rbind(all_stfs, mypif)
      } else {
        # devo ricaloclare le iifs con il parametro di stanchezza 
        original_parameters$e = exp(-speed*(ncol(selected_iifs)+1))
        my_iifs = item_info(original_parameters[index_item, ], theta)
        selected_iifs = cbind(selected_iifs, 
                              my_iifs)
        colnames(selected_iifs)[length(colnames(selected_iifs))] = rownames(parameters)[index_item]
        pif = rowMeans(selected_iifs)
        temp_pif = data.frame(nitems = ncol(selected_iifs), 
                              items = paste(colnames(selected_iifs), collapse = " "), 
                              pif = pif)
        all_stfs = rbind(all_stfs, temp_pif)
      }
      parameters[index_item, ] = NA
      # contiene le distanze tra la target e la tif stf (quelle che vanno confrontate con la pif)
      distances_tif =c(distances_tif, mean(abs(my_target$mean_tif - my_target$tif_stf))) 
      names(distances_tif)[length(distances_tif)] = paste("temp", i, sep = "_")
      distances_pif =c(distances_pif, mean(abs(my_target$mean_tif - pif))) 
      names(distances_pif)[length(distances_pif)] = paste("temp", i, sep = "_")
      # qua testa il criterio di uscita
      #   browser()
      if (i == 0) {
        token = TRUE
      } else if (distances_pif[i+1] >= distances_tif[i+1]) {
        token = FALSE
        warn_too_many = FALSE
      } else {
        token = TRUE
      }
    } else {
      if (all(is.na(parameters$b)) |length(sel_item) == nrow(parameters)) {
        warn_too_many = TRUE
      }
      sel_item = sel_item
      all_items = sel_item
    } 
  }
  # questo testa se ho selezionato tutti gli item, quindi non c'è la forma breve, ma comunque tutti gli item permettono 
  # di far diminuire la distanza
  if (warn_too_many == TRUE) {
    sel_item = sel_item
    warning("I ran out of items wwithout finding a STF")
  } else {
    warn_too_many = FALSE
    all_items = sel_item
    sel_item = all_items[-length(all_items)] 
  }
  if (length(sel_item) == 0) {
    warning("Apparently there were no items able to minimize the distance")
    warn_not_found = TRUE
  } else {
    temp_item = sel_item
    temp_item = as.numeric(gsub("item_", "", temp_item))
    temp_item = temp_item[order(temp_item)]
    sel_item = paste("item", temp_item, sep = "_")
    # sel_item = paste(sel_item, collapse = " ")
    warn_not_found = FALSE
  }
  my_target$n_stf = length(sel_item)
  my_target$item_stf = paste(sel_item, collapse = " ")
  results = list(q_ila = paste(sel_item, collapse = " "), 
                 all_items = all_items,
                 stf = my_target, 
                 distances_tif = distances_tif, 
                 distances_pif = distances_pif, 
                 all_stfs = all_stfs, 
                 warning = c(item_not_found = warn_not_found, stf_not_found = warn_too_many))
  return(results)
}
# riscrivo frank per la stanchezza 
leon = function(parameters, target, speed = 0.01) {
  all_iifs = item_info(parameters, theta)
  original_parameters = parameters
  token = TRUE
  mod_e = 1
  while (token == TRUE) {
    # qui potrei fare che itero per tutte le colonne che non sono NA del mio dataframe di item 
    # creo gli indici degli item nell'insieme A (gli available items)
    if (all(which(is.na(parameters[, 1])) == FALSE) ) {
      item_indexes = 1:nrow(parameters)
      iif_stf = numeric(length(target$theta))
    } else {
      item_indexes = which(!is.na(parameters[,1]))
    }
    # adesso il ciclo for itera negli item indexes e calcola la PIF
    difference = rep(NA, nrow(parameters))
    for (i in item_indexes) {
      if (length(item_indexes) == nrow(parameters)) {
        pif = data.frame(all_iifs[,i]) 
      } else {
        # qui devo mdoficare all_iifs nel senso che le devo ricalcolare 
        # però considerando la stanchezza
        # so dire esattamente 
        original_parameters$e = exp(-speed*(ncol(iif_stf)+1))
        # qui ora si può ottimizzare 
        all_iifs = item_info(original_parameters, theta)
        pif = data.frame(cbind(iif_stf, all_iifs[,i]))
        pif = data.frame(rowMeans(pif))
      }
      difference[i] = mean(abs(target$mean_tif - pif)[,1])  
      # qui scelgo un item temporaneo che è quello con la distanza minima
    }
    mod_e = c(mod_e, unique(original_parameters$e))
    # qui trovo l'item che minimizza e lo metto in d
    d_index = which(difference == min(difference, na.rm = T))
    # adesso devo testare il criterio di uscita dove prendo la pif con l'ìitem che
    # minimizza, faccio la differenza dalla target e guardo come si comporta 
    # rispetto alla distanza dalla target dello step precedente 
    # qui ho riscoruito la provisional tif 
    pif = data.frame(iif_stf, all_iifs[, d_index])
    if (all(pif[,1] == 0) ) {
      distance_target_tif = mean(abs(target$mean_tif - iif_stf))
      pif = pif[,-1]
      iif_stf = data.frame(pif)
      colnames(iif_stf) = paste("item", d_index, sep = "_")
    } else {
      distance_target_tif = mean(abs(target$mean_tif - rowMeans(iif_stf)))
      iif_stf = pif
      colnames(iif_stf)[ncol(iif_stf)] = paste("item", d_index, sep = "_")
    }
    # guardo le differenze 
    # effettivamente non c'è bisogno di mettere su tutto questo cinema mi dovrebbe 
    # bastare prendere la differenza già calcolata in difference 
    # if (difference[d_index] < distance_target_tif & all(which(is.na(parameters[, 1])) == FALSE) ) {
    #   token = TRUE
    #   parameters[d_index, ] = NA
    # } else 
    if (difference[d_index] >= distance_target_tif) {
      token = FALSE
      # sel_items = colnames(iif_stf)[-ncol(iif_stf)]
      # sel_items = sel_items[order(sel_items)]
      # sel_items = paste(sel_items, collapse = " ")
      temp_item = colnames(iif_stf)[-ncol(iif_stf)]
      temp_item = as.numeric(gsub("item_", "", temp_item))
      temp_item = temp_item[order(temp_item)]
      sel_items = paste("item", temp_item, sep = "_")
      sel_items = paste(sel_items, collapse = " ")
    } else {
      parameters[d_index, ] = NA
    }
  } 
  iif_stf = data.frame(iif_stf[,-ncol(iif_stf)])
  results = list(q_leon = sel_items, 
                 iif_stf = iif_stf, 
                 mod_e = mod_e)
  
  return(results)
}

# isa 
dc = function(parameters, target, speed = 0.01) {
  if (is.null(target)) {
    stop("I can't work without a target :)")
  }
  theta = target$theta
  all_iifs = item_info(parameters, theta)
  my_target = target
  sel_item = NULL
  token = TRUE
  distances_tif = NULL
  distances_pif = NULL
  #   browser()
  all_stfs = data.frame(matrix(nrow = 1, ncol = 3))
  colnames(all_stfs) = c("nitems", "items", "pif")
  for (i in 0:nrow(parameters)) {
    if (token == TRUE & i < nrow(parameters)) {
      if (i == 0) {
        my_target$tif_stf = 0
      } else {
        my_target$tif_stf = pif
      }
      my_target$distance = abs(my_target$mean_tif - my_target$tif_stf) 
      
      parameters$temp_theta = my_target[which(my_target$distance == max(my_target$distance)), "theta"]
      parameters$e =  exp(-speed*i)
      parameters$temp_iif = i_info(parameters[, "b"], parameters[, "a"], parameters[, "c"], parameters[, "e"], 
                                   theta = unique(parameters$temp_theta))
      index_item = which(parameters$temp_iif == max(parameters$temp_iif, na.rm = TRUE))
      sel_item = c(sel_item, rownames(parameters)[index_item])
      if (i == 0) {
        selected_iifs = data.frame(all_iifs[, index_item])
        colnames(selected_iifs) = rownames(parameters)[index_item]
        pif = selected_iifs[,1]
        mypif = data.frame(nitems = 1, 
                           items = sel_item, 
                           pif = pif)
        all_stfs = rbind(all_stfs, mypif)
      } else {
        selected_iifs = cbind(selected_iifs, 
                              all_iifs[, index_item])
        colnames(selected_iifs)[length(colnames(selected_iifs))] = rownames(parameters)[index_item]
        pif = rowMeans(selected_iifs)
        temp_pif = data.frame(nitems = ncol(selected_iifs), 
                              items = paste(colnames(selected_iifs), collapse = " "), 
                              pif = pif)
        all_stfs = rbind(all_stfs, temp_pif)
      }
      parameters[index_item, ] = NA
      # contiene le distanze tra la target e la tif stf (quelle che vanno confrontate con la pif)
      distances_tif =c(distances_tif, mean(abs(my_target$mean_tif - my_target$tif_stf))) 
      names(distances_tif)[length(distances_tif)] = paste("temp", i, sep = "_")
      distances_pif =c(distances_pif, mean(abs(my_target$mean_tif - pif))) 
      names(distances_pif)[length(distances_pif)] = paste("temp", i, sep = "_")
      # qua testa il criterio di uscita
      if (i == 0) {
        token = TRUE
      } else if (distances_pif[i+1] >= distances_tif[i+1]) {
        token = FALSE
        warn_too_many = FALSE
      } else {
        token = TRUE
      }
    } else {
      if (all(is.na(parameters$b)) |length(sel_item) == nrow(parameters)) {
        warn_too_many = TRUE
      }
      sel_item = sel_item
      all_items = sel_item
    } 
  }
  if (warn_too_many == TRUE) {
    sel_item = sel_item
    warning("I ran out of items wwithout finding a STF")
  } else {
    warn_too_many = FALSE
    all_items = sel_item
    sel_item = all_items[-length(all_items)] 
  }
  if (length(sel_item) == 0) {
    warning("Apparently there were no items able to minimize the distance")
    warn_not_found = TRUE
  } else {
    temp_item = sel_item
    temp_item = as.numeric(gsub("item_", "", temp_item))
    temp_item = temp_item[order(temp_item)]
    sel_item = paste("item", temp_item, sep = "_")
    # sel_item = paste(sel_item, collapse = " ")
    warn_not_found = FALSE
  }
  my_target$n_stf = length(sel_item)  
  my_target$item_stf = paste(sel_item, collapse = " ")
  results = list(q_isa =  paste(sel_item, collapse = " "), 
                 all_items = all_items,
                 stf = my_target, 
                 distances_tif = distances_tif, 
                 distances_pif = distances_pif, 
                 all_stfs = all_stfs, 
                 warning = c(item_not_found = warn_not_found, stf_not_found = warn_too_many)
  )
  return(results)
}
# item bank, 12 items with their diffiuclty and discirmianton parameters
set.seed(1312)
original_q = data.frame(b = runif(50, -3, 3), 
               a = runif(50, .9, 2), 
               c = rep(0,50), e= rep(1, 50))
rownames(original_q) = paste("item", 1:nrow(original_q), sep ="_")
original_q
theta = seq(-4, 4, length.out = 1000)
# ICC dellìtem bank senza errore 
plot(theta, IRT(theta, b= original_q[1, "b"], a = original_q[1, "a"]), type = "l")
for (i in 2:nrow(original_q)) {
  lines(theta, IRT(theta, b= original_q[i, "b"], a = original_q[i, "a"]), type = "l", col = i)
}

all_iif = item_info(original_q, theta)
tif = data.frame(theta, tif = rowSums(all_iif), 
                 mean_tif = rowMeans(all_iif))
# tif senza careless error 
plot(theta, tif$mean_tif, type = "l")

# metto la stanchezza  
# dove k è il parametro di velocità della stanchezza
# n è l'n item somministrato 
tired_q = original_q
tired_q$e = exp(-0.01*(0:(nrow(original_q)-1)))
tired_iif = item_info(tired_q, theta)
tired_tif = data.frame(theta, tired_tif = rowSums(tired_iif), 
                       mean_tired_tif = rowMeans(tired_iif))
plot(theta, tif$mean_tif, type = "l", main = "original vs tired")
lines(theta, tired_tif$mean_tired_tif, type = "l", col = "red")
legend("topright", c("TIF", "Tired TIF"), fill = c("black", "red"))

# la tif target che si vuole riprodurre è quella della item bank senza la stanchezza
mytarget = tif

# calcolo la tif per la item bank con stanchezza costante 
costante = original_q
costante$e = .95

# calcolo anche al tif stanca costante 
tiredc_iif = item_info(costante, theta)
mytarget$mean_tif_costante = rowMeans(tiredc_iif)

plot(theta, tif$mean_tif, type = "l", main = "original vs tired")
lines(theta, tired_tif$mean_tired_tif, type = "l", col = "red")
lines(theta, mytarget$mean_tif_costante, type = "l", col = "blue")

legend("topright", c("TIF", "Tired TIF", "constant tired TIF .95"), 
       fill = c("black", "red", "blue"))

# frank basic
# parametri originali, traget
baseFrank = frank(original_q, mytarget)
mytarget$baseFrank = rowMeans(baseFrank$iif_stf)

plot(mytarget$theta, mytarget$mean_tif, type = "l")
lines(mytarget$theta, mytarget$baseFrank, type = "l", col = "red")
lines(theta, tired_tif$mean_tired_tif, type = "l", col = "blue")

legend("topright", c("TIF", "Frank - original", "tired TIF"), 
       fill = c("black", "red", "blue"))


# stanchezza costante su frank 
costFrank = frank(costante, mytarget)
mytarget$cost_frank = rowMeans(costFrank$iif_stf)

plot(mytarget$theta, mytarget$mean_tif, type = "l", ylim = c(0, 0.5))
lines(mytarget$theta, mytarget$cost_frank, type = "l", col = "red")
lines(theta, mytarget$mean_tif_costante, type = "l", col = "blue")

legend("topright", c("TIF", "Frank - constant tired", "constant tired TIF"), 
       fill = c("black", "red", "blue"))

# leon si automette i oarametri di stanchezza 
firstleon = leon(original_q, mytarget)
speedLeon = leon(original_q, mytarget, speed = .10)
mytarget$leon = rowMeans(firstleon$iif_stf)
mytarget$speedLeon = rowMeans(speedLeon$iif_stf)
firstNellie = nellie(original_q, mytarget)
mytarget$nellie = firstNellie$stf$tif_stf
firstIla = ila(original_q,  mytarget)
firstIsa = isa(original_q,  mytarget)
firstDc = dc(original_q, mytarget, speed = 0.05)
# very tired 
verytired_q = original_q
verytired_q$e = exp(-0.10*(0:(nrow(original_q)-1)))
verytired_iif = item_info(verytired_q, theta)
verytired_tif = data.frame(theta, tired_tif = rowSums(verytired_iif), 
                       mean_tired_tif = rowMeans(verytired_iif))

# ila e nellie selezionano gli stessi identici item (ma per forza perché sono basate 
# sulle location)

plot(mytarget$theta, mytarget$mean_tif, type = "l", ylim= c(0, .4))
lines(mytarget$theta, mytarget$leon, type = "l", col = "red")
lines(theta, mytarget$speedLeon, type = "l", col = "salmon")
lines(theta, verytired_tif$mean_tired_tif, type = "l", col = "blue")
# sono una cretina perché devono tenere fissa la velocità del decadimento anche nella tired TIF 


legend("topright", c("TIF", "Leon", "tired TIF", "speed leon"), 
       fill = c("black", "red", "blue", "salmon"))
# ma chi è nellie? chiedo per un'amica 
mean(abs(mytarget$mean_tif - mytarget$leon))
mean(abs(mytarget$mean_tif - tired_tif$mean_tired_tif))


# questa è la tif dell'item banck senza l'influenza della stanchezza 

# ci sono diverse possibiliutà per definire la stanchezza, ovvero per capire se inizia a rompere le apll dal primo item, dal secondo 
# item eccetera. 
# forse la cosa più appropiata sarebbe una funzione esponenziale in funzione 
# del numero di item  ma questo è un problema per la me del futuro 
IRT(theta, b = q[1, "b"], a = q[1, "a"])
IRT(theta[1], b = q[1, "b"], a = q[1, "a"], e = .99)
# qui ho fatto il plot della ICC di un item che rimane fisso per quello che concerne 
# difficoltà e discriminatività ma che aumenta nella probabilità di careless error
# si vee chiaramente che la probabilità di rispondere correttamente all'item quando 
# theta = 1 diminuisce all'aumentarre della careless error ed è minore .50
plot(theta,  IRT(theta, b = q[1, "b"], a = q[1, "a"]), type = "l")
e = seq(.99, .90, by = -.02)
for (i in 1:length(e)) {
   lines(theta, IRT(theta, b = q[1, "b"], a = q[1, "a"], e = e[i]), type = "l", col = i)
 }
abline(a = .5, b = 0, lty = 2, lwd = 3, col = "red")
abline(v = q[1, "b"], lty = 2, lwd = 3, col = "red")

# plotto la IIF di questo stesso item 
plot(theta,  i_info(b = q[1, "b"], a = q[1, "a"], theta = theta), type = "l")
for (i in 1:length(e)) {
  lines(theta,  i_info(b = q[1, "b"], a = q[1, "a"],e = e[i], theta = theta), type = "l", col = i)
}
abline(v = q[1, "b"])
# la iif per il theta corrispondente al b dell'item è effettivamente leggermente più bassa e 
# traslata verso dx

#seq(1, .90, by = -0.03)
# metto un careless error fisso su tutti gli item 
q$e = rep(.95, 12)
# stessi item di prima ma con la stanchezza 
plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"], e = q[1, "e"]), 
     type = "l", ylim= c(0,1))
for (i in 2:nrow(q)) {
  lines(theta, IRT(theta, b= q[i, "b"], a = q[i, "a"], e = q[i, "e"],),  
        type = "l", ylim= c(0,1), col = i)
}
plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"], e = 1), 
     type = "l", ylim= c(0,1))
mye = seq(95, 85, by = -1 )
mye = mye/100
for (i in 1:length(mye)) {
  lines(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"], e = mye[i],),  
        type = "l", ylim= c(0,1), col = i)
}
abline(v = q[1, "b"])
abline(h = .5)

# ora calcolo la TIF con la stessa stanchezza su tutti gli item 
tired_iif = item_info(q, theta)
tired_tif = data.frame(theta, tired_tif = rowSums(tired_iif), 
                       mean_tired_tif = rowMeans(tired_iif))
plot(theta, tif$tif, type = "l", ylim = c(0, 5))
lines(theta, tired_tif$tired_tif, type = "l", col = "red")

plot(theta, all_iif$item_12, type = "l", ylim = c(0,1))
abline(v =q[12, "b"])
lines(theta, tired_iif$item_12, type = "l", col = "red")
# NON HA SENSO DIO PORCO DEVE SCENDERE NO SALIRE 

plot(theta, all_iif$item_11, type = "l")
abline(v =q[11, "b"])
lines(theta, tired_iif$item_11, type = "l", col = "red")

P = IRT(q[11, "b"], b = q[11, "b"], a = q[11, "a"], e = q[11, "e"])
Q = 1- p

P1 = IRT(q[11, "b"], b = q[11, "b"], a = q[11, "a"], e = 1, c = .20)
Q1 = 1- P1

myinfo = (q[11, "a"]^2)*P1*Q1
(q[11, "a"]^2)*(Q1/P1)*((P1 - 0)/(1 -0))^2
(q[11, "a"]^2)*(Q/P)*((P - 0)/(.95 -0))^2

IRT(1.5, a = 1, b = 1.5)

# deve funzionare dio bestia 
b = 1
theta = 1
a = 1.5
c = 0
e = .95

IRT(theta, a =a, b = b, c =c, e = e)

i_info(b, a, c,e, theta)

theta = seq(-3,3, length.out = 1000)
b = 1
a = 1.5
c = 0
e = 1

plot(theta, IRT(theta, a =a, b = b, c =c, e = e), type = "l", ylim = c(0,1))
abline(v = 1)
abline(h = .5)
lines(theta, IRT(theta, a =a, b = b, c =0.20, e = e), type = "l", col ="red")
abline(h = IRT(1, a =a, b = b, c =0.20, e = e), col = "red")
lines(theta, IRT(theta, a =a, b = b, c =0.20, e = .95), type = "l", col ="green")
abline(h = IRT(1, a =a, b = b, c =0.20, e = .95), col = "green")
lines(theta, IRT(theta, a =a, b = b, c =0, e = .95), type = "l", col ="royalblue")
abline(h = IRT(1, a =a, b = b, c =0.0, e = .95), col = "royalblue")
o
# IIF 
plot(theta, i_info(theta = theta, a =a, b = b, c =0, e = 1), type = "l", ylim = c(0,1))
abline(v = 1)
abline(h = i_info(1, a =a, b = b, c =c, e = e))
lines(theta, i_info(theta =theta, a =a, b = b, c =0.20, e = e), type = "l", col ="red")
abline(h = i_info(1.2, a =a, b = b, c =0.20, e = e), col = "red")
lines(theta, i_info(theta, a =a, b = b, c =0.20, e = .95), type = "l", col ="green")
abline(h = i_info(1.2, a =a, b = b, c =0.20, e = .95), col = "green")
lines(theta, i_info(theta, a =a, b = b, c =0, e = .95), type = "l", col ="royalblue")
abline(h = i_info(1, a =a, b = b, c =0.0, e = .95), col = "royalblue")

