# provo a simulare tutto da zero sulla stanchezza, facendo sia la selezione 
# degli item sia i bias dei theta 
library(TAM)
library(tidyverse)
source("functions-new.R")
# riscrivo frank per la stanchezza 
leon = function(parameters, target, speed = 0.01) {
  theta = target$theta
  all_iifs = item_info(parameters, theta)
  original_parameters = parameters
  start_e = parameters$e
  token = TRUE
  mod_e = 1
  j = 0
  while (token == TRUE) {
    j = j +1
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
        # CAMBIA QUI METTENDO CHE PESCA LA E DEGLI ITEM ORIGIANLI IN ABSE ALLA LORO POSIZIONE DI SOMMNISTRAZIONE 
        # PER CUI ALL'I-ESIMA ITERAZIONE PRENDE LA STANCHEZZA DELL'ITEM SOMMINISTRATO PER I-ESIMO
       # original_parameters$e = exp(-speed*(ncol(iif_stf)+1))
        original_parameters$e = start_e[j]
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
n_item =10
set.seed(1312)
original_q = data.frame(b = runif(n_item, -2, 2), 
                        a = runif(n_item, .9, 2), 
                        c = rep(0,n_item), e= rep(1, n_item))
rownames(original_q) = paste("item", 1:nrow(original_q), sep ="_")
original_q
true_theta = rnorm(1000, mean=0, sd=1)
true_theta = true_theta[order(true_theta)] 
hist(true_theta)
summary(true_theta)
# la tif target è quella che io otterei somministrando i n item se non ci fosse la stanchezza 
al_iifs = item_info(original_q, theta = seq(-4,4, length.out = 1000))
tif = data.frame(theta = seq(-4,4, length.out = 1000), 
                        mean_tif = rowMeans(al_iifs))
plot(tif$theta, tif$mean_tif, type = "l", lwd = 2, main = "TIF traget 50 item")
# stimo theta, tenendo fissi i parametri degli item 
# DATI SIMULATI SENZA STANCHEZZA  -----
data = data.frame(matrix(nrow = length(true_theta), 
                         ncol = nrow(original_q)))
colnames(data) = rownames(original_q)
for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
    data[i, j] = rbinom(1,1, IRT(true_theta[i],b = original_q[j, "b"], 
                                 a = original_q[j, "a"], 
                                 c = original_q[j, "c"],original_q[j, "e"]))
  }
}
colMeans(data)
original_q
# THETA STIMATO SENZA STANCHEZZA ----
diff_true <- matrix(cbind(1:length(original_q$b), 
                          original_q$b), 
                    ncol = 2)
discr_true = array(c(rep(0, length(original_q$a)), original_q$a), 
                   c(length(original_q$a),2,1), 
                   dimnames = list(paste0("I", 1:length(original_q$a)), 
                                   c("Cat0", "Cat1"), 
                                   "Dim01"))

m2pl <- tam.mml(resp=data, xsi.fixed = diff_true, 
                B = discr_true)
summary(m2pl)

plot( m2pl$person$EAP, true_theta, asp = 1, 
      main = "Theta stimato su tutti gli item, no stanchezza")
abline(0, 1, col = "red", lwd = 2)
abline(lm(true_theta ~ m2pl$person$EAP), lwd = 2)
legend(x = -2, y = 1, 
       legend = c("Theta stimato", "Attesa"), 
       fill = c("black", "red"))

# applcia Frank su dati SENZA STANCHEZZA -----
resFrank = frank(original_q, tif)
resFrank$q_frank
q_frank = as.numeric(gsub("item_", "", colnames(resFrank$iif_stf)))
tif_frank = data.frame(theta = seq(-4,4, length.out=1000), mean_frank = rowMeans(resFrank$iif_stf))
# ristimo il modello 2pl selezionando solo gli item che dice frank 
# (c'è da ridere per la selezione delle discriminatività)
diff_frank = diff_true[q_frank, ]
diff_frank[,1] = 1:nrow(diff_frank)
discr_frank = array(c(rep(0, length(original_q$a[q_frank])), 
                      original_q$a[q_frank]), 
                    c(length(original_q$a[q_frank]),2,1), 
                    dimnames = list(paste0("I", 1:length(q_frank)), 
                                    c("Cat0", "Cat1"), 
                                    "Dim01"))
m2pl_frank = tam.mml(resp=data[,q_frank], 
                     xsi.fixed = diff_frank, 
                     B = discr_frank)
summary(m2pl_frank)
plot( m2pl$person$EAP, true_theta, asp = 1,
      main = "Theta stimato su tutti gli item, no stanchezza")
abline(0, 1, col = "red", lwd = 2)
abline(lm(true_theta ~ m2pl$person$EAP), lwd = 2)
points(m2pl_frank$person$EAP, true_theta, col = "green", pch = 3)
abline(lm(true_theta ~ m2pl_frank$person$EAP), lwd = 2, col = "green")

legend(x = -6, y = 1, 
       legend = c("Theta stimato", "Attesa", "Frank"), 
       fill = c("black", "red", "green"))

# Mette la stanchezza ---- 
tired_q = original_q
# per iniziare metto una stanchezza costante (quindi devo usare frank)
tired_q$e =  exp(-0.01*(0:(nrow(original_q)-1)))

# genera i dati delle persone stanche ----
dataFixed = data.frame(matrix(nrow = length(true_theta), 
                                   ncol = nrow(tired_q)))
colnames(dataFixed) = rownames(tired_q)
for (i in 1:nrow(dataFixed)) {
  for (j in 1:ncol(dataFixed)) {
    dataFixed[i, j] = rbinom(1,1, IRT(true_theta[i],b = tired_q[j, "b"], 
                                      a = tired_q[j, "a"], 
                                      c = tired_q[j, "c"],tired_q[j, "e"]))
  }
}
prop = data.frame(item = c(names(colMeans(dataFixed)),names(colMeans(dataFixed))), 
           prop = c(colMeans(dataFixed), colMeans(data)), 
           data = c(rep("Fixed", n_item), rep("Clean", n_item)))
ggplot(prop, 
       aes( x= reorder(item, prop), 
            y = prop, color = data, shape = data, group = data)) + geom_point() + geom_line()

# stimo di nuovo il 2PL su tutti gli item CON STANCHEZZA ----- 
m2pl_tired <- tam.mml(resp=dataFixed, xsi.fixed = diff_true, 
                B = discr_true)
summary(m2pl_tired)
plot(m2pl_tired$person$EAP, true_theta , asp =1,
     main = "stima su 50 item CON STANCHEZZA")
abline(lm(true_theta ~ m2pl_tired$person$EAP), lwd = 2)
abline(0,1, lwd = 2, col = "red")
legend(x = -7, y = 1, 
       legend = c("Theta stimato", "Attesa"), 
       fill = c("black", "red"))

#  APPLICA LEON SUGLI ITEM CON LA STANCHEZZA ------
resLeon = leon(tired_q, tif)
resFranTi = frank(tired_q, tif)
resLeon$q_leon
resFranTi$q_frank
q_leon = as.numeric(gsub("item_", "", colnames(resLeon$iif_stf)))
q_frank = as.numeric(gsub("item_", "", colnames(resFranTi$iif_stf)))
diff_leon = diff_true[q_leon, ]
diff_frank = diff_true[q_frank, ]
diff_frank[,1] = 1:nrow(diff_frank)
diff_leon[,1] = 1:nrow(diff_leon)
discr_leon = array(c(rep(0, length(original_q$a[q_leon])), 
                     original_q$a[q_leon]), 
                   c(length(original_q$a[q_leon]),2,1), 
                   dimnames = list(paste0("I", 1:length(q_leon)), 
                                   c("Cat0", "Cat1"), 
                                   "Dim01"))
discr_frank = array(c(rep(0, length(original_q$a[q_frank])), 
                     original_q$a[q_frank]), 
                   c(length(original_q$a[q_frank]),2,1), 
                   dimnames = list(paste0("I", 1:length(q_frank)), 
                                   c("Cat0", "Cat1"), 
                                   "Dim01"))
m2pl_leon = tam.mml(resp=dataFixed[,q_leon], 
                    xsi.fixed = diff_leon, 
                    B = discr_leon)
m2pl_frankti = tam.mml(resp=dataFixed[,q_frank], 
                    xsi.fixed = diff_frank, 
                    B = discr_frank)
summary(m2pl_leon)
plot(m2pl_tired$person$EAP,true_theta,  main = "stima su 50 item CON STANCHEZZA", 
     ylim= c(-4,4), xlim = c(-4,4))
abline(lm(true_theta  ~  m2pl_tired$person$EAP), lwd = 2)
abline(0,1, lwd = 2, col = "red")
points( m2pl_leon$person$EAP,true_theta, col = "seagreen", pch = 3)
abline(lm(true_theta~ m2pl_leon$person$EAP), col = "seagreen", lwd = 3)
# points( m2pl_frankti$person$EAP,true_theta, col = "pink", pch = 3)
# abline(lm(true_theta~ m2pl_frankti$person$EAP), col = "pink", lwd = 3)
legend(x = -3, y = 2, 
       legend = c("Theta stimato", "Attesa", "Leon"), 
       fill = c("black", "red", "seagreen"))



theta_full_test$theta_tired_fixed = m2pl_tired$person$EAP
plot(theta, 
     true_theta, type = "l")
lines(theta, theta_full_test$EAP[order(theta_full_test$EAP)], type = "l", col = "blue")
lines(theta, theta_full_test$theta_tired_fixed[order(theta_full_test$theta_tired_fixed)], type = "l", col = "red")
# c'è effettivamente un effetto della stanchezza suelle stime di abilità dei 
# soggetti 
# mi creo la TIF target che è la tif che otterei con i 50 item 
# se le persone non si stancassero
all_iif = item_info(original_q, theta)
tif = data.frame(theta, tif = rowSums(all_iif), 
                 mean_tif = rowMeans(all_iif))
plot(tif$theta, tif$mean_tif, type = "l")
# se gli do gli item senza stanchezza (i.e., dove e = 1) seleziona 3 item
resFrankOR = frank(original_q, tif)
resFrankTR = frank(tired_q, tif)
# questi sono gli item che mi suggerisce di prendere Frank tenendno fissa 
# la stanchezza
colnames(resFrankOR$iif_stf)
q_frank = as.numeric(gsub("item_", "", colnames(resFrankCost$iif_stf)))
plot(tif$theta, tif$mean_tif, type = "l")
tif_frank = data.frame(theta, mean_frank = rowMeans(resFrankCost$iif_stf))
lines(theta, tif_frank$mean_frank, type = "l")
# ristimo il modello 2pl selezionando solo gli item che dice frank 
# (c'è da ridere per la selezione delle discriminatività)
diff_frank = diff_true[q_frank, ]
diff_frank[,1] = 1:nrow(diff_frank)
discr_frank = array(c(rep(0, length(original_q$a[q_frank])), 
                      original_q$a[q_frank]), 
                   c(length(original_q$a[q_frank]),2,1), 
                   dimnames = list(paste0("I", 1:length(q_frank)), 
                                   c("Cat0", "Cat1"), 
                                   "Dim01"))
m2pl_frank = tam.mml(resp=dataFixed[,q_frank], 
                     xsi.fixed = diff_frank, 
                     B = discr_frank)
summary(m2pl_frank)
theta_frank = m2pl_frank$person
plot(theta, 
     true_theta, type = "l")
lines(theta, theta_full_test$EAP[order(theta_full_test$EAP)], type = "l", col = "blue")
lines(theta, theta_full_test$theta_tired_fixed[order(theta_full_test$theta_tired_fixed)], type = "l", col = "red")
lines(theta, theta_frank$EAP[order(theta_frank$EAP)], col = "green")
# calcolo le differenze 

data_bias = data.frame(theta = theta_full_test$theta, 
                       bias_all = (theta_full_test$EAP-theta_full_test$true_theta ), 
                       bias_all_tired = (theta_full_test$theta_tired_fixed - theta_full_test$true_theta ), 
                       bias_frank_tired = (theta_frank$EAP - theta_full_test$true_theta))

data_bias = pivot_longer(data_bias, cols = !theta)
# non lo so non ne ho idea pietà 
ggplot(data_bias, 
       aes( x = theta, y = value, color = name, shape = name)) + geom_point() +
  facet_wrap(~name)

# va secondo me consdierata una diversa tif target perché così la scelta che fa frank è giusta
# ma quello che è sbagliato è la TIF target 



# dveo provare con la stanchezza che uamenta 
tired_exp = original_q
tired_exp$e = exp(-0.01*(0:(nrow(original_q)-1)))

resLeon = leon(tired_exp, tif)
resLeon$q_leon
q_leon = as.numeric(gsub("item_", "", colnames(resLeon$iif_stf)))
# ristimo il modello 2pl selezionando solo gli item che dice frank 
# (c'è da ridere per la selezione delle discriminatività)
diff_leon = diff_true[q_leon, ]
diff_leon[,1] = 1:nrow(diff_leon)
discr_leon = array(c(rep(0, length(original_q$a[q_leon])), 
                      original_q$a[q_leon]), 
                    c(length(original_q$a[q_leon]),2,1), 
                    dimnames = list(paste0("I", 1:length(q_leon)), 
                                    c("Cat0", "Cat1"), 
                                    "Dim01"))

dataExp = data.frame(matrix(nrow = length(theta), 
                            ncol = nrow(tired_q)))
colnames(dataExp) = rownames(tired_q)

for (i in 1:nrow(dataExp)) {
  for (j in 1:ncol(dataExp)) {
    dataExp[i, j] = rbinom(1,1, IRT(true_theta[i],b = tired_q[j, "b"], 
                                    a = tired_q[j, "a"], 
                                    c = tired_q[j, "c"],tired_q[j, "e"]))
  }
}


theta_frank$leon = m2pl_leon$person$EAP
lines(theta, theta_frank$leon, col = "firebrick")

m2pl_tiredExp = tam.mml(resp=dataExp, 
                        xsi.fixed = diff_true, 
                        B = discr_true)
theta_frank$theta_all_tiredExp = m2pl_tiredExp$person$EAP
plot(theta, theta_frank$theta_all_tiredExp, type = "l")
lines(theta, theta_frank$leon, col = "firebrick")
lines(theta, true_theta)



# conviene fare il plot delle differenze
differenze = data.frame(true_full = theta_full_test$true_theta - theta_full_test$EAP, 
                        true_full_tired = theta_full_test$true_theta - theta_full_test$theta_tired_fixed, 
                        true_frank = theta_full_test$true_theta - theta_frank$EAP)

plot(theta, differenze$true_full, type = "l")
lines(theta, differenze$true_full_tired, type = "l", col = "red")
lines(theta, differenze$true_frank, type = "l", col = "blue")
