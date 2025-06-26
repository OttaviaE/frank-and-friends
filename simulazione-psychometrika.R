library(tidyverse)
source("functions-new.R")
#' Léon
#' 
#' Find the item selection best able to approximate a pre defined tif target 
#' also accounting for the response fatigue
#'
#' @param parameters  matrix with the item  parameters (b = difficulty, a = discrimination, c = guessing, e = careless error)
#' @param start_e vector with upper asymptote according to the item administration 
#' @param target TIF target
#' @param speed I don't know whether it is still implemented 
#' @param nmin Minimum number of items to include in the STF
#'
#' @returns
#' @export
#'
#' @examples
leonL = function(parameters, start_e = NULL, 
                 target, speed = 0.01, 
                 nmin = 1) {
  if (is.null(start_e)) {
    warning("There are no tiredness parameters, hope it's okay")
    start_e = rep(1, nrow(parameters))
  } else {
    start_e = start_e
  }
  theta = target$theta
  original_parameters = parameters
  token = TRUE
  mod_e = NULL
  j = 0
  iif_stf = matrix(,length(target$theta), 0)
  distance_target_tif = Inf
  while (token == TRUE) {
    j = j +1
    # qui potrei fare che itero per tutte le colonne che non sono NA del mio dataframe di item 
    # creo gli indici degli item nell'insieme A (gli available items)
    item_indexes = which(!is.na(parameters[,1]))
    parameters[item_indexes, "e"] = start_e[j]
    # adesso il ciclo for itera negli item indexes e calcola la PIF
    difference = rep(NA, nrow(parameters))
    for (i in item_indexes) {
      # qui devo mdoficare all_iifs nel senso che le devo ricalcolare 
      # però considerando la stanchezza
      # so dire esattamente 
      # CAMBIA QUI METTENDO CHE PESCA LA E DEGLI ITEM ORIGIANLI IN ABSE ALLA LORO POSIZIONE DI SOMMNISTRAZIONE 
      # PER CUI ALL'I-ESIMA ITERAZIONE PRENDE LA STANCHEZZA DELL'ITEM SOMMINISTRATO PER I-ESIMO
      # original_parameters$e = exp(-speed*(ncol(iif_stf)+1))
      all_iifs = item_info(parameters[i,], theta)
      pif = data.frame(cbind(iif_stf, all_iifs))
      pif = data.frame(rowMeans(pif))
      # difference[i] = mean(abs(target$mean_tif - pif)[,1]/target$mean_tif)  
      difference[i] = mean(abs(target$mean_tif - pif)[,1])  
      # qui scelgo un item temporaneo che è quello con la distanza minima
    }
    # qui trovo l'item che minimizza e lo metto in d
    d_index = which(difference == min(difference, na.rm = T))
    # adesso devo testare il criterio di uscita dove prendo la pif con l'ìitem che
    # minimizza, faccio la differenza dalla target e guardo come si comporta 
    # rispetto alla distanza dalla target dello step precedente 
    # qui ho riscoruito la provisional tif 
    iif_stf = data.frame(iif_stf,  item_info(parameters[d_index,], theta))
    colnames(iif_stf)[ncol(iif_stf)] = paste("item", d_index, sep = "_")
    # guardo le differenze 
    if (j <= nmin) {
      token = TRUE 
      parameters[d_index, ] = NA
      # distance_target_tif = mean(abs(target$mean_tif - rowMeans(iif_stf))/target$mean_tif)
      distance_target_tif = mean(abs(target$mean_tif - rowMeans(iif_stf)))
    } else  if (difference[d_index] >= distance_target_tif) {
      token = FALSE
      temp_item = colnames(iif_stf)[-ncol(iif_stf)]
      temp_item = as.numeric(gsub("item_", "", temp_item))
      temp_item = temp_item[order(temp_item)]
      sel_items = paste("item", temp_item, sep = "_")
      sel_items = paste(sel_items, collapse = " ")
    } else {
      parameters[d_index, ] = NA
      # distance_target_tif = mean(abs(target$mean_tif - rowMeans(iif_stf))/target$mean_tif)
      distance_target_tif = mean(abs(target$mean_tif - rowMeans(iif_stf)))
    }
  } 
  iif_stf = data.frame(iif_stf[,-ncol(iif_stf)])
  if (ncol(iif_stf) == 1) {
    colnames(iif_stf) = sel_items
  }
  results = list(q_leon = sel_items, 
                 iif_stf = iif_stf, 
                 mod_e = start_e[1:j])
  
  return(results)
}


compute_P <- function(theta,a,b,c ,delta ,D ,N = 1,n){
  P <- c + (delta - c) / (1 + exp(-D *  
                                    matrix(a,N,n,byrow = TRUE) * 
                                    (matrix(theta,N,n) - matrix(b,N,n,byrow = TRUE)))) # sogg x item:  N x n
  
}
# log likelihood 
loglik <- function(theta,a,b,c ,delta ,D ,N,n,u){
  P=compute_P(theta=theta,a=a,b=b,c=c ,delta=delta ,D=D ,N=N,n=n)
  #u e Pij sono matrici, la somma ci dà la log-verosimiglianza (Lord, p. 58)
  sum(u*log(P) + (1-u)*log(1-P))
}

# simulazione 1 ------
# approssimazione della tif target -----


set.seed(1312)
percent = c(.10, .25, .50)
n=70 # item
N=1000 # sogg
D = 1.7
delta = rep(1,n)
theta = seq(-3,3, length.out = N)
e = exp(-0.01*(0:(n-1)))
# original_q = data.frame(b = b, 
#                         a = a, 
#                         c = rep(0, n), 
#                         e = delta)
# rownames(original_q) = paste("item", 1:n, sep ="_")
# al_iifs = item_info(original_q, theta = theta)
# tif = data.frame(theta = theta, 
#                  mean_tif = rowMeans(al_iifs))
# qui devo creare i vari frabk leon e compagnia per le varie percentuali ---- 
resLeon = list()
resFrank = list()
selLeon = list()
selFrank = list()
selRandom = list()
numsmall = round(nrow(original_q)*percent)
e_short = list()
resRandom = NULL
myLeon = NULL
myFrank = NULL
both = NULL
MCMC = 3
results = NULL
all_q = list()
allTired = NULL
for (f in 1:MCMC) {
  set.seed(1312+f)
  a= runif(n, .9, 2)
  b=runif(n, -3,3)
  original_q = data.frame(b = b, 
                          a = a, 
                          c = rep(0, n), 
                          e = delta)
  rownames(original_q) = paste("item", 1:n, sep ="_")
  al_iifs = item_info(original_q, theta = theta)
  tif = data.frame(theta = theta, 
                   mean_tif = rowMeans(al_iifs))
  for (i in 1:length(percent)) {
    resLeon[[i]] = leonL(original_q, 
                         start_e = e, target = tif, 
                         nmin = numsmall[i])
    selLeon[[i]] = original_q[colnames(resLeon[[i]]$iif_stf), ]
    resFrank[[i]] = frank(original_q, target = tif, nmin = numsmall[i])
    selFrank[[i]] = original_q[colnames(resFrank[[i]]$iif_stf), ]
    selFrank[[i]][,"e"] = exp(-0.01*(0:(nrow(selFrank[[i]])-1)))
    my = item_info(selFrank[[i]], theta = tif$theta)
    resFrank[[i]]$iif_stf = my
    
    selRandom[[i]] = original_q[sample(nrow(original_q), nrow(selLeon[[i]])), ]
    e_short[[i]] = exp(-0.01*(0:(nrow(selLeon[[i]])-1)))
    
    selRandom[[i]][,"e"] = exp(-0.01*(0:(nrow(selRandom[[i]])-1)))
    temp = data.frame(theta = tif$theta, 
                      tif_stf = rowMeans(item_info(selRandom[[i]], tif$theta)), 
                      nitem = nrow(selRandom[[i]]), 
                      type = "random", 
                      replica = f, 
                      percent = percent[i])
    temp = cbind(tif, temp)
    resRandom = rbind(resRandom, temp)
    temp = data.frame(theta = tif$theta, 
                      tif_stf = rowMeans(resLeon[[i]]$iif_stf), 
                      nitem = ncol(resLeon[[i]]$iif_stf), 
                      type = "leon", 
                      replica = f, 
                      percent = percent[i])
    temp = cbind(tif, temp)
    myLeon = rbind(myLeon, temp)
    temp = data.frame(theta = tif$theta, 
                      tif_stf = rowMeans(resFrank[[i]]$iif_stf), 
                      nitem = ncol(resFrank[[i]]$iif_stf), 
                      type = "frank", 
                      replica = f, 
                      percent = percent[i])
    temp = cbind(tif, temp)
    myFrank = rbind(myFrank, temp)
    both = rbind(resRandom, myLeon, myFrank)
  }
  tired_q = original_q
  tired_q$e = exp(-0.01*(0:(nrow(tired_q)-1)))
  mytired = data.frame(theta = tif$theta, 
                       tif_stf = rowMeans(item_info(tired_q, 
                                                theta = tif$theta)), 
                       nitem = 70, 
                       type = "tired", 
                       replica = f, 
                       percent = 100)
  allTired = rbind(allTired, mytired)
  results = rbind(results, both)
  original_q$replica = f
  all_q[[f]] = original_q
  names(all_q)[[f]] = f
  cat("iter ",f)
}

results$diff = abs(results$mean_tif - results$tif_stf)

ggplot(results[,-3], 
       aes(x = factor(nitem), y = diff, 
           color = type)) + geom_boxplot() + 
  facet_grid(~percent, scales = "free") + theme_light()

mysummary = results[,-3] %>%  
  group_by(percent, nitem, type, replica) %>%  
  summarise(mean = mean(diff), 
            sd = sd(diff), n = n()/1000)
smallsmmary = results[,-3] %>%  
  group_by(percent, nitem, type) %>%  
  summarise(mean = mean(diff), 
            sd = sd(diff), n = n()/1000)
ggplot(smallsmmary, 
       aes(x = factor(nitem), 
           y = mean, color = type, group = type)) + geom_point() + 
  geom_line() + facet_grid(~percent, scales = "free") + theme_light()

# devo scegliere la replica migliore ovvero quella da cui 
# so che leon sceglie il migliore 
# prendo solo leon 
l = mysummary[mysummary$type %in% "leon", ]
selreplica = as.numeric(l[which(l$mean == min(l$mean)), "replica"])

original_q = all_q[[selreplica]]
original_q = original_q[, !colnames(original_q) %in% "replica"]
# questo mi permette di scegliere la replica per cui leon performa meglio 
# (credo)

# il fatto che la tif si allontani con più item è angosciante ma è 
# sensato perché per l'appunto ci sono più item, quindi più stanchezza
# e comuqnue questa è la tif media 

# faccio la stessa cosa con le 3 selezioni random
# devo mettere la stanchezza per le 3 lunghezza, calcolare le iif 
# calcolare le tif 


# simulazione 2 ----- 
# stima di theta 
# qui genero le probabilità che mi servono sulla base della selezione fatta 
# da Leon
# siccome sono una volpe, devo rifare andare il codice su Leon su questa 
# replica perché non sono certa di dove sia finita non dire 
# nulla, sono gnocca non posso pure essere intelligente 

# mi prendo gli item della replica corretta 
# ho metito, dentro results io ho tutta la roba che credo mi serva 
# non è vero, devo riprendermi tutti i cazzi vari tipo la selezione di item 
# avrei potuto pensarci prima ma sono stanca

# devo anche recuparare la tif target della seconda replica 
tif = results[results$replica %in% selreplica,  
              c("theta", "mean_tif")]

resLeon = list()
resFrank = list()
selLeon = list()
selFrank = list()
selRandom = list()
numsmall = round(nrow(original_q)*percent)
e_short = list()
resRandom = NULL
myLeon = NULL
myFrank = NULL
for (i in 1:length(percent)) {
  resLeon[[i]] = leonL(original_q, 
                       start_e = e, target = tif, 
                       nmin = numsmall[i])
  resFrank[[i]] = frank(original_q, target = tif, nmin = numsmall[i])
  selLeon[[i]] = original_q[colnames(resLeon[[i]]$iif_stf), ]
  selFrank[[i]] = original_q[colnames(resFrank[[i]]$iif_stf), ]
  selFrank[[i]][,"e"] = exp(-0.01*(0:(nrow(selFrank[[i]])-1)))
  my = item_info(selFrank[[i]], theta = tif$theta)
  resFrank[[i]]$iif_stf = my
  selRandom[[i]] = original_q[sample(nrow(original_q), nrow(selLeon[[i]])), ]
  e_short[[i]] = exp(-0.01*(0:(nrow(selLeon[[i]])-1)))
  selRandom[[i]][,"e"] = exp(-0.01*(0:(nrow(selRandom[[i]])-1)))
  temp = data.frame(theta = tif$theta, 
                    tif = rowMeans(item_info(selRandom[[i]], tif$theta)), 
                    nitem = nrow(selRandom[[i]]), 
                    type = "random")
  resRandom = rbind(resRandom, temp)
  temp = data.frame(theta = tif$theta, 
                    tif = rowMeans(resLeon[[i]]$iif_stf), 
                    nitem = ncol(resLeon[[i]]$iif_stf), 
                    type = "leon")
  myLeon = rbind(myLeon, temp)
  temp = data.frame(theta = tif$theta, 
                    tif = rowMeans(resLeon[[i]]$iif_stf), 
                    nitem = ncol(resLeon[[i]]$iif_stf), 
                    type = "leon")
  temp = data.frame(theta = tif$theta, 
                    tif = rowMeans(resFrank[[i]]$iif_stf), 
                    nitem = ncol(resFrank[[i]]$iif_stf), 
                    type = "frank")
  myFrank = rbind(myFrank, temp)
}
tired_q = original_q
tired_q$e = e

allTired = data.frame(theta = tif$theta, 
                      tif = rowMeans(item_info(tired_q, 
                                               theta = tif$theta)), 
                      nitem = 70, 
                      type = "tired")

both = rbind(resRandom, myLeon, myFrank, allTired)
colnames(tif)[2] = "tif"
tif$nitem = nrow(original_q)
tif$type = "target"
ggplot(rbind(both, tif), 
       aes(x = theta, y = tif, 
           color = factor(nitem),  
           linetype = factor(type))) + geom_line(linewidth=1) + 
  scale_linetype_manual(values = c(2, 1,3,4,5))

# ora parte lòa stima 
# mi salvo le selezioni perché non si sa mai 
# myranomdselection = selRandom
# myLeonselection = selLeon

# sulla base della replica selezionata mi genero le probabilità 
# queste probabilità verranno usate per sempre per generare i millemila 
# dataset ma perché non ho fatto statistica in partenza 

probLeon = list()
probRandom = list()
probTired = list()
probAll = list()

for (i in 1:length(percent)) {
  Pall = NULL
  Ptired = NULL
  Prandom = NULL
  Pleon = NULL
  for (j in 1:N) {
    tempf=compute_P(theta=theta[j],a=selRandom[[i]]$a,
                    b=selRandom[[i]]$b,c=selRandom[[i]]$c, 
                    delta=e_short[[i]],D=D ,N=1,
                    n=nrow(selRandom[[i]]))
    Prandom = rbind(Prandom, tempf)
    templ =compute_P(theta=theta[j],a=selLeon[[i]]$a,b=selLeon[[i]]$b,
                     c=selLeon[[i]]$c, 
                     delta=e_short[[i]] ,D=D ,N=1,n=nrow(selLeon[[i]]))
    Pleon = rbind(Pleon, templ)
    tempt = compute_P(theta=theta[j],a=a,b=b,c=c, 
                      delta=e ,D=D ,N=1,n=n)
    Ptired = rbind(Ptired, tempt)
    tempa = compute_P(theta=theta[j],a=a,b=b,c=c, 
                      delta=delta ,D=D ,N=1,n=n)
    Pall = rbind(Pall, tempa)
  }
  probLeon[[i]] = Pleon
  probRandom[[i]] = Prandom
  probAll[[i]] = Pall
  probTired[[i]] = Ptired
}


# genera MCMC volte ----
res = list()
resTemp = NULL
MCMC = 3
randomSelection = selRandom
leonSelection = selLeon
hat_theta_leon = numeric(N)
hat_theta_random = numeric(N)
hat_theta_leon_clean = numeric(N)
hat_theta_random_clean = numeric(N)
hat_theta_all = numeric(N)
hat_theta_tired = numeric(N)
hat_theta_tired_clean = numeric(N)
for (i in 1:length(percent)) {
  urandom = NULL
  uleon = NULL
  utired = NULL 
  uall = NULL
  selRandom = randomSelection[[i]]
  selLeon = leonSelection[[i]]
  for (f in 1:MCMC) {
    urandom = matrix(rbinom(length(probRandom[[i]]), 
                            size = 1, 
                            prob = as.vector(probRandom[[i]])), 
                     nrow = nrow(probRandom[[i]]), 
                     ncol = ncol(probRandom[[i]]))
    uleon <- matrix(rbinom(length(probLeon[[i]]),
                           size = 1, 
                           prob = as.vector(probLeon[[i]])),
                    nrow = nrow(probLeon[[i]]), 
                    ncol = ncol(probLeon[[i]]))
    utired <- matrix(rbinom(length(probTired[[i]]), 
                            size = 1, 
                            prob = as.vector(probTired[[i]])),
                     nrow = nrow(probTired[[i]]), 
                     ncol = ncol(probTired[[i]]))
    uall <- matrix(rbinom(length(probAll[[i]]), size = 1, prob = as.vector(probAll[[i]])),
                   nrow = nrow(probAll[[i]]), ncol = ncol(probAll[[i]]))
    for (j in 1:N) {
      # isolate the subet of chosen items
      hat_theta_random[j]=optimize(loglik, 
                                   interval = c(-5,5),
                                   u = urandom[j,], 
                                   a=selRandom$a,b=selRandom$b,c=selRandom$c, 
                                   delta=exp(-0.01*(0:(nrow(selRandom)-1))),
                                   D=D ,N=1,n=nrow(selRandom), 
                                   maximum = TRUE)$maximum
      hat_theta_random_clean[j]=optimize(loglik, 
                                         interval = c(-5,5),
                                         u = urandom[j,],
                                         a=selRandom$a,b=selRandom$b,c=selRandom$c,
                                         delta= rep(1, nrow(selRandom)),
                                         D=D ,N=1,n=nrow(selRandom),
                                         maximum = TRUE)$maximum
      hat_theta_leon[j]=optimize(loglik, interval = c(-5,5),  u = uleon[j, ], 
                                 a=selLeon$a,b=selLeon$b,c=selLeon$c, 
                                 delta=e_short[[i]] ,D=D ,N=1,n=nrow(selLeon), 
                                 maximum = TRUE)$maximum
      hat_theta_leon_clean[j]=optimize(loglik, interval = c(-5,5),  u = uleon[j, ],
                                       a=selLeon$a,b=selLeon$b,c=selLeon$c,
                                       delta=rep(1, nrow(selRandom)) ,D=D ,N=1,n=nrow(selLeon),
                                       maximum = TRUE)$maximum
      hat_theta_tired[j]=optimize(loglik, 
                                  interval = c(-5,5), u = utired[j,], 
                                  a=a,b=b,c=c, 
                                  delta=e,D=D ,N=1,n=n, 
                                  maximum = TRUE)$maximum
      hat_theta_tired_clean[j]=optimize(loglik,
                                        interval = c(-5,5), u = utired[j,],
                                        a=a,b=b,c=c,
                                        delta=delta,D=D ,N=1,n=n,
                                        maximum = TRUE)$maximum
      hat_theta_all[j]=optimize(loglik, 
                                interval = c(-5,5), u = uall[j,], 
                                a=a,b=b,c=c, 
                                delta=delta ,D=D ,N=1,n=n, 
                                maximum = TRUE)$maximum
    }
    temp = data.frame(theta, 
                      hat_random = hat_theta_random, 
                      hat_random_clean = hat_theta_random_clean,
                      hat_leon = hat_theta_leon, 
                      hat_leon_clean = hat_theta_leon_clean, 
                      hat_all = hat_theta_all, 
                      hat_tired = hat_theta_tired, 
                      hat_tired_clean = hat_theta_tired_clean, 
                      replica = f)
    resTemp = rbind(resTemp, temp)
    cat("iter", f, i)
  }
  res[[i]] = resTemp
}


ohmy = NULL

for (i in 1:length(res)) {
  temp = res[[i]] %>%  
    group_by(theta) %>%  
    summarise(random = mean(hat_random), leon = mean(hat_leon), 
              all = mean(hat_all), tired = mean(hat_tired))
  temp =  pivot_longer(temp, cols = !theta)
  temp$percentage = percent[i]
  ohmy = rbind(ohmy, temp)
}


ggplot(ohmy, 
       aes(x = theta, y = value, 
           color = name)) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~percentage) 

ohmy = NULL

for (i in 1:length(res)) {
  temp = res[[i]] %>%  
    group_by(theta) %>%  
    summarise(random = mean(hat_random), 
              random_clean = mean(hat_random_clean),
              leon = mean(hat_leon), 
              leon_clean = mean(hat_leon_clean),
              all = mean(hat_all), 
              tired = mean(hat_tired), 
              tired_clean = mean(hat_tired_clean))
  temp =  pivot_longer(temp, cols = !theta)
  temp$percentage = percent[i]
  ohmy = rbind(ohmy, temp)
}

df <- ohmy %>%
  mutate(
    is_clean = str_detect(name, "_clean"),          # TRUE se contiene "_clean"
    name = str_replace(name, "_clean", "")          # rimuove "_clean" da name
  ) 
ggplot(df, 
       aes(x = theta, y = value, 
           color = name, shape = factor(is_clean))) + geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm") + 
  facet_wrap(factor(is_clean)~percentage) 
