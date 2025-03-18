# provo a simulare tutto da zero sulla stanchezza, facendo sia la selezione 
# degli item sia i bias dei theta 
library(TAM)
library(tidyverse)
source("functions-new.R")
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
        all_iifs = item_info(original_parameters[i,], theta)
        pif = data.frame(cbind(iif_stf, all_iifs))
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
    pif = data.frame(iif_stf, item_info(original_parameters[d_index,], theta))
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
set.seed(1312)
n_sbj = 1000
true_theta = rnorm(n_sbj)
true_theta = true_theta[order(true_theta)] 
n_items = c(50)
estimate = NULL
estimateTired = NULL
estimateLeon = NULL
tempEstimate = NULL
temp = NULL

labels_replica = paste("replica", 1:100, sep = "_")
residualsModel = data.frame(matrix(nrow=1000, ncol = 100)) 
residualsAll = data.frame(matrix(nrow=1000, ncol = 100)) 
residualsLeon = data.frame(matrix(nrow=1000, ncol = 100)) 
colnames(residualsAll) = labels_replica
colnames(residualsModel) = labels_replica
colnames(residualsLeon) = labels_replica

modelCoef = NULL
modelCoefTired = NULL
modelCoefLeon = NULL 
num_leon = data.frame(replica = 1:100, 
                      q_leon = numeric(100))
for (f in 1:100) {
  set.seed(1312+f)
  original_q = data.frame(b = runif(n_items, -3, 3), 
                          a = runif(n_items, .9, 2), 
                          c = rep(0,n_items), e= rep(1, n_items))
  rownames(original_q) = paste("item", 1:nrow(original_q), sep ="_")
  al_iifs = item_info(original_q, theta = seq(-4,4, length.out = 1000))
  tif = data.frame(theta = seq(-4,4, length.out = 1000), 
                   mean_tif = rowMeans(al_iifs))
  data = data.frame(matrix(nrow = length(true_theta), 
                           ncol = nrow(original_q)))
  colnames(data) = rownames(original_q)
  for (k in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      data[k, j] = rbinom(1,1, IRT(true_theta[k],b = original_q[j, "b"], 
                                   a = original_q[j, "a"], 
                                   c = original_q[j, "c"],original_q[j, "e"]))
    }
  }
  diff_true <- matrix(cbind(1:length(original_q$b), 
                            original_q$b), 
                      ncol = 2)
  discr_true = array(c(rep(0, length(original_q$a)), original_q$a), 
                     c(length(original_q$a),2,1), 
                     dimnames = list(paste0("I", 1:length(original_q$a)), 
                                     c("Cat0", "Cat1"), 
                                     "Dim01"))
  
  m2pl <- tam.mml(resp=data, xsi.fixed = diff_true, 
                  B = discr_true, verbose = F)
  tempEstimate = m2pl$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsModel[, f] = m$residuals
  modelCoef = rbind(modelCoef, temp)
  estimate = cbind(estimate, tempEstimate)
  colnames(estimate)[ncol(estimate)] = paste("replica", f, sep = "_")
  tired_q = original_q
  # per iniziare metto una stanchezza costante (quindi devo usare frank)
  tired_q$e =  exp(-0.01*(0:(nrow(original_q)-1)))
  dataFixed = data.frame(matrix(nrow = length(true_theta), 
                                ncol = nrow(tired_q)))
  colnames(dataFixed) = rownames(tired_q)
  for (k in 1:nrow(dataFixed)) {
    for (j in 1:ncol(dataFixed)) {
      dataFixed[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_q[j, "b"], 
                                        a = tired_q[j, "a"], 
                                        c = tired_q[j, "c"],tired_q[j, "e"]))
    }
  }
  m2plTired <- tam.mml(resp=dataFixed, xsi.fixed = diff_true, 
                       B = discr_true, verbose = F)
  tempEstimate = m2plTired$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsAll[, f] = m$residuals
  modelCoefTired = rbind(modelCoefTired, temp)
  estimateTired = cbind(estimateTired, tempEstimate)
  colnames(estimateTired)[ncol(estimateTired)] = paste("replica", f, sep = "_")
  
  resLeon = leon(tired_q, tif)
  q_leon = as.numeric(gsub("item_", "", colnames(resLeon$iif_stf)))
  num_leon[f, "q_leon"] = ncol(resLeon$iif_stf)
  diff_leon = diff_true[q_leon, ]
  diff_leon[,1] = 1:nrow(diff_leon)
  discr_leon = array(c(rep(0, length(original_q$a[q_leon])), 
                       original_q$a[q_leon]), 
                     c(length(original_q$a[q_leon]),2,1), 
                     dimnames = list(paste0("I", 1:length(q_leon)), 
                                     c("Cat0", "Cat1"), 
                                     "Dim01"))
  
  dataLeon = data.frame(matrix(nrow = length(true_theta), 
                               ncol = length(q_leon)))
  colnames(dataLeon) = paste0("item_", q_leon)
  tired_leon = tired_q[q_leon, ]
  tired_leon$e = exp(-0.01*(0:(nrow(tired_leon)-1)))
  for (k in 1:nrow(dataLeon)) {
    for (j in 1:nrow(tired_leon)) {
      dataLeon[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_leon[j, "b"], 
                                       a = tired_leon[j, "a"], 
                                       c = tired_leon[j, "c"],tired_leon[j, "e"]))
    }
  }
  
  m2plLeon = tam.mml(resp=dataLeon, 
                     xsi.fixed = diff_leon, 
                     B = discr_leon, verbose = F)
  
  tempEstimate = m2plLeon$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsLeon[, f] = m$residuals
  modelCoefLeon = rbind(modelCoefLeon, temp)
  estimateLeon = cbind(estimateLeon, tempEstimate)
  colnames(estimateLeon)[ncol(estimateLeon)] = paste("replica", f, sep = "_")
  
  cat(paste("replica", f))
}


modelCoef = data.frame(modelCoef)
modelCoef$type = "all"
modelCoef$replica = rep(1:100, each = 2)

ggplot(modelCoef[!grepl("Intercept", rownames(modelCoef)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


modelCoefLeon = data.frame(modelCoefLeon)
modelCoefLeon$type = "Leon"
modelCoefLeon$replica = modelCoef$replica

ggplot(modelCoefLeon[!grepl("Intercept", rownames(modelCoefLeon)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


modelCoefTired = data.frame(modelCoefTired)
modelCoefTired$type = "All-Tired"
modelCoefTired$replica = modelCoef$replica

ggplot(modelCoefTired[!grepl("Intercept", rownames(modelCoefTired)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


allCoefs = rbind(modelCoef, modelCoefTired, modelCoefLeon)

ggplot(allCoefs[!grepl("Intercept", rownames(allCoefs)), ], 
       aes( x = reorder(factor(replica), Estimate), 
            y = Estimate, color = type)) + geom_point() +
  geom_hline(yintercept = 1, color = "red")


ggplot(allCoefs[!grepl("Intercept", rownames(allCoefs)), ], 
       aes( x = reorder(factor(replica), Estimate), 
            y = Std..Error, color = type)) + geom_point() 
temp = allCoefs[!grepl("Intercept", rownames(allCoefs)), ]

temp1 = pivot_longer(temp, 
             cols = !c(type, replica))

ggplot(temp1, 
       aes(x = factor(replica), 
           y = value, shape = name, color = type)) + geom_point() + 
  facet_grid(~type) + guides(color = "none") +
theme(legend.position = "bottom", 
      legend.title = element_blank())
  


par(mfrow = c(2,2))
plot(1, 1, type = "n", asp = 1,xlim = c(-3,3), ylim =c(-3,3),
    main = "All")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoef$replica))) {
  d = modelCoef[modelCoef$replica %in% unique(modelCoef$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "All tired")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefTired$replica))) {
  d = modelCoefTired[modelCoefTired$replica %in% unique(modelCoefTired$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Leon")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefLeon[modelCoefLeon$replica %in% unique(modelCoefLeon$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
modelCoef

plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Comparison")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefLeon[modelCoefLeon$replica %in% unique(modelCoefLeon$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "gray76")
  
}

for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoef[modelCoef$replica %in% unique(modelCoef$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "lightblue")
  
}

for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefTired[modelCoefTired$replica %in% unique(modelCoefTired$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "lightpink")
  
}

allCoefs$parametro = gsub("X.","", rownames(allCoefs))
allCoefs$parametro = gsub("[.0-9]", "", allCoefs$parametro)

saummaryPar = allCoefs %>%  
  group_by(type, parametro) %>%  
  summarise(mean = mean(Estimate), sd = sd(Estimate))




# voglio vedere i residui del modello 
residualsLeon$theta = true_theta
resiLeon = pivot_longer(residualsLeon, 
             cols = !theta)
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Comparison - Summary")
abline(0,1, lwd = 3)
colors = c("gray76", "lightblue", "lightpink")

for (i in 1:length(unique(saummaryPar$type))) {
  d = data.frame(saummaryPar[saummaryPar$type %in% unique(saummaryPar$type)[i], ])
  abline(d[1, "mean"], d[2, "mean"], 
         col = colors[i], lwd = 2)
}
legend(x = -7, y = 2, 
       legend = c("All", "Leon", "All-Tired"), 
       fill = c("gray76", "lightblue", "lightpink"))

# stessa cosa ma con la somma ----- 

rm(list = ls())


# provo a simulare tutto da zero sulla stanchezza, facendo sia la selezione 
# degli item sia i bias dei theta 
library(TAM)
library(tidyverse)
source("functions-new.R")
leonSum = function(parameters, target, speed = 0.01) {
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
        all_iifs = item_info(original_parameters[i,], theta)
        pif = data.frame(cbind(iif_stf, all_iifs))
        pif = data.frame(rowSums(pif))
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
    pif = data.frame(iif_stf, item_info(original_parameters[d_index,], theta))
  #  browser()
    if (all(pif[,1] == 0) ) {
      distance_target_tif = mean(abs(target$mean_tif - iif_stf))
      pif = pif[,-1]
      iif_stf = data.frame(pif)
      colnames(iif_stf) = paste("item", d_index, sep = "_")
      penalty = 1
      penalty_tif = penalty
    } else {
      penalty = log(ncol(iif_stf))
      distance_target_tif = mean(abs(target$mean_tif - rowSums(iif_stf)))
      iif_stf = pif
      colnames(iif_stf)[ncol(iif_stf)] = paste("item", d_index, sep = "_")
      penalty = log(ncol(iif_stf))
    }
    # guardo le differenze 
    # effettivamente non c'è bisogno di mettere su tutto questo cinema mi dovrebbe 
    # bastare prendere la differenza già calcolata in difference 
    # if (difference[d_index] < distance_target_tif & all(which(is.na(parameters[, 1])) == FALSE) ) {
    #   token = TRUE
    #   parameters[d_index, ] = NA
    # } else 
    if (difference[d_index]*penalty >= distance_target_tif*penalty_tif) {
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
set.seed(1312)
n_sbj = 1000
true_theta = rnorm(n_sbj)
true_theta = true_theta[order(true_theta)] 
n_items = c(50)
estimate = NULL
estimateTired = NULL
estimateLeon = NULL
tempEstimate = NULL
temp = NULL

labels_replica = paste("replica", 1:100, sep = "_")
residualsModel = data.frame(matrix(nrow=1000, ncol = 100)) 
residualsAll = data.frame(matrix(nrow=1000, ncol = 100)) 
residualsLeon = data.frame(matrix(nrow=1000, ncol = 100)) 
colnames(residualsAll) = labels_replica
colnames(residualsModel) = labels_replica
colnames(residualsLeon) = labels_replica

modelCoef = NULL
modelCoefTired = NULL
modelCoefLeon = NULL 
for (f in 1:100) {
  set.seed(1312+f)
  original_q = data.frame(b = runif(n_items, -3, 3), 
                          a = runif(n_items, .9, 2), 
                          c = rep(0,n_items), e= rep(1, n_items))
  rownames(original_q) = paste("item", 1:nrow(original_q), sep ="_")
  al_iifs = item_info(original_q, theta = seq(-4,4, length.out = 1000))
  tif = data.frame(theta = seq(-4,4, length.out = 1000), 
                   mean_tif = rowSums(al_iifs))
  data = data.frame(matrix(nrow = length(true_theta), 
                           ncol = nrow(original_q)))
  colnames(data) = rownames(original_q)
  for (k in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      data[k, j] = rbinom(1,1, IRT(true_theta[k],b = original_q[j, "b"], 
                                   a = original_q[j, "a"], 
                                   c = original_q[j, "c"],original_q[j, "e"]))
    }
  }
  diff_true <- matrix(cbind(1:length(original_q$b), 
                            original_q$b), 
                      ncol = 2)
  discr_true = array(c(rep(0, length(original_q$a)), original_q$a), 
                     c(length(original_q$a),2,1), 
                     dimnames = list(paste0("I", 1:length(original_q$a)), 
                                     c("Cat0", "Cat1"), 
                                     "Dim01"))
  
  m2pl <- tam.mml(resp=data, xsi.fixed = diff_true, 
                  B = discr_true, verbose = F)
  tempEstimate = m2pl$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsModel[, f] = m$residuals
  modelCoef = rbind(modelCoef, temp)
  estimate = cbind(estimate, tempEstimate)
  colnames(estimate)[ncol(estimate)] = paste("replica", f, sep = "_")
  tired_q = original_q
  # per iniziare metto una stanchezza costante (quindi devo usare frank)
  tired_q$e =  exp(-0.01*(0:(nrow(original_q)-1)))
  dataFixed = data.frame(matrix(nrow = length(true_theta), 
                                ncol = nrow(tired_q)))
  colnames(dataFixed) = rownames(tired_q)
  for (k in 1:nrow(dataFixed)) {
    for (j in 1:ncol(dataFixed)) {
      dataFixed[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_q[j, "b"], 
                                        a = tired_q[j, "a"], 
                                        c = tired_q[j, "c"],tired_q[j, "e"]))
    }
  }
  m2plTired <- tam.mml(resp=dataFixed, xsi.fixed = diff_true, 
                       B = discr_true, verbose = F)
  tempEstimate = m2plTired$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsAll[, f] = m$residuals
  modelCoefTired = rbind(modelCoefTired, temp)
  estimateTired = cbind(estimateTired, tempEstimate)
  colnames(estimateTired)[ncol(estimateTired)] = paste("replica", f, sep = "_")
  
  resLeon = leonSum(tired_q, tif)
  q_leon = as.numeric(gsub("item_", "", colnames(resLeon$iif_stf)))
  diff_leon = diff_true[q_leon, ]
  diff_leon[,1] = 1:nrow(diff_leon)
  discr_leon = array(c(rep(0, length(original_q$a[q_leon])), 
                       original_q$a[q_leon]), 
                     c(length(original_q$a[q_leon]),2,1), 
                     dimnames = list(paste0("I", 1:length(q_leon)), 
                                     c("Cat0", "Cat1"), 
                                     "Dim01"))
  
  dataLeon = data.frame(matrix(nrow = length(true_theta), 
                               ncol = length(q_leon)))
  colnames(dataLeon) = paste0("item_", q_leon)
  tired_leon = tired_q[q_leon, ]
  tired_leon$e = exp(-0.01*(0:(nrow(tired_leon)-1)))
  for (k in 1:nrow(dataLeon)) {
    for (j in 1:nrow(tired_leon)) {
      dataLeon[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_leon[j, "b"], 
                                       a = tired_leon[j, "a"], 
                                       c = tired_leon[j, "c"],tired_leon[j, "e"]))
    }
  }
  
  m2plLeon = tam.mml(resp=dataLeon, 
                     xsi.fixed = diff_leon, 
                     B = discr_leon, verbose = F)
  
  tempEstimate = m2plLeon$person$EAP
  m = lm(true_theta ~ tempEstimate)
  temp = summary(m)$coefficients[,1:2]
  residualsLeon[, f] = m$residuals
  modelCoefLeon = rbind(modelCoefLeon, temp)
  estimateLeon = cbind(estimateLeon, tempEstimate)
  colnames(estimateLeon)[ncol(estimateLeon)] = paste("replica", f, sep = "_")
  
  cat(paste("replica", f))
}


modelCoef = data.frame(modelCoef)
modelCoef$type = "all"
modelCoef$replica = rep(1:100, each = 2)

ggplot(modelCoef[!grepl("Intercept", rownames(modelCoef)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


modelCoefLeon = data.frame(modelCoefLeon)
modelCoefLeon$type = "Leon"
modelCoefLeon$replica = modelCoef$replica

ggplot(modelCoefLeon[!grepl("Intercept", rownames(modelCoefLeon)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


modelCoefTired = data.frame(modelCoefTired)
modelCoefTired$type = "All-Tired"
modelCoefTired$replica = modelCoef$replica

ggplot(modelCoefTired[!grepl("Intercept", rownames(modelCoefTired)), ], 
       aes(x = reorder(factor(replica), 
                       Estimate), y = Estimate)) + 
  geom_point() + geom_hline(aes(yintercept = 1), color = "red")


allCoefs = rbind(modelCoef, modelCoefTired, modelCoefLeon)

ggplot(allCoefs[!grepl("Intercept", rownames(allCoefs)), ], 
       aes( x = reorder(factor(replica), Estimate), 
            y = Estimate, color = type)) + geom_point() +
  geom_hline(yintercept = 1, color = "red")


ggplot(allCoefs[!grepl("Intercept", rownames(allCoefs)), ], 
       aes( x = reorder(factor(replica), Estimate), 
            y = Std..Error, color = type)) + geom_point() 


par(mfrow = c(2,2))
plot(1, 1, type = "n", asp = 1,xlim = c(-3,3), ylim =c(-3,3),
     main = "All")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoef$replica))) {
  d = modelCoef[modelCoef$replica %in% unique(modelCoef$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "All tired")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefTired$replica))) {
  d = modelCoefTired[modelCoefTired$replica %in% unique(modelCoefTired$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Leon")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefLeon[modelCoefLeon$replica %in% unique(modelCoefLeon$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], col = "gray76")
  
}
modelCoef

plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Comparison")
abline(0,1, lwd = 3)
for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefLeon[modelCoefLeon$replica %in% unique(modelCoefLeon$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "gray76")
  
}

for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoef[modelCoef$replica %in% unique(modelCoef$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "lightblue")
  
}

for (i in 1:length(unique(modelCoefLeon$replica))) {
  d = modelCoefTired[modelCoefTired$replica %in% unique(modelCoefTired$replica)[i],]
  abline(d[1, "Estimate"], d[2, "Estimate"], 
         col = "lightpink")
  
}

allCoefs$parametro = gsub("X.","", rownames(allCoefs))
allCoefs$parametro = gsub("[.0-9]", "", allCoefs$parametro)

saummaryPar = allCoefs %>%  
  group_by(type, parametro) %>%  
  summarise(mean = mean(Estimate), sd = sd(Estimate))




# voglio vedere i residui del modello 
residualsLeon$theta = true_theta
resiLeon = pivot_longer(residualsLeon, 
                        cols = !theta)
plot(1, 1, type = "n", asp = 1, xlim = c(-3,3), ylim =c(-3,3),
     main = "Comparison - Summary")
abline(0,1, lwd = 3)
colors = c("gray76", "lightblue", "lightpink")

for (i in 1:length(unique(saummaryPar$type))) {
  d = data.frame(saummaryPar[saummaryPar$type %in% unique(saummaryPar$type)[i], ])
  abline(d[1, "mean"], d[2, "mean"], 
         col = colors[i], lwd = 2)
}
legend(x = -7, y = 2, 
       legend = c("All", "Leon", "All-Tired"), 
       fill = c("gray76", "lightblue", "lightpink"))

# estimate = list(item25 = NULL, 
#                 item50 = NULL, 
#                 item75 = NULL, 
#                 item100 = NULL)
# estimateTired = list(item25 = NULL, 
#                      item50 = NULL, 
#                      item75 = NULL, 
#                      item100 = NULL)
# estimateLeon = list(item25 = NULL, 
#                     item50 = NULL, 
#                     item75 = NULL, 
#                     item100 = NULL)
# modelCoef = list(
#   coef25 = NULL,
#   coef50 = NULL,
#   coef75 = NULL,
#   coef100 = NULL
# )
# modelCoefTired = list(
#   coef25 = NULL,
#   coef50 = NULL,
#   coef75 = NULL,
#   coef100 = NULL
# )
# modelCoefLeon = list(
#   coef25 = NULL,
#   coef50 = NULL,
#   coef75 = NULL,
#   coef100 = NULL
# )
# for (i in 1:length(n_items)) {
  # for (f in 1:100) {
  #   set.seed(1312+f)
  #   original_q = data.frame(b = runif(n_items, -3, 3), 
  #                           a = runif(n_items, .9, 2), 
  #                           c = rep(0,n_items), e= rep(1, n_items))
  #   rownames(original_q) = paste("item", 1:nrow(original_q), sep ="_")
  #   al_iifs = item_info(original_q, theta = seq(-4,4, length.out = 1000))
  #   tif = data.frame(theta = seq(-4,4, length.out = 1000), 
  #                    mean_tif = rowMeans(al_iifs))
  #   data = data.frame(matrix(nrow = length(true_theta), 
  #                            ncol = nrow(original_q)))
  #   colnames(data) = rownames(original_q)
  #   for (k in 1:nrow(data)) {
  #     for (j in 1:ncol(data)) {
  #       data[k, j] = rbinom(1,1, IRT(true_theta[k],b = original_q[j, "b"], 
  #                                    a = original_q[j, "a"], 
  #                                    c = original_q[j, "c"],original_q[j, "e"]))
  #     }
  #   }
  #   diff_true <- matrix(cbind(1:length(original_q$b), 
  #                             original_q$b), 
  #                       ncol = 2)
  #   discr_true = array(c(rep(0, length(original_q$a)), original_q$a), 
  #                      c(length(original_q$a),2,1), 
  #                      dimnames = list(paste0("I", 1:length(original_q$a)), 
  #                                      c("Cat0", "Cat1"), 
  #                                      "Dim01"))
  # 
  #     m2pl <- tam.mml(resp=data, xsi.fixed = diff_true, 
  #                             B = discr_true)
  #     tempEstimate = m2pl$person$EAP
  #     temp = coef(lm(true_theta ~ tempEstimate))
  #     modelCoef[[i]] = rbind(modelCoef[[i]], temp)
  #     estimate[[i]] = cbind(estimate[[i]], tempEstimate)
  #     colnames(estimate[[i]]) = paste("replica", f, sep = "_")
  #     tired_q = original_q
  #     # per iniziare metto una stanchezza costante (quindi devo usare frank)
  #     tired_q$e =  exp(-0.01*(0:(nrow(original_q)-1)))
  #     dataFixed = data.frame(matrix(nrow = length(true_theta), 
  #                                   ncol = nrow(tired_q)))
  #     colnames(dataFixed) = rownames(tired_q)
  #     for (k in 1:nrow(dataFixed)) {
  #       for (j in 1:ncol(dataFixed)) {
  #         dataFixed[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_q[j, "b"], 
  #                                           a = tired_q[j, "a"], 
  #                                           c = tired_q[j, "c"],tired_q[j, "e"]))
  #       }
  #     }
  #     m2plTired <- tam.mml(resp=dataFixed, xsi.fixed = diff_true, 
  #                     B = discr_true)
  #     tempEstimate = m2plTired$person$EAP
  #     temp = coef(lm(true_theta ~ tempEstimate))
  #     modelCoefTired[[i]] = rbind(modelCoefTired[[i]], temp)
  #     estimateTired[[i]] = cbind(estimateTired[[i]], tempEstimate)
  #     colnames(estimateTired[[i]]) = paste("replica", f, sep = "_")
  #     
  #     resLeon = leon(tired_q, tif)
  #     q_leon = as.numeric(gsub("item_", "", colnames(resLeon$iif_stf)))
  #     diff_leon = diff_true[q_leon, ]
  #     diff_leon[,1] = 1:nrow(diff_leon)
  #     discr_leon = array(c(rep(0, length(original_q$a[q_leon])), 
  #                          original_q$a[q_leon]), 
  #                        c(length(original_q$a[q_leon]),2,1), 
  #                        dimnames = list(paste0("I", 1:length(q_leon)), 
  #                                        c("Cat0", "Cat1"), 
  #                                        "Dim01"))
  #     
  #     dataLeon = data.frame(matrix(nrow = length(true_theta), 
  #                                  ncol = length(q_leon)))
  #     colnames(dataLeon) = paste0("item_", q_leon)
  #     tired_leon = tired_q[q_leon, ]
  #     for (k in 1:nrow(dataLeon)) {
  #       for (j in 1:nrow(tired_leon)) {
  #         dataLeon[k, j] = rbinom(1,1, IRT(true_theta[k],b = tired_q[j, "b"], 
  #                                          a = tired_q[j, "a"], 
  #                                          c = tired_q[j, "c"],tired_q[j, "e"]))
  #       }
  #     }
  #     
  #     m2plLeon = tam.mml(resp=dataLeon, 
  #                         xsi.fixed = diff_leon, 
  #                         B = discr_leon)
  #     
  #     tempEstimate = m2plLeon$person$EAP
  #     temp = coef(lm(true_theta ~ tempEstimate))
  #     modelCoefLeon[[i]] = rbind(modelCoefLeon[[i]], temp)
  #     estimateLeon[[i]] = cbind(estimateLeon[[i]], tempEstimate)
  #     colnames(estimateLeon[[i]]) = paste("replica", f, sep = "_")
  # }

  
#}
