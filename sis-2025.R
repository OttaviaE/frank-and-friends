# sis 2025 ------
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

set.seed(1312)
n_item = 50
theta = seq(-4, 4, length.out = 1500)
# preparo gli oggetti da riempire ----
q = list()
q_tired = list()
tif = list()
temp_iifs = NULL
resFrank = list()
resLeon = list()
tempFrank = NULL
# qui dentro devo mettere solo la TIF calcolata sui 50 item con stanchezza
resAllitems = list()
startAll = NULL
endAll = NULL
startFrank = NULL
endFrank = NULL
startLeon = NULL
endLeon = NULL
for (i in 1:100){
  set.seed(1312+i)
  q[[i]] = data.frame(b = runif(n_item, -3, 3), 
                                 a = runif(n_item, .9, 2), 
                                 c = rep(0,n_item), e= rep(1, n_item))
  rownames(q[[i]]) = paste("item", 1:nrow(q[[i]]), sep = "_")
  temp_iifs = item_info(q[[i]], theta = theta)
  tif[[i]] = data.frame(theta = theta, 
                        mean_tif = rowMeans(temp_iifs))
  # metto la stanchezza negli item 
  q_tired[[i]] = q[[i]]
  q_tired[[i]]$e =  exp(-0.01*(0:(nrow(q_tired[[i]])-1)))
  temp_tired_iifs = item_info(q_tired[[i]], theta = theta)
  # all items
  startTemp = Sys.time()
  resAllitems[[i]] = data.frame(theta = theta, 
                                tif_all = rowMeans(temp_tired_iifs))
  endTemp = Sys.time()
  startAll = c(startAll, startTemp)
  endAll = c(endAll, endTemp)
  # Frank
  startTemp = Sys.time()
  resFrank[[i]] = frank(q[[i]], tif[[i]])
  endTemp = Sys.time()
  startFrank = c(startFrank, startTemp)
  endFrank = c(endFrank, endTemp)
  # ricalcola la tif mettendo la stanchezza degli item
  tempIndex = as.numeric(gsub("item_", "", colnames(resFrank[[i]]$iif_stf)))
  tempItem = q[[i]][tempIndex, ]
  tempItem$e = exp(-0.01*(0:(nrow(tempItem)-1)))
  tempIIFs = item_info(tempItem, theta = theta)
  # Leon
  startTemp = Sys.time()
  resLeon[[i]] = leon(q_tired[[i]], tif[[i]])
  endTemp = Sys.time()
  startLeon = c(startLeon, startTemp)
  endLeon = c(endLeon, endTemp)
  print(paste("Replication", i))
}
# cose che posso fare
# guardare la differenza simmetrica tra gl item selezionati dai due algfortimi 
# calcolare la distanza media dalla TIF target (forse questa è la cosa migliore)
colnames(resFrank[[1]]$iif_stf)[order(colnames(resFrank[[1]]$iif_stf))]
for (i in 1:20) {
  plot(theta, tif[[i]]$mean_tif, type = "l", main = i)
  lines(theta, resAllitems[[i]]$tif_all, 
        type = "l", col = "red")
  lines(theta, rowMeans(resFrank[[i]]$iif_stf),
        type = "l", col = "green")
  lines(theta, rowMeans(resLeon[[i]]$iif_stf), 
        type = "l", col = "royalblue")
}


# per ogni iterazione, devo prendere la tif target in tif[[i]]$mean_tif
# la tif con tutti gli item stanchi
# la tif di frank e quella di leon 
# per loro due potrebbe valer la pena di tenere anche gli insiemi q_leon e q_frank

temp = NULL
myresults = NULL
res_tot = NULL
for (i in 1:length(resAllitems)) {
  temp = data.frame(theta = theta, 
                    target = tif[[i]]$mean_tif, 
                    all = resAllitems[[i]]$tif_all, 
                    frank = rowMeans(resFrank[[i]]$iif_stf), 
                    leon =  rowMeans(resLeon[[i]]$iif_stf), 
                    n_frank = ncol(resFrank[[i]]$iif_stf), 
                    n_leon =  ncol(resLeon[[i]]$iif_stf), 
                    q_frank = resFrank[[i]]$q_frank, 
                    q_leon = resLeon[[i]]$q_leon,
                    iteration = i)
  myresults = rbind(myresults, temp)
  tempAll = data.frame(theta = theta, 
                       target = tif[[i]]$mean_tif, 
                       type = "all", 
                       value = resAllitems[[i]]$tif_all, 
                       iteration = i)
  tempFrank = data.frame(theta = theta, 
                       target = tif[[i]]$mean_tif, 
                       type = "frank", 
                       value =rowMeans(resFrank[[i]]$iif_stf), 
                       iteration = i)
  tempLeon = data.frame(theta = theta, 
                         target = tif[[i]]$mean_tif, 
                         type = "leon", 
                         value =rowMeans(resLeon[[i]]$iif_stf), 
                         iteration = i)
  res_tot = rbind(res_tot, tempAll, tempFrank, tempLeon)
}
res_tot$difference = abs(res_tot$target - res_tot$value)
library(wesanderson)
wes_palette("GrandBudapest2", n=3)
mycolor = c(wes_palette("Chevalier1", n = 2), wes_palette("Chevalier1", n = 4)[4])
ggplot(res_tot, 
       aes(x = type, y = difference, color = type)) + 
  geom_boxplot(linewidth=1.2) + 
  theme_light() + ylab(expression(paste("|", TIF[B], " - ",TIF[x], "|"))) + 
  scale_x_discrete(labels = c(expression(paste("B","'")), "Frank", "Léon")) +
  theme(axis.text = element_text(size = 26), 
        axis.title.y = element_text(size = 28), 
        axis.title.x = element_blank(), 
        legend.position = "none") + 
  scale_color_manual(values= mycolor)

ggsave("C:/Users/Ottavia/Documents/GitHub/frank-and-friends/sis2025/LaTex+Package/styles/img/box-plot-alogirthms.pdf", 
       device = "pdf", width = 14, height = 8.5, units = "in")
myRes = aggregate(difference ~ type + iteration, data = res_tot, mean)
quantile(myRes$difference)
mean_difference = aggregate(difference ~ type , data = res_tot, mean)
colnames(mean_difference)[2] = "meandiff"
myRes = merge(myRes, mean_difference)
ggplot(myRes, 
       aes(x = reorder(factor(iteration), difference), 
           y = difference, color = type, shape = type)) + 
  geom_point(size = 4)  + 
  theme_classic() + xlab("Replication") + ylab(expression(paste("|TIF", "* - ",TIF[x], "|"))) + 
  scale_x_discrete(labels = c("All items", "Frank", "Léon")) +
  theme(axis.text = element_text(size = 26), 
        axis.text.x = element_blank(),
        axis.title = element_text(size = 28),
        legend.position = "none") + 
  scale_color_manual(values= mycolor) + 
  geom_hline(aes(yintercept  = meandiff, color = type), 
             linewidth=2, linetype = 2)

ggsave("C:/Users/Ottavia/Documents/GitHub/frank-and-friends/sis2025/LaTex+Package/styles/img/points-alogirthms.pdf", 
       device = "pdf", width = 14, height = 8.5, units = "in")


res_tot %>%  
  group_by(type) %>%  
  summarise(mean = round(mean(difference),2), 
            sd = round(sd(difference),2), 
            min = round(min(difference), 8), 
            max = round(max(difference), 2))

# mi prendo la cardnialità di q frank e q leon 
numberItems = data.frame(iteration = 1:100, 
                         q_frank = numeric(100), 
                         q_leon = numeric(100))
for (i in 1:100) {
  numberItems[i, "q_frank"] = ncol(resFrank[[i]]$iif_stf)
  numberItems[i, "q_leon"] = ncol(resLeon[[i]]$iif_stf)
}
colMeans(numberItems)
t.test(numberItems$q_leon, numberItems$q_frank)
