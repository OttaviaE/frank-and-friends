# frank ------
rm(list = ls())
library(tidyverse)
# dovresti lanciare dalle funzioni fino in fondo :) grazie 
# funzioni forme brevi
# funzione per stimare la probabilità dati i parametrid egli item e del soggetto
IRT <- function(theta, a = 1, b = 0, c = 0,e = 1) {
  y <- c + (e - c) * exp(a * (theta - b)) / (1 + exp(a * (theta - b)))
  return(y)
}
# calcola l'IIF per un item specifico
i_info <- function(b, a=1,c=0, theta = seq(-5,5,length.out=1000)){
  P <- NULL
  Q <- NULL
  Ii <- NULL
  
  for(i in 1:length(theta)){
    P[i] <- 1/(1+ exp (-a*(theta[i] - b)))
    Q[i]= 1-P[i]
    Ii[i] =(a*Q[i]*(P[i]-c)^2)/(P[i]*((1-c)^2)) # (3PL)
  }
  return(Ii)
}
# calcola l'IIF di tutti gli item e restituisce in una lista di lunghezza ugaule a tutti 
# gli item per cui si è calcolata l'IIF
item_info <- function(ipar, theta = seq(-5,5,length.out=1000)){
  item <- NULL
  for(i in 1:nrow(ipar)){
    item[[i]] <- i_info(ipar[i, "b"],ipar[i, "a"], theta = theta)
  }
  return(item)
}
# lo chiamo ILA ma è lo stesso di Ilocate
ila = function(TIF, parameters, print = TRUE) {
  # tif target

  mygraphs = list()
  data_graphs = list()
  mytif = TIF
  token = TRUE
  mysel = NULL
  interim_infos = list()
  interim_infos_temp = list()
 # browser()
  for (i in 1:nrow(parameters)) {
    if (token == TRUE) {
      if (i == 1) {
        parameters$temp_theta = mytif[which(mytif$mean_tif == max(mytif$mean_tif)), "theta"] 
      } else {
        parameters$temp_theta = widesel[which(widesel$difference == max(widesel$difference)), "theta"]
        
      }
      # questo codice funziona molto bene per Rasch, è la cosa migliore 
      parameters$temp_iif = with(parameters,
                                 abs(temp_theta - b))
      if (is.null(mysel)) {
        tempsel = parameters[which(parameters$temp_iif == min(parameters$temp_iif)), ]
        mysel = rbind(mysel, tempsel)
      } else {
        tempparameters = parameters[rownames(parameters)[!rownames(parameters) %in% rownames(mysel)], ]
        tempsel = tempparameters[which(tempparameters$temp_iif == min(tempparameters$temp_iif)), ]
        mysel = rbind(mysel, tempsel)
        
      }
      tempiifs <- item_info(mysel[,c("b","a")],
                            theta = mytif$theta)
      temp_info = do.call("cbind", tempiifs)
      temp_info = cbind(mytif$theta, temp_info)
      temp_info = data.frame(temp_info)
      colnames(temp_info)[-1] = rownames(mysel)
      colnames(temp_info)[1] = "theta"
      if (sum(grepl("item", colnames(temp_info))) == 1) {
        temp_info$tif = temp_info[,-1]
      } else {
        temp_info$tif = rowSums(temp_info[,-1]) 
      }
      # browser()
      temp_info$mean_tif =temp_info$tif/ nrow(mysel)
      temp_info_small = temp_info[, c("theta", "mean_tif")]
      temp_info_small$type = paste("temp", nrow(mysel), sep = "_")
      thesel = mytif[, c("theta", "mean_tif")] 
      thesel$type = "target"
      final = rbind(thesel, temp_info_small)
      data_graphs[[i]] = final
      mygraphs[[i]] = ggplot(data_graphs[[i]], 
                             aes(x = theta, y = mean_tif, 
                                 color = type, 
                                 linetype = type)) + 
        geom_line() 
      # temp_info_small va messo dentro una lista, in modo da poter prendere il processo tutte 
      # le volte 
      interim_infos_temp[[i]] = temp_info_small
      names(interim_infos_temp)[i] = unique(temp_info_small$type)
      interim_infos[[i]] = merge(temp_info_small, mytif, by = "theta")
      names(interim_infos)[i] = unique(temp_info_small$type)
      interim_infos[[i]]$last_item = rownames(mysel)[nrow(mysel)]
      interim_infos[[i]]$distance_tif = with(interim_infos[[i]], 
                                             abs(mean_tif.x - mean_tif.y))
      if (print == TRUE) {
        interim_plots = do.call("rbind", interim_infos_temp)
        interim_plots = rbind(thesel, interim_plots)
        g_plot = ggplot(interim_plots, 
                        aes(x = theta, y = mean_tif, 
                            color = type)) + geom_line()
        print(g_plot)
      }
      
      widesel = merge(mytif, temp_info_small, by = "theta")
      widesel$difference = widesel$mean_tif.x - widesel$mean_tif.y
      full_selection = mysel
      
      if (length(interim_infos) == 1) {
        token = TRUE
      } else if (i == nrow(parameters)) {
        token = FALSE
        end = full_selection[-nrow(full_selection), ]
      } else if (mean(interim_infos[[i]]$distance_tif) == 0 | mean(interim_infos[[i]]$distance_tif) >= mean(interim_infos[[i-1]]$distance_tif)) {
        token = FALSE
      } else if (mean(interim_infos[[i]]$distance_tif) < mean(interim_infos[[i-1]]$distance_tif)) {
        token = TRUE
      }
      
    } else {
      end = full_selection[-nrow(full_selection), ]
    }
    
  }
  results = list(all_attempts = interim_infos,
                 select_attempts = interim_infos[-length(interim_infos)],
                 original  = full_selection, 
                 end = end, 
                 the_graphs = mygraphs)
  return(results)
}
# FRANK because FRANly I don't know ho to call it  
frank = function(target, parameters) {
  target = target[, c("theta", "mean_tif")]
  # calcolo le iifs di tutti gli item dato il theta e i parametri
  all_iifs = item_info(parameters, theta = target$theta)
  all_iifs = data.frame(do.call("cbind", all_iifs))
  colnames(all_iifs) = rownames(parameters)
  all_iifs = cbind(theta, all_iifs)
  # prende solo le IIF
  iif_start = data.frame(all_iifs[, grep("item", colnames(all_iifs))])
  # inizializza le varie robe che servon nella funzione 
  index_item = NULL
  temp_sel = NULL
  sel_temp = NULL
  dist_k = NULL
  temp_dist = NULL
  tif_k = target
  distances = numeric(ncol(iif_start))
  names(distances) = colnames(iif_start)
  token = TRUE
  # questo è frank 
    for (i in 1:ncol(iif_start)){
      if (token == TRUE) {  
      if (i == 1) {
          for (j in 1:length(distances)) {
            distances[j] = mean(abs(target$mean_tif - 
                                      iif_start[, grep("item", colnames(iif_start))[j]]))
          }
          sel_temp = names(which(distances == min(distances, na.rm = TRUE)))
          # deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
          index_item = c(index_item, sel_temp)
          # attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
          temp_item = all_iifs[,colnames(all_iifs) %in% index_item]
          temp_sel = cbind(target, temp_item)
          colnames(temp_sel)[3:ncol(temp_sel)] = index_item
          tif_k$temp =  all_iifs[,colnames(all_iifs) %in% index_item]
          colnames(tif_k)[ncol(tif_k)] = paste0("temp_tif_", i)
          temp_dist = mean(abs(tif_k$mean_tif - tif_k[, ncol(tif_k)])) 
          dist_k = c(dist_k, temp_dist)
        } else {
          # calcolo al TIF media consdierando solo gli item che non sono ancora stati messi 
          # nella selezione
          
          for (j in as.numeric(gsub("\\D", "", setdiff(colnames(iif_start), index_item)))) {
            temp_sel$temp_item = iif_start[,j]
            temp_sel$temp_tif = rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
            distances[j] = mean(abs(temp_sel$mean_tif - temp_sel$temp_tif))
          }
          distances[names(distances) %in% index_item] = NA
          sel_temp = names(which(distances == min(distances, na.rm = TRUE)))
          # deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
          index_item = c(index_item, sel_temp)
          # attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
          temp_item = cbind(temp_item, all_iifs[,colnames(all_iifs) %in% index_item[i]])
          temp_sel = cbind(target, temp_item)
          colnames(temp_sel)[3:ncol(temp_sel)] = index_item
          tif_k$temp =  rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
          colnames(tif_k)[ncol(tif_k)] = paste0("temp_tif_", i)
          temp_dist = mean(abs(tif_k$mean_tif - tif_k[, ncol(tif_k)])) 
          dist_k = c(dist_k, temp_dist)
        }
        if (i == 1) {
          token = TRUE
        } else if (dist_k[i] >= dist_k[i-1]) {
          token  = FALSE
        } else {
          token = TRUE
        }
    }
  }
 
  results = list(tif = tif_k, 
                 distances = dist_k, 
                 chosen_items = index_item)
  return(results)
}
frank_graph = function(frank_results, selection = NULL) {
  
  tif_k_long = pivot_longer(frank_results$tif, 
                            cols = !theta, 
                            names_to = "tif")
  
  if (is.null(selection)) {
    temp_frank = tif_k_long
  } else {
    temp_frank = tif_k_long[tif_k_long$tif %in% selection, ]
  }
  
  g = ggplot(temp_frank, 
         aes(x = theta, y = value, color = tif, linetype = tif)) + geom_line()
  return(g)
}
# FRANK because FRANly I don't know ho to call it  
frankie = function(target, parameters) {
  target = target[, c("theta", "mean_tif")]
  # calcolo le iifs di tutti gli item dato il theta e i parametri
  all_iifs = item_info(parameters, theta = target$theta)
  all_iifs = data.frame(do.call("cbind", all_iifs))
  colnames(all_iifs) = rownames(parameters)
  all_iifs = cbind(theta, all_iifs)
  # prende solo le IIF
  iif_start = data.frame(all_iifs[, grep("item", colnames(all_iifs))])
  # inizializza le varie robe che servon nella funzione 
  index_item = NULL
  temp_sel = NULL
  sel_temp = NULL
  dist_k = NULL
  temp_dist = NULL
  tif_k = target
  distances = (matrix(nrow=length(theta), ncol=(ncol(iif_start))))
  colnames(distances) = colnames(iif_start)
  dist = numeric(ncol(iif_start))
  names(dist) = colnames(iif_start)
  token = TRUE
# questo è frankie 
    for (i in 1:ncol(iif_start)){
      if (token == TRUE) {
        if (i == 1) {
          for (j in 1:ncol(distances)) {
            distances[,j] = abs(target$mean_tif - 
                                      iif_start[, grep("item", colnames(iif_start))[j]])
            dist[j] = mean(abs(target$mean_tif - 
                                 iif_start[, grep("item", colnames(iif_start))[j]]))
          }
          indexing = matrix(nrow=nrow(distances), ncol= 1)
         for (j in 1:nrow(distances)) {
           indexing[j, 1] = names(which(distances[j, ] == min(distances[j, ])))[1]
         } 
          sel_temp = names(which(table(indexing) == max(table(indexing))))
          # sel_temp = names(which(dist == min(dist, na.rm = TRUE)))
          # deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
          index_item = c(index_item, sel_temp)
          # attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
          temp_item = all_iifs[,colnames(all_iifs) %in% index_item]
          temp_sel = cbind(target, temp_item)
          colnames(temp_sel)[3:ncol(temp_sel)] = index_item
          tif_k$temp =  all_iifs[,colnames(all_iifs) %in% index_item]
          colnames(tif_k)[ncol(tif_k)] = paste0("temp_tif_", i)
          temp_dist = table(indexing)[which(table(indexing) == max(table(indexing)))]
          #temp_dist = max(table(indexing))
          dist_k = c(dist_k, temp_dist)
        } else {
          # calcolo al TIF media consdierando solo gli item che non sono ancora stati messi 
          # nella selezione
          for (j in as.numeric(gsub("\\D", "", setdiff(colnames(iif_start), index_item)))) {
            temp_sel$temp_item = iif_start[,j]
            temp_sel$temp_tif = rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
            distances[, j] = abs(temp_sel$mean_tif - temp_sel$temp_tif)
          }
          distances[,colnames(distances) %in% index_item] = NA
          indexing = matrix(nrow=nrow(distances), ncol= 1)
          for (j in 1:nrow(distances)) {
            indexing[j, 1] = names(which(distances[j, ] == min(distances[j, ], na.rm = T)))
          } 
          sel_temp = names(which(table(indexing) == max(table(indexing))))
          # deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
          index_item = c(index_item, sel_temp)
          # attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
          temp_item = cbind(temp_item, all_iifs[,colnames(all_iifs) %in% index_item[i]])
          temp_sel = cbind(target, temp_item)
          colnames(temp_sel)[3:ncol(temp_sel)] = index_item
          tif_k$temp =  rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
          colnames(tif_k)[ncol(tif_k)] = paste0("temp_tif_", i)
          temp_dist = table(indexing)[which(table(indexing) == max(table(indexing)))]
          dist_k = c(dist_k, temp_dist)
        }
        if (i == 1) {
          token = TRUE
        } else if (dist_k[i] <= dist_k[i-1]) {
          token  = FALSE
        } else {
          token = TRUE
        }
        
      }
    }
  
  results = list(tif = tif_k, 
                 distances = dist_k, 
                 chosen_items = index_item)
  return(results)
}
#' ilafrank
#' 
#' Confronta le robe ottenute con ila frank e frankie per fare i grafici
#'
#' @param ila l'oggetto dei risultati ottenuto con ila
#' @param frank l'oggetto dei risultati ottenuto con frank 
#' @param frankie l'oggetto dei risultati ottenuto con frankie
#' @param n il numero di iterazione di cui si vogliono vedere i risultati (se non ci sono iterazioni ci si attacca al cazzo perché non funge)
#' @param last se falso si vedono TUTTI gli step di item inclusion fatti da frank e frankie
#'
#' @return
#' @export
#'
#' @examples
ilafrank = function(ila, frank, frankie = NULL, n=1, last = TRUE) {
  # browser()
  if (is.null(frankie)) {
    tempAttempt = ila[[n]]$select_attempts[[length(ila[[n]]$select_attempts)]]
    ilaGraph = tempAttempt[, c("theta", "type","mean_tif.x")]
    colnames(ilaGraph) = c("theta",    "type", "mean_tif")
    ilaGraph$type = gsub("temp", "ila", ilaGraph$type)
    
    frankLong =  pivot_longer(frank[[n]]$tif, 
                              cols = !theta, 
                              names_to = "tif")
    colnames(frankLong) = colnames(ilaGraph)
    frankLong$type = gsub("temp", "frank", frankLong$type)
    frank_lab = unique(frankLong$type)
    frank_lab = frank_lab[c(length(frank_lab))]
    frankieLong = NULL
  } else {
    frankieLong =  pivot_longer(frankie[[n]]$tif, 
                                cols = !theta, 
                                names_to = "tif")
    colnames(frankieLong) = colnames(ilaGraph)
    frankieLong$type = gsub("temp", "frankie", frankieLong$type)
    frankie_lab = unique(frankieLong$type)
    frankie_lab = frankie_lab[c(length(frankie_lab))]
  }

  if (last == TRUE & !is.null(frankie)) {
    frankieLong = frankieLong[frankieLong$type %in% c("mean_tif", frankie_lab),]
    frankLong = frankLong[frankLong$type %in% c("mean_tif", frank_lab),]
  } else if (last == TRUE & is.null(frankie)) {
    frankLong = frankLong[frankLong$type %in% c("mean_tif", frank_lab),]
  }
  
  allGraphs = rbind(ilaGraph, frankLong, frankieLong)
  allGraphs$type = gsub("mean_tif", "TARGET", allGraphs$type)
  
  return(allGraphs)
  
}
set.seed(1312)
theta =  seq(-4, 4, length.out = 5000)
myfrank = list()
myfrankie = list()
myila = list()
# creo il dataset per gli items che sono inclusi nella TIF target  
iterations = 10
target_items = data.frame(matrix(nrow = iterations, ncol= 2))
colnames(target_items)  = c("nitems", "items")
for (i in 1:iterations) {
  # confronto la performance di ila e frank su 100 iterazioni 
  # ad ogni iterazione creo una banca item (per ora parto con 20 item)
  # mi salvo gli item che erano nella tif target originale e anche la lunghezza dele tif originale 
  # applico 
  # genera i parametri degli item
  parameters = data.frame(b = runif(15, -3, 3),
                          a = runif(15, 0.9, 2))
  rownames(parameters) = paste("item", 1:nrow(parameters), sep = "_")
  # estrae un tif target a target con un numero di item a caso e aggiunge i paramtetri di modifica
  length_target = 2:(nrow(parameters)-1)
  length_target = sample(length_target, 1)
  possible_comb = combn(nrow(parameters), length_target)
  index_selected_comb = sample(ncol(possible_comb), 1)
  sel_items_rows = possible_comb[, index_selected_comb]
  sel_items = parameters[sel_items_rows, ]
  sel_items$b = sel_items$b + runif(nrow(sel_items), -.10, .10)
  sel_items$a = sel_items$a + runif(nrow(sel_items), -.10, .10)
  # metto gli item della tif dentro il dataframe che ho creato 
  target_items[i, "nitems"] = nrow(sel_items)
  target_items[i, "items"] = paste(rownames(sel_items),collapse = " ")
  
  # calcola la tif target
  iifs <- item_info(sel_items,
                    theta = theta)
  target_info = do.call("cbind", iifs)
  target_info = cbind(theta, target_info)
  target_info = data.frame(target_info)
  colnames(target_info)[-1] = rownames(sel_items)
  target_info$mean_tif = rowMeans(target_info[, grep("item", colnames(target_info))])
  
  
  myfrank[[i]] = frank(target_info, parameters = parameters)
  myfrankie[[i]] = frankie(target_info, parameters = parameters)
  myila[[i]] = ila(target_info, parameters = parameters, print = F)
}

item_comparison = data.frame(target = target_items$nitems)
item_select = data.frame(taregt_items = target_items$items)
for (i in 1:length(myfrank)) {
  item_comparison[i, "nfrank"] = length(myfrank[[i]]$chosen_items)
  item_select[i, "ifrank"] = paste(myfrank[[i]]$chosen_items[order(myfrank[[i]]$chosen_items, decreasing = T)], collapse = " ")
  item_comparison[i, "nfrankie"] = length(myfrankie[[i]]$chosen_items)
  item_select[i, "ifrankie"] = paste(myfrankie[[i]]$chosen_items[order(myfrankie[[i]]$chosen_items, decreasing = T)], collapse = " ")
  item_comparison[i, "nila"] = nrow(myila[[i]]$end)
  item_select[i, "iila"] = paste(rownames(myila[[i]]$end)[order(rownames(myila[[i]]$end), decreasing = T)], collapse = " ")
}
sum(item_comparison$nfrank > item_comparison$nila)  
sum(item_comparison$nfrank == item_comparison$nila)  
item_comparison[which(item_comparison$nfrank == item_comparison$nila), ]


tempAttempt = myila[[4]]$select_attempts[[length(myila[[4]]$select_attempts)]]
ilaGraph = tempAttempt[, c("theta", "type","mean_tif.x")]
colnames(ilaGraph) = c("theta",    "type", "mean_tif")
# ilaGraph$algortithm = "ILA"

ggplot(ilaGraph, 
       aes(x = theta, y = mean_tif, color = type)) + geom_line()

frankLong =  pivot_longer(myfrank[[4]]$tif, 
                                         cols = !theta, 
                                         names_to = "tif")
colnames(frankLong) = colnames(ilaGraph)

allGraphs = rbind(ilaGraph, frankLong)

ggplot(allGraphs[allGraphs$type %in% c("mean_tif", "temp_4", "temp_tif_4"), ], 
       aes( x= theta, y = mean_tif, color = type)) + geom_line()






fourthiteration = ilafrank(myila, myfrank, myfrankie,n = 2)

ggplot(fourthiteration[fourthiteration$type %in% c("TARGET", "frankie_tif_2", "frank_tif_5"), ], 
       aes( x= theta, y = mean_tif, color = type)) + geom_line()

ggplot(fourthiteration, 
       aes( x= theta, y = mean_tif, color = type)) + geom_line() + 
  ggtitle(paste("frank = ", item_comparison[2, "nfrank"], 
                "frankie = ", item_comparison[2, "nfrankie"], 
                "ila =", item_comparison[2, "nila"])) +  
  theme(legend.position = c(0.7, 0.7)) + 
  scale_color_manual(values = c("red", "royalblue", "seagreen", "black"))
# rosso franke
# blue frankie
# verde ila 
# nera target

# voglio printare tutte le iterazioni per la mia salute mentale 
temp = NULL 
iter_graph = NULL 
g=list()
for (i in 1:length(myila)) {
  temp = ilafrank(myila, myfrank, myfrankie,n = i)
  temp$iter = i
  temp$title = paste("frank = ", item_comparison[i, "nfrank"], 
                     "frankie = ", item_comparison[i, "nfrankie"], 
                     "ila =", item_comparison[i, "nila"])
  g[[i]] = ggplot(temp, 
                  aes( x= theta, y = mean_tif, color = type)) + geom_line(linewidth=1.2) + 
    ggtitle(paste("iter = ", i, ", frank = ", item_comparison[i, "nfrank"], 
                  ", frankie = ", item_comparison[i, "nfrankie"], 
                  ", ila =", item_comparison[i, "nila"])) + 
    scale_color_manual(values = c("red", "royalblue", "seagreen", "black")) +
    theme(legend.position = "none") 
#   print(g[[i]])
  iter_graph = rbind(temp, iter_graph)
  
}

library(patchwork)
wrap_plots(g)

# voglio vedere la distanza assoluta di ognuno degli algoritmi dalla target 
# nelle 10 iterazioi 
# da ila va preso myila[[n]]$select_attempts[[length(myila[[n]]$select_attempts)]]
# per frank e frankie è più complesso perché devo calcolare la media ad ongi iterazione 

tempFrank = NULL
tempfrankie = NULL 
tempIla = NULL 

distance_tif = data.frame(matrix(nrow=iterations, ncol = 3))
colnames(distance_tif) = c("ila","frank", "frankie")

for(i in 1:(iterations)) {
  tempFrank = myfrank[[i]]$tif
  tempFrank$temp_tif = rowMeans(tempFrank[, grep("temp_", colnames(tempFrank))])
  tempFrank$distance = abs(tempFrank$mean_tif - tempFrank$temp_tif)
  tempfrankie = myfrankie[[i]]$tif
  tempfrankie$temp_tif = rowMeans(tempfrankie[, grep("temp_", colnames(tempfrankie))])
  tempfrankie$distance = abs(tempfrankie$mean_tif - tempfrankie$temp_tif)
  
  distance_tif[i, "ila"] = mean(myila[[i]]$select_attempts[[length(myila[[i]]$select_attempts)]]$distance_tif)
  distance_tif[i, "frank"] = mean(tempFrank$distance)
  distance_tif[i, "frankie"] =mean(tempfrankie$distance)
  }

distance_tif

the_min = numeric(0)

for(i in 1:nrow(distance_tif)) {
  the_min[i] = which(distance_tif[i, ] == min(distance_tif[i, ]))  
}
table(the_min)

distance_tif = cbind(rownames(distance_tif), 
                     distance_tif)
distance_tif$ilafrankie = with(distance_tif, 
                             ila-frankie)
distance_tif$ilafrank = with(distance_tif, 
                               ila-frank)


ggplot(distance_tif, 
       aes(x = iter, y = ilafrankie)) + geom_point()


colnames(distance_tif)[1] = "iter"
dist_long = pivot_longer(distance_tif,
                         names_to = "alg", 
                         cols = !iter)


ggplot(dist_long, 
       aes(x = iter, y = value, 
           color = alg, shape=alg)) + geom_point(size = 2)


ggplot(iter_graph, 
       aes( x= theta, y = mean_tif, color = type)) + geom_line() + 
 facet_wrap(~iter)


