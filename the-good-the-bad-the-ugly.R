# confronot the good the bad and the ugly 
rm(list = ls())
library(tidyverse)
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
select_combos = function(vector, element_to_find) {
  my_selections = NULL
  for (i in element_to_find) {
    index <- 1
    while (index <= length(vector) && vector[index] != i) {
      index <- index + 1
    }
    my_selections = c(index, my_selections)
  }
  return(my_selections)
}
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
        tif_k = tif_k[,-ncol(tif_k)]
        index_item = index_item[-length(index_item)]
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
# bruto 
bruto = function(parameters, 
                        theta = seq(-4,4, length.out = 1000), 
                        length_target = NULL, 
                        add_difficulty = NULL, 
                        add_discriminativity = NULL, 
                        seed = 1312) {
  set.seed(seed)
  if (is.null(parameters)) {
    stop("I need the item paramters, you fool!")
  }
  
  if (is.null(length_target)) {
    length_target = 2:(nrow(parameters)-1)
    length_target = sample(length_target, 1)
  }
  # tutte le possibili combinazioni di lunghezza N 
  
  possible_comb = combn(nrow(parameters), length_target)
  index_selected_comb = sample(ncol(possible_comb), 1)
  sel_items_rows = possible_comb[, index_selected_comb]
  # seleziona gli item della forma breve target
  sel_items = parameters[sel_items_rows, ]
  if (is.null(add_difficulty)) {
    warning("Something will be randomly added to the difficulties of the items")
    add_difficulty = runif(1000, min = -0.2, 0.2)
    add_difficulty = sample(add_difficulty, nrow(sel_items))
  } else if (length(add_difficulty) > 1) {
    add_difficulty = sample(add_difficulty, nrow(sel_items))
  } 
  if (is.null(add_discriminativity)) {
    warning("Something will be randomly added to the discriminativities of the items")
    add_discriminativity = runif(1000, min =  -0.1, 0.1)
    add_discriminativity = sample(add_discriminativity, nrow(sel_items))
  } else if (length(add_discriminativity) > 1) {
    add_discriminativity = sample(add_discriminativity, nrow(sel_items))
  }
  # modifica i paramatri degli item per poter creare la tif target 
  sel_items$b = sel_items$b + add_difficulty
  sel_items$a = sel_items$a + add_discriminativity
  if (any(sel_items$a< 0) == TRUE ) {
    sel_items[which(sel_items$a < 0), "a"] = sel_items[which(sel_items$a < 0), "a"]*-1
    warning("There were negative discrimination parameters, I changed them!")
  } else {
    sel_items = sel_items 
  }
    # calcola la TIF TARGET MEDIA basata su qtheta = # calcola la TIF TARGET MEDIA basata su quegli item 
  iifs_target <- item_info(sel_items,
                    theta = theta)
  target_info = do.call("cbind", iifs_target)
  target_info = cbind(theta, target_info)
  target_info = data.frame(target_info)
  colnames(target_info)[-1] = rownames(sel_items)
  if (sum(grepl("item", colnames(target_info))) == 1) {
    target_info$mean_tif = target_info[,-1]
  } else {
    target_info$mean_tif = rowMeans(target_info[,-1]) 
  }
  # creo un dataframe ad hoc per poterlo unire a tutti gli altri 
  info_target = target_info[, c("theta", "mean_tif" )]
  info_target$category = length_target
  info_target$item = paste(colnames(target_info)[grep("item", colnames(target_info))], collapse = " ")
  info_target$new_item = "stf-target"
  # calcola le iif di tutti i possibili item 
  all_iifs = item_info(parameters, theta = theta)
  all_iifs = data.frame(do.call("cbind", all_iifs))
  colnames(all_iifs) = rownames(parameters)
  all_iifs = cbind(theta, all_iifs)
  # ora deve calcolare tutte le possibili forme brevi da 2 a nrow(parameters)-1 sui parametri iniziali 
  my_comb = 1:(nrow(parameters)-1)
  # ora estraggo le varie terzine cinquine eccetera 
  item = NULL
  mod = NULL
  mod_info = NULL
  info_temp = NULL
  info_tot = NULL
  combinations = NULL
  the_info_temp = NULL
  the_info = NULL
  # la J NULL# la NULL# la J NULL# la J poi deve operare attravreso le colonne della matrice dentro i 
  # una volta selezionati gli item, stima il modello, stima la TIF
  for(i in my_comb) {
    #browser()
    combinations =  combn(nrow(parameters), i)
    for (j in 1:(dim(combinations)[2])) {
      info_temp = all_iifs[, combinations[,j]+1, drop = F]
      info_temp = cbind(theta, info_temp)
      if (ncol(info_temp) == 2) {
        info_temp$mean_tif = info_temp[,-1]
      } else {
        info_temp$mean_tif = rowMeans(info_temp[,-1])  
      }
      lab_items = colnames(info_temp[grepl("item", colnames(info_temp))])
      info_temp = info_temp[, !grepl("item", colnames(info_temp))]
      info_temp$category = i
      info_temp$item = paste(lab_items, collapse = " ")
      info_temp$new_item = paste("combo", i, sep ="_")
      info_tot = rbind(info_temp, info_tot)
    }
    info_target_temp = info_target
    info_target_temp$category = i
    the_info_temp = rbind(info_target_temp, info_tot)
    the_info = rbind(the_info, the_info_temp)
  }
  all_info = the_info
  # ora vanno unite tutte quelle combo con quella iniziale 
  temp_ti = target_info[, c("theta", "mean_tif")]
  names(temp_ti)[grep("tif", colnames(temp_ti))] = paste("target", 
                                                         names(temp_ti)[grep("tif", colnames(temp_ti))], 
                                                         sep = "_")
  the_info = merge(temp_ti, info_tot)
  the_info$diff_mtif = abs(the_info$target_mean_tif - the_info$mean_tif)
  temp=NULL
  temp_mean = NULL
  difference = NULL
  for (i in unique(the_info$category)) {
    temp = the_info[the_info$category %in% i, ]
    temp_mean =   temp %>% 
      group_by(item) %>% 
      summarise(av_diff_mtif = mean(diff_mtif), sd_diff_mtif = sd(diff_mtif)) %>% 
      mutate(cat = i) 
    difference = rbind(difference, temp_mean)
  }
  diff_order = difference[order(difference$av_diff_mtif), ]
  my_selections = select_combos(diff_order$cat, my_comb)
  best_selections = diff_order[my_selections, "item"]$item
  tif_best = the_info[the_info$item %in% best_selections, ]
  best_sel =  diff_order[my_selections, ]
  best_sel = best_sel[order(best_sel$av_diff_mtif), ]
  # questo vede se la forma breve selezionata è la stessa di quella originale 
  the_output = list(info_target = info_target, 
                   # all_info = all_info, 
                  # distance = diff_order, 
                    #best_selections_info = tif_best, 
                    best_selections_summary = best_sel,
                    the_info = the_info, 
                   # same_sel =  diff_order$item[1] == info_target$item[1], 
                    starting_items = rownames(parameters))
  
  return(the_output)
}
isa = function(TIF, parameters, print = FALSE) {
  # tif target
#  browser()
  mygraphs = list()
  data_graphs = list()
  mytif = TIF
  token = TRUE
  mysel = NULL
  interim_infos = list()
  interim_infos_temp = list()
  browser()
  for (i in 1:nrow(parameters)) {
    if (token == TRUE) {
      if (i == 1) {
        parameters$temp_theta = mytif[which(mytif$mean_tif == max(mytif$mean_tif)), "theta"] 
      } else {
        parameters$temp_theta = widesel[which(widesel$difference == max(widesel$difference)), "theta"]
        
      }
      # per il 2 pl conviene basare tutto sula IIF
      
      parameters$temp_iif =  parameters$a^2 *(IRT(parameters$temp_theta,
                                                  parameters$a, parameters$b) * (1-IRT(parameters$temp_theta, parameters$a, parameters$b)))
      if (is.null(mysel)) {
        tempsel = parameters[which(parameters$temp_iif == max(parameters$temp_iif)), ]
        mysel = rbind(mysel, tempsel)
      } else {
        tempparameters = parameters[rownames(parameters)[!rownames(parameters) %in% rownames(mysel)], ]
        tempsel = tempparameters[which(tempparameters$temp_iif == max(tempparameters$temp_iif)), ]
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
      temp_info$mean_tif =temp_info$tif/ nrow(mysel)
      temp_info_small = temp_info[, c("theta", "tif", "mean_tif")]
      temp_info_small$type = paste("temp", nrow(mysel), sep = "_")
      thesel = mytif 
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
b <- runif(6, -3, 3)
a <- runif(6, 0.9, 2)
parameters = data.frame(a,b)
rownames(parameters) = paste("item", rownames(parameters), sep = "_")
theta = seq(-4,4, length.out = 1000)

parameters = list()
iterations = 100
resBruto = list()

for (i in 1:iterations) {
  set.seed(i)
  parameters[[i]] = data.frame(b = runif(6, -3, 3), 
                               a = runif(6, 0.9, 2))
  rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
  resBruto[[i]] = bruto(parameters[[i]], theta, 
                        add_difficulty = runif(nrow(parameters[[i]]), -.2, .2),
                        add_discriminativity  = runif(nrow(parameters[[i]]), -.2, .2), 
                        seed = i)
}

# bruto è la miglior forma breve possibile. adesso voglio fare lo stesso usando ila 
# e frank 
myfrank = list()
myila = list()
myisa = list()
for (i in 1:iterations) {
  myfrank[[i]] = frank(resBruto[[i]]$info_target[, c("theta", "mean_tif")], 
                       parameters = parameters[[i]])
  myila[[i]] = ila(resBruto[[i]]$info_target[, c("theta", "mean_tif")], 
                       parameters = parameters[[i]], 
                   print = FALSE)
  # myisa[[i]] = isa(resBruto[[i]]$info_target[, c("theta", "mean_tif")], 
  #                  parameters = parameters[[i]], 
  #                  print = FALSE)
}


item_comparison = data.frame(matrix(nrow = iterations, ncol= 1))
colnames(item_comparison) = "nbruto"
item_select = data.frame(matrix(nrow = iterations, ncol= 1))
colnames(item_select) = "items"

for (i in 1:length(myfrank)) {
  item_comparison[i, "nbruto"] = resBruto[[i]]$info_target$category[1]
  item_select[i, "items"] = resBruto[[i]]$info_target$item[1]
  item_comparison[i, "nfrank"] = length(myfrank[[i]]$chosen_items)
  item_select[i, "ifrank"] = paste(myfrank[[i]]$chosen_items[order(myfrank[[i]]$chosen_items, decreasing = T)], collapse = " ")
  item_comparison[i, "nila"] = nrow(myila[[i]]$end)
  item_select[i, "iila"] = paste(rownames(myila[[i]]$end)[order(rownames(myila[[i]]$end), decreasing = T)], collapse = " ")
}
# I saved the enrivorment till here 

x = item_select[1,-3]



equal = as.numeric(rownames(item_comparison[which(item_comparison$nfrank == item_comparison$nila), ]))
item_comparison[which(item_comparison$nfrank == item_comparison$nila), ]
nrow(item_comparison[which(item_comparison$nfrank == item_comparison$nila), ])
item_comparison[item_comparison$nbruto > item_comparison$nfrank, ]
item_comparison[item_comparison$nbruto == item_comparison$nfrank, ]
nrow(item_comparison[item_comparison$nbruto == item_comparison$nfrank, ])
item_comparison[item_comparison$nbruto == item_comparison$nila, ]
nrow(item_comparison[item_comparison$nbruto == item_comparison$nila, ])
# blue bruto
# rosso franke
# verde ila 
# nera target


temp = NULL 
tempBruto = NULL
iter_graph = NULL 
g=list()
for (i in 1:length(myfrank)) {
  temp = ilafrank(myila, myfrank,n = i)
  temp$iter = i
  temp$title = paste("frank=", item_comparison[i, "nfrank"], 
                     "ila=", item_comparison[i, "nila"])
  tempBruto = resBruto[[i]]$the_info 
  tempBruto = tempBruto[tempBruto$item %in% resBruto[[i]]$best_selections_summary$item[1], 
                            c("theta", "new_item", "mean_tif")]
  colnames(tempBruto)[2] = "type"
  tempBruto$type = gsub("combo", "bruto", tempBruto$type)
  tempBruto$iter = i
  tempBruto$title = paste("Bruto= ", resBruto[[i]]$best_selections_summary$cat[1])
  temp = rbind(tempBruto, temp)
  g[[i]] = ggplot(temp, 
                  aes( x= theta, y = mean_tif, color = type)) + 
    geom_line(linewidth=1.2) + 
    ggtitle(paste("frank=", item_comparison[i, "nfrank"], 
                  "ila=", item_comparison[i, "nila"], 
                  paste("bruto=", resBruto[[i]]$best_selections_summary$cat[1]))) + 
    scale_color_manual(values = c("royalblue","red",  "seagreen", "black"))  +
    theme(legend.position = "none") 
  #   print(g[[i]])
  iter_graph = rbind(temp, iter_graph)
  
}
library(patchwork)
wrap_plots(g[equal])
g[[1]]

item_comparison$iter = 1:100
ic_long = pivot_longer(item_comparison, cols = !iter)

ggplot(ic_long, 
       aes(x = name, y = value)) + geom_boxplot()


ic = ic_long %>%  
  group_by(name) %>%  
  summarise(min = min(value), max = max(value), mean = mean(value), median = mean(value), 
            sd = sd(value))

ggplot(ic, 
       aes(x = name, y = mean, color = name)) + geom_point(shape = 5, size = 6) + ylim(0,6) + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = .2)+ 
  geom_point(aes(x = name, y = min), shape = 19, size = 6) + geom_point(aes(x = name, y = max), 
                                                                       shape = 15, size = 6) + 
  geom_point(aes(x = name, y = median), shape = 4, size = 6) + 
  scale_color_manual(values = c("royalblue","red",  "seagreen")) + 
  ylab("Item") + theme_light() + 
  theme(axis.text = element_text(size = 25), 
        axis.title.x = element_blank(), 
        legend.position = "none")


apply(item_comparison,2, min)apply(item_comparisiteron,2, min)
apply(item_comparison,2, max)
apply(item_comparison,2, median)
apply(item_comparison,2, mean)


# calcolo la distanza media per ogni iteraione di frank e ila da bruto 
# per non sbagliare mi calcolo anche la distanza media di bruto 


# questa calcola la distanza dalla target ----
tempBruto = NULL
tempBF = NULL
allBrutos = matrix(nrow = 1000)
allBF = matrix(nrow = 1000)
tempIla = NULL
tempBI = NULL
allIlas = matrix(nrow = 1000)
allBI = matrix(nrow=1000)
tempFrank = NULL
tempTarget = NULL
allTarget =  matrix(nrow = 1000)
allFranks = matrix(nrow = 1000)
for (i in 1:length(resBruto)) {
  target = resBruto[[i]]$info_target[, c("theta", "mean_tif")]
  colnames(target)[2] = "tif_target"
  tempTarget = data.frame(target$tif_target)
  colnames(tempTarget) = paste(colnames(tempTarget), i, sep = "_")
  allTarget = cbind(allTarget, tempTarget)
  tempBruto = resBruto[[i]]$the_info
  tempBruto = tempBruto[tempBruto$item %in% resBruto[[i]]$best_selections_summary$item[1], 
                        "mean_tif"]
  tempBF = tempBruto
  tempBruto = data.frame(diff_target_bruto = abs(target$tif_target - tempBruto))
  colnames(tempBruto) = paste(colnames(tempBruto), i, sep = "_")
  allBrutos = cbind(allBrutos, tempBruto)
  
  tempFrank = myfrank[[i]]$tif
  tempFrank =  data.frame(diff_target_frank = abs(target$tif_target - tempFrank[, ncol(tempFrank)])) 
  colnames(tempFrank) = paste(colnames(tempFrank), i, sep = "_")
  allFranks = cbind(allFranks, tempFrank)
  tempBF = data.frame(diff_bruto_frank = abs(tempBF -  tempFrank[, ncol(tempFrank)]))
  colnames(tempBF) = paste(colnames(tempBF), i, sep = "_")
  allBF = cbind(allBF, tempBF)
  
  tempIla = data.frame(diff_target_ila = myila[[i]]$select_attempts[[length(myila[[i]]$select_attempts)]]$distance_tif)
  colnames(tempIla) = paste(colnames(tempIla), i, sep = "_")
  allIlas = cbind(allIlas, tempIla)
  
  tempBI = data.frame(diff_bruto_ila = abs(tempBF -  myila[[i]]$select_attempts[[length(myila[[i]]$select_attempts)]]$mean_tif.x))
  colnames(tempBI) = paste(colnames(tempBI), i, sep = "_")
  allBI = cbind(allBI, tempBI)
}

allIlas$allIlas = target$theta
longIlas = pivot_longer(allIlas, cols = !allIlas, names_to = "iter")
allBrutos$allBrutos = target$theta
longBrutos = pivot_longer(allBrutos, cols = !allBrutos, names_to = "iter")
allFranks$allFranks = target$theta
longFranks = pivot_longer(allFranks, cols = !allFranks, names_to = "iter")

longIlas$iter = as.numeric(gsub("diff_target_ila_", "", longIlas$iter))
longFranks$iter = as.numeric(gsub("diff_target_frank_", "", longFranks$iter))
longBrutos$iter = as.numeric(gsub("diff_target_bruto_", "", longBrutos$iter))

compIla = item_comparison[, c("iter", "nila")]
colnames(compIla)[2] = "l_stf"
compFrank = item_comparison[, c("iter", "nfrank")]
colnames(compFrank)[2] = "l_stf"
compBruto = item_comparison[, c("iter", "nbruto")]
colnames(compBruto)[2] = "l_stf"

longIlas = merge(longIlas, compIla, by = "iter")
longIlas$algo = "ila"
longFranks = merge(longFranks, compFrank, by = "iter")
longFranks$algo = "frank"
longBrutos = merge(longBrutos, compBruto, by = "iter")
longBrutos$algo = "bruto"
colnames(longBrutos)[2] = "theta"
colnames(longIlas)[2] = "theta"
colnames(longFranks)[2] = "theta"

allalgo = rbind(longBrutos, longIlas, longFranks)

ggplot(allalgo, 
       aes(x = factor(l_stf), y = value, color = algo)) + geom_violin()

ggplot(allalgo, 
       aes(x = factor(l_stf), y = value, color = algo)) + geom_boxplot() + theme_light() +
  ylab(expression(paste(TIF^"* - ", TIF[STF]))) +  xlab("Length STF") +
  scale_color_manual(values = c("royalblue","red",  "seagreen")) +
  theme(axis.title = element_text(size = 28), 
        axis.text = element_text(size = 26), 
        legend.position = "none")


sum_algo = allalgo %>%  
  group_by(algo, l_stf) %>%  
  summarise(mean = mean(value), sd = sd(value), n()/100000)

ggplot(sum_algo, 
       aes(x = l_stf, y = mean, color = algo, 
           shape = algo)) + geom_line(linewidth = 1.2) + geom_point(size = 4) + theme_light() +
  ylab(expression(paste(TIF^"* - ", TIF[STF]))) +  xlab("Length STF") +
  scale_color_manual(values = c("royalblue","red",  "seagreen")) +
  theme(axis.title = element_text(size = 28), 
        axis.text = element_text(size = 26), 
        legend.position = "none") + 
  scale_x_continuous(n.breaks = 6)



allBrutos = allBrutos[,-1]
allFranks = allFranks[,-1]
allIlas = allIlas[,-1]
allTarget = allTarget[,-1]
allBI = allBI[,-1]
allBF = allBF[,-1]


equal_IF = as.numeric(rownames(item_comparison[item_comparison$nfrank == item_comparison$nila, ]))
smaller_IF = as.numeric(rownames(item_comparison[item_comparison$nfrank > item_comparison$nila, ]))
greater_IF = as.numeric(rownames(item_comparison[item_comparison$nfrank < item_comparison$nila, ]))

wrap_plots(g[equal_IF])
wrap_plots(g[greater_IF])
wrap_plots(g[smaller_IF])


# mi prendo queste combinazioni più lunghe più brevi eccetera e calcolo la media per queste forme brevi 
equal_franks = allFranks[, equal_IF]
equal_ila = allIlas[, equal_IF]
equal_bruto = allBrutos[, equal_IF]
equal_target = allTarget[, equal_IF]

equal = data.frame(theta = target$theta, 
                   mfrank = rowMeans(equal_franks), 
                   mila = rowMeans(equal_ila), 
                   mbruto = rowMeans(equal_bruto), 
                   mtarget = rowMeans(equal_target))
equal = pivot_longer(equal, cols=!theta, names_to = "algo")

ggplot(equal, 
       aes(x = theta, y = value, color = algo)) + geom_line()


colMeans(allIlas[,as.numeric(rownames(item_comparison[item_comparison$nbruto == item_comparison$nila, ]))])
# interesezioni tra  bruto e frank 
bf = (apply(item_select[,-3], 1, function(x) intersect(strsplit(x[1], " ")[[1]],strsplit(x[2], " ")[[1]])))
# intersezione tra bruto e ila  
bi = (apply(item_select[,-2], 1, function(x) intersect(strsplit(x[1], " ")[[1]],strsplit(x[2], " ")[[1]])))
item_comparison$bf_inter = sapply(bf, length)
item_comparison$bi_inter = sapply(bi, length)

item_select[item_comparison$bf_inter == item_comparison$bi_inter, ]
item_select[item_comparison$bi_inter == 0, ]


item_comparison = item_comparison %>% 
  mutate(filter = if_all(nfrank:nila,  `==`, nbruto))
table(item_comparison$filter)

item_equal = item_comparison[item_comparison$filter == "TRUE", ]
select_equal = item_select[as.numeric(rownames(item_equal)),]

select_equal1 = select_equal

tempFrank = NULL
tempIla = NULL

for (i in 1:nrow(select_equal1)) {
  tempFrank = unlist(strsplit(select_equal[i, "ifrank"], split = " "))
  tempFrank = tempFrank[order(tempFrank, decreasing = F)]
  tempFrank = paste(tempFrank, collapse = " ")
  
  tempIla = unlist(strsplit(select_equal[i, "iila"], split = " "))
  tempIla = tempIla[order(tempIla, decreasing = F)]
  tempIla = paste(tempIla, collapse = " ")
  
  select_equal1[i, "ifrank"] = tempFrank
  select_equal1[i, "iila"] = tempIla
}

select_equal1 = select_equal1 %>% 
  mutate(is_equal_all = if_all(items:ifrank, `==`, iila))

table(select_equal1$is_equal_all)

rownames(select_equal1[select_equal1$is_equal_all == TRUE, ])
# faccio il grafico esatto diq aundo sono effettivamente tutte della stessa lunghezza
all_equals = as.numeric(rownames(select_equal1[select_equal1$is_equal_all == FALSE, ])
)
wrap_plots(g[all_equals])


mean_distances = data.frame(bruto = rowMeans(allBrutos), 
                            frank = rowMeans(allFranks), 
                            ila = rowMeans(allIlas), 
                            bi = rowMeans(allBI), 
                            bF = rowMeans(allBF))
round(colMeans(mean_distances), 3)
mean_distances = cbind(target$theta, mean_distances)
colnames(mean_distances)[1] = "theta"

plot(mean_distances$bruto, mean_distances$ila, asp = 1)
plot(mean_distances$bruto, mean_distances$frank, asp = 1)

mean_distances = pivot_longer(mean_distances, cols = !theta, 
                              names_to = "algorithm")


ggplot(mean_distances, 
       aes( x= theta, y = value, color = algorithm)) + geom_line()

allBrutos$mean_distance = rowMeans(allBrutos)
allBrutos = cbind(target, allBrutos)

ggplot(allBrutos, 
       aes(x = theta, y = mean_distance))


colnames(tempBruto)[-1] = c("type_frank", "tif_bruto")
tempBruto$type = gsub("combo", "bruto", tempBruto$type)

tempFrank = myfrank[[1]]$tif
tempFrank = tempFrank[, c(1, ncol(tempFrank))]
colnames(tempFrank) = gsub("temp", "frank", colnames(tempFrank))

tempIla = myila[[1]]$select_attempts[[length(myila[[1]]$select_attempts)]]$distance_tif
tempIla = tempIla[, c("theta", "type","mean_tif.x")]
colnames(tempIla) = c("theta",    "type_ila", "tif_ila")
tempIla$type = gsub("temp", "ila", tempIla$type)

all_tifs = merge(target, tempIla, by = "theta")
all_tifs = merge(all_tifs, tempFrank, by = "theta")
all_tifs = merge(all_tifs, tempBruto, by = "theta")


# cose prese dalla presentazione per psicostata 


tif_k = myfrank[[1]]$tif
tif_k1 = tif_k[,-ncol(tif_k)]
head(tif_k1)

myfrank[[1]]$chosen_items[-length(myfrank[[1]]$chosen_items)]

myt = pivot_longer(tif_k, cols=!theta)

ggplot(myt, 
       aes(x = theta, y = value, color = name)) + geom_line()
