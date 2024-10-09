# simulazione per stabilire quale è la massima accuratezza raggiungibile 
# con l'algoritmo intelligente Iselect 

# die algorithm

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

# questa funzione serve per selezionare (dal dataframe oridnato per differenze di tif medie )
# le forme brevi con diversa lunghezza più vicine possibili alla forma target
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
# questa è la forza bruta che prova tutte le possibili combinazioni di item 
# da 1 a n item - 1
select_items = function(parameters, 
                        theta = seq(-5,5, length.out = 1000), 
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
  iifs <- item_info(sel_items,
                    theta = theta)
  target_info = do.call("cbind", iifs)
  target_info = cbind(theta, target_info)
  target_info = data.frame(target_info)
  colnames(target_info)[-1] = rownames(sel_items)
  if (sum(grepl("item", colnames(target_info))) == 1) {
    target_info$tif = target_info[,-1]
  } else {
    target_info$tif = rowSums(target_info[,-1]) 
  }
  # calcola la tif media 
  target_info$mean_tif = target_info$tif/sum(grepl("item", colnames(target_info)))
  # creo un dataframe ad hoc per poterlo unire a tutti gli altri 
  info_target = target_info[, c("theta", "tif", "mean_tif" )]
  info_target$category = length_target
  info_target$item = paste(colnames(target_info)[grep("item", colnames(target_info))], collapse = " ")
  info_target$new_item = "stf-target"
  # calcola le iif di tutti i possibili cosi 
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
      # item = parameters[combinations[,j], ]
      # info_temp = item_info(item, 
      #                       theta = theta)
      # info_temp = do.call("cbind", info_temp)
      # info_temp = data.frame(cbind(theta, info_temp))
      # colnames(info_temp)[-1] = rownames(item)
      # browser()
      info_temp = all_iifs[, combinations[,j]+1, drop = F]
      info_temp = cbind(theta, info_temp)
      if (ncol(info_temp) == 2) {
        info_temp$tif = info_temp[,-1]
      } else {
        info_temp$tif = rowSums(info_temp[,-1])  
      }
      info_temp$mean_tif = info_temp$tif/length(colnames(info_temp)[grepl("item", colnames(info_temp))])
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
  temp_ti = target_info[, c("theta", "tif", "mean_tif")]
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
                    all_info = all_info, 
                    distance = diff_order, 
                    best_selections_info = tif_best, 
                    best_selections_summary = best_sel,
                    the_info = the_info, 
                    same_sel =  diff_order$item[1] == info_target$item[1], 
                    starting_items = rownames(parameters))
  
  return(the_output)
}
select_range = function(parameters, 
                        theta = seq(-5,5, length.out = 1000), 
                        min_length = 1, 
                        max_length = NULL, 
                        add_difficulty = NULL, 
                        add_discriminativity = NULL, 
                        seed = 1312) {
  set.seed(seed)
  if (is.null(parameters)) {
    stop("I need the item paramters, you fool!")
  }
  if (is.null(add_difficulty)) {
    warning("Something will be randomly added to the difficulties of the items")
    add_difficulty = runif(1000, min = -0.2, 0.2)
    add_difficulty = sample(add_difficulty, 1)
  }
  if (is.null(add_discriminativity)) {
    warning("Something will be randomly added to the discriminativities of the items")
    add_discriminativity = runif(1000, min =  -0.1, 0.1)
    add_discriminativity = sample(add_discriminativity, 1)
  }
  if (!is.null(max_length) & min_length != 1) {
    length_target = min_length:max_length
    length_target = sample(length_target, 1)
  }
  # tutte le possibili combinazioni di lunghezza N 
  possible_comb = combn(nrow(parameters), length_target)
  index_selected_comb = sample(ncol(possible_comb), 1)
  sel_items_rows = possible_comb[, index_selected_comb]
  # seleziona gli item della forma breve target
  sel_items = parameters[sel_items_rows, ]
  # modifica i paramatri degli item per poter creare la tif target 
  sel_items$b = sel_items$b + add_difficulty
  sel_items$a = sel_items$a + add_discriminativity
  # calcola la TIF TARGET MEDIA basata su quegli item 
  iifs <- item_info(sel_items,
                    theta = theta)
  target_info = do.call("cbind", iifs)
  target_info = cbind(theta, target_info)
  target_info = data.frame(target_info)
  colnames(target_info)[-1] = rownames(sel_items)
  if (sum(grepl("item", colnames(target_info))) == 1) {
    target_info$tif = target_info[,-1]
  } else {
    target_info$tif = rowSums(target_info[,-1]) 
  }
  # calcola la tif media 
  target_info$mean_tif = target_info$tif/sum(grepl("item", colnames(target_info)))
  # creo un dataframe ad hoc per poterlo unire a tutti gli altri 
  info_target = target_info[, c("theta", "tif", "mean_tif" )]
  info_target$category = length_target
  info_target$item = paste(colnames(target_info)[grep("item", colnames(target_info))], collapse = " ")
  info_target$new_item = "stf-target"
  # ora deve calcolare tutte le possibili forme brevi da 2 a nrow(parameters)-1 sui parametri iniziali 
  my_comb = min_length:max_length
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
    combinations =  combn(nrow(parameters), i)
    for (j in 1:(dim(combinations)[2])) {
      item = parameters[combinations[,j], ]
      info_temp = item_info(item, 
                            theta = theta)
      info_temp = do.call("cbind", info_temp)
      info_temp = data.frame(cbind(theta, info_temp))
      colnames(info_temp)[-1] = rownames(item)
      if (ncol(info_temp) == 2) {
        info_temp$tif = info_temp[,-1]
      } else {
        info_temp$tif = rowSums(info_temp[,-1])  
      }
      info_temp$mean_tif = info_temp$tif/length(colnames(info_temp)[grepl("item", colnames(info_temp))])
      lab_items =  rownames(item)
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
  temp_ti = target_info[, c("theta", "tif", "mean_tif")]
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
                    all_info = all_info, 
                    distance = diff_order, 
                    best_selections_info = tif_best, 
                    best_selections_summary = best_sel,
                    the_info = the_info, 
                    same_sel =  diff_order$item[1] == info_target$item[1], 
                    starting_items = rownames(parameters))
  
  return(the_output)
}
plot_tif = function(the_output, the_combos = NULL) {
  tif_best = the_output$the_info[the_output$the_info$item %in% the_output$best_selections_info$item, ]
  # aggiungo la target 
  temp_tif_best = tif_best[, colnames(tif_best) %in% colnames(the_output$info_target)]
  tif_best = rbind(temp_tif_best, the_output$info_target)
  if(is.null(the_combos)) {
    g_tif_best = ggplot(tif_best,
                        aes(x = theta, y = mean_tif,
                            color = new_item,
                            linetype = new_item)) + geom_line() 
  } else {
    g_tif_best = ggplot(tif_best[tif_best$new_item %in% the_combos, ],
                        aes(x = theta, y = mean_tif,
                            color = new_item,
                            linetype = new_item)) + geom_line() 
  }
  
  
  # grafico delle tif medie per ogni possibile combianzione
  all_info = the_output$all_info
  all_info$dummy = ifelse(all_info$new_item == "stf-target", "target", "other")
  g_dummy = ggplot(all_info,
                   aes(x = theta, y = mean_tif, color = new_item,
                       group = item)) + geom_line() + facet_wrap(~category)
  the_graphs = list(tif_best = g_tif_best, dummy= g_dummy) 
  return(the_graphs)
}
distance = function(element, best = NULL) {
  if (is.null(best)) {
    recovered_items = strsplit(as.character(element$best_selections_summary[1, "item"]), 
                               split = " ")[[1]]
  } else {
    recovered_items = strsplit(as.character(element$best_selections_summary[best, "item"]), 
                               split = " ")[[1]]
  }
  starting_items = element$starting_items
  target_items = strsplit(unique(element$info_target$item), 
                          split = " ")[[1]]
  items = data.frame(items = starting_items, 
                     target_items = numeric(length(starting_items)), 
                     recovered_items = numeric(length(starting_items)))
  
  not_chosen_target = starting_items[!starting_items %in% target_items]
  not_chosen_recovered = starting_items[!starting_items %in% recovered_items]
  
  items[!items$items %in% not_chosen_target, "target_items"] = 1
  items[!items$items %in% not_chosen_recovered, "recovered_items" ] = 1
  items$distance = items$target_items == items$recovered_items
  return(items)
}
distance_select = function(brute_force, algorithm) {
  starting_items = brute_force$starting_items
  best_items = strsplit(as.character(brute_force$best_selections_summary[1, "item"]), 
                        split = " ")[[1]]
  recovered_items = rownames(algorithm$end)
  items = data.frame(items = starting_items, 
                     best_items = numeric(length(starting_items)), 
                     recovered_items = numeric(length(starting_items)))
  
  not_chosen_target = starting_items[!starting_items %in% best_items]
  not_chosen_recovered = starting_items[!starting_items %in% recovered_items]
  
  items[!items$items %in% not_chosen_target, "best_items"] = 1
  items[!items$items %in% not_chosen_recovered, "recovered_items" ] = 1
  items$distance = items$best_items == items$recovered_items
  return(items)
}
# sensitivity -----
sensitivity = function (distance_element) {
  if (any(grepl("best", colnames(distance_element))) == TRUE) {
    name = "best_items"
  } else {
    name = "target_items"
  }
  spec_sens = xtabs(~ distance_element[, name] + recovered_items, distance_element)
  specificity = spec_sens[1, 1]/(spec_sens[1,1] + spec_sens[1,2])
  sensitivity = spec_sens[2, 2]/(spec_sens[2,1] + spec_sens[2,2])
  accuracy = (spec_sens[1,1] + spec_sens[2,2])/(spec_sens[1,1] + spec_sens[1,2] + spec_sens[2,1] + spec_sens[2,2])
  all_details = list(distance = distance_element,
                     cross_table = spec_sens, 
                     specificity = specificity, 
                     sensitivity = sensitivity, 
                     accuracy = accuracy)
  return(all_details)
}
# Iselect 
Ilocate = function(TIF, parameters, print = TRUE) {
  # tif target
  # browser()
  mygraphs = list()
  data_graphs = list()
  mytif = TIF
  token = TRUE
  mysel = NULL
  interim_infos = list()
  interim_infos_temp = list()
  #browser()
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
      # per il 2 pl conviene basare tutto sula IIF
      
      # parameters$temp_iif =  parameters$a^2 *(IRT(parameters$temp_theta, 
      #                       parameters$a, parameters$b) * (1-IRT(parameters$temp_theta, parameters$a, parameters$b)))
      #  if (is.null(mysel)) {
      #    tempsel = parameters[which(parameters$temp_iif == max(parameters$temp_iif)), ]
      #    mysel = rbind(mysel, tempsel)
      #  } else {
      #    tempparameters = parameters[rownames(parameters)[!rownames(parameters) %in% rownames(mysel)], ]
      #    tempsel = tempparameters[which(tempparameters$temp_iif == max(tempparameters$temp_iif)), ]
      #    mysel = rbind(mysel, tempsel)
      #    
      #  }
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

Iselect = function(TIF, parameters, print = TRUE) {
  # tif target
  # browser()
  mygraphs = list()
  data_graphs = list()
  mytif = TIF
  token = TRUE
  mysel = NULL
  interim_infos = list()
  interim_infos_temp = list()
  #browser()
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
the_graphs = function(smart_element) {
  prova_grafici = smart_element$the_graphs[[1]]$data
  for (i in 2:(length(smart_element$the_graphs)-1)) {
    temp = smart_element$the_graphs[[i]]$data[!smart_element$the_graphs[[2]]$data$type %in% "target", ]
    prova_grafici = rbind(prova_grafici, temp)
  }
  prova_grafici$new_type = gsub("temp_", "", prova_grafici$type)
  prova_grafici$new_type = gsub("target", 0, prova_grafici$new_type)
  prova_grafici$new_type = as.integer(prova_grafici$new_type)
  g = ggplot(prova_grafici, 
             aes(x = theta, y = mean_tif, 
                 color = type)) + geom_line() + 
    theme(legend.position =  "bottom") 
  return(g)
}

iterations = 100
both = list()
items_select_both = list()
items_locate_both = list()
from_both_select = list()
sensitivity_algorithm_select = list()
sensitivity_systematic_select = list()
summary_sensitivity_select_both = data.frame(accuracy_algo = numeric(length(iterations)), 
                                             spec_algo = numeric(length(iterations)), 
                                             sens_algo = numeric(length(iterations)), 
                                             accuracy_syst = numeric(length(iterations)), 
                                             spec_syst = numeric(length(iterations)), 
                                             sens_syst = numeric(length(iterations)))


items_locate_both = list()
from_both_locate = list()
sensitivity_algorithm_locate = list()
sensitivity_systematic_locate = list()

summary_sensitivity_locate_both = data.frame(accuracy_algo = numeric(length(iterations)), 
                                             spec_algo = numeric(length(iterations)), 
                                             sens_algo = numeric(length(iterations)), 
                                             accuracy_syst = numeric(length(iterations)), 
                                             spec_syst = numeric(length(iterations)), 
                                             sens_syst = numeric(length(iterations)))

temp_locate_both = NULL
temp_select_both = NULL
my_locate_both = list()
my_select_both = list()
my_distance_both = NULL
parameters_both = list()
theta_both = list()
set.seed(123)
# mod_indexes = data.frame(runif(100, -.20, .20), 
#                          runif(100, -.10, .20))
mod_b = list()
mod_a = list()
# crea vincolo per le discriminatività 
# trovato il modo di mettere il vincolo
# mod_a = runif(6, -4, 1)
# 
# par$mod = par$a + mod_a
# par
# if (any(par$mod< 0) == TRUE ) {
#   par[which(par$mod < 0), "mod"] = par[which(par$mod < 0), "mod"]*-1
# } else {
#   par = par 
# }


for (i in 1:iterations) {
  set.seed(i)
  # Simulate person and item parameters_both
  b <- runif(6, -3, 3)
  a <- runif(6, 0.9, 2)
  parameters_both[[i]] <- data.frame(b, a)
  rownames(parameters_both[[i]]) = paste("item", 1:nrow(parameters_both[[i]]), sep ="_")
  # theta_both_temp <- matrix(rnorm(1000, 0))
  # theta_both[[i]] <- theta_both_temp[order(theta_both_temp[,1]),]
  starting_items=rownames(parameters_both[[i]])
  mod_b[[i]] = runif(length(b), -0.20, 0.20)
  mod_a[[i]] = runif(length(a), -.20, 0.20)
  both[[i]] = select_items(parameters_both[[i]], theta = seq(-4,4, length.out=5000), #theta_both[[i]], 
                           add_difficulty = mod_b[[i]], 
                           add_discriminativity = mod_a[[i]], 
                           seed = i)
  # starting_items = both[[i]]$starting_items
  # target_items = strsplit(unique(both[[i]]$info_target$item), 
  #                         split = " ")[[1]]
  # items_select_both[[i]] = data.frame(items = starting_items, 
  #                                target_items = numeric(length(starting_items)), 
  #                                recovered_items = numeric(length(starting_items)))
  # items_locate_both[[i]] = data.frame(items = starting_items, 
  #                                target_items = numeric(length(starting_items)), 
  #                                recovered_items = numeric(length(starting_items)))
  # selecy with I select
  from_both_select[[i]] = Iselect(both[[i]]$info_target[, c("theta", "tif", "mean_tif")],
                                  parameters_both[[i]], print = FALSE)
  # temp_recovered_select = rownames(from_both_select[[i]]$end)[order(rownames(from_both_select[[i]]$end))]
  # not_chosen_target_select = starting_items[!starting_items %in% target_items]
  # not_chosen_recovered_select = starting_items[!starting_items %in% temp_recovered_select]
  # select with i locate 
  from_both_locate[[i]] = Ilocate(both[[i]]$info_target[, c("theta", "tif", "mean_tif")],
                                  parameters_both[[i]], print = FALSE)
  # temp_recovered_locate = rownames(from_both_locate[[i]]$end)[order(rownames(from_both_locate[[i]]$end))]
  # not_chosen_target_locate = starting_items[!starting_items %in% target_items]
  # not_chosen_recovered_locate = starting_items[!starting_items %in% temp_recovered_locate]
  # 
  # items_select_both[[i]][!items_select_both[[i]]$items %in% not_chosen_target_locate, "target_items"] = 1
  # items_select_both[[i]][!items_select_both[[i]]$items %in% not_chosen_recovered_locate, "recovered_items" ] = 1
  # items_select_both[[i]]$distance = items_select_both[[i]]$target_items == items_select_both[[i]]$recovered_items
  sensitivity_algorithm_select[[i]] = sensitivity(distance_select(both[[i]], from_both_select[[i]]))
  sensitivity_systematic_select[[i]] = sensitivity(distance(both[[i]]))
  summary_sensitivity_select_both[i, "accuracy_algo"] = sensitivity_algorithm_select[[i]]$accuracy 
  summary_sensitivity_select_both[i, "spec_algo"] = sensitivity_algorithm_select[[i]]$specificity
  summary_sensitivity_select_both[i, "sens_algo"] = sensitivity_algorithm_select[[i]]$sensitivity
  summary_sensitivity_select_both[i, "accuracy_syst"] = sensitivity_systematic_select[[i]]$accuracy 
  summary_sensitivity_select_both[i, "spec_syst"] = sensitivity_systematic_select[[i]]$specificity
  summary_sensitivity_select_both[i, "sens_syst"] = sensitivity_systematic_select[[i]]$sensitivity
  
  
  
  # check coherence con Ilocate 
  # items_locate_both[[i]][!items_locate_both[[i]]$items %in% not_chosen_target_locate, "target_items"] = 1
  # items_locate_both[[i]][!items_locate_both[[i]]$items %in% not_chosen_recovered_locate, "recovered_items" ] = 1
  # items_locate_both[[i]]$distance = items_locate_both[[i]]$target_items == items_locate_both[[i]]$recovered_items
  sensitivity_algorithm_locate[[i]] = sensitivity(distance_select(both[[i]], from_both_locate[[i]]))
  sensitivity_systematic_locate[[i]] = sensitivity(distance(both[[i]]))
  summary_sensitivity_locate_both[i, "accuracy_algo"] = sensitivity_algorithm_locate[[i]]$accuracy 
  summary_sensitivity_locate_both[i, "spec_algo"] = sensitivity_algorithm_locate[[i]]$specificity
  summary_sensitivity_locate_both[i, "sens_algo"] = sensitivity_algorithm_locate[[i]]$sensitivity
  summary_sensitivity_locate_both[i, "accuracy_syst"] = sensitivity_systematic_locate[[i]]$accuracy 
  summary_sensitivity_locate_both[i, "spec_syst"] = sensitivity_systematic_locate[[i]]$specificity
  summary_sensitivity_locate_both[i, "sens_syst"] = sensitivity_systematic_locate[[i]]$sensitivity
  
  my_distance_both = both[[i]]$distance
  my_distance_both$type = "systematic"
  temp_locate_both = data.frame(item = paste(rownames(from_both_locate[[i]]$end), 
                                             collapse = " "), 
                                av_diff_mtif = mean(from_both_locate[[i]]$select_attempts[[length(from_both_locate[[i]]$select_attempts)]]$distance_tif), 
                                sd_diff_mtif = sd(from_both_locate[[i]]$select_attempts[[length(from_both_locate[[i]]$select_attempts)]]$distance_tif), 
                                cat = nrow(from_both_locate[[i]]$end), 
                                type = "SELECTED")
  temp_select_both = data.frame(item = paste(rownames(from_both_select[[i]]$end), 
                                             collapse = " "), 
                                av_diff_mtif = mean(from_both_select[[i]]$select_attempts[[length(from_both_select[[i]]$select_attempts)]]$distance_tif), 
                                sd_diff_mtif = sd(from_both_select[[i]]$select_attempts[[length(from_both_select[[i]]$select_attempts)]]$distance_tif), 
                                cat = nrow(from_both_select[[i]]$end), 
                                type = "SELECTED")
  my_locate_both[[i]] = rbind(my_distance_both, temp_locate_both)
  my_select_both[[i]] = rbind(my_distance_both, temp_select_both)
}

# controlla che la forma breve selezionata dall'algoritmo abbia la stessa lunghezza della best possible 
# selezionata dalla forza bruta
check_coherence_locate_both = data.frame(same_length = logical(iterations), 
                                         common_itemsBest = character(iterations))
check_coherence_select_both = data.frame(same_length = logical(iterations), 
                                         common_itemsBest = character(iterations))
for (i in 1:length(both)) {
  if (both[[i]]$best_selections_summary[1, "cat"] == nrow(from_both_select[[i]]$end)) {
    check_coherence_select_both[i, "same_length"] = TRUE
  } else {
    check_coherence_select_both[i, "same_length"] = FALSE
  }
  check_coherence_select_both[i, "common_itemsBest"] = paste(intersect(strsplit(as.character(both[[i]]$best_selections_summary[1, "item"]), 
                                                                                split = " ")[[1]], 
                                                                       rownames(from_both_select[[i]]$end)), collapse = " ")
  check_coherence_select_both[i, "item_1best"] = as.character(both[[i]]$best_selections_summary[1, "item"])
  check_coherence_select_both[i, "item_algorithm"] =  paste(rownames(from_both_select[[i]]$end), collapse = " ")
  
  
  if (both[[i]]$best_selections_summary[1, "cat"] == nrow(from_both_locate[[i]]$end)) {
    check_coherence_locate_both[i, "same_length"] = TRUE
  } else {
    check_coherence_locate_both[i, "same_length"] = FALSE
  }
  check_coherence_locate_both[i, "common_itemsBest"] = paste(intersect(strsplit(as.character(both[[i]]$best_selections_summary[1, "item"]), 
                                                                                split = " ")[[1]], 
                                                                       rownames(from_both_locate[[i]]$end)), collapse = " ")
  check_coherence_locate_both[i, "item_1best"] = as.character(both[[i]]$best_selections_summary[1, "item"])
  check_coherence_locate_both[i, "item_algorithm"] =  paste(rownames(from_both_locate[[i]]$end), collapse = " ")
}
table(check_coherence_locate_both$same_length)
# FALSE  TRUE 
# 43    57
table(check_coherence_select_both$same_length)
# FALSE  TRUE 
# 40    60 
# un'altra cosa che si potrebbe controllare é: 
# se Ilocate e Iselect risultano nella stessa lugnhezza 
# se selezionano gli stessi item e quante volte selezionano gli stessi item 

# questo controlla se ho effettivamente selezionato gli stessi item della forma target 
same_length_select = check_coherence_select_both[check_coherence_select_both$same_length %in% TRUE, ]
for (i in 1:nrow(same_length_select)) {
  if (length(strsplit(same_length_select[i, "common_itemsBest"], split = " ")[[1]]) == length(strsplit(same_length_select[i, "item_1best"], split = " ")[[1]])) {
    same_length_select[i, "same_selection"] = TRUE
  } else {
    same_length_select[i, "same_selection"] = FALSE
  }
}
nrow(same_length_select)
table(same_length_select$same_selection)
# FALSE  TRUE 
# 28    32 
# questo invece mi dice se ha preso meno item, più item o stesso numero 
#compare_item_select = check_coherence_select_both[check_coherence_select_both$same_length %in% FALSE, c("common_itemsBest", "item_1best", "item_algorithm")]
compare_item_select = check_coherence_select_both[, c("common_itemsBest", "item_1best", "item_algorithm")]
compare_item_select$length_best = numeric(nrow(compare_item_select))
for (i in 1:nrow(compare_item_select)) {
  compare_item_select[i, "length_best"] = length(strsplit(compare_item_select[i, "item_1best"], split = " ")[[1]])
  if (length(strsplit(compare_item_select[i, "item_algorithm"], split = " ")[[1]]) > compare_item_select[i, "length_best"]) {
    compare_item_select[i, "comparison"] = "piu-item"
  } else if (length(strsplit(compare_item_select[i, "item_algorithm"], split = " ")[[1]]) < compare_item_select[i, "length_best"]) {
    compare_item_select[i, "comparison"] = "meno-item"
  } else {
    compare_item_select[i, "comparison"] = "item"
  }
  compare_item_select[i, "length_algo"] = length(strsplit(compare_item_select[i, "item_algorithm"], split = " ")[[1]])
}

compare_item_select$differenza = compare_item_select$length_best - compare_item_select$length_algo
nrow(compare_item_select)
table(compare_item_select$comparison)
# item meno-item  piu-item 
# 60        27        13 
compare_item_select %>%  
  group_by(comparison) %>%  
  summarise(mean = mean(abs(differenza)), min = min(min = abs(differenza)), max = max(abs(differenza)), 
            n = n())

mean(abs(compare_item_select$differenza)) # media pesata

same_length_locate = check_coherence_locate_both[check_coherence_locate_both$same_length %in% TRUE, ]
for (i in 1:nrow(same_length_locate)) {
  if (length(strsplit(same_length_locate[i, "common_itemsBest"], split = " ")[[1]]) == length(strsplit(same_length_locate[i, "item_1best"], split = " ")[[1]])) {
    same_length_locate[i, "same_selection"] = TRUE
  } else {
    same_length_locate[i, "same_selection"] = FALSE
  }
}
nrow(same_length_locate)
table(same_length_locate$same_selection)
# FALSE  TRUE 
# 16    41 
# compare_item_locate = check_coherence_locate_both[check_coherence_locate_both$same_length %in% FALSE, c("common_itemsBest", "item_1best", "item_algorithm")]
compare_item_locate = check_coherence_locate_both[, c("common_itemsBest", "item_1best", "item_algorithm")]
compare_item_locate$length_best = numeric(nrow(compare_item_locate))
for (i in 1:nrow(compare_item_locate)) {
  compare_item_locate[i, "length_best"] = length(strsplit(compare_item_locate[i, "item_1best"], split = " ")[[1]])
  if (length(strsplit(compare_item_locate[i, "item_algorithm"], split = " ")[[1]]) > compare_item_locate[i, "length_best"]) {
    compare_item_locate[i, "comparison"] = "piu-item"
  } else if (length(strsplit(compare_item_locate[i, "item_algorithm"], split = " ")[[1]]) < compare_item_locate[i, "length_best"]) {
    compare_item_locate[i, "comparison"] = "meno-item"
  } else {
    compare_item_locate[i, "comparison"] = "item"
  }
  compare_item_locate[i, "length_algo"] = length(strsplit(compare_item_locate[i, "item_algorithm"], split = " ")[[1]])
}
nrow(compare_item_locate)
table(compare_item_locate$comparison)
compare_item_locate$differenza = compare_item_locate$length_best - compare_item_locate$length_algo
# item meno-item  piu-item 
# 57        34         9 
compare_item_locate %>%  
  group_by(comparison) %>%  
  summarise(mean = mean(abs(differenza)), min = min(min = abs(differenza)), max = max(abs(differenza)), 
            n = n())


temp = compare_item_locate[!compare_item_locate$differenza %in% 0, ]
ggplot(temp, 
       aes(x = reorder(rownames(temp), differenza), 
           y = differenza)) + 
  geom_point(size=5, color="black", 
             fill="gray78", alpha=0.7, shape=21, stroke=2) + 
  geom_hline(yintercept = 0) + ylab(expression(paste("||", 
                                                     Q[BFP], "||"
                                                     -"||", Q[ILA], "||" )
                                               ) )+
  xlab("Data frame") +
  geom_segment(aes(x=rownames(temp), 
                   xend=rownames(temp), 
                   y=0, yend=differenza)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title = element_text(size = 26), 
        axis.text = element_text(size = 24)) + ylim(-4, 4)
ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/bfp-ila.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")

# summary accuracy 
colMeans(summary_sensitivity_select_both)
apply(summary_sensitivity_select_both, 2, min)
apply(summary_sensitivity_select_both, 2, max)
table(summary_sensitivity_select_both$spec_algo)
table(summary_sensitivity_select_both$accuracy_algo)


colMeans(summary_sensitivity_locate_both)
apply(summary_sensitivity_locate_both, 2, min)
apply(summary_sensitivity_locate_both, 2, max)

ohmy_select_both = NULL
ohmy_locate_both = NULL
mysummary_select = data.frame(iteration = numeric(length(my_select_both)), 
                              
                              rank_selected = numeric(length(my_select_both)), 
                              equal_previous = numeric(length(my_select_both)), 
                              equal_following = numeric(length(my_select_both)))
mysummary_locate = data.frame(iteration = numeric(length(my_select_both)), 
                              
                              rank_selected = numeric(length(my_select_both)), 
                              equal_previous = numeric(length(my_select_both)), 
                              equal_following = numeric(length(my_select_both)))

for (i in 1:length(my_select_both)) {
  # ohmy_select_both = my_select_both[[i]][order(my_select_both[[i]]$av_diff_mtif), ]
  # temp_item_select = as.character(ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "item"])
  # temp_item_select = strsplit(temp_item_select, split = " ")[[1]]
  # temp_item_select = temp_item_select[order(temp_item_select)]
  # temp_item_select = paste(temp_item_select, collapse = " ")
  # temp_select = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), ]
  # ohmy_select_both = ohmy_select_both[!ohmy_select_both$type %in% "SELECTED", ]
  # ohmy_select_both = ohmy_select_both[!ohmy_select_both$item %in% temp_item_select, ]
  # ohmy_select_both = rbind(ohmy_select_both, temp_select)
  # ohmy_select_both = ohmy_select_both[order(ohmy_select_both$av_diff_mtif), ]
  # ohmy_select_both$rank = rank(ohmy_select_both$av_diff_mtif, ties = "average")
  # ohmy_select_both$rank_first = rank(ohmy_select_both$av_diff_mtif, ties = "min")
  # ohmy_select_both$z = scale(ohmy_select_both$av_diff_mtif)
  
  # stessa cosa ma locate 
  ohmy_locate_both = my_locate_both[[i]][order(my_locate_both[[i]]$av_diff_mtif), ]
  temp_item_locate = as.character(ohmy_locate_both[which(ohmy_locate_both$type == "SELECTED"), "item"])
  temp_item_locate = strsplit(temp_item_locate, split = " ")[[1]]
  temp_item_locate = temp_item_locate[order(temp_item_locate)]
  temp_item_locate = paste(temp_item_locate, collapse = " ")
  temp_locate = ohmy_locate_both[which(ohmy_locate_both$type == "SELECTED"), ]
  ohmy_locate_both = ohmy_locate_both[!ohmy_locate_both$type %in% "SELECTED", ]
  ohmy_locate_both = ohmy_locate_both[!ohmy_locate_both$item %in% temp_item_locate, ]
  ohmy_locate_both = rbind(ohmy_locate_both, temp_locate)
  ohmy_locate_both = ohmy_locate_both[order(ohmy_locate_both$av_diff_mtif), ]
  ohmy_locate_both$rank = rank(ohmy_locate_both$av_diff_mtif, ties = "average")
  ohmy_locate_both$rank_first = rank(ohmy_locate_both$av_diff_mtif, ties = "min")
  ohmy_locate_both$z = scale(ohmy_locate_both$av_diff_mtif)
  # mysummary_select[i, "iteration"] = i
  # mysummary_select[i, "rank_selected"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "rank"]
  # mysummary_select[i, "rank_selected_first"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "rank_first"]
  if( nrow(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), ]) == 0)   {
    mysummary_locate[i, "equal_previous"] = 0
  } else {
    mysummary_locate[i, "equal_previous"] = round(ohmy_locate_both[grep("SELECTED", 
                                                                        ohmy_locate_both$type), "av_diff_mtif"], 10 ) == 
      round(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), "av_diff_mtif"], 10)
  }                                                                                                      
  
  # mysummary_select[i, "equal_following"] = round(ohmy_select_both[grep("SELECTED", 
  #                                                                      ohmy_select_both$type), 
  #                                                                 "av_diff_mtif"], 10 )== round(ohmy_select_both[(grep("SELECTED", ohmy_select_both$type) + 1), "av_diff_mtif"], 
  #                                                                                               10 )
  # mysummary_select[i, "rp_selected"] = (mysummary_select[i, "rank_selected"]*100)/(nrow(ohmy_select_both) + 1)
  # mysummary_select[i, "rp_selected_first"] = (mysummary_select[i, "rank_selected_first"]*100)/(nrow(ohmy_select_both) + 1)
  # mysummary_select[i, "z"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "z"]
  
  mysummary_locate[i, "iteration"] = i
  mysummary_locate[i, "rank_selected"] = ohmy_locate_both[which(ohmy_locate_both$type == "SELECTED"), "rank"]
  mysummary_locate[i, "rank_selected_first"] = ohmy_locate_both[which(ohmy_locate_both$type == "SELECTED"), "rank_first"]
  if( nrow(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), ]) == 0)   {
    mysummary_locate[i, "equal_previous"] = 0
  } else {
    mysummary_locate[i, "equal_previous"] = round(ohmy_locate_both[grep("SELECTED", 
                                                                        ohmy_locate_both$type), "av_diff_mtif"], 10 ) == 
      round(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), "av_diff_mtif"], 10)
  }                                                                                                      
  
  mysummary_locate[i, "equal_following"] = round(ohmy_locate_both[grep("SELECTED", 
                                                                       ohmy_locate_both$type), "av_diff_mtif"], 10 )== round(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) + 1), "av_diff_mtif"], 10)
  mysummary_locate[i, "rp_selected"] = (mysummary_locate[i, "rank_selected"]*100)/(nrow(ohmy_locate_both) + 1)
  mysummary_locate[i, "rp_selected_first"] = (mysummary_locate[i, "rank_selected_first"]*100)/(nrow(ohmy_locate_both) + 1)
  mysummary_locate[i, "z"] = ohmy_locate_both[which(ohmy_locate_both$type == "SELECTED"), "z"]
  
}



mysummary_select$filter = ifelse(mysummary_select$rank_selected_first >= 10, 
                                 "look", "ok")
mysummary_locate$filter = ifelse(mysummary_locate$rank_selected_first >= 10, 
                                 "look", "ok")

table(mysummary_select$rp_selected_first < 10)
median(mysummary_select$rp_selected_first)
mean(mysummary_select$z)
quantile(mysummary_select$z)
mean(mysummary_select$equal_previous)

table(mysummary_locate$rp_selected_first < 10)
median(mysummary_locate$rp_selected_first)
mean(mysummary_locate$z)
compare_item_locate$iteration = 1:100
small_compare = compare_item_locate[, c("iteration", "comparison")]
mysummary_locate = merge(mysummary_locate, small_compare, by = "iteration")
# mysummary_locate$comparison = gsub("piu-item", expression(paste("||", 
#                                                                 Q[BFP], "||"
#                                                                 -"||", Q[ILA], "||" )),
#                                    mysummary_locate$comparison)
# mysummary_locate$comparison = gsub("meno-item", expression(BFP - ILA > 0), 
#                                    mysummary_locate$comparison)
# mysummary_locate$comparison = gsub("item", "BFP - ILA = 0", 
#                                    mysummary_locate$comparison)

# pallino stesso numero 
# triangolo meno item 
# quadrato più item
ggplot(mysummary_locate, 
       aes(x = reorder(iteration, rp_selected_first), 
           y = rp_selected_first, 
           shape = comparison, 
           color = comparison)) + 
  geom_point(size = 3, alpha = .70) + xlab("Data frame") + ylab("Percentile Rank") + 
  ylim(0, 100) + theme_light() + 
  theme(legend.position = c(.15,.75), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26), 
        axis.text.x = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(size = 24)) +
  scale_color_manual(values = c("black", "grey50", "#9B0014"), 
                     labels = c(expression(paste("||", 
                                               Q[BFP], "||"
                                               -"||", Q[ILA], "|| = 0 " )), 
                                expression(paste("||", 
                                                 Q[BFP], "||"
                                                 -"||", Q[ILA], "|| > 0 ")), 
                                expression(paste("||", 
                                                 Q[BFP], "||"
                                                 -"||", Q[ILA], "|| < 0 " )))) + 
  scale_shape_manual(values = c(15,17,19), labels = c(expression(paste("||", 
                                                 Q[BFP], "||"
                                                 -"||", Q[ILA], "|| = 0 " )), 
                                expression(paste("||", 
                                                 Q[BFP], "||"
                                                 -"||", Q[ILA], "|| > 0 ")), 
                                expression(paste("||", 
                                                 Q[BFP], "||"
                                                 -"||", Q[ILA], "|| < 0 " ))))+
    guides(color = guide_legend(override.aes = list(size = 8)))

ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/rp.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")

# alcune tif di esempio 
# iterazione 21
# BFP 4 item 
# ILA 2 item
mysummary_locate[21, ]
# rp 4.76
lessClose = both[[21]]$all_info
lessClose = rbind(lessClose[lessClose$new_item %in% "stf-target", ], 
                  lessClose[lessClose$item %in% both[[21]]$best_selections_summary$item[1], ])
lessClose = lessClose[, c("theta", "mean_tif", "new_item")]
ggplot(lessClose, 
       aes(x = theta, y = mean_tif, 
           color = new_item)) + geom_line()
prova = both[[21]]
head(prova$info_target)
tempAttempt = from_both_locate[[21]]$select_attempts[[length(from_both_locate[[21]]$select_attempts)]]

prova = tempAttempt[, c("theta",  "mean_tif.x", "type")]
colnames(prova) = c("theta", "mean_tif",  "new_item")

total = rbind(lessClose, prova)
total$new_item = factor(total$new_item, 
                        levels = c("stf-target", "combo_4", "temp_2"))
ggplot(total, 
       aes(x = theta, y = mean_tif, 
           color = new_item, 
           linetype = new_item)) + geom_line(linewidth=1.8) + 
  ylab("Mean TIF") + xlab(expression(theta)) + theme_bw() + 
  theme(legend.position = c(.85, .7), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26),
        legend.text  = element_text(size = 28),
        
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(2.5, "cm"))  + 
  scale_linetype_manual(values = c(1,3,5), 
                        labels = c("TIF-target", 
                                   "BFP", "ILA")) + 
  scale_color_manual(values = c("black", "gray45", "gray60"), 
                     labels = c("TIF-target", 
                                "BFP", "ILA"))
ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/lessClose.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")


# iterazione 76, meno item, lontana
# rp 12.70
lessFar = both[[76]]$all_info
lessFar = rbind(lessFar[lessFar$new_item %in% "stf-target", ], 
                lessFar[lessFar$item %in% both[[76]]$best_selections_summary$item[1], ])
lessFar = lessFar[, c("theta", "mean_tif", "new_item")]
ggplot(lessFar, 
       aes(x = theta, y = mean_tif, 
           color = new_item)) + geom_line()
prova = both[[76]]
head(prova$info_target)

tempAttempt = from_both_locate[[76]]$select_attempts[[length(from_both_locate[[76]]$select_attempts)]]

prova = tempAttempt[, c("theta",  "mean_tif.x", "type")]
colnames(prova) = c("theta", "mean_tif",  "new_item")

total = rbind(lessFar, prova)
table(total$new_item)
total$new_item = factor(total$new_item, 
                        levels = c("stf-target", "combo_2", "temp_1"))
ggplot(total, 
       aes(x = theta, y = mean_tif, 
           color = new_item, 
           linetype = new_item)) + geom_line(linewidth=1.8) + 
  ylab("Mean TIF") + xlab(expression(theta)) + theme_light() + 
  theme(legend.position = c(.25, .7), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26),
        legend.text  = element_text(size = 28),
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(2.5, "cm"))  + 
  scale_linetype_manual(values = c(1,3,5), 
                        labels = c("TIF-target", 
                                   "BFP", "ILA")) + 
  scale_color_manual(values = c("black", "gray45", "gray60"), 
                     labels = c("TIF-target", 
                                "BFP", "ILA"))
ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/lessFar.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")
# iterazione 75
mysummary_locate[75, ]
# rp 3.17
moreClose = both[[75]]$all_info
moreClose = rbind(moreClose[moreClose$new_item %in% "stf-target", ], 
                  moreClose[moreClose$item %in% both[[75]]$best_selections_summary$item[1], ])
moreClose = moreClose[, c("theta", "mean_tif", "new_item")]
ggplot(moreClose, 
       aes(x = theta, y = mean_tif, 
           color = new_item)) + geom_line()
prova = both[[75]]
head(prova$info_target)

tempAttempt = from_both_locate[[75]]$select_attempts[[length(from_both_locate[[75]]$select_attempts)]]

prova = tempAttempt[, c("theta",  "mean_tif.x", "type")]
colnames(prova) = c("theta", "mean_tif",  "new_item")

total = rbind(moreClose, prova)
table(total$new_item)
total$new_item = factor(total$new_item, 
                        levels = c("stf-target", "combo_4", "temp_5"))
ggplot(total, 
       aes(x = theta, y = mean_tif, 
           color = new_item, 
           linetype = new_item)) + geom_line(linewidth=1.8) + 
  ylab("Mean TIF") + xlab(expression(theta)) + theme_bw() + 
  theme(legend.position = c(.85, .7), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26),
        legend.text  = element_text(size = 28),
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(2.5, "cm")) + 
  scale_linetype_manual(values = c(1,3,5), 
                        labels = c("TIF-target", 
                                   "BFP", "ILA")) + 
  scale_color_manual(values = c("black", "gray45", "gray60"), 
                     labels = c("TIF-target", 
                                "BFP", "ILA"))
ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/moreClose.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")


# iterazione 4, stesso numero, selezione diversa, vicino
mysummary_locate[4, ]
# rp3.17
equalClose = both[[4]]$all_info
equalClose = rbind(equalClose[equalClose$new_item %in% "stf-target", ], 
                   equalClose[equalClose$item %in% both[[4]]$best_selections_summary$item[1], ])
equalClose = equalClose[, c("theta", "mean_tif", "new_item")]
ggplot(equalClose, 
       aes(x = theta, y = mean_tif, 
           color = new_item)) + geom_line()
prova = both[[4]]
head(prova$info_target)

tempAttempt = from_both_locate[[4]]$select_attempts[[length(from_both_locate[[4]]$select_attempts)]]

prova = tempAttempt[, c("theta",  "mean_tif.x", "type")]
colnames(prova) = c("theta", "mean_tif",  "new_item")

total = rbind(equalClose, prova)
table(total$new_item)
total$new_item = factor(total$new_item, 
                        levels = c("stf-target", "combo_5", "temp_5"))
ggplot(total, 
       aes(x = theta, y = mean_tif, 
           color = new_item, 
           linetype = new_item)) + geom_line(linewidth=1.8) + 
  ylab("Mean TIF") + xlab(expression(theta)) + theme_bw() + 
  theme(legend.position = c(.85, .7), 
        legend.title = element_blank(), 
        axis.text = element_text(size = 24), 
        axis.title = element_text(size = 26),
        legend.text  = element_text(size = 28),
        
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(2.5, "cm")) + 
  scale_linetype_manual(values = c(1,3,5), 
                        labels = c("TIF-target", 
                                   "BFP", "ILA")) + 
  scale_color_manual(values = c("black", "gray45", "gray60"), 
                     labels = c("TIF-target", 
                                "BFP", "ILA"))
ggsave("C:/Users/ottae/Documents/ASA2024/ASA2024/epifania/presentazione/img/equalClose.pdf",
       dpi = 1200, device = cairo_pdf,
       width = 14.00, height = 8.50, units = "in")

# provo a mettere tutto insieme ------

total_temp = NULL
total = NULL
for (i in 1:length(from_both_locate)) {
  equalClose = both[[i]]$all_info
  equalClose = rbind(equalClose[equalClose$new_item %in% "stf-target", ], 
                     equalClose[equalClose$item %in% both[[i]]$best_selections_summary$item[1], ])
  equalClose = equalClose[, c("theta", "mean_tif", "new_item")]
  equalClose$iteration = i
  prova = both[[i]]
  tempAttempt = from_both_locate[[i]]$select_attempts[[length(from_both_locate[[i]]$select_attempts)]]
  prova = tempAttempt[, c("theta",  "mean_tif.x", "type")]
  colnames(prova) = c("theta", "mean_tif",  "new_item")
  prova$iteration = i
  
  total_temp = rbind(equalClose, prova)
  total = rbind(total_temp, total)
}




temp_select_both = mysummary_select[mysummary_select$filter %in% "look", ]
temp_locate_both = mysummary_locate[mysummary_locate$filter %in% "look", ]
for (i in temp_select_both$iteration) {
  print(ggplot(my_select_both[[i]], 
               aes(x = reorder(item, av_diff_mtif, decreasing = T), 
                   y = av_diff_mtif, 
                   color = as.factor(type), 
                   shape = factor(cat))) + geom_point() + 
          geom_hline(yintercept = quantile(my_select_both[[i]]$av_diff_mtif)[2]) + 
          ggtitle(paste("iteration", i, "rank =", temp_select_both[temp_select_both$iteration %in% i, "rank"])))
}
parameters_both[[3]]
print(my_select_both[[3]][order(my_select_both[[3]]$av_diff_mtif),], n = 100)
temp3 = my_select_both[[3]][order(my_select_both[[3]]$av_diff_mtif),]
temp3$rank = 1:nrow(temp3)

my1 = data.frame(table(temp3$item))
my1 = my1[order(my1$Freq, decreasing = T), ]
my1$freCum = cumsum(my1$Freq)
library(DescTools)

Gini(temp3$av_diff_mtif)
# oltrre al rango, una msiura stadnardizzata
temp3$z = scale(temp3$av_diff_mtif)[,1]


for (i in temp_select_both$iteration) {
  print(ggplot(my_select_both[[i]], 
               aes(x = reorder(item, av_diff_mtif, decreasing = T), 
                   y = av_diff_mtif, 
                   color = as.factor(type), 
                   shape = factor(cat))) + geom_point() + 
          geom_hline(yintercept = quantile(my_select_both[[i]]$av_diff_mtif)[2]) + 
          ggtitle(paste("iteration", i, "rank =", temp_select_both[temp_select_both$iteration %in% i, "rank"])))
}



my_select_both[[5]][order(my_select_both[[5]]$av_diff_mtif), ]

mean(from_both_locate[[i]]$select_attempts[length(from_both_locate[[i]]$select_attempts)]$distance_tif)

mean(from_both_locate[[i]]$select_attempts[[length(from_both_locate[[i]]$select_attempts)]]$distance_tif)

my = both[[1]]$distance
my$type = "systematic"
my1 = from_both_locate[[1]]
temp = data.frame(item = paste(rownames(my1$end), collapse = " "), 
                  av_diff_mtif = mean(my1$select_attempts$temp_2$distance_tif), 
                  sd_diff_mtif = sd(my1$select_attempts$temp_2$distance_tif), 
                  cat = nrow(my1$end), 
                  type = "SELECTED")
my = rbind(my, temp)
my[order(my$av_diff_mtif), ]
quantile(my$av_diff_mtif)
mean(my1$select_attempts$temp_2$distance_tif)
ggplot(my, 
       aes(x = reorder(item, av_diff_mtif, decreasing = T), 
           y = av_diff_mtif, 
           color = as.factor(type), 
           shape = factor(cat))) + geom_point() + 
  geom_hline(yintercept = quantile(my$av_diff_mtif)[2])


small_locate = check_coherence_locate_both[check_coherence_locate_both$same_length == TRUE, ]
small_locate$same_selection = small_locate$common_items == small_locate$item_2best 
sum(small_locate$same_selection)/nrow(small_locate)

small_select = check_coherence_select_both[check_coherence_select_both$same_length == TRUE, ]
small_select$same_selection = small_select$common_items == small_select$item_2best 
sum(small_select$same_selection)/nrow(small_select)


tif = both[[2]]$info_target[, c("theta_both", "tif", "mean_tif")]

b = Iselect(tif,
            parameters_both, print = FALSE)
the_graphs(b)

bloc = Ilocate(tif,
               parameters_both, print = FALSE)
the_graphs(bloc) + 
  transition_reveal(new_type)

bloc$end
b$end

both[[4]]$best_selections_summary

both[[2]]$best_selections_summary
from_both[[2]]$end
from_both[[2]]$original


both[[3]]$best_selections_summary
from_both[[3]]$end
from_both[[3]]$original
# se avessi usato la locatio, avrebbe selezionato l'item 1 





# questo dataframe contiene gli item che contengono la forma breve 
# target 



# metto tutti insieme e calcolo la media ----- 

tempBruto = NULL
allBrutos = matrix(nrow = 1000)
tempIla = NULL
allIlas = matrix(nrow = 1000)
for (i in 1:length(both)) {
  tempBruto = both[[i]]$the_info
  tempBruto = tempBruto[tempBruto$item %in% both[[i]]$best_selections_summary$item[1], 
                        "mean_tif"]
  #tempBruto = data.frame(diff_target_bruto = abs(target$tif_target - tempBruto))
  colnames(tempBruto) = paste(colnames(tempBruto), i, sep = "_")
  allBrutos = cbind(allBrutos, tempBruto)
  
  tempIla = data.frame(diff_target_ila = from_both_locate[[i]]$select_attempts[[length(from_both_locate[[i]]$select_attempts)]]$distance_tif)
  colnames(tempIla) = paste(colnames(tempIla), i, sep = "_")
  allIlas = cbind(allIlas, tempIla)
}
allBrutos = allBrutos[,-1]
allFranks = allFranks[,-1]
allIlas = allIlas[,-1]

mean_distances = data.frame(bruto = rowMeans(allBrutos), 
                            frank = rowMeans(allFranks), 
                            ila = rowMeans(allIlas))
mean_distances = cbind(target$theta, mean_distances)
colnames(mean_distances)[1] = "theta"

mean_distances = pivot_longer(mean_distances, cols = !theta, 
                              names_to = "algorithm")


ggplot(mean_distances, 
       aes( x= theta, y = value, color = algorithm)) + geom_line()




item_target = strplit(unique(both$info_target$item))
strsplit(as.character(both$info_target$item), 
         split = " ")[[1]]
data.frame(item_target = )