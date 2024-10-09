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

# calcola la differenza simmetrica 
# ma la calcola dalla target 
# devo fare una funzione simile dove quelli che qui sono i recovered items diventano 
# gli item della forma target migliore e io poi mi calcolo con lo stesso codice 
# la distanza della forma selected. 
# io da qualche parte ho un oggetto dove ho sia quelle fatte dalla froza bruta sia quelle fatte dall'alkgortimo 
# quella che si chiama selected. devo vedere l'accuratezza rispetto a questa.
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

# initialize variables ----
iterations = 100
both = list()
items_select_both = list()
items_locate_both = list()
from_both_select = list()

items_locate_both = list()
from_both_locate = list()

temp_locate_both = NULL
temp_select_both = NULL
my_locate_both = list()
my_select_both = list()
my_distance_both = NULL
parameters_both = list()
theta_both = list()
nitem = 6
# modification indexes 
mod_b = data.frame(matrix(nrow = nitem, ncol = iterations))
mod_a = data.frame(matrix(nrow = nitem, ncol = iterations))

set.seed(999)
try = select_items(parameters_both[[1]],
                   theta = seq(-3,3, length.out=5000),
                   add_difficulty = mod_b[,1],
                   add_discriminativity = mod_a[,1])
rownames(parameters_both[[i]]) = paste("item", 1:nrow(parameters_both[[i]]), sep ="_")
try
# just hope for the best 
start = Sys.time()
for (i in 1:iterations) {
  set.seed(i)
  # Simulate person and item parameters_both
  b <- runif(nitem, -3, 3)
  a <- runif(nitem, 0.9, 2)
  parameters_both[[i]] <- data.frame(b, a)
  rownames(parameters_both[[i]]) = paste("item", 1:nrow(parameters_both[[i]]), sep ="_")
  starting_items=rownames(parameters_both[[i]])
  mod_b[,i] = runif(length(b), -0.20, 0.20)
  mod_a[, i] = runif(length(a), -.20, 0.20)
  both[[i]] = select_items(parameters_both[[i]], 
                           theta = seq(-3,3, length.out=5000), #theta_both[[i]], 
                           add_difficulty = mod_b[,i], 
                           add_discriminativity = mod_a[,i], 
                           seed = i)
  # selecy with I select
  from_both_select[[i]] = Iselect(both[[i]]$info_target[, c("theta", "tif", "mean_tif")],
                                  parameters_both[[i]], print = FALSE)
  # select with i locate 
  from_both_locate[[i]] = Ilocate(both[[i]]$info_target[, c("theta", "tif", "mean_tif")],
                                  parameters_both[[i]], print = FALSE)

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
end = Sys.time()
elapsed = end-start
elapsed
save.image("die-algorithm-10-items-mod-index.RData")


## Descrittive paper ASA ----- 

le_target = matrix(nrow = 100, ncol = 1)
for (i in 1:length(both)) {
  le_target[i,] = length(strsplit(unique(both[[i]]$info_target$item), " ")[[1]])
}
strsplit(unique(both[[4]]$info_target$item), " ")[[1]]
head(both[[1]]$info_target)
mean(le_target)
sd(le_target)

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
# 42    58
table(check_coherence_select_both$same_length)
# FALSE  TRUE 
# 55    45 
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
# 37    31
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
# 58        28        14
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
# 18    40
#compare_item_locate = check_coherence_locate_both[check_coherence_locate_both$same_length %in% FALSE, c("common_itemsBest", "item_1best", "item_algorithm")]
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
# meno-item  piu-item 
# 35        7 
compare_item_locate %>%  
  group_by(comparison) %>%  
  summarise(mean = mean(abs(differenza)), min = min(min = abs(differenza)), max = max(abs(differenza)), 
            n = n())

mean(abs(compare_item_locate$differenza)) # media pesata

# grafico ASA differenza numero di item -----
temp = compare_item_locate[!compare_item_locate$differenza %in% 0, ]
ggplot(temp, 
       aes(x = reorder(rownames(temp), differenza), 
           y = differenza)) + 
  geom_point(size=5, colob="black", 
             fill="gray78", alpha=0.7, shape=21, stroke=2) + 
  geom_hline(yintercept = 0) + ylab(expression(BFP-ILA) )+
  xlab("Iteration") +
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
  ohmy_select_both = my_select_both[[i]][order(my_select_both[[i]]$av_diff_mtif), ]
  temp_item_select = as.character(ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "item"])
  temp_item_select = strsplit(temp_item_select, split = " ")[[1]]
  temp_item_select = temp_item_select[order(temp_item_select)]
  temp_item_select = paste(temp_item_select, collapse = " ")
  temp_select = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), ]
  ohmy_select_both = ohmy_select_both[!ohmy_select_both$type %in% "SELECTED", ]
  ohmy_select_both = ohmy_select_both[!ohmy_select_both$item %in% temp_item_select, ]
  ohmy_select_both = rbind(ohmy_select_both, temp_select)
  ohmy_select_both = ohmy_select_both[order(ohmy_select_both$av_diff_mtif), ]
  ohmy_select_both$rank = rank(ohmy_select_both$av_diff_mtif, ties = "average")
  ohmy_select_both$rank_first = rank(ohmy_select_both$av_diff_mtif, ties = "min")
  ohmy_select_both$z = scale(ohmy_select_both$av_diff_mtif)
  
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
  mysummary_select[i, "iteration"] = i
  mysummary_select[i, "rank_selected"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "rank"]
  mysummary_select[i, "rank_selected_first"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "rank_first"]
  if( nrow(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), ]) == 0)   {
    mysummary_locate[i, "equal_previous"] = 0
  } else {
    mysummary_locate[i, "equal_previous"] = round(ohmy_locate_both[grep("SELECTED", 
                                                                        ohmy_locate_both$type), "av_diff_mtif"], 10 ) == 
      round(ohmy_locate_both[(grep("SELECTED", ohmy_locate_both$type) - 1), "av_diff_mtif"], 10)
  }                                                                                                      
  
  mysummary_select[i, "equal_following"] = round(ohmy_select_both[grep("SELECTED", 
                                                                       ohmy_select_both$type), 
                                                                  "av_diff_mtif"], 10 )== round(ohmy_select_both[(grep("SELECTED", ohmy_select_both$type) + 1), "av_diff_mtif"], 
                                                                                                10 )
  mysummary_select[i, "rp_selected"] = (mysummary_select[i, "rank_selected"]*100)/(nrow(ohmy_select_both) + 1)
  mysummary_select[i, "rp_selected_first"] = (mysummary_select[i, "rank_selected_first"]*100)/(nrow(ohmy_select_both) + 1)
  mysummary_select[i, "z"] = ohmy_select_both[which(ohmy_select_both$type == "SELECTED"), "z"]
  
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

# grafico ASA percentile di distanza ---- 

ggplot(mysummary_locate, 
       aes(x = reorder(iteration, rp_selected_first), 
           y= rp_selected_first)) + geom_point() + 
  theme_bw() + ylim(0, 100)


