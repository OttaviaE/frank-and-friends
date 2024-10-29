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
  Ii = a^2*IRT(theta, b = b, a = a )*(1- IRT(theta, b = b, a = a ))
  return(Ii)
}
# calcola l'IIF di tutti gli item e restituisce in una lista di lunghezza ugaule a tutti 
# gli item per cui si è calcolata l'IIF
item_info <- function(ipar, theta = seq(-5,5,length.out=1000)){
  item <- NULL
  for(i in 1:nrow(ipar)){
    item[[i]] <- i_info(ipar[i, "b"],ipar[i, "a"], theta = theta)
  }
  item = data.frame(do.call("cbind", item))
  colnames(item) = rownames(ipar)
  return(item)
}

tif_target = function(parameters, 
                      theta = seq(-3,3, legnth.out = 1000), 
                      length_target = NULL,
                      add_difficulty = NULL, 
                      add_discriminativity = NULL, 
                      seed = 999) {
  set.seed(seed)
  if (is.null(parameters)) {
    stop("I need the item paramters, you fool!")
  }
  if (is.null(length_target)) {
    length_target = sample(1:(nrow(parameters)-1), 1)
  } else {
    length_target = length_target
  }
  # tutte le possibili combinazioni di lunghezza N 
  possible_comb = sample(nrow(parameters), length_target)
  possible_comb = possible_comb[order(possible_comb)] 
  # seleziona gli item della forma breve target
  sel_items = parameters[possible_comb, ]
  if (is.null(add_difficulty)) {
    warning("Something will be randomly added to the difficulties of the items")
    add_difficulty = sample(runif(1000, min = -0.2, 0.2), nrow(sel_items))
  } else if (length(add_difficulty) > 1) {
    add_difficulty = sample(add_difficulty, nrow(sel_items))
  } 
  if (is.null(add_discriminativity)) {
    warning("Something will be randomly added to the discriminativities of the items")
    add_discriminativity = sample(runif(1000, min =  -0.1, 0.1), nrow(sel_items))
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
  target_info = data.frame(cbind(theta, iifs_target))
  colnames(target_info)[-1] = rownames(sel_items)
  if (sum(grepl("item", colnames(target_info))) == 1) {
    target_info$mean_tif = target_info[,-1]
  } else {
    target_info$mean_tif = rowMeans(target_info[,-1]) 
  }
  # creo un dataframe ad hoc per poterlo unire a tutti gli altri 
  target_info$length_target = length_target
  target_info$item_target = paste(colnames(target_info)[grep("item", colnames(target_info))], collapse = " ")
  target_info = target_info[, c("theta", "mean_tif",  "length_target", "item_target")]
  return(target_info)
}
best_combos = function(vector, element_to_find) {
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
# volendo si potrebbe anche dare a bruto solo la matrice di IIFs
#' bruto
#'
#' @param parameters data.frame that contains the item parameters with two columns denoted as b (difficulty) and a (discrimination parameters)
#' @param target data.frame with theta (col name theta) and tif target (mean_tif)
#'
#' @return A list with four 4 objects: 
#'          - q_bruto: best item selection
#'          - differences: dataframe with all teh item combinations and their distance from the tif target
#'          - all_infos: all the information functions for each length 
#'          - best dataframe with the best item combinations of different length in terms of distance fro the tif target
#' @export
#'
#' @examples
bruto = function(parameters, target = NULL) {
  if (is.null(target)) {
    stop("I can't work without a target :)")
  }
  theta = target$theta
  # generate all the possible item combinations of differet lengths
  # from the item bank 
  caligraphic_q<-expand.grid(rep(list(0:1),nrow(parameters)))
  caligraphic_q = caligraphic_q[-c(1,nrow(caligraphic_q)), ]
  # non c'è bisogno di metterle in ordine se non per una perversione mia
  caligraphic_q$sum = rowSums(caligraphic_q)
  caligraphic_q = caligraphic_q[order(caligraphic_q$sum), ]
  caligraphic_q$sum = NULL
  caligraphic_q[caligraphic_q == 0] <- NA 
  # computes the IIFs of all the items 
  all_iifs = item_info(parameters, theta = theta)
  
  # this summarizes the results of all the STFs
  differences = data.frame(n_item = numeric(nrow(caligraphic_q)), 
                           item = numeric(nrow(caligraphic_q)), 
                           mean_distance = numeric(nrow(caligraphic_q)))
  my_target = NULL
  all_targets = NULL
  # now bruto starts 
  for (i in 1:nrow(caligraphic_q)) {
    i_index = caligraphic_q[i, ]
    i_index = do.call("rbind", replicate(length(theta), 
                                         i_index, simplify = FALSE))
    temp_iifs = i_index * all_iifs
    colnames(temp_iifs) = colnames(all_iifs)
    if (length(which(!is.na(temp_iifs[1, ]))) == 1) {
      my_target = cbind(target, 
                        temp_iifs[,which(!is.na(temp_iifs[1, ]))])
      
    } else {
      my_target = cbind(target, 
                        rowSums(temp_iifs[,which(!is.na(temp_iifs[1, ]))])/length(which(!is.na(temp_iifs[1, ]))))
    }
    colnames(my_target)[ncol(my_target)] = "mean_tif_stf"
    my_target$n_item = length(which(!is.na(temp_iifs[1, ])))
    my_target$item = paste(colnames(temp_iifs)[which(!is.na(temp_iifs[1, ]))], 
                           collapse = " ")
    my_target$distance = abs(my_target$mean_tif - my_target$mean_tif_stf)
    
    differences[i, "n_item"] = unique(my_target$n_item) 
    differences[i, "item"]  =  unique(my_target$item) 
    differences[i, "mean_distance"] = mean(my_target$distance)
    all_targets = rbind(all_targets, my_target) 
  }
  differences = differences[order(differences$mean_distance, 
                                  decreasing = FALSE), ]
  best = differences[best_combos(differences$n_item, 
                          1:(nrow(parameters)-1)), ]
  best = best[order(best$mean_distance), ]
  q_bruto = differences[1, ]
  results = list(q_bruto = q_bruto, 
                 differences = differences, 
                 all_infos = all_targets, 
                 best = best 
  )
  return(results)
}
# ILA 
ila = function(parameters, target) {
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
  all_stfs = data.frame(matrix(nrow = 1, ncol = 3))
  colnames(all_stfs) = c("nitems", "items", "pif")
  for (i in 0:nrow(parameters)) {
    if (token == TRUE) {
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
        selected_iifs = cbind(selected_iifs, 
                              all_iifs[, index_item])
        colnames(selected_iifs)[length(colnames(selected_iifs))] = rownames(parameters)[index_item]
        pif = rowMeans(selected_iifs)
        temp_pif = data.frame(nitems = ncol(selected_iifs), 
                              items = paste(colnames(selected_iifs), collapse = " "), 
                              pif = pif)
        all_stfs = rbind(all_stfs, mypif)
      }
      parameters[index_item, ] = NA
      # contiene le distanze tra la target e la tif stf (quelle che vanno confrontate con la pif)
      distances_tif =c(distances_tif, mean(abs(my_target$mean_tif - my_target$tif_stf))) 
      names(distances_tif)[length(distances_tif)] = paste("temp", i, sep = "_")
      distances_pif =c(distances_pif, mean(abs(my_target$mean_tif - pif))) 
      names(distances_pif)[length(distances_pif)] = paste("temp", i, sep = "_")
      # qua testa il criterio di uscita
   #   browser()
      if (mean(abs(my_target$mean_tif - pif)) < mean(abs(my_target$mean_tif - my_target$tif_stf))) {
        token = TRUE
      } else {
        token = FALSE
      }
    }else {
      sel_item = sel_item
    } 
  }
  all_stfs = all_stfs[-1, ]
  all_items = sel_item
  if (length(sel_item) == 1) {
    sel_item = sel_item
    warning("Apparetly there were no items able to minimize the distance")
  } else {
    sel_item = all_items[-length(all_items)] 
  }
  temp_item = sel_item
  temp_item = as.numeric(gsub("item_", "", temp_item))
  temp_item = temp_item[order(temp_item)]
  sel_item = paste("item", temp_item, sep = "_")
  my_target$n_stf = length(sel_item)
  my_target$item_stf = paste(sel_item, collapse = " ")
  results = list(q_ila = paste(sel_item, collapse = " "), 
                 all_items = all_items,
                 stf = my_target, 
                 distances_tif = distances_tif, 
                 distances_pif = distances_pif, 
                 all_stfs = all_stfs)
  return(results)
}
# isa 
isa = function(parameters, target) {
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
 # browser()
  all_stfs = data.frame(matrix(nrow = 1, ncol = 3))
  colnames(all_stfs) = c("nitems", "items", "pif")
  for (i in 0:nrow(parameters)) {
    if (token == TRUE) {
      if (i == 0) {
        my_target$tif_stf = 0
      } else {
        my_target$tif_stf = pif
      }
      my_target$distance = abs(my_target$mean_tif - my_target$tif_stf) 
      
      parameters$temp_theta = my_target[which(my_target$distance == max(my_target$distance)), "theta"]
      parameters$temp_iif = parameters$a^2 *(IRT(parameters$temp_theta,
                                                 parameters$a, parameters$b) * (1-IRT(parameters$temp_theta, parameters$a, parameters$b)))
      # attenzione eprché facendo così rischio di prendere di nuovo lo stesso item
      # potrei sostituire gli item con na
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
        all_stfs = rbind(all_stfs, mypif)
      }
      parameters[index_item, ] = NA
      # contiene le distanze tra la target e la tif stf (quelle che vanno confrontate con la pif)
      distances_tif =c(distances_tif, mean(abs(my_target$mean_tif - my_target$tif_stf))) 
      names(distances_tif)[length(distances_tif)] = paste("temp", i, sep = "_")
      distances_pif =c(distances_pif, mean(abs(my_target$mean_tif - pif))) 
      names(distances_pif)[length(distances_pif)] = paste("temp", i, sep = "_")
      # qua testa il criterio di uscita
      if (i == (nrow(parameters) -1)) {
        token = FALSE
        warning("I ran out of items wwithout finding a STF")
      } else if (mean(abs(my_target$mean_tif - pif)) < mean(abs(my_target$mean_tif - my_target$tif_stf))) {
        token = TRUE
      } else {
        token = FALSE
      }
    }else {
      sel_item = sel_item
    } 
  }
  all_stfs = all_stfs[-1, ]
  all_items = sel_item
  if (length(sel_item) == 1) {
    sel_item = sel_item
    warning("Apparetly there were no items able to minimize the distance")
  } else {
    sel_item = all_items[-length(all_items)] 
  }
  temp_item = sel_item
  temp_item = as.numeric(gsub("item_", "", temp_item))
  temp_item = temp_item[order(temp_item)]
  sel_item = paste("item", temp_item, sep = "_")
  my_target$n_stf = length(sel_item)
  my_target$item_stf = paste(sel_item, collapse = " ")
  results = list(q_isa = paste(sel_item, collapse = " "), 
                 all_items = all_items,
                 stf = my_target, 
                 distances_tif = distances_tif, 
                 distances_pif = distances_pif, 
                 all_stfs = all_stfs)
  return(results)
}
# Frank -------
frank = function(parameters, target) {
  all_iifs = item_info(parameters, theta)
  token = TRUE
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
        pif = data.frame(cbind(iif_stf, all_iifs[,i]))
        pif = data.frame(rowMeans(pif))
      }
      difference[i] = mean(abs(target$mean_tif - pif)[,1])  
      # qui scelgo un item temporaneo che è quello con la distanza minima
    }
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
    if (difference[d_index] < distance_target_tif) {
      token = TRUE
      parameters[d_index, ] = NA
    } else {
      token = FALSE
      # sel_items = colnames(iif_stf)[-ncol(iif_stf)]
      # sel_items = sel_items[order(sel_items)]
      # sel_items = paste(sel_items, collapse = " ")
      temp_item = colnames(iif_stf)[-ncol(iif_stf)]
      temp_item = as.numeric(gsub("item_", "", temp_item))
      temp_item = temp_item[order(temp_item)]
      sel_items = paste("item", temp_item, sep = "_")
      sel_items = paste(sel_items, collapse = " ")
    }
  } 
  iif_stf = data.frame(iif_stf[,-ncol(iif_stf)])
  results = list(q_frank = sel_items, 
                 iif_stf = iif_stf)
  
  return(results)
}
# questa va fatta meglio a partire dai risultati degli algoritmi, per ora va bene così 
delta = function(all_q, nitems = NULL, replica = 1, 
                 target = "item_target", 
                 comparison = "item") {
  if (is.null(nitems)) {
    stop("Please tell how many items you have :)")
  } else {
    distance = data.frame(starting_items = paste("item", 1:nitems, sep ="_"), 
                          target_items = numeric(nitems), 
                          stf_items = numeric(nitems))
    target_items = strsplit(all_q[replica, target], split = " ")[[1]]
    recovered_items = strsplit(all_q[replica, comparison], split = " ")[[1]]
    not_chosen_target = distance$starting_items[!distance$starting_items %in% target_items]
    not_chosen_recovered = distance$starting_items[!distance$starting_items %in% recovered_items]
    
    distance[!distance$starting_items %in% not_chosen_target, "target_items"] = 1
    distance[!distance$starting_items %in% not_chosen_recovered, "stf_items" ] = 1
    distance$distance = distance$target_items == distance$stf_items
  }
return(distance)
}
performance = function(distance_data, target_items = "target_items", stf_items = "stf_items") {
  myT = table(distance_data[, target_items], distance_data[,stf_items])
  accuracy = (myT[1,1] + myT[2,2])/sum(myT)
  sens = myT[1,1]/sum(myT[1,])
  spec = myT[2,2]/sum(myT[2,])
  performance = c(sensitivity00 = sens, specificity11 = spec, accuracy = accuracy)
  results = list(table = myT, 
                 performance = performance)
  return(results)
}

