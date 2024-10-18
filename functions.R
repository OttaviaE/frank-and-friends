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
  target_info = data.frame(cbind(theta, do.call("cbind", iifs_target)))
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

bruto = function(parameters, tif_target){
  
}

