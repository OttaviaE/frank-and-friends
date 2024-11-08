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
  browser()
    while (token == TRUE) {
        for (i in 0:nrow(parameters)) {
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
      if (i == (nrow(parameters))) {
        token = FALSE
        warning("I ran out of items wwithout finding a STF")
        warn_too_many = TRUE
      } else if (distances_pif[i+1] >= distances_tif[i+1]) {
        token = FALSE
        warn_too_many = FALSE
      } else {
        token = TRUE
      }
    } 
      
    sel_item = sel_item
      all_items = sel_item
      if (warn_too_many == TRUE & distances_pif[i+1] >= distances_tif[i+1]) {
        sel_item = sel_item
      } else {
        all_items = sel_item
        sel_item = all_items[-length(all_items)] 
      }
  }
  if (length(sel_item) == 0) {
    warning("Apparently there were no items able to minimize the distance")
    warn_not_found = TRUE
  } else {
    temp_item = sel_item
    temp_item = as.numeric(gsub("item_", "", temp_item))
    temp_item = temp_item[order(temp_item)]
    sel_item = paste("item", temp_item, sep = "_")
    # sel_item = paste(sel_item, collapse = " ")
    warn_not_found = FALSE
  }
  my_target$n_stf = length(sel_item)
  my_target$item_stf = paste(sel_item, collapse = " ")
  results = list(q_ila = paste(sel_item, collapse = " "), 
                 all_items = all_items,
                 stf = my_target, 
                 distances_tif = distances_tif, 
                 distances_pif = distances_pif, 
                 all_stfs = all_stfs, 
                 warning = c(item_not_found = warn_not_found, stf_not_found = warn_too_many))
  return(results)
}
