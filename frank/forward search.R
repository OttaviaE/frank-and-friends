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
#' Franco
#'
#' @param target dataframe con i valori theta (theta) e la tif target (target)
#' @param parameters dataframe con as many rowas as number of items e due colonne, difficoltà (b) e discriminatività (a)
#'
#' @return 
#' @export
#'
#' @examples
franco = function(target, parameters) {
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
  while (token == TRUE) {
    # questo è franco 
    for (i in 1:ncol(iif_start)){
      if (token == TRUE) {
        if (i == 1) {
          for (j in 1:length(distances)) {
            distances[j] = mean(abs(target$target - 
                                      iif_start[, grep("item", colnames(iif_start))[j]]))
          }
         # browser()
          sel_temp = names(which(distances == min(distances, na.rm = TRUE)))
          # deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
          index_item = c(index_item, sel_temp)
          # attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
          temp_item = all_iifs[,colnames(all_iifs) %in% index_item]
          temp_sel = cbind(target, temp_item)
          colnames(temp_sel)[3:ncol(temp_sel)] = index_item
          tif_k$temp =  all_iifs[,colnames(all_iifs) %in% index_item]
          colnames(tif_k)[ncol(tif_k)] = paste0("temp_tif_", i)
          temp_dist = mean(abs(tif_k$target - tif_k[, ncol(tif_k)])) 
          dist_k = c(dist_k, temp_dist)
        } else {
          # calcolo al TIF media consdierando solo gli item che non sono ancora stati messi 
          # nella selezione
          for (j in as.numeric(gsub("\\D", "", setdiff(colnames(iif_start), index_item)))) {
            temp_sel$temp_item = iif_start[,j]
            temp_sel$temp_tif = rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
            distances[j] = mean(abs(temp_sel$target - temp_sel$temp_tif))
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
          temp_dist = mean(abs(tif_k$target - tif_k[, ncol(tif_k)])) 
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
  }
  
  results = list(tif = tif_k, 
                 distances = dist_k, 
                 chosen_items = index_item)
  return(results)
}
#' Grafici delle TIF di Franco
#'
#' @param franco_results La lista di robe che risulta da Franco
#' @param selection se vuoi vedere delle speicifiche tif temporanee altrimenti te le butta tutte
#'
#' @return
#' @export
#'
#' @examples
franco_graph = function(franco_results, selection = NULL) {
  
  tif_k_long = pivot_longer(franco_results$tif, 
                            cols = !theta, 
                            names_to = "tif")
  
  if (is.null(selection)) {
    temp_franco = tif_k_long
  } else {
    temp_franco = tif_k_long[tif_k_long$tif %in% selection, ]
  }
  
  g = ggplot(temp_franco, 
             aes(x = theta, y = value, color = tif, linetype = tif)) + geom_line()
  return(g)
}
theta = seq(-5, 5, length.out = 5000)
iterations = 100
target_items = data.frame(matrix(nrow = iterations, ncol= 2))
colnames(target_items)  = c("nitems", "items")
myfranco = list()
set.seed(1212)
for (i in 1:iterations) {
  # Simulate person and item parameters_both
  b <- runif(20, -3, 3)
  a <- runif(20, 0.9, 2)
  parameters <- data.frame(b, a)
  rownames(parameters) = paste("item", 1:nrow(parameters), sep ="_")
  
  #  genera la tif target ----
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
  iifs <- item_info(sel_items,
                    theta = theta)
  target_info = do.call("cbind", iifs)
  target_info = cbind(theta, target_info)
  target_info = data.frame(target_info)
  colnames(target_info)[-1] = rownames(sel_items)
  target_info$tif = rowSums(target_info[, grep("item", colnames(target_info))])
  target_info$mean_tif = target_info$tif/sum(grepl("item", colnames(target_info)))
  
  myfranco[[i]] = franco(target, parameters = parameters)
}


item_comparison = data.frame(target = target_items$nitems)

for (i in 1:length(myfranco)) {
  item_comparison[i, "nfranco"] = length(myfranco[[i]]$chosen_items)
}
colMeans(item_comparison)
apply(item_comparison, 2, min)
apply(item_comparison, 2, max)
item_comparison$difference = with(item_comparison, target - nfranco)

equal = as.integer(rownames(item_comparison[item_comparison$difference %in% 0, ]))

tif_k_long = pivot_longer(franco_results$tif, 
                          cols = !theta, 
                          names_to = "tif")
temp = NULL
temp_long = NULL
comparison_equal = NULL
for (i in equal) {
  temp = myfranco[[i]]
  temp_long = pivot_longer(temp$tif, 
                           cols = !theta, 
                           names_to = "tif")
  temp_long$iteration = i
 temp_long = temp_long[temp_long$tif %in% c("target", unique(temp_long$tif)[c(length(unique(temp_long$tif)), length(unique(temp_long$tif))-1)]), ] 
  comparison_equal = rbind(comparison_equal, temp_long)
  
}


ggplot(comparison_equal, 
       aes(x = theta, y = value, color = tif, linetype = tif)) + geom_line() + 
  facet_wrap(~iteration)



franco_graph(myfranco[[58]], selection = c("target",  "temp_tif_4"))


ggplot(target_info,  
       aes(x = theta, y = mean_tif)) + geom_line()


all_iifs = item_info(parameters, theta = theta)
all_iifs = data.frame(do.call("cbind", all_iifs))
colnames(all_iifs) = rownames(parameters)
all_iifs = cbind(theta, all_iifs)
target = data.frame(theta = target_info$theta, target = target_info$mean_tif)



my = franco(target, parameters = parameters)
tif_k_long = pivot_longer(my$tif, 
                          cols = !theta, 
                          names_to = "tif")
ggplot(tif_k_long, 
       aes(x = theta, y = value, color = tif, linetype = tif)) + geom_line()



# adesso devo creare un dataset temporaneo dove metto le iif degli item 
# le sottraggo alla tif target media 

# metto gli item in un daframe temporaneo, li aggiungo uno alla volta, calcolo la tif con l'ultimo 
# aggiunto e lo confronto 
iif_start = data.frame(all_iifs[, grep("item", colnames(all_iifs))])
# potrebbe partire da qui
index_item = NULL
temp_sel = NULL
sel_temp = NULL
dist_k = NULL
temp_dist = NULL
tif_k = target

distances = numeric(ncol(iif_start))
names(distances) = colnames(iif_start)
token = TRUE
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
      temp_dist = mean(abs(tif_k$target - tif_k[, ncol(tif_k)])) 
      dist_k = c(dist_k, temp_dist)
    } else {
      # calcolo al TIF media consdierando solo gli item che non sono ancora stati messi 
      # nella selezione
      for (j in as.numeric(gsub("\\D", "", setdiff(colnames(iif_start), index_item)))) {
        temp_sel$temp_item = iif_start[,j]
        temp_sel$temp_tif = rowMeans(temp_sel[, grep("item", colnames(temp_sel))])
        distances[j] = mean(abs(temp_sel$target - temp_sel$temp_tif))
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
      temp_dist = mean(abs(tif_k$target - tif_k[, ncol(tif_k)])) 
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


tif_k_long = pivot_longer(tif_k, 
                          cols = !theta, 
                          names_to = "tif")


ggplot(tif_k_long[tif_k_long$tif %in% c("target", "temp_tif_4", "temp_tif_5"), ], 
       aes(x = theta, y = value, color = tif, linetype = tif)) + geom_line()


sel_temp = names(which(distances == min(distances, na.rm = TRUE)))
index_item = NULL
# deve essere aggiornato a ogni iterazione con gli indici degli item che vengono selezionati via via
index_item = c(index_item, sel_temp)
# attenzione perché questo temp item in realtà è quello che si prende gli item selezionati
temp_item = all_iifs[,colnames(all_iifs) %in% index_item]
temp_sel = cbind(target, temp_item)
colnames(temp_sel)[3:ncol(temp_sel)] = index_item
# TEORICAMENTE QUI FINISCE UN'ITERAZIONE 

# IO HO IL VETTORE DI TUTTI GLI ITEM 
colnames(iif_start)
# il vettore degli item  selzionati 
index_item
which(!grepl(index_item, colnames(iif_start)) == TRUE)

distances = numeric(ncol(iif_start))
names(distances) = colnames(iif_start)
 



# aggiungo un item alla volta, calcolo la TIF media, sottraggo alla target e salvo 
# la media distanza assoluta in un vettore 
# distances va creato ad ogni iterazione
distances = numeric(ncol(iif_start))
names(distances) = colnames(iif_start)


# ora questo va aggiunto a index_item 


# qui metto gli NA 
distances[, colnames(distances) %in% index_item]

iif_start[, colnames(iif_start) %in% index_item] = NA

# if (length(grep("item", colnames(temp_sel))) == 1) {
#   temp_sel$temp_tif = temp_sel[, grep("item", colnames(temp_sel))]
# } else {
#   temp_sel$temp_tif = colMeans(temp_sel[, grep("item", colnames(temp_sel))])
# }
# devo aggiungere all'item selezionato le iif di tuti gli altri item e sottrarre 
# dalla tif target media 

# potenzialmente è un'altra iterazione (?)
# ora devo calcolare la distanza tra la tif target e una tif temporanea (media) 
# ottenuta sommando a temp item le iif degli altri item tranne la sua 

# myitemtemp = start_item[, !colnames(start_item) %in% index_item]
myitemtemp = start_item
temp_tif = NULL 

for (i in 1:ncol(myitemtemp)) {
  temp_tif = cbind(temp_item, myitemtemp[,i])
}

