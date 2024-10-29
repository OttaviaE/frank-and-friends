library(tidyverse)
source("functions-new.R")
# nuova simulazione bruto 
parameters = list()
replications = 100
resBruto = list()
bruto_elapsed = NULL
theta = seq(-4,4, length.out = 1500)
for (i in 1:replications) {
  set.seed(i)
  parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                               a = runif(10, 0.9, 2))
  rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
  target = tif_target(parameters[[i]], theta, 
                      add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                      add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                      seed = i)
  bruto_start = Sys.time()
  resBruto[[i]] = bruto(parameters[[i]], target)
  bruto_end = Sys.time()
  temp_elapsed = bruto_end - bruto_start
  bruto_elapsed = c(bruto_elapsed, temp_elapsed)
}
# ho fatto casino e non ho i tempi per bruto se non dell'utlima replicazione
bruto_end - bruto_start
all_q_bruto = NULL
temp = NULL
for (i in 1:length(resBruto)) {
  temp = resBruto[[i]]$q_bruto
  temp$replication = i
  temp$n_item_target = unique(resBruto[[i]]$all_infos$length_target)
  temp$item_target = unique(resBruto[[i]]$all_infos$item_target)
  all_q_bruto = rbind(all_q_bruto, temp)
}

sum(all_q_bruto$n_item == all_q_bruto$n_item_target)
sum(all_q_bruto$item == all_q_bruto$item_target)

mydBruto = list()
temp= NULL
distance_bruto=NULL
for (i in 1:nrow(all_q_bruto)) {
  mydBruto[[i]] = delta(all_q_bruto, nitems = 10, replica = i)
  temp = 10- sum(mydBruto[[i]]$distance)
  distance_bruto = c(distance_bruto, temp)
}
myT = table(mydBruto[[1]]$target_items, mydBruto[[1]]$stf_items)
sum(table(mydBruto[[1]]$target_items, mydBruto[[1]]$stf_items)[1,])
table(mydBruto[[1]]$target_items, mydBruto[[1]]$stf_items)[1,1]
table(mydBruto[[1]]$target_items, mydBruto[[1]]$stf_items)[2,2]
sum(table(mydBruto[[1]]$target_items, mydBruto[[1]]$stf_items)[2,])
distance_bruto
table(distance_bruto)

