# carico bruto 
load("bruto-10item.RData")
source("functions-new.R")
library(tidyverse)
parameters = list()
replications = 100
resFrank = list()
frank_elapsed = NULL
theta = seq(-4,4, length.out = 1500)
resIla = list()
ila_elapsed = NULL
resIsa = list()
isa_elapsed = NULL
for (i in 1:replications) {
  set.seed(i)
  parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                               a = runif(10, 0.9, 2))
  rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
  target = tif_target(parameters[[i]], theta, 
                      add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                      add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                      seed = i)
  # Frank
  frank_start = Sys.time()
  resFrank[[i]] = frank(parameters[[i]], target)
  frank_end = Sys.time()
  temp_elapsed = frank_end - frank_start
  frank_elapsed = c(frank_elapsed, temp_elapsed)
  # ILA 
  ila_start = Sys.time()
  resIla[[i]] = ila(parameters[[i]], target)
  ila_end = Sys.time()
  temp_elapsed = ila_end - ila_start
  ila_elapsed = c(ila_elapsed, temp_elapsed)
  # ISA 
  isa_start = Sys.time()
  resIsa[[i]] = isa(parameters[[i]], target)
  isa_end = Sys.time()
  temp_elapsed = isa_end - isa_start
  isa_elapsed = c(isa_elapsed, temp_elapsed)
}


item_stf = data.frame(item_ila = character(replications), 
                      n_ila = numeric(replications), 
                      item_isa = character(replications), 
                      n_isa = numeric(replications), 
                      item_frank = character(replications), 
                      n_frank = numeric(replications))
for (i in 1:replications) {
  item_stf[i, "item_ila"] = resIla[[i]]$q_ila
  item_stf[i, "n_ila"] = unique(resIla[[i]]$stf$n_stf)
  item_stf[i, "item_isa"] = resIsa[[i]]$q_isa
  item_stf[i, "n_isa"] = unique(resIsa[[i]]$stf$n_stf)
  item_stf[i, "item_frank"] = resFrank[[i]]$q_frank
  item_stf[i, "n_frank"] = ncol(resFrank[[i]]$iif_stf)
  item_stf[i, "replication"]= i
}
# add rank to bruto: it means the distance of teh STF developed with Bruto from the target
# io sarei dell'idea di arrontarder a 4 decimali
for (i in 1:length(resBruto)) {
  resBruto[[i]]$differences$rank = rank(resBruto[[i]]$differences$mean_distance, ties = "min")
}

rank_ila = NULL
rank_isa = NULL
rank_frank = NULL

for (i in 1:replications) {
  tempBruto = resBruto[[i]]$differences
  tempFrank = tempBruto[tempBruto$item %in% item_stf[i, "item_frank"], ] 
  tempFrank$rp = (tempFrank$rank*100)/nrow(tempBruto)
  tempFrank$alorigthm = "frank"
  tempFrank$iter = i
  tempIla = tempBruto[tempBruto$item %in% item_stf[i, "item_ila"], ] 
  tempIla$rp = (tempIla$rank*100)/nrow(tempBruto)
  tempIla$alorigthm = "ila"
  tempIla$iter = i
  tempIsa = tempBruto[tempBruto$item %in% item_stf[i, "item_isa"], ] 
  tempIsa$rp = (tempIsa$rank*100)/nrow(tempBruto)
  tempIsa$alorigthm = "isa"
  tempIsa$iter = i
  rank_ila = rbind(rank_ila, tempIla)
  rank_isa = rbind(rank_isa, tempIsa)
  rank_frank = rbind(rank_frank, tempFrank)
}
head(rank_ila)
ggplot(rank_ila, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point()
ggplot(rank_isa, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point()
ggplot(rank_frank, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point()
all_ranks = rbind(rank_ila, rank_isa, rank_frank)
myrank = aggregate(rp ~  n_item, all_ranks, mean)
colnames(myrank)[2] = "mean_rp"
all_ranks = merge(all_ranks, myrank) 
ggplot(all_ranks, 
       aes(x = reorder(iter, rp), 
           y = rp, shape = alorigthm, 
           color = alorigthm)) + geom_point() 

aggregate(rp ~ alorigthm, all_ranks, quantile)
# decisamente frank è il migliore 
# calcolo quanti sono il 50% nei tre casi
all_ranks$sup_50 = ifelse(all_ranks$rp >= 50, 
                         "sup", "inf")

all_ranks %>%  
  group_by(alorigthm, sup_50) %>%  
  summarise(n = n())

# ora provo a fare la distanza simmetrica 

all_q_bruto = NULL
temp = NULL
for (i in 1:length(resBruto)) {
  temp = resBruto[[i]]$q_bruto
  temp$replication = i
  temp$n_item_target = unique(resBruto[[i]]$all_infos$length_target)
  temp$item_target = unique(resBruto[[i]]$all_infos$item_target)
  all_q_bruto = rbind(all_q_bruto, temp)
}
# qui c'è la distanza di ogni bruto dalla target
mydBruto = list()
temp= NULL
distance_bruto=NULL
for (i in 1:nrow(all_q_bruto)) {
  mydBruto[[i]] = delta(all_q_bruto, nitems = 10, replica = i)
  temp = 10- sum(mydBruto[[i]]$distance)
  distance_bruto = c(distance_bruto, temp)
}
# voglio vedere la distanza degli item delle forme brevi ottenute con i vari algoritmi algoritmi 
# dalla selezione di item di bruto 
all_stf = merge(all_q_bruto, item_stf, by = "replication")
all_stf[order(all_stf$replication, decreasing = F), ]
d_ila  = list()
d_isa  = list()
d_frank = list()
the_distances = data.frame(bruto_d = distance_bruto, 
                           ila_d = numeric(length(distance_bruto)), 
                           isa_d = numeric(length(distance_bruto)), 
                           frank_d = numeric(length(distance_bruto)))
for (i in 1:nrow(the_distances)) {
  d_ila[[i]] = delta(all_stf, nitems = 10, replica = i, 
                     target = "item", comparison = "item_ila")
  d_isa[[i]] = delta(all_stf, nitems = 10, replica = i, 
                     target = "item", comparison = "item_isa")
  d_frank[[i]] = delta(all_stf, nitems = 10, replica = i, 
                     target = "item", comparison = "item_frank")
  the_distances[i, "ila_d"] = 10- sum(d_ila[[i]]$distance)
  the_distances[i, "isa_d"] = 10- sum(d_isa[[i]]$distance)
  the_distances[i, "frank_d"] = 10- sum(d_frank[[i]]$distance)
}

colMeans(the_distances)

# ila_acc = data.frame(accuracy = numeric(100), 
#                      sens00 = numeric(100), 
#                      spec00 = numeric(100), 
#                      algo = "ila")
# isa_acc = data.frame(accuracy = numeric(100), 
#                      sens00 = numeric(100), 
#                      spec00 = numeric(100), 
#                      algo = "isa")
# frank_acc = data.frame(accuracy = numeric(100), 
#                      sens00 = numeric(100), 
#                      spec00 = numeric(100), 
#                      algo = "frank")
# 
# bruto_acc = data.frame(accuracy = numeric(100), 
#                        sens00 = numeric(100), 
#                        spec00 = numeric(100), 
#                        algo = "bruto")
ila_acc = NULL
isa_acc = NULL
frank_acc = NULL
bruto_acc = NULL
for (i in 1:replications) {
  ila_acc = rbind(ila_acc, performance(d_ila[[i]])$performance)
  isa_acc = rbind(isa_acc, performance(d_isa[[i]])$performance)
  frank_acc = rbind(frank_acc, performance(d_frank[[i]])$performance)
  bruto_acc = rbind(bruto_acc, performance(mydBruto[[i]])$performance)
}
ila_acc = data.frame(ila_acc)
isa_acc = data.frame(isa_acc)
frank_acc = data.frame(frank_acc)
bruto_acc = data.frame(bruto_acc)

all_acc = rbind(ila_acc, isa_acc, frank_acc, bruto_acc)
all_acc$algo = rep(c("ila", "isa", "frank", "bruto"), each = 100)
all_acc$replication = 1:100

ggplot(all_acc, 
       aes(x = reorder(replication, accuracy), y = accuracy, 
           shape = algo, 
           color = algo)) + geom_point()
ggplot(all_acc, 
       aes(x = reorder(replication, specificity11), y = specificity11, 
           shape = algo, 
           color = algo)) + geom_point()
ggplot(all_acc, 
       aes(x = reorder(replication, sensitivity00), y = sensitivity00, 
           shape = algo, 
           color = algo)) + geom_point()
