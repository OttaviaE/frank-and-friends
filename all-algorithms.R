rm(list = ls())
# carico bruto 
load("bruto-sim11item.RData")
source("functions-new.R")
library(tidyverse)
# parameters = list()
replications = 100
resFrank = list()
frank_elapsed = NULL
# theta = seq(-4,4, length.out = 1000)
resIla = list()
ila_elapsed = NULL
resIsa = list()
isa_elapsed = NULL
# target = list()
for (i in 1:replications) {
  set.seed(i)
  # parameters[[i]] = data.frame(b = runif(10, -3, 3), 
  #                              a = runif(10, 0.9, 2))
  # rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
  # target[[i]] = tif_target(parameters[[i]], theta, 
  #                     add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
  #                     add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
  #                     seed = i)
  # Frank
  frank_start = Sys.time()
  resFrank[[i]] = frank(parameters[[i]], target[[i]])
  frank_end = Sys.time()
  temp_elapsed = frank_end - frank_start
  frank_elapsed = c(frank_elapsed, temp_elapsed)
  # ILA 
  ila_start = Sys.time()
  resIla[[i]] = ila(parameters[[i]], target[[i]])
  ila_end = Sys.time()
  temp_elapsed = ila_end - ila_start
  ila_elapsed = c(ila_elapsed, temp_elapsed)
  # ISA 
  isa_start = Sys.time()
  resIsa[[i]] = isa(parameters[[i]], target[[i]])
  isa_end = Sys.time()
  temp_elapsed = isa_end - isa_start
  isa_elapsed = c(isa_elapsed, temp_elapsed)
}

mean(frank_elapsed)
mean(ila_elapsed)
mean(isa_elapsed)

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
  item_stf[i, "item_not_found_ila"] = resIla[[i]]$warning[1]
  item_stf[i, "stf_not_found_ila"] = resIla[[i]]$warning[2]
  item_stf[i, "item_not_found_isa"] = resIsa[[i]]$warning[1]
  item_stf[i, "stf_not_found_isa"] = resIsa[[i]]$warning[2]
  item_stf[i, "replication"]= i
}
# 7/100 ila finisce gli item
table(item_stf$stf_not_found_ila)
#18/100 non isa finisce gli item
table(item_stf$stf_not_found_isa)
# quelle di ila sono incluse in quelle di ila 
item_stf[item_stf$stf_not_found_ila == TRUE, ]
# le due non trovate da ila sono include in quelle non prese 
# da isa 
item_stf[item_stf$item_not_found_ila %in% TRUE,]
# 33, 47, 73, 98
item_stf[item_stf$n_isa == 10,]
item_stf[item_stf$n_ila == 10,]

all_item_stf = item_stf
# add rank to bruto: it means the distance of teh STF developed with Bruto from the target
# io sarei dell'idea di arrontarder a 4 decimali
for (i in 1:length(resBruto)) {
  resBruto[[i]]$differences$rank = rank(resBruto[[i]]$differences$mean_distance, 
                                        ties = "min")
}

# faccio una sottoselezione solo di quelle forme brevi che sono effettivamnente riuscite 
item_stf = all_item_stf[all_item_stf$stf_not_found_isa == F,]
apply(item_stf[, c("n_frank", "n_ila", "n_isa")], 2, max)
apply(item_stf[, c("n_frank", "n_ila", "n_isa")], 2, min)
apply(item_stf[, c("n_frank", "n_ila", "n_isa")], 2, mean)

rank_ila = NULL
rank_isa = NULL
rank_frank = NULL

for (i in item_stf$replication) {
  tempBruto = resBruto[[i]]$differences
  tempFrank = tempBruto[tempBruto$item %in% item_stf[item_stf$replication == i, "item_frank"], ] 
  tempFrank$rp = (tempFrank$rank*100)/nrow(tempBruto)
  tempFrank$alorigthm = "frank"
  tempFrank$iter = item_stf[item_stf$replication == i, "replication"]
  tempIla = tempBruto[tempBruto$item %in% item_stf[item_stf$replication == i, "item_ila"], ] 
  tempIla$rp = (tempIla$rank*100)/nrow(tempBruto)
  tempIla$alorigthm = "ila"
  tempIla$iter = item_stf[item_stf$replication == i, "replication"]
  tempIsa = tempBruto[tempBruto$item %in% item_stf[item_stf$replication == i, "item_isa"], ] 
  tempIsa$rp = (tempIsa$rank*100)/nrow(tempBruto)
  tempIsa$alorigthm = "isa"
  tempIsa$iter = item_stf[item_stf$replication == i, "replication"]
  rank_ila = rbind(rank_ila, tempIla)
  rank_isa = rbind(rank_isa, tempIsa)
  rank_frank = rbind(rank_frank, tempFrank)
}
head(rank_ila)
ggplot(rank_ila, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point() +ylim(0, 100)
ggplot(rank_isa, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point() +ylim(0, 100)
ggplot(rank_frank, 
       aes(x = reorder((iter), rp), 
           y = rp, color = factor(n_item))) + geom_point() +ylim(0, 100)
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
for (i in item_stf$replication) {
  mydBruto[[i]] = delta(all_q_bruto, nitems = 11, replica = i)
  temp = 11- sum(mydBruto[[i]]$distance)
  names(temp) = paste("replica", i, sep = "_")
  distance_bruto = c(distance_bruto, temp)
}
# voglio vedere la distanza degli item delle forme brevi ottenute con i vari algoritmi algoritmi 
# dalla selezione di item di bruto 
all_stf = merge(all_q_bruto, item_stf, by = "replication")
all_stf[order(all_stf$replication, decreasing = F), ]
d_ila  = list()
d_isa  = list()
d_frank = list()
the_distances = data.frame(replica = as.numeric(gsub("replica_", "", names(distance_bruto))),  
                           bruto_d = distance_bruto, 
                           ila_d = numeric(length(distance_bruto)), 
                           isa_d = numeric(length(distance_bruto)), 
                           frank_d = numeric(length(distance_bruto)))
for (i in the_distances$replica) {
  d_ila[[i]] = delta(all_stf, nitems = 11, replica = i, 
                     target = "item", comparison = "item_ila")
  d_isa[[i]] = delta(all_stf, nitems = 11, replica = i, 
                     target = "item", comparison = "item_isa")
  d_frank[[i]] = delta(all_stf, nitems = 11, replica = i, 
                     target = "item", comparison = "item_frank")
  the_distances[i, "ila_d"] = 11- sum(d_ila[[i]]$distance)
  the_distances[i, "isa_d"] = 11- sum(d_isa[[i]]$distance)
  the_distances[i, "frank_d"] = 11- sum(d_frank[[i]]$distance)
}
the_distances = the_distances[!is.na(the_distances$replica), ]
colMeans(the_distances)
# frank è qyuello che si avvicina di più in assoluto 

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
small_dila = d_ila[the_distances$replica]
small_disa = d_isa[the_distances$replica]
small_dfrank = d_frank[the_distances$replica]
small_dbruto = mydBruto[the_distances$replica]
for (i in 1:length(small_dila)) {
  ila_acc = rbind(ila_acc, performance(small_dila[[i]])$performance)
  isa_acc = rbind(isa_acc, performance(small_disa[[i]])$performance)
  frank_acc = rbind(frank_acc, performance(small_dfrank[[i]])$performance)
  bruto_acc = rbind(bruto_acc, performance(small_dbruto[[i]])$performance)
}
ila_acc = data.frame(ila_acc)
ila_acc$algo = "ila"
isa_acc = data.frame(isa_acc)
isa_acc$algo = "isa"
frank_acc = data.frame(frank_acc)
frank_acc$algo = "frank"
bruto_acc = data.frame(bruto_acc)
bruto_acc$algo = "bruto"

all_acc = rbind(ila_acc, isa_acc, frank_acc, bruto_acc)
#all_acc$algo = rep(c("ila", "isa", "frank", "bruto"), each = 100)
all_acc$replication = the_distances$replica
ggplot(all_acc[!all_acc$algo %in% "bruto",], 
       aes(x = reorder(replication, accuracy), y = accuracy, 
           shape = algo, 
           color = algo)) + geom_point(alpha = .8)

ggplot(all_acc[!all_acc$algo %in% "bruto",], 
       aes(x = algo, y = accuracy, 
           shape = algo, 
           color = algo)) +geom_boxplot()
ggplot(all_acc, 
       aes(x = reorder(replication, specificity11), y = specificity11, 
           shape = algo, 
           color = algo)) + geom_point()
ggplot(all_acc[!all_acc$algo %in% "bruto",], 
       aes(x = algo, y = specificity11, 
           shape = algo, 
           color = algo)) +geom_boxplot()

ggplot(all_acc, 
       aes(x = reorder(replication, sensitivity00), y = sensitivity00, 
           shape = algo, 
           color = algo)) + geom_point()
ggplot(all_acc[!all_acc$algo %in% "bruto",], 
       aes(x = algo, y = sensitivity00, 
           shape = algo, 
           color = algo)) +geom_boxplot()

all_acc %>%  
  group_by(algo) %>%  
  summarise(mean_acc = mean(accuracy), sd_acc = sd(accuracy), 
            min_acc = min(accuracy), max_acc = max(accuracy))
aggregate(accuracy ~ algo, all_acc, quantile)
