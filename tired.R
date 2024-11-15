# i'm just so tired, please have mercy (Cit. anyone during the administration of the MMPI)
library(tidyverse)
source("functions-new.R")

# b = 1
# a = 2
# c = 0
# e = .95
# i_info(b,a,c,1, theta = seq(-3,3))
# i_info1(b,a,.20,1, theta = seq(-3,3))
# 
# plot(seq(-3,3, length.out = 100), 
#      i_info1(b,a,c,1, theta = seq(-3,3, length.out = 100)), type = "l")
# lines(seq(-3,3, length.out = 100), 
#      i_info1(b,a,0,.95, theta = seq(-3,3, length.out = 100)), type = "l", col = "red")


# item bank, 12 items with their diffiuclty and discirmianton parameters
set.seed(1312)
q = data.frame(b = runif(12, -3, 3), 
               a = runif(12, .9, 2), 
               c = rep(0,12), e= rep(1, 12))
rownames(q) = paste("item", 1:nrow(q), sep ="_")
q
theta = seq(-4, 4, length.out = 1000)
# ICC dellìtem bank senza errore 
plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"]), type = "l")
for (i in 2:nrow(q)) {
  lines(theta, IRT(theta, b= q[i, "b"], a = q[i, "a"]), type = "l", col = i)
}

all_iif = item_info(q, theta)
tif = data.frame(theta, tif = rowSums(all_iif), 
                 mean_tif = rowMeans(all_iif))
# tif senza careless error 
plot(theta, tif$tif, type = "l")
# questa è la tif dell'item banck senza l'influenza della stanchezza 

# ci sono diverse possibiliutà per definire la stanchezza, ovvero per capire se inizia a rompere le apll dal primo item, dal secondo 
# item eccetera. 
# forse la cosa più appropiata sarebbe una funzione esponenziale in funzione 
# del numero di item  ma questo è un problema per la me del futuro 
IRT(theta, b = q[1, "b"], a = q[1, "a"])
IRT(theta[1], b = q[1, "b"], a = q[1, "a"], e = .99)
# qui ho fatto il plot della ICC di un item che rimane fisso per quello che concerne 
# difficoltà e discriminatività ma che aumenta nella probabilità di careless error
# si vee chiaramente che la probabilità di rispondere correttamente all'item quando 
# theta = 1 diminuisce all'aumentarre della careless error ed è minore .50
plot(theta,  IRT(theta, b = q[1, "b"], a = q[1, "a"]), type = "l")
e = seq(.99, .90, by = -.02)
for (i in 1:length(e)) {
   lines(theta, IRT(theta, b = q[1, "b"], a = q[1, "a"], e = e[i]), type = "l", col = i)
 }
abline(a = .5, b = 0, lty = 2, lwd = 3, col = "red")
abline(v = q[1, "b"], lty = 2, lwd = 3, col = "red")

# plotto la IIF di questo stesso item 
plot(theta,  i_info(b = q[1, "b"], a = q[1, "a"], theta = theta), type = "l")
for (i in 1:length(e)) {
  lines(theta,  i_info(b = q[1, "b"], a = q[1, "a"],e = e[i], theta = theta), type = "l", col = i)
}
abline(v = q[1, "b"])
# la iif per il theta corrispondente al b dell'item è effettivamente leggermente più bassa e 
# traslata verso dx

#seq(1, .90, by = -0.03)
# metto un careless error fisso su tutti gli item 
q$e = rep(.95, 12)
# stessi item di prima ma con la stanchezza 
plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"], e = q[1, "e"]), 
     type = "l", ylim= c(0,1))
for (i in 2:nrow(q)) {
  lines(theta, IRT(theta, b= q[i, "b"], a = q[i, "a"], e = q[i, "e"],),  
        type = "l", ylim= c(0,1), col = i)
}
# ora calcolo la TIF con la stessa stanchezza su tutti gli item 
tired_iif = item_info(q, theta)
tired_tif = data.frame(theta, tired_tif = rowSums(tired_iif), 
                       mean_tired_tif = rowMeans(tired_iif))
plot(theta, tif$tif, type = "l", ylim = c(0, 5))
lines(theta, tired_tif$tired_tif, type = "l", col = "red")

plot(theta, all_iif$item_12, type = "l")
abline(v =q[12, "b"])
lines(theta, tired_iif$item_12, type = "l", col = "red")
# NON HA SENSO DIO PORCO DEVE SCENDERE NO SALIRE 

plot(theta, all_iif$item_11, type = "l")
abline(v =q[11, "b"])
lines(theta, tired_iif$item_11, type = "l", col = "red")

P = IRT(q[11, "b"], b = q[11, "b"], a = q[11, "a"], e = q[11, "e"])
Q = 1- p

P1 = IRT(q[11, "b"], b = q[11, "b"], a = q[11, "a"], e = 1, c = .20)
Q1 = 1- P1

myinfo = (q[11, "a"]^2)*P1*Q1
(q[11, "a"]^2)*(Q1/P1)*((P1 - 0)/(1 -0))^2
(q[11, "a"]^2)*(Q/P)*((P - 0)/(.95 -0))^2



(1- IRT(1.5, b = q[11, "b"], a = q[11, "a"]))* IRT(1.5, b = q[11, "b"], a = q[11, "a"])
(1- IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"]))*(IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"]))

(q[11, "a"]^2)*((1-IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0))/IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0))*(((IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"],c=0) -c)/(q[11, "e"]-0)))^2
(q[11, "a"]^2)*((1-IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = 1, c=0))/IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = 1, c=0))*(((IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = 1,c=0) -c)/(1-0)))^2

(q[11, "a"]^2)*((1-IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0))/IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0))*(((IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"],c=0) -c)/(q[11, "e"]-0)))^2
(q[11, "a"]^2)*((1-IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = 1, c=0))/IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = 1, c=0))*(((IRT(-1.17, b = q[11, "b"], a = q[11, "a"], e = 1,c=0) -c)/(1-0)))^2


(q[11, "a"]^2)*(1-IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0))
IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"], c=0)
(((IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"],c=0) -c)/(q[11, "e"]-0)))^2
