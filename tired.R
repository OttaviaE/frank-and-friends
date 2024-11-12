# i'm just so tired, please have mercy (Cit. anyone during the administration of the MMPI)
library(tidyverse)
source("functions-new.R")

# item bank, 11 items with their diffiuclty and discirmianton parameters
set.seed(1312)
q = data.frame(b = runif(12, -3, 3), 
               a = runif(12, .9, 2))
rownames(q) = paste("item", 1:nrow(q), sep ="_")
q
theta = seq(-4, 4, length.out = 1000)

plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"]), type = "l")
for (i in 2:nrow(q)) {
  lines(theta, IRT(theta, b= q[i, "b"], a = q[i, "a"]), type = "l")
}

all_iif = item_info(q, theta)
tif = data.frame(theta, tif = rowSums(all_iif), 
                 mean_tif = rowMeans(all_iif))
plot(theta, tif$tif, type = "l")
# questa è la tif dell'item banck senza l'influenza della stanchezza 

# ci sono diverse possibiliutà per definire la stanchezza, ovvero per capire se inizia a rompere le apll dal primo item, dal secondo 
# item eccetera. 
# forse la cosa più appropiata sarebbe una funzione esponenziale in funzione 
# del numero di item  ma questo è un problema per la me del futuro 
IRT(theta, b = q[1, "b"], a = q[1, "a"])
IRT(theta[1], b = q[1, "b"], a = q[1, "a"], e = .99)
plot(theta,  IRT(theta, b = q[1, "b"], a = q[1, "a"]), type = "l")
e = seq(.99, .90, by = -.02)
 for (i in 1:length(e)) {
   lines(theta, IRT(theta, b = q[1, "b"], a = q[1, "a"], e = e[i]), type = "l", col = i)
 }
abline(a = .5, b = 0, lty = 2, lwd = 3, col = "red")
abline(v = q[1, "b"], lty = 2, lwd = 3, col = "red")

seq(1, .90, by = -0.03)
q$e = rep(seq(1, .90, by = -0.03), each = 3)
# stessi item di prima ma con la stanchezza 

plot(theta, IRT(theta, b= q[1, "b"], a = q[1, "a"], e = q[1, "e"]), 
     type = "l", ylim= c(0,1))
for (i in 2:nrow(q)) {
  lines(theta, IRT(theta, b= q[i, "b"], a = q[i, "a"], e = q[i, "e"],),  
        type = "l", ylim= c(0,1), col = i)
}
tired_iif = item_info(q, theta)
tired_tif = data.frame(theta, tired_tif = rowSums(tired_iif), 
                       mean_tired_tif = rowMeans(tired_iif))
plot(theta, tif$tif, type = "l", ylim = c(0, 5))
lines(theta, tired_tif$tired_tif, type = "l", col = "red")

plot(theta, all_iif$item_12, type = "l")
abline(v =q[12, "b"])
lines(theta, tired_iif$item_12, type = "l", col = "red")


plot(theta, all_iif$item_11, type = "l")
abline(v =q[11, "b"])
lines(theta, tired_iif$item_11, type = "l", col = "red")
(1- IRT(1.5, b = q[11, "b"], a = q[11, "a"]))* IRT(1.5, b = q[11, "b"], a = q[11, "a"])
(1- IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"]))*(IRT(1.5, b = q[11, "b"], a = q[11, "a"], e = q[11, "e"]))
