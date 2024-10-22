library(tidyverse)
source("functions-new.R")
# nuova simulazione bruto 
parameters = list()
replications = 100
resBruto = list()
bruto_elapsed = numeric(replications)
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
  bruto_elapsed[i] = c(bruto_elapsed, bruto_end - bruto_start)
}
bruto_start = Sys.time()
resBruto[[i]] = bruto(parameters[[i]], target)
bruto_end = Sys.time()
bruto_elapsed[i] = c(bruto_elapsed, bruto_end - bruto_start)

parameters = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
target = tif_target(parameters, theta, 
                    add_difficulty = runif(nrow(parameters), -.2, .2), 
                    add_discriminativity = runif(nrow(parameters), -.2, .2), 
                    seed = i)
# provo FRANCO ---- 
# all'inizio, guarda solo la distanza della IIF di ogni item dalla target 
res_frank = frank(parameters, target)



