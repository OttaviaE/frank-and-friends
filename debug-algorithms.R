# test algorithms ----- 
source("functions-new.R")
parameters = list()
resFrank = list()
theta = seq(-4,4, length.out = 1500)
target = list()
resIla = list()
resIsa = list()
# i = 1 not okay -----
i = 1
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                    add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                    add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                    seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])

# guardo meglio i risultati di ILA per i = 1
# questi sono i parametri di ILA i = 1 a k = 0
#                 b        a temp_theta temp_distance
# item_1  -1.4069480 1.126572   1.011341    2.41828892
# item_2  -0.7672566 1.094212   1.011341    1.77859750
# item_3   0.4371202 1.655725   1.011341    0.57422071
# item_4   2.4492467 1.322514   1.011341    1.43790585
# item_5  -1.7899084 1.746826   1.011341    2.80124931
# item_6   2.3903381 1.447469   1.011341    1.37899722
# item_7   2.6680516 1.689380   1.011341    1.65671072
# item_8   0.9647868 1.991097   1.011341    0.04655414
# item_9   0.7746843 1.318039   1.011341    0.23665663
# item_10 -2.6292824 1.755190   1.011341    3.64062327
# effettivamente item 8 non solo ha la location più vicina ma anche la a 
# più alta 

# distances_pif (target - tif con item selezionato)
# temp_0 
# 0.2104157 
# 
# distances_tif (target - tif senza l'item, in questo caso 0)
# temp_0 
# 0.18404 
# sono disposta ad accettare questa situazione. bisogna riflettere se però 
# continuare con la ricerca di un altro item (il secondo con la distanza migliore?)
# oppure decretare che SECONDO QUESTA PROCEDURA non ci sono item (ed eventualmente)
# tenere l'unico item non selezionato
# per il momento implemento il fato che restituisca un warning e dia indietro la forma breve 
# con il solo item che ha trovato
ilai1 = resIla[[1]]$stf[, c("theta", "mean_tif", "tif_stf")] 
#ilai1$tif_stf = resIla[[1]]$all_stfs$pif[-1]
ilai1 = pivot_longer(ilai1, 
                     cols = !theta)
ggplot(ilai1, 
       aes(x = theta, y = value, color = name)) + geom_line()
# ha senso che faccia aumentare la distanza assoluta! la funzione informativa temporanea 
# è altissima ma in un singolo punto del tratto 
# per forza di cose la disanza aumenta rispeeto alla situazione inziale 
# isa è coerente con ILA
# vedo cosa fa isa nella stessa situazione 
#                   b        a temp_theta    temp_iif
# item_1  -1.4069480 1.126572   1.011341 0.073309169
# item_2  -0.7672566 1.094212   1.011341 0.130929899
# item_3   0.4371202 1.655725   1.011341 0.551139558
# item_4   2.4492467 1.322514   1.011341 0.197715542
# item_5  -1.7899084 1.746826   1.011341 0.022536231
# item_6   2.3903381 1.447469   1.011341 0.220640492
# item_7   2.6680516 1.689380   1.011341 0.154387043
# item_8   0.9647868 1.991097   1.011341 0.988990614
# item_9   0.7746843 1.318039   1.011341 0.423911437
# item_10 -2.6292824 1.755190   1.011341 0.005152958

# i = 2 ok -----

i = 2
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
# tutto liscio 
# ILA GRAFICOi = 2
ilai2 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai2 = pivot_longer(ilai2, 
                     cols = !theta)
ggplot(ilai2, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 2
isai2 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai2 = pivot_longer(isai2, 
                     cols = !theta)
ggplot(isai2, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 3 ok -----
i = 3
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])


# ILA GRAFICOi = 3
ilai3 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai3 = pivot_longer(ilai3, 
                     cols = !theta)
ggplot(ilai3, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 3
isai3 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai3 = pivot_longer(isai3, 
                     cols = !theta)
ggplot(isai3, 
       aes(x = theta, y = value, color = name)) + geom_line()


# i = 4 not ok -----
i = 4
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
# isa dà problemi di nuovo perché non trova il primo item 
resIsa[[4]]$warning
# item_not_found  stf_not_found 
# TRUE          FALSE 
resIla[[4]]$warning
# item_not_found  stf_not_found 
# FALSE          FALSE 
resIla[[4]]$q_ila
# "item_5 item_6"
resIla[[4]]$all_items
# "item_6" "item_5" "item_2"

# ILA GRAFICOi = 4
ilai4 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai4 = pivot_longer(ilai4, 
                     cols = !theta)
ggplot(ilai4, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 4
resIsa[[4]]$all_items
# item 4 
#                   b        a temp_theta    temp_iif
# item_1   0.5148018 1.730143  -1.416945 0.098737945
# item_2  -2.9463252 1.214601  -1.416945 0.172256283
# item_3  -1.2375623 1.010059  -1.416945 0.252972859
# item_4  -1.3357503 1.949476  -1.416945 0.944187411
# item_5   1.8814453 1.357168  -1.416945 0.020480394
# item_6  -1.4374334 1.400613  -1.416945 0.490328001
# item_7   1.3464354 1.968161  -1.416945 0.016684942
# item_8   2.4365529 1.542387  -1.416945 0.006206489
# item_9   2.6942413 1.958425  -1.416945 0.001221341
# item_10 -2.5611332 1.737873  -1.416945 0.319897966

isai4 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai4$tif_stf = resIsa[[i]]$all_stfs$pif
isai4 = pivot_longer(isai4, 
                     cols = !theta)
ggplot(isai4, 
       aes(x = theta, y = value, color = name)) + geom_line()

# sceglie un item che è troppo informativo rispetto a un solo punto di theta
# per questa ragione poi la distanza aumenta e non seleziona alcun item

# i = 5 ok -----
i = 5
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
# tutto liscio

# ILA GRAFICOi = 5
ilai5 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai5 = pivot_longer(ilai5, 
                     cols = !theta)
ggplot(ilai5, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 5
isai5 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai5 = pivot_longer(isai5, 
                     cols = !theta)
ggplot(isai5, 
       aes(x = theta, y = value, color = name)) + geom_line()
# isa sembra essere meno precisa 

# i = 6 not ok ------
i = 6
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
# entrambe non vanno 
resIla[[i]]$warning
resIsa[[i]]$warning

resIla[[i]]$q_ila
resIsa[[i]]$q_isa
resIla[[i]]$all_items
resIsa[[i]]$all_items

# ila 
#                   b        a temp_theta temp_distance
# item_1   0.63760979 1.607930  -2.244163     2.8817726
# item_2   2.62585184 1.907500  -2.244163     4.8700146
# item_3  -1.41388759 1.004756  -2.244163     0.8302752
# item_4  -0.71943648 1.224910  -2.244163     1.5247263
# item_5   1.84490033 1.746925  -2.244163     4.0890631
# item_6   2.86845435 1.181483  -2.244163     5.1126171
# item_7   2.74760230 1.469685  -2.244163     4.9917651
# item_8   1.57639114 1.645635  -2.244163     3.8205539
# item_9   0.05789121 1.061951  -2.244163     2.3020540
# item_10 -2.61313923 1.670579  -2.244163     0.3689765


# ISA 

#                   b        a temp_theta     temp_iif
# item_1   0.63760979 1.607930  -2.244163 0.0246460857
# item_2   2.62585184 1.907500  -2.244163 0.0003360839
# item_3  -1.41388759 1.004756  -2.244163 0.2131061320
# item_4  -0.71943648 1.224910  -2.244163 0.1739081558
# item_5   1.84490033 1.746925  -2.244163 0.0024075355
# item_6   2.86845435 1.181483  -2.244163 0.0033070981
# item_7   2.74760230 1.469685  -2.244163 0.0014052647
# item_8   1.57639114 1.645635  -2.244163 0.0050181160
# item_9   0.05789121 1.061951  -2.244163 0.0828399272
# item_10 -2.61313923 1.670579  -2.244163 0.6354156118

ilai6 = resIla[[i]]$stf[, c("theta", "mean_tif")] 
ilai6$tif_stf = resIla[[i]]$alli_stfs$pif
ilai6 = pivot_longer(ilai6, 
                     cols = !theta)
ggplot(ilai6, 
       aes(x = theta, y = value, color = name)) + geom_line()


# i = 7 ok ------
i = 7
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 7
ilai7 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai7 = pivot_longer(ilai7, 
                     cols = !theta)
ggplot(ilai7, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 6
isai7 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai7 = pivot_longer(isai7, 
                     cols = !theta)
ggplot(isai7, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 8 not ok-----

i = 8
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 8
ilai8 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai8 = pivot_longer(ilai8, 
                     cols = !theta)
ggplot(ilai8, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 8
isai8 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai8$tif_stf = resIsa[[i]]$all_stfs$pif
isai8 = pivot_longer(isai8, 
                     cols = !theta)
ggplot(isai8, 
       aes(x = theta, y = value, color = name)) + geom_line()



# i = 9 ok-----

i = 9
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 9
ilai9 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai9 = pivot_longer(ilai9, 
                     cols = !theta)
ggplot(ilai9, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 9
isai9 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai9 = pivot_longer(isai9, 
                     cols = !theta)
ggplot(isai9, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 10 ok-----

i = 10
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 10
ilai10 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai10 = pivot_longer(ilai10, 
                      cols = !theta)
ggplot(ilai10, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 10
isai10 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai10 = pivot_longer(isai10, 
                      cols = !theta)
ggplot(isai10, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 11 ok-----

i = 11
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 11
ilai11 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai11 = pivot_longer(ilai11, 
                      cols = !theta)
ggplot(ilai11, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 11
isai11 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai11 = pivot_longer(isai11, 
                      cols = !theta)
ggplot(isai11, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 12 ok-----

i = 12
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 12
ilai12 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai12 = pivot_longer(ilai12, 
                      cols = !theta)
ggplot(ilai12, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 12
isai12 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai12 = pivot_longer(isai12, 
                      cols = !theta)
ggplot(isai12, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 13 not ok-----

i = 13
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

resIla[[i]]$all_items
resIsa[[i]]$all_items

# ILA GRAFICOi = 13
isai13 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai13$tif_stf = resIsa[[i]]$all_stfs$pif
isai13 = pivot_longer(isai13, 
                     cols = !theta)
ggplot(isai13, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 14 ok-----

i = 14
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa

# ILA GRAFICOi = 14
ilai14 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai14 = pivot_longer(ilai14, 
                      cols = !theta)
ggplot(ilai14, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 14
isai14 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai14 = pivot_longer(isai14, 
                      cols = !theta)
ggplot(isai14, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 15 not ok-----

i = 15
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa
resIla[[i]]$all_items
resIsa[[i]]$all_items

# ILA GRAFICOi = 15
ilai15 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai15 = pivot_longer(ilai15, 
                      cols = !theta)
ggplot(ilai15, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 15
isai15 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai15 = pivot_longer(isai15, 
                      cols = !theta)
ggplot(isai15, 
       aes(x = theta, y = value, color = name)) + geom_line()

# i = 73 perché non trova una soluzione in entrambe ----

i = 73
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa
resIla[[i]]$all_items
resIsa[[i]]$all_items

# ILA GRAFICOi = 73
ilai73 = resIla[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
ilai73 = pivot_longer(ilai73, 
                      cols = !theta)
ggplot(ilai73, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ISa grafico i = 73
isai73 = resIsa[[i]]$stf[, c("theta", "mean_tif", "tif_stf")] 
isai73 = pivot_longer(isai73, 
                      cols = !theta)
ggplot(isai73, 
       aes(x = theta, y = value, color = name)) + geom_line()

# ila debug replica 73 
# k = 0
#                  b         a temp_theta temp_distance
# item_1  -0.3459785 1.5117436  0.2428286    0.58880705
# item_2  -2.5013409 1.7083240  0.2428286    2.74416941
# item_3   0.6876674 1.9316964  0.2428286    0.44483886
# item_4   0.7368383 1.7381694  0.2428286    0.49400978
# item_5   0.2241897 1.9966090  0.2428286    0.01863881
# item_6  -2.1754317 1.9065964  0.2428286    2.41826026
# item_7  -0.3040523 1.9335301  0.2428286    0.54688089
# item_8   2.9313842 1.9157147  0.2428286    2.68855565
# item_9  -1.8087321 1.6455778  0.2428286    2.05156068
# item_10  1.5921825 0.9135603  0.2428286    1.34935390
# k = 1
#                   b         a temp_theta temp_distance
# item_1  -0.3459785 1.5117436  0.2161441     0.5621226
# item_2  -2.5013409 1.7083240  0.2161441     2.7174849
# item_3   0.6876674 1.9316964  0.2161441     0.4715233
# item_4   0.7368383 1.7381694  0.2161441     0.5206942
# item_5          NA        NA  0.2161441            NA
# item_6  -2.1754317 1.9065964  0.2161441     2.3915758
# item_7  -0.3040523 1.9335301  0.2161441     0.5201964
# item_8   2.9313842 1.9157147  0.2161441     2.7152401
# item_9  -1.8087321 1.6455778  0.2161441     2.0248762
# item_10  1.5921825 0.9135603  0.2161441     1.3760384
# k = 2
#                 b         a temp_theta temp_distance
# item_1  -0.3459785 1.5117436    0.51501     0.8609885
# item_2  -2.5013409 1.7083240    0.51501     3.0163509
# item_3          NA        NA    0.51501            NA
# item_4   0.7368383 1.7381694    0.51501     0.2218283
# item_5          NA        NA    0.51501            NA
# item_6  -2.1754317 1.9065964    0.51501     2.6904417
# item_7  -0.3040523 1.9335301    0.51501     0.8190623
# item_8   2.9313842 1.9157147    0.51501     2.4163742
# item_9  -1.8087321 1.6455778    0.51501     2.3237421
# item_10  1.5921825 0.9135603    0.51501     1.0771724
# k = 3 
#                 b         a temp_theta temp_distance
# item_1  -0.3459785 1.5117436  0.6430954     0.9890739
# item_2  -2.5013409 1.7083240  0.6430954     3.1444363
# item_3          NA        NA  0.6430954            NA
# item_4          NA        NA  0.6430954            NA
# item_5          NA        NA  0.6430954            NA
# item_6  -2.1754317 1.9065964  0.6430954     2.8185271
# item_7  -0.3040523 1.9335301  0.6430954     0.9471477
# item_8   2.9313842 1.9157147  0.6430954     2.2882888
# item_9  -1.8087321 1.6455778  0.6430954     2.4518275
# item_10  1.5921825 0.9135603  0.6430954     0.9490871
# k = 4
#                  b         a temp_theta temp_distance
# item_1  -0.3459785 1.5117436  0.4136091     0.7595876
# item_2  -2.5013409 1.7083240  0.4136091     2.9149499
# item_3          NA        NA  0.4136091            NA
# item_4          NA        NA  0.4136091            NA
# item_5          NA        NA  0.4136091            NA
# item_6  -2.1754317 1.9065964  0.4136091     2.5890408
# item_7          NA        NA  0.4136091            NA
# item_8   2.9313842 1.9157147  0.4136091     2.5177751
# item_9  -1.8087321 1.6455778  0.4136091     2.2223412
# item_10  1.5921825 0.9135603  0.4136091     1.1785734
# k = 5
#                 b         a temp_theta temp_distance
# item_1         NA        NA   0.269513            NA
# item_2  -2.501341 1.7083240   0.269513      2.770854
# item_3         NA        NA   0.269513            NA
# item_4         NA        NA   0.269513            NA
# item_5         NA        NA   0.269513            NA
# item_6  -2.175432 1.9065964   0.269513      2.444945
# item_7         NA        NA   0.269513            NA
# item_8   2.931384 1.9157147   0.269513      2.661871
# item_9  -1.808732 1.6455778   0.269513      2.078245
# item_10  1.592182 0.9135603   0.269513      1.322669
# k = 6 
#                 b        a temp_theta temp_distance
# item_1         NA       NA  -2.014676            NA
# item_2  -2.501341 1.708324  -2.014676     0.4866644
# item_3         NA       NA  -2.014676            NA
# item_4         NA       NA  -2.014676            NA
# item_5         NA       NA  -2.014676            NA
# item_6  -2.175432 1.906596  -2.014676     0.1607553
# item_7         NA       NA  -2.014676            NA
# item_8   2.931384 1.915715  -2.014676     4.9460607
# item_9  -1.808732 1.645578  -2.014676     0.2059443
# item_10        NA       NA  -2.014676            NA
# k = 7 
#                 b        a temp_theta temp_distance
# item_1         NA       NA  -1.843896            NA
# item_2  -2.501341 1.708324  -1.843896    0.65744492
# item_3         NA       NA  -1.843896            NA
# item_4         NA       NA  -1.843896            NA
# item_5         NA       NA  -1.843896            NA
# item_6         NA       NA  -1.843896            NA
# item_7         NA       NA  -1.843896            NA
# item_8   2.931384 1.915715  -1.843896    4.77528013
# item_9  -1.808732 1.645578  -1.843896    0.03516381
# item_10        NA       NA  -1.843896            NA
# k = 8 
#                 b        a temp_theta temp_distance
# item_1         NA       NA   3.076718            NA
# item_2  -2.501341 1.708324   3.076718     5.5780587
# item_3         NA       NA   3.076718            NA
# item_4         NA       NA   3.076718            NA
# item_5         NA       NA   3.076718            NA
# item_6         NA       NA   3.076718            NA
# item_7         NA       NA   3.076718            NA
# item_8   2.931384 1.915715   3.076718     0.1453336
# item_9         NA       NA   3.076718            NA
# item_10        NA       NA   3.076718            NA
# k = 9 
#                 b        a temp_theta temp_distance
# item_1         NA       NA  -1.817211            NA
# item_2  -2.501341 1.708324  -1.817211     0.6841294
# item_3         NA       NA  -1.817211            NA
# item_4         NA       NA  -1.817211            NA
# item_5         NA       NA  -1.817211            NA
# item_6         NA       NA  -1.817211            NA
# item_7         NA       NA  -1.817211            NA
# item_8         NA       NA  -1.817211            NA
# item_9         NA       NA  -1.817211            NA
# item_10        NA       NA  -1.817211            NA
# distances_pif
# temp_0     temp_1     temp_2     temp_3     temp_4     temp_5     temp_6     temp_7     temp_8     temp_9 
# 0.16833077 0.16686408 0.15867138 0.14009466 0.12158197 0.09887855 0.05438776 0.03281194 0.02011827 0.03206860
# distances_tif
# temp_0     temp_1     temp_2     temp_3     temp_4     temp_5     temp_6     temp_7     temp_8     temp_9 
# 0.22681708 0.16833077 0.16686408 0.15867138 0.14009466 0.12158197 0.09887855 0.05438776 0.03281194 0.02011827

# i = 33 -----

i = 33
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIla[[i]]$q_ila
resIsa[[i]]$q_isa
resIla[[i]]$all_items
resIsa[[i]]$all_items
resIsa[[i]]$distances_pif
# k = 0 
# b        a temp_theta   temp_iif
# item_1  -0.32435712 1.660084 -0.1307538 0.67147995
# item_2  -0.63209813 1.186534 -0.1307538 0.32257749
# item_3  -0.09762675 1.147556 -0.1307538 0.32910246
# item_4   2.51325574 1.276625 -0.1307538 0.05211861
# item_5   2.06328864 1.760077 -0.1307538 0.06249947
# item_6   0.10409770 1.827571 -0.1307538 0.79769889
# item_7  -0.37724997 1.752224 -0.1307538 0.73285836
# item_8  -0.94081071 1.325912 -0.1307538 0.33366612
# item_9  -2.90689824 1.049342 -0.1307538 0.05379563
# item_10 -2.29205303 1.890393 -0.1307538 0.05810644
# k = 1
# b        a temp_theta   temp_iif
# item_1  -0.32435712 1.660084  0.1627752 0.58755895
# item_2  -0.63209813 1.186534  0.1627752 0.28398973
# item_3  -0.09762675 1.147556  0.1627752 0.32197978
# item_4   2.51325574 1.276625  0.1627752 0.07358218
# item_5   2.06328864 1.760077  0.1627752 0.10191288
# item_6           NA       NA  0.1627752         NA
# item_7  -0.37724997 1.752224  0.1627752 0.61848420
# item_8  -0.94081071 1.325912  0.1627752 0.26834243
# item_9  -2.90689824 1.049342  0.1627752 0.04063693
# item_10 -2.29205303 1.890393  0.1627752 0.03383592
# k = 2
# b        a temp_theta   temp_iif
# item_1  -0.32435712 1.660084 -0.1094063 0.66749527
# item_2  -0.63209813 1.186534 -0.1094063 0.32017813
# item_3  -0.09762675 1.147556 -0.1094063 0.32920634
# item_4   2.51325574 1.276625 -0.1094063 0.05346078
# item_5   2.06328864 1.760077 -0.1094063 0.06479022
# item_6           NA       NA -0.1094063         NA
# item_7           NA       NA -0.1094063         NA
# item_8  -0.94081071 1.325912 -0.1094063 0.32901362
# item_9  -2.90689824 1.049342 -0.1094063 0.05272420
# item_10 -2.29205303 1.890393 -0.1094063 0.05588128
# k = 3
# b        a temp_theta   temp_iif
# item_1           NA       NA -0.2001334         NA
# item_2  -0.63209813 1.186534 -0.2001334 0.32982632
# item_3  -0.09762675 1.147556 -0.2001334 0.32808512
# item_4   2.51325574 1.276625 -0.2001334 0.04796948
# item_5   2.06328864 1.760077 -0.2001334 0.05557793
# item_6           NA       NA -0.2001334         NA
# item_7           NA       NA -0.2001334         NA
# item_8  -0.94081071 1.325912 -0.2001334 0.34850562
# item_9  -2.90689824 1.049342 -0.2001334 0.05741069
# item_10 -2.29205303 1.890393 -0.2001334 0.06594377
# k = 4
# b        a temp_theta   temp_iif
# item_1           NA       NA -0.3122081         NA
# item_2  -0.63209813 1.186534 -0.3122081 0.33958757
# item_3  -0.09762675 1.147556 -0.3122081 0.32428070
# item_4   2.51325574 1.276625 -0.3122081 0.04191305
# item_5   2.06328864 1.760077 -0.3122081 0.04592823
# item_6           NA       NA -0.3122081         NA
# item_7           NA       NA -0.3122081         NA
# item_8           NA       NA -0.3122081         NA
# item_9  -2.90689824 1.049342 -0.3122081 0.06369516
# item_10 -2.29205303 1.890393 -0.3122081 0.08078661
# k = 5 
# b        a temp_theta   temp_iif
# item_1           NA       NA -0.3815877         NA
# item_2           NA       NA -0.3815877         NA
# item_3  -0.09762675 1.147556 -0.3815877 0.32063412
# item_4   2.51325574 1.276625 -0.3815877 0.03853275
# item_5   2.06328864 1.760077 -0.3815877 0.04078972
# item_6           NA       NA -0.3815877         NA
# item_7           NA       NA -0.3815877         NA
# item_8           NA       NA -0.3815877         NA
# item_9  -2.90689824 1.049342 -0.3815877 0.06787190
# item_10 -2.29205303 1.890393 -0.3815877 0.09151382
# k = 6 
# b        a temp_theta   temp_iif
# item_1         NA       NA -0.3869246         NA
# item_2         NA       NA -0.3869246         NA
# item_3         NA       NA -0.3869246         NA
# item_4   2.513256 1.276625 -0.3869246 0.03828371
# item_5   2.063289 1.760077 -0.3869246 0.04041845
# item_6         NA       NA -0.3869246         NA
# item_7         NA       NA -0.3869246         NA
# item_8         NA       NA -0.3869246         NA
# item_9  -2.906898 1.049342 -0.3869246 0.06820250
# item_10 -2.292053 1.890393 -0.3869246 0.09239248
# k = 7 
# b        a temp_theta   temp_iif
# item_1         NA       NA  -0.563042         NA
# item_2         NA       NA  -0.563042         NA
# item_3         NA       NA  -0.563042         NA
# item_4   2.513256 1.276625  -0.563042 0.03087371
# item_5   2.063289 1.760077  -0.563042 0.02985544
# item_6         NA       NA  -0.563042         NA
# item_7         NA       NA  -0.563042         NA
# item_8         NA       NA  -0.563042         NA
# item_9  -2.906898 1.049342  -0.563042 0.07988083
# item_10        NA       NA  -0.563042         NA
# k = 8 
# b        a temp_theta  temp_iif
# item_1        NA       NA   2.366911        NA
# item_2        NA       NA   2.366911        NA
# item_3        NA       NA   2.366911        NA
# item_4  2.513256 1.276625   2.366911 0.4039080
# item_5  2.063289 1.760077   2.366911 0.7217031
# item_6        NA       NA   2.366911        NA
# item_7        NA       NA   2.366911        NA
# item_8        NA       NA   2.366911        NA
# item_9        NA       NA   2.366911        NA
# item_10       NA       NA   2.366911        NA
# k = 9
# b        a temp_theta  temp_iif
# item_1        NA       NA   2.740494        NA
# item_2        NA       NA   2.740494        NA
# item_3        NA       NA   2.740494        NA
# item_4  2.513256 1.276625   2.740494 0.3989893
# item_5        NA       NA   2.740494        NA
# item_6        NA       NA   2.740494        NA
# item_7        NA       NA   2.740494        NA
# item_8        NA       NA   2.740494        NA
# item_9        NA       NA   2.740494        NA
# item_10       NA       NA   2.740494        NA

# i = 89 -----

i = 89
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])

# i = 8 -----
i = 8
set.seed(i)
parameters[[i]] = data.frame(b = runif(10, -3, 3), 
                             a = runif(10, 0.9, 2))
rownames(parameters[[i]]) = paste("item", rownames(parameters[[i]]), sep = "_")
target[[i]] = tif_target(parameters[[i]], theta, 
                         add_difficulty = runif(nrow(parameters[[i]]), -.2, .2), 
                         add_discriminativity = runif(nrow(parameters[[i]]), -.2, .2), 
                         seed = i)
ggplot(target[[i]], 
       aes(x = theta, y = mean_tif)) + geom_line()
resFrank[[i]] = frank(parameters[[i]], target[[i]])
resIla[[i]] = ila(parameters[[i]], target[[i]])
resIsa[[i]] = isa(parameters[[i]], target[[i]])
resIsa[[8]]$all_items
resIsa[[8]]$q_isa
