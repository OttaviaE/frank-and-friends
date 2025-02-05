source("functions-new.R")
library(tidyverse)
item_sk = data.frame(b = c(-2, -2), 
                     a = c(1.9, 2), 
                     c = c(0,0),
                     e = c(1,1))
item_n = data.frame(b = c(-2.6, -1.5, -1, 0,0, 1, 1.5, 2.6), 
                     a = c(1.7, 1.7, 1.7, 1.7,1.7, 1.7, 1.7, 1.7), 
                     c = rep(0,8),
                     e = rep(1,8))
i_sk = item_info(item_sk, theta = seq(-4,4, length.out = 1500))
i_n = item_info(item_n, theta = seq(-4,4, length.out = 1500))

myinfo = data.frame(theta = seq(-4,4, length.out = 1500), 
                    tif_sk = rowSums(i_sk), 
                    tif_n = rowSums(i_n))

ggplot(myinfo, 
       aes(x = theta, y = tif_sk)) + geom_line(linewidth=1.2) + 
  ylab("TIF-Target") + xlab(expression(theta)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 24), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title = element_text(size = 26)) +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = seq(-4, 4, by = 2))
# Save the plot as a PDF with US legal dimensions
ggsave(paste0(getwd(),"/paper/img/target_sk.pdf"), 
       width = 8.5, height = 14, units = "in", dpi = 300)

ggplot(myinfo, 
       aes(x = theta, y = tif_n)) + geom_line(linewidth=1.2) + 
  ylab("TIF-Target") + xlab(expression(theta)) + 
  theme_classic() + 
  theme(axis.text.x = element_text(size = 24), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title = element_text(size = 26)) +
  scale_x_continuous(limits = c(-4, 4), 
                     breaks = seq(-4, 4, by = 2))

ggsave(paste0(getwd(),"/paper/img/target_n.pdf"), 
       width = 8.5, height = 14, units = "in", dpi = 300)


