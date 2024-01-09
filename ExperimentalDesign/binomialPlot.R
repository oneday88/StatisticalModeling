library(ggplot2)
library(data.table)
library(dplyr)
#library(scales)


plotDt  <- data.table(heads=0:10, prob=dbinom(x=0:10, size=10., prob=0.5))

plotDt[, observed:='other']
plotDt[heads>=7, observed:='heads>=7']

p1      <- ggplot(plotDt,aes(x=factor(heads), y=prob, fill=observed))+geom_col()+
        geom_text(aes(label = round(prob,3), y = prob + 0.01),
        position = position_dodge(0.9), size = 3,vjust = 0)+
        ggtitle("Probability of heads up")+theme(plot.title = element_text(hjust = 0.5))

plot(p1)
ggsave("coinExample.png", width = 18, height = 10, units = "cm")





