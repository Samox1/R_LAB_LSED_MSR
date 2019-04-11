### MSR - Zadanie z LAB 3

rm(list=ls())

library(fields)
library(Hmisc)

kurczaki <- datasets::ChickWeight

#megakurczaki <- kurczaki[which(kurczaki$Diet==1),names(kurczaki) %in% c("weight","Time","Chick", "Diet")]
megakurczaki <- kurczaki[which(kurczaki$Diet==1),]

xy.sb <- stats.bin(megakurczaki$Time, megakurczaki$weight)
x.sb <- xy.sb$centers
y.sb <- xy.sb$stats[2,]
xy.lm <- lm(y.sb ~ x.sb)
a.xy <- xy.lm$coefficients[2]
b.xy <- xy.lm$coefficients[1]

plot(megakurczaki$Time, megakurczaki$weight, xlab = "Dzieñ", ylab = "Waga [g]")
with(xy.sb, points(centers, stats[2,], col="red", pch=19, cex=1.5))
lines(x.sb, a.xy*x.sb+b.xy,col="blue", lwd=2)
