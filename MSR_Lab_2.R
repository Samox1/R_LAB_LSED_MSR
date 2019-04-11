# MSR - Laboratorium 2
# n - iloœæ liczb z rozk³adu normalnego
# m - œrednia
# s - odchylenie standardowe
# histogram -> empiryczna gestoœæ prawd. w postaci punktóW
# druga seria -> czerwona linia -> teoretyczna gêstoœæ prawdopodobieñstwa

rm(list=ls())

n = 1000
m = 0
s = 0.5

x <- rnorm(n, m, s)
h <- hist(x, plot=F)
plot(h$mids, h$density, ylim=c(0,1), pch = 19, col = "black", xlab = "X", ylab = "Y", font = 2)

z <- seq(min(h$mids), max(h$mids), 0.05)
g <- dnorm(z,m,s)
points(z, g, type="l", col = "red", lwd = 1)
