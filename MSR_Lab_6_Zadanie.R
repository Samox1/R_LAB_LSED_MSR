### MSR Zadanie - LAB 6

rm(list=ls())
data1 <- read.table("http://www.if.pw.edu.pl/~paluch/MSR/zad6.txt", sep = "\n")
dane <- data1$V1


norm1 <- ks.test(dane, "pnorm", 0.5, 0.5)
print(norm1)

y <- rpois(200, 0.5)
pois1 <- ks.test(dane, y)
print(pois1)

wyk1 <- ks.test(dane, "pexp", 2)
print(wyk1)

Prawdo <- round(norm1$p.value,7)
Prawdo <- rbind(Prawdo, round(pois1$p.value, 7))
Prawdo <- rbind(Prawdo, round(wyk1$p.value, 7))
rownames <- c("Norm", "Poisson", "Wyk³ad")
print(Prawdo)

hist1 <- hist(dane, breaks = 14)
plot(hist1$mids, hist1$density, pch = 19, col = "black", xlab = "X", ylab = "P(X)", font = 2)
x <- seq(min(hist1$mids), max(hist1$mids), 0.05)
points(x, dexp(x,2), type="l", col="red")
