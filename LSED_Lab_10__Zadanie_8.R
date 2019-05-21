### LSED - Zadanie 8 - LAB nr 10
# Autor: Szymon Baczyñski 270626

# Polecenie: 
# Wykonaæ analizê PCA dla zbioru win. 
# Wykreœliæ skumulowane odchylenie standardowe od liczby sk³adowych 
# oraz punkty w nowych zmiennych dla 1 i 2 oraz 2 i 3 sk³adowej. 

rm(list=ls())
library(MASS)


### --- Punkt 1 - Wczytaæ dane --- ###
cat("\n"); print("--- Punkt nr 1 zadania ---"); 

ifelse(!file.exists("wine.dat"), write.table(wina <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=","), file="wine.dat", sep=","), wina <- read.table("wine.dat", sep=","))
print("Wczytano dane i utworzono plik 'wine.dat' (jeœli taki nie istnia³)")


### --- Punkt 2 - Nazwaæ kolumny --- ###
cat("\n"); print("--- Punkt nr 2 zadania ---"); 
colnames(wina) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines", "Proline")
print("Nazwano kolumny wed³ug pliku pomocniczego")


### --- Punkt 3 - Analiza PCA --- ###
cat("\n"); print("--- Punkt nr 3 zadania ---");
print("Analiza PCA")
wina <- data.frame(wina)
# Macierz kowariancji
S <- cov(wina)

# Macierz korelacji
Sc <- cor(wina)

# Wyznaczenie wartoœci i wektorów w³asnych
eS <- eigen(S)
eSc <- eigen(Sc)

# print(eS)
# print(eSc)

# Wykonanie analizy sk³adowych glownych
wina.pc <- princomp(~., cor=T, data=wina[,-1])
kappa <- wina.pc$sdev
sum_sdev <- 0.0
# suma <- sapply(wina.pc$sdev, function(v) sum(v[1:v,]))

for (i in c(1:length(wina.pc$sdev))){
  sum_sdev[i] <- sum(wina.pc$sdev[1:i])
}


# Wykreœlenie wariancji zwiaz¹nych ze sk³adowymi
plot(sum_sdev, main="Skumulowane Odchylenie Standardowe", xlab="Liczba sk³adowych", ylab="Skumulowane Odchylenie standardowe",type="b", col="red" )
axis(side=1, at=c(1:length(sum_sdev)))

print("Odchylenie standardowe z PCA:")
print(wina.pc$sdev)

# Wykres we wspó³rzêdnych sk³adowych g³ównych
plot(wina.pc$scores, xlim=c(-2,2), ylim=c(-2,2), xlab="Sk³adowa 1", ylab="Sk³adowa 2")
title("Sk³adowe", cex.main=1.4) 
