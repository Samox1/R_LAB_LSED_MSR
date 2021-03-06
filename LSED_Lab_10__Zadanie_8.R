### LSED - Zadanie 8 - LAB nr 10
# Autor: Szymon Baczy�ski 270626

# Polecenie: 
# Wykona� analiz� PCA dla zbioru win. 
# Wykre�li� skumulowane odchylenie standardowe od liczby sk�adowych 
# oraz punkty w nowych zmiennych dla 1 i 2 oraz 2 i 3 sk�adowej. 

rm(list=ls())
library(MASS)

start_time <- Sys.time()

### --- Punkt 1 - Wczyta� dane --- ###
cat("\n"); print("--- Punkt nr 1 zadania ---"); 

ifelse(!file.exists("wine.dat"), write.table(wina <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=","), file="wine.dat", sep=","), wina <- read.table("wine.dat", sep=","))
print("Wczytano dane i utworzono plik 'wine.dat' (je�li taki nie istnia�)")


### --- Punkt 2 - Nazwa� kolumny --- ###
cat("\n"); print("--- Punkt nr 2 zadania ---"); 
colnames(wina) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines", "Proline")
print("Nazwano kolumny wed�ug pliku pomocniczego")


### --- Punkt 3 - Analiza PCA --- ###
cat("\n"); print("--- Punkt nr 3 zadania ---");
print("Analiza PCA"); cat("\n")

wina <- data.frame(wina)
# plot(wina[,2], wina[,3], pch=19, xlab="Test 1", ylab="Test 2")
# plot(wina[,3], wina[,4], pch=19, xlab="Test 2", ylab="Test 3")


# Wykonanie analizy sk�adowych glownych
wina.pc <- princomp(~., cor=T, data=wina[,-1])

# sum_sdev <- 0.0
# for (i in c(1:length(wina.pc$sdev))){
#   sum_sdev[i] <- sum(wina.pc$sdev[1:i])
# }
sum_test <- sapply(c(1:length(wina.pc$sdev)), function(v) sum(wina.pc$sdev[1:v]))
suma <- sum(wina.pc$sdev)

# Wykre�lenie wariancji zwiaz�nych ze sk�adowymi
plot(sum_test/suma, main="Znormalizowane Skumulowane Odchylenie Standardowe od Liczby Sk�adowych", xlab="Liczba sk�adowych", ylab="Znormalizowane Skumulowane Odchylenie standardowe",type="b", col="red" )
axis(side=1, at=c(1:length(sum_test)))

print("Odchylenie standardowe z PCA:")
print(round(wina.pc$sdev,6));  cat("\n")
print("Skumulowane odchylenie standardowe:")
cat(sum_test); cat("\n\n")

print("Narysowano wykresy dla sk�adowych 1 i 2 oraz 2 i 3")

# Wykres we wsp�rz�dnych sk�adowych g��wnych
plot(wina.pc$scores[,1:2], col = as.factor(wina[,1]), xlab="Sk�adowa 1", ylab="Sk�adowa 2")
title("Sk�adowe 1 i 2", cex.main=1.4) 

# Wykres we wsp�rz�dnych sk�adowych g��wnych
plot(wina.pc$scores[,2:3], col = as.factor(wina[,1]), xlab="Sk�adowa 2", ylab="Sk�adowa 3")
title("Sk�adowe 2 i 3", cex.main=1.4) 

cat("\n"); print("Czas wykonania programu: ")
end_time <- Sys.time()
print(end_time-start_time)