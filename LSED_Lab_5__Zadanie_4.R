### LSED - Zadanie 4 - LAB nr 5
# Autor: Szymon Baczyñski

rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)

### --- Punkt 1 - Wczytaæ dane --- ###
cat("\n"); print("--- Punkt nr 1 zadania ---"); 

ifelse(!file.exists("wine.dat"), write.table(wina <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=","), file="wine.dat", sep=","), wina <- read.table("wine.dat", sep=","))
print("Wczytano dane i utworzono plik 'wine.dat' (jeœli taki nie istnia³)")

### --- Punkt 2 - Nazwaæ kolumny --- ###
cat("\n"); print("--- Punkt nr 2 zadania ---"); 
colnames(wina) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines", "Proline")
print("Nazwano kolumny wed³ug pliku pomocniczego")

### --- Punkt 3 - Stworzyæ pe³ne drzewo --- ###
cat("\n"); print("--- Punkt nr 3 zadania ---")
wina$class <- factor(wina$class)
tree <- rpart(class ~ ., wina, minsplit = 0, cp = 0)
print("Stworzono drzewo")

### --- Punkt 4 - Narysowaæ pe³ne drzewo --- ###
cat("\n"); print("--- Punkt nr 4 zadania ---")
# Rysowanie drzewa
rpart.plot(tree, type = 1, extra = 1)
print("Narysowano drzewo")

### --- Punkt 5 - Spr. skutecznoœæ pe³nego drzewa przez PP i CV --- ###
cat("\n"); print("--- Punkt nr 5 zadania ---")
print("Skutecznoœci pe³nego drzewa ")
CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  # Skutecznoœæ klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  TP1 <- CM[1,1]
  TP2 <- CM[2,2]
  TP3 <- CM[3,3]
  gsums <- sum(diag(CM))
  sums <- apply(CM, 1, sum)
  return(c(ACC = round(ACC,4), TP1 = TP1, TP2 = TP2, TP3 = TP3, GSUM = gsums, ALL = sum(CM), row.names = NULL))
}

ACC.PP <- table(wina$class, predict(tree, wina, type = "class"))
print("Skutecznoœæ PP:"); print(CM.large(wina$class, predict(tree, wina, type = "class")))
