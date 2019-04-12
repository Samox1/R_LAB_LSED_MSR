### LSED - Zadanie 4 - LAB nr 5
# Autor: Szymon Baczyñski

rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)

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
ACC.PP <- table(wina$class, predict(tree, wina, type = "class"))
print(c("Skutecznoœæ PP: ", CM.large(wina$class, predict(tree, wina, type = "class"))[1]))

# - Kroswalidacja - #

k <- 5        #kroswalidacja, podzielenie iloœci wierszy przez k 
k_rows <- nrow(losowanie) / k
k_rows <- as.integer(round(k_rows))

wina.class <- rpart(class ~ ., wina[-(1:k_rows),], minsplit = 0, cp = 0)     #Czêœæ K1 - od -> 1:k_rows
wina.predict <- predict(wina.class, losowanie[1:k_rows,])
wina.CM <- CM.large(losowanie[1:k_rows,]$class, walid.lda.val$class)

for(x in 1:(k-2)) 
{
  class.lda <- lda(class ~ ., losowanie[-((x*k_rows+1):(x*k_rows+k_rows)),])
  walid.lda.val <- predict(class.lda, losowanie[((x*k_rows+1):(x*k_rows+k_rows)),])
  LDA.val <- rbind(LDA.val, CM.large(losowanie[((x*k_rows+1):(x*k_rows+k_rows)),]$class, walid.lda.val$class))
}

class.lda <- lda(class ~ ., losowanie[-(((k-1)*k_rows+1):(nrow(losowanie))),])    #Czêœæ K5 - od -> (4*k_rows+1):(nrow(losowanie))
walid.lda.val <- predict(class.lda, losowanie[((k-1)*k_rows+1):(nrow(losowanie)),])
LDA.val <- rbind(LDA.val, CM.large(losowanie[(((k-1)*k_rows+1):(nrow(losowanie))),]$class, walid.lda.val$class))

ifelse(k==5,(rownames(LDA.val) <- c("LDA K1", "LDA K2", "LDA K3", "LDA K4", "LDA K5")), rownames(LDA.val) <- c(1:k))
res.old_LDA_6 <- LDA.val

cat("\n"); print("Uczenie na kroswalidacji - Test K: parametry LDA")
print(res.old_LDA_6)

kap = as.numeric(gsub("[a-zA-Z ]", "",rownames(LDA.val)[which.max(LDA.val[,1])]))
kroswalid_acc = sum(res.old_LDA_6[,5])/sum(res.old_LDA_6[,6])

cat("\n");
print("Prawid³owe predykcje / wszystkie dane - dla kroswalidacji: ")
print(kroswalid_acc)


