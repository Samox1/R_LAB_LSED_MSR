### LSED Lab 3 - Zadanie 2
# Autor: Szymon Baczyñski

rm(list=ls())
library(MASS)
library(klaR)
library(e1071)
library(Hmisc)

### --- Punkt 1 - wczytaæ dane --- ###
print(" "); print("--- Punkt nr 1 zadania ---"); 

ifelse(!file.exists("wine.dat"), write.table(wina <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=","), file="wine.dat", sep=","), wina <- read.table("wine.dat", sep=","))
print("Wczytano dane i utworzono plik 'wine.dat' (jeœli taki nie istnia³)")

### --- Punkt 2 - nazwaæ kolumny --- ###
print(" "); print("--- Punkt nr 2 zadania ---"); 
colnames(wina) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines", "Proline")
print("Nazwano kolumny wed³ug pliku pomocniczego")

### --- Punkt 3 - parametry klasyfikatorów LDA, QDA i NB na pe³nym zbiorze --- ###
print(" "); print("--- Punkt nr 3 zadania ---"); 

wina$class <- factor(wina$class)

# Trenowanie klasyfikatorów na PU
class.lda <- lda(class ~ ., wina)
class.qda <- qda(class ~ ., wina)
class.nb <- naiveBayes(class ~ ., wina)

### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ###
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

CM.values <- function(org.class, pred.posterior, threshold) {
  
  pred.class <- factor(ifelse(pred.posterior >= threshold, 2, 1))
  TP <- sum(pred.class == 2 & org.class == 2)
  TN <- sum(pred.class == 1 & org.class == 1)
  sum.neg <- sum(org.class == 1)
  sum.pos <- sum(org.class == 2)
  TPR <- TP / sum.pos
  FPR <- 1 - TN / sum.neg
  return(c(FPR,TPR))
} 

get.roc.values <- function(class, posterior) {
  progi <- c(-Inf, sort(unique(posterior)), +Inf)
  z <- t(sapply(1:length(progi), function(i) CM.values(class, posterior, progi[i])))
  return(z)
} 
### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ###

# Powtórne podstawienie
wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

# G³ówne wartoœci z macierzy pomy³ek dla powtórnego podstawienia
res.old <- CM.large(wina$class, wina.lda.old$class)
res.old <- rbind(res.old, CM.large(wina$class, wina.qda.old$class))
res.old <- rbind(res.old, CM.large(wina$class, wina.nb.old))
rownames(res.old) <- c("LDA ++", "QDA ++", "NB  ++") 
res.oldall <- res.old

print(" "); print("Uczenie na wszystkich kolumnach")
print(res.old)


### --- Punkt 4 - ograniczyc siê do 2 pierwszych, 5 pierwszych i 10 pierwszych sk³adowych i sprawdziæ skutecznoœci klasyfikatorów --- ###
print(" "); print("--- Punkt nr 4 zadania ---");
# Na 2 pierwszych kolumnach

# Trenowanie klasyfikatorów na PU
class.lda <- lda(class ~ ., wina[,1:3])
class.qda <- qda(class ~ ., wina[,1:3])
class.nb <- naiveBayes(class ~ ., wina[,1:3])

# Powtórne podstawienie
wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

# G³ówne wartoœci z macierzy pomy³ek dla powtórnego podstawienia
res.old <- CM.large(wina$class, wina.lda.old$class)
res.old <- rbind(res.old, CM.large(wina$class, wina.qda.old$class))
res.old <- rbind(res.old, CM.large(wina$class, wina.nb.old))
rownames(res.old) <- c("LDA 2", "QDA 2", "NB  2") 
res.old2 <- res.old

#print(" "); print("Uczenie na 2 pierwszych kolumnach")
#print(res.old)

# Na 5 pierwszych kolumnach
class.lda <- lda(class ~ ., wina[,1:6])
class.qda <- qda(class ~ ., wina[,1:6])
class.nb <- naiveBayes(class ~ ., wina[,1:6])

wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

res.old <- CM.large(wina$class, wina.lda.old$class)
res.old <- rbind(res.old, CM.large(wina$class, wina.qda.old$class))
res.old <- rbind(res.old, CM.large(wina$class, wina.nb.old))
rownames(res.old) <- c("LDA 5", "QDA 5", "NB  5") 
res.old5 <- res.old

#print(" "); print("Uczenie na 5 pierwszych kolumnach")
#print(res.old)

# Na 10 pierwszych kolumnach
class.lda <- lda(class ~ ., wina[,1:11])
class.qda <- qda(class ~ ., wina[,1:11])
class.nb <- naiveBayes(class ~ ., wina[,1:11])

wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

res.old <- CM.large(wina$class, wina.lda.old$class)
res.old <- rbind(res.old, CM.large(wina$class, wina.qda.old$class))
res.old <- rbind(res.old, CM.large(wina$class, wina.nb.old))
rownames(res.old) <- c("LDA 10", "QDA 10", "NB  10") 
res.old10 <- res.old

#print(" "); print("Uczenie na 10 pierwszych kolumnach")
#print(res.old)

res.all <- rbind(res.old2, res.old5, res.old10, res.oldall)
print(" "); print("Kumulacja danych")
print(res.all)


### --- Punkt 5 - ograniczyæ siê do 2 pierwszych zmiennych, podzieliæ zbiór (PU/PW/PT) 50/25/25 i w ten sposób dokonaæ wyboru spoœród LDA, QDA, NB --- ###
print(" "); print("--- Punkt nr 5 zadania ---");
#library(dplyr)

bound_train <- floor((nrow(wina)/2))         
bound_walid <- floor((nrow(wina)/4))
bound_test <- floor((nrow(wina)/4))

losowanie <- wina[sample(nrow(wina)), 1:3] 

### Metoda 1 - zbieranie poszczególnych danych z tablicy losowania
train <- losowanie[1:bound_train,]
walid <- losowanie[(bound_train+1):(bound_train+bound_walid),]
test <- losowanie[(bound_train+bound_walid+1):nrow(losowanie),]

### Metoda 2 - usuwanie losowania co próbkê (nauka usuwania wierszy w ró¿ny sposób)
#train <- losowanie[1:bound_train,] 
##losowanie <- losowanie[-as.numeric(rownames(train)),]
#losowanie <- anti_join(losowanie, train)
#walid <- losowanie[1:bound_walid,]
#losowanie <- losowanie[-as.numeric(rownames(walid)),]
#test <- losowanie[1:bound_test,]

### Uczenie na tablicy TRAIN i macierz pomy³ki dla powtórnego podstawienia.
# Trenowanie klasyfikatorów na PU
class.lda <- lda(class ~ ., train)
class.qda <- qda(class ~ ., train)
class.nb <- naiveBayes(class ~ ., train)

# Powtórne podstawienie
train.lda.old <- predict(class.lda, train)
train.qda.old <- predict(class.qda, train)
train.nb.old <- predict(class.nb, train)
train.nb.old.p <- predict(class.nb, walid, type = "raw")

# G³ówne wartoœci z macierzy pomy³ek dla powtórnego podstawienia
res.old <- CM.large(train$class, train.lda.old$class)
res.old <- rbind(res.old, CM.large(train$class, train.qda.old$class))
res.old <- rbind(res.old, CM.large(train$class, train.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB") 
res.old_train <- res.old

#print(" "); print("Uczenie na TRAIN i parametry klasyfikatorów po ponownym podstawieniu")
#print(res.old_train)

### Sprawdzenie nauczonych klasyfikatorów na próbie walidacyjnej
# Predykcja na zbiorze walidacyjnym
walid.lda.val <- predict(class.lda, walid)
walid.qda.val <- predict(class.qda, walid)
walid.nb.val <- predict(class.nb, walid)
walid.nb.val.p <- predict(class.nb, walid, type = "raw")

res.val <- CM.large(walid$class, walid.lda.val$class)
res.val <- rbind(res.val, CM.large(walid$class, walid.qda.val$class))
res.val <- rbind(res.val, CM.large(walid$class, walid.nb.val))
rownames(res.val) <- c("LDA", "QDA", "NB")
res.old_walid <- res.val

print(" "); print("Uczenie na TRAIN i sprawdzenie na WALID: parametry klasyfikatorów")
print(res.old_walid)

### Sprawdzenie nauczonych klasyfikatorów na próbie testowej
# Predykcja na zbiorze testowym
wygrana <- rownames(res.val)[which.max(res.val[,1])]
print(" "); print("Wygra³o tym razem: ")
print(wygrana)


if(wygrana == "LDA"){
  test.lda.test <- predict(class.lda, test)
  res.test <- rbind(CM.large(test$class, test.lda.test$class))
}
if(wygrana == "QDA"){
  test.qda.test <- predict(class.qda, test)
  res.test <- rbind(CM.large(test$class, test.qda.test$class))
}
if(wygrana == "NB"){
  test.nb.test <- predict(class.nb, test)
  test.nb.test.p <- predict(class.nb, test, type = "raw")
  res.test <- rbind(CM.large(test$class, test.nb.test))
}

res.old_test <- res.test
rownames(res.old_test) <- c(wygrana)

print(" "); print("Uczenie na PU (TRAIN) i sprawdzenie na PT (próbce TEST): ")
#print(wygrana)
#print(" ");
print(res.old_test)


### ---
### --- Punkt 6 - ograniczyæ siê do 2 pierwszych zmiennych, wykonaæ kroswalidacjê w przypadku LDA, porównac z poprzednim punktem oraz powtórnym podstawieniem. 
### ---
print(" "); print("--- Punkt nr 6 zadania ---");

# ramka "losowanie" ma ju¿ wymieszane rzeczy
# teraz kroswalidacja dla LDA
# porównaæ z punktem 5: walidacj¹ i ponownym podstawieniem (te¿ punkt 5)

k <- 5        #kroswalidacja, podzielenie iloœci wierszy przez k 
k_rows <- nrow(losowanie) / k
k_rows <- as.integer(k_rows)

## Ogólny wzór na pêtle kroswalidacji:
#class.lda <- lda(class ~ ., losowanie[-((x*k_rows+1):(x*k_rows+k_rows)),])
#walid.lda.val <- predict(class.lda, losowanie[((x*k_rows+1):(x*k_rows+k_rows)),])
#LDA.val <- rbind(LDA.val, CM.large(losowanie[((x*k_rows+1):(x*k_rows+k_rows)),]$class, walid.lda.val$class))


class.lda <- lda(class ~ ., losowanie[-(1:k_rows),])                   #Czêœæ K1 - od -> 1:k_rows
walid.lda.val <- predict(class.lda, losowanie[1:k_rows,])
LDA.val <- CM.large(losowanie[1:k_rows,]$class, walid.lda.val$class)

for(x in 1:(k-2)) 
{
  class.lda <- lda(class ~ ., losowanie[-((x*k_rows+1):(x*k_rows+k_rows)),])
  walid.lda.val <- predict(class.lda, losowanie[((x*k_rows+1):(x*k_rows+k_rows)),])
  LDA.val <- rbind(LDA.val, CM.large(losowanie[((x*k_rows+1):(x*k_rows+k_rows)),]$class, walid.lda.val$class))
}

class.lda <- lda(class ~ ., losowanie[-(((k-1)*k_rows+1):(nrow(losowanie))),])    #Czêœæ K5 - od -> (4*k_rows+1):(nrow(losowanie))
walid.lda.val <- predict(class.lda, losowanie[((k-1)*k_rows+1):(nrow(losowanie)),])
LDA.val <- rbind(LDA.val, CM.large(losowanie[(((k-1)*k_rows+1):(nrow(losowanie))),]$class, walid.lda.val$class))

rownames(LDA.val) <- c("LDA K1", "LDA K2", "LDA K3", "LDA K4", "LDA K5")
res.old_LDA_6 <- LDA.val

print(" "); print("Uczenie na kroswalidacji - Test K1: parametry LDA")
print(res.old_LDA_6)

kap = as.numeric(gsub("[a-zA-Z ]", "",rownames(LDA.val)[which.max(LDA.val[,1])]))
kroswalid_acc = sum(res.old_LDA_6[,5])/sum(res.old_LDA_6[,6])

print(" ");
print("Prawid³owe predykcje / wszystkie dane - dla kroswalidacji: ")
print(kroswalid_acc)

LDAvs <- rbind(c(round(res.old_train[1,1],4), round(res.old_walid[1,1],4), round(kroswalid_acc,4)))
colnames(LDAvs) <- c("PP", "WALID", "CV")
rownames(LDAvs) <- c("ACC")

print(" "); print(LDAvs)
