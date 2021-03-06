### LSED Lab 3 - Zadanie 2
# Autor: Szymon Baczy�ski 270626

rm(list=ls())
library(MASS)
library(klaR)
library(e1071)
library(Hmisc)

### --- Punkt 1 - wczyta� dane --- ###
cat("\n"); print("--- Punkt nr 1 zadania ---"); 

ifelse(!file.exists("wine.dat"), write.table(wina <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=","), file="wine.dat", sep=","), wina <- read.table("wine.dat", sep=","))
print("Wczytano dane i utworzono plik 'wine.dat' (je�li taki nie istnia�)")

### --- Punkt 2 - nazwa� kolumny --- ###
cat("\n"); print("--- Punkt nr 2 zadania ---"); 
colnames(wina) <- c("class","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines", "Proline")
print("Nazwano kolumny wed�ug pliku pomocniczego")

### --- Punkt 3 - parametry klasyfikator�w LDA, QDA i NB na pe�nym zbiorze --- ###
cat("\n"); print("--- Punkt nr 3 zadania ---"); 

wina$class <- factor(wina$class)

# Trenowanie klasyfikator�w na PU
class.lda <- lda(class ~ ., wina)
class.qda <- qda(class ~ ., wina)
class.nb <- naiveBayes(class ~ ., wina)

### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ###
CM.large <- function(org.class, pred.class) {

  CM <- table(org.class, pred.class)
  
  # Skuteczno�� klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  TP1 <- CM[1,1]
  TP2 <- CM[2,2]
  TP3 <- CM[3,3]
  
  gsums <- sum(diag(CM))
  sums <- apply(CM, 1, sum)
  
  return(c(ACC = round(ACC,4), TP1 = TP1, TP2 = TP2, TP3 = TP3, GSUM = gsums, ALL = sum(CM), row.names = NULL))
}

### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ### ### --- Funkcje ---- ###

# Powt�rne podstawienie
wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(wina$class, wina.lda.old$class)
res.old <- rbind(res.old, CM.large(wina$class, wina.qda.old$class))
res.old <- rbind(res.old, CM.large(wina$class, wina.nb.old))
rownames(res.old) <- c("LDA ++", "QDA ++", "NB  ++") 
res.oldall <- res.old

cat("\n"); print("Uczenie na wszystkich kolumnach")
print(res.old)


### --- Punkt 4 - ograniczyc si� do 2 pierwszych, 5 pierwszych i 10 pierwszych sk�adowych i sprawdzi� skuteczno�ci klasyfikator�w --- ###
cat("\n"); print("--- Punkt nr 4 zadania ---");
# Na 2 pierwszych kolumnach

# Trenowanie klasyfikator�w na PU
class.lda <- lda(class ~ ., wina[,1:3])
class.qda <- qda(class ~ ., wina[,1:3])
class.nb <- naiveBayes(class ~ ., wina[,1:3])

# Powt�rne podstawienie
wina.lda.old <- predict(class.lda, wina)
wina.qda.old <- predict(class.qda, wina)
wina.nb.old <- predict(class.nb, wina)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
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
cat("\n"); print("Kumulacja danych")
print(res.all)


### --- Punkt 5 - ograniczy� si� do 2 pierwszych zmiennych, podzieli� zbi�r (PU/PW/PT) 50/25/25 i w ten spos�b dokona� wyboru spo�r�d LDA, QDA, NB --- ###
cat("\n"); print("--- Punkt nr 5 zadania ---");

bound_train <- floor((nrow(wina)/2))         
bound_walid <- floor((nrow(wina)/4))
bound_test <- floor((nrow(wina)/4))

losowanie <- wina[sample(nrow(wina)), 1:3] 

### Metoda 1 - zbieranie poszczeg�lnych danych z tablicy losowania
train <- losowanie[1:bound_train,]
walid <- losowanie[(bound_train+1):(bound_train+bound_walid),]
test <- losowanie[(bound_train+bound_walid+1):nrow(losowanie),]

### Uczenie na tablicy TRAIN i macierz pomy�ki dla powt�rnego podstawienia.
# Trenowanie klasyfikator�w na PU
class.lda <- lda(class ~ ., train)
class.qda <- qda(class ~ ., train)
class.nb <- naiveBayes(class ~ ., train)

# Powt�rne podstawienie
train.lda.old <- predict(class.lda, train)
train.qda.old <- predict(class.qda, train)
train.nb.old <- predict(class.nb, train)

# G��wne warto�ci z macierzy pomy�ek dla powt�rnego podstawienia
res.old <- CM.large(train$class, train.lda.old$class)
res.old <- rbind(res.old, CM.large(train$class, train.qda.old$class))
res.old <- rbind(res.old, CM.large(train$class, train.nb.old))
rownames(res.old) <- c("LDA", "QDA", "NB") 
res.old_train <- res.old

#print(" "); print("Uczenie na TRAIN i parametry klasyfikator�w po ponownym podstawieniu")
#print(res.old_train)

### Sprawdzenie nauczonych klasyfikator�w na pr�bie walidacyjnej
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

cat("\n"); print("Uczenie na TRAIN i sprawdzenie na WALID: parametry klasyfikator�w")
print(res.old_walid)

### Sprawdzenie nauczonych klasyfikator�w na pr�bie testowej
# Predykcja na zbiorze testowym
wygrana <- rownames(res.val)[which.max(res.val[,1])]
cat("\n"); print("Wygra�o tym razem: ")
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

cat("\n"); print("Uczenie na PU (TRAIN) i sprawdzenie na PT (pr�bce TEST): ")
#print(wygrana)
#print(" ");
print(res.old_test)


### ---
### --- Punkt 6 - ograniczy� si� do 2 pierwszych zmiennych, wykona� kroswalidacj� w przypadku LDA, por�wnac z poprzednim punktem oraz powt�rnym podstawieniem. 
### ---
cat("\n"); print("--- Punkt nr 6 zadania ---");

# ramka "losowanie" ma ju� wymieszane rzeczy
# teraz kroswalidacja dla LDA
# por�wna� z punktem 5: walidacj� i ponownym podstawieniem (te� punkt 5)

k <- 5        #kroswalidacja, podzielenie ilo�ci wierszy przez k 
k_rows <- nrow(losowanie) / k
k_rows <- as.integer(round(k_rows))

## Og�lny wz�r na p�tle kroswalidacji:
#class.lda <- lda(class ~ ., losowanie[-((x*k_rows+1):(x*k_rows+k_rows)),])
#walid.lda.val <- predict(class.lda, losowanie[((x*k_rows+1):(x*k_rows+k_rows)),])
#LDA.val <- rbind(LDA.val, CM.large(losowanie[((x*k_rows+1):(x*k_rows+k_rows)),]$class, walid.lda.val$class))


class.lda <- lda(class ~ ., losowanie[-(1:k_rows),])                   #Cz�� K1 - od -> 1:k_rows
walid.lda.val <- predict(class.lda, losowanie[1:k_rows,])
LDA.val <- CM.large(losowanie[1:k_rows,]$class, walid.lda.val$class)

for(x in 1:(k-2)) 
{
  class.lda <- lda(class ~ ., losowanie[-((x*k_rows+1):(x*k_rows+k_rows)),])
  walid.lda.val <- predict(class.lda, losowanie[((x*k_rows+1):(x*k_rows+k_rows)),])
  LDA.val <- rbind(LDA.val, CM.large(losowanie[((x*k_rows+1):(x*k_rows+k_rows)),]$class, walid.lda.val$class))
}

class.lda <- lda(class ~ ., losowanie[-(((k-1)*k_rows+1):(nrow(losowanie))),])    #Cz�� K5 - od -> (4*k_rows+1):(nrow(losowanie))
walid.lda.val <- predict(class.lda, losowanie[((k-1)*k_rows+1):(nrow(losowanie)),])
LDA.val <- rbind(LDA.val, CM.large(losowanie[(((k-1)*k_rows+1):(nrow(losowanie))),]$class, walid.lda.val$class))

ifelse(k==5,(rownames(LDA.val) <- c("LDA K1", "LDA K2", "LDA K3", "LDA K4", "LDA K5")), rownames(LDA.val) <- c(1:k))
res.old_LDA_6 <- LDA.val

cat("\n"); print("Uczenie na kroswalidacji - Test K: parametry LDA")
print(res.old_LDA_6)

kap = as.numeric(gsub("[a-zA-Z ]", "",rownames(LDA.val)[which.max(LDA.val[,1])]))
kroswalid_acc = sum(res.old_LDA_6[,5])/sum(res.old_LDA_6[,6])

cat("\n");
print("Prawid�owe predykcje / wszystkie dane - dla kroswalidacji: ")
print(kroswalid_acc)
print("Najlepsza predykcja - dla kroswalidacji: ")
print(res.old_LDA_6[kap,1])

LDAvs <- rbind(c(round(res.old_train[1,1],4), round(res.old_walid[1,1],4), round(kroswalid_acc,4)))
colnames(LDAvs) <- c("PP", "WALID", "CV")
rownames(LDAvs) <- c("ACC")

cat("\n"); print(LDAvs)
