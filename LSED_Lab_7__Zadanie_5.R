### LSED - Zadanie 5 - LAB nr 7
# Autor: Szymon Baczyñski

# Polecenie:
# Nale¿y zmodyfikowac funkcjê bagging.own() z przyk³adu tak, aby zamiast drzew decyzyjnych wykorzystaæ klasyfikator LDA. 
# Nastêpnie wykorzystaæ pierwotn¹ i zmodyfikowan¹ funkcjê do zbioru iris (podzieliæ zbiór PU - 80%, PT - 20%) 
# i sprawdziæ liczbê b³êdnych klasyfikacji dla M=1,2,5,10,20,50 klasyfikatorów. 


rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)

Irys = datasets::iris
Iris = datasets::iris

colnames(Irys)[colnames(Irys)=="Species"] <- "class"

Irys$class <- ifelse((Irys$class=="setosa"), 1, ifelse(Irys$class=="versicolor", 2, 3))
Irys$class <- factor(Irys$class)


# Wyznaczanie blêdu klasyfikatora
err.rate <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  return(1 - sum(diag(CM)) / sum(CM))
}

# Funkcja do przeprowadzanie procedury bagging
bagging.own <- function(data, N) {
  # Dane: losowanie z oryginalnej próby
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  # tworzenie drzew
  trees <- lapply(1:N, function(i) rpart(class ~ ., data = data[dane[,i],], maxdepth = 1))
  tmp <- list(dane = dane)
  tmp$N <- N
  tmp$data <- data
  tmp$trees <- trees
  tmp1 <- bagging.own.pred(tmp, data)
  tmp$trees.class <- tmp1$trees.class
  tmp$votes <- tmp1$votes
  tmp$class <- tmp1$class
  tmp$err <- tmp1$err
  return(tmp)
}

# Funkcja do przeprowadzania przewidywania za pomoca baggingu
bagging.own.pred <- function(bag, data) {
  tmp <- list()
  trees.class <- sapply(1:bag$N, function(i) predict(bag$trees[[i]], newdata = data, type = "class"))
  votes <- t(sapply(1:nrow(trees.class), function(i) table(factor(trees.class[i,], levels = levels(data$class)))))
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  tmp$trees.class <- trees.class
  tmp$votes <- votes
  tmp$class <- class
  tmp$err <- err.rate(data$class, tmp$class)
  return(tmp)
} 

# ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- #
# Funkcja do przeprowadzanie procedury bagging LDA
LDAbagging.own <- function(data, N) {
  # Dane: losowanie z oryginalnej próby
  dane <- replicate(N, sample(seq(1:nrow(data)),nrow(data), rep = T))
  
  # tworzenie klasyfikatorów LDA
  LDA <- apply(1:nrow(data),2, function(i) lda(class ~ ., data=data[dane[i,],] ))
  tmp <- list(dane = dane)
  tmp$N <- N
  tmp$data <- data
  tmp$LDA <- LDA
  # tmp1 <- bagging.own.pred(tmp, data)
  # tmp$LDA.class <- tmp1$LDA.class
  # tmp$votes <- tmp1$votes
  # tmp$class <- tmp1$class
  # tmp$err <- tmp1$err
  return(LDA)
}

# Funkcja do przeprowadzania przewidywania za pomoca baggingu LDA
LDAbagging.own.pred <- function(bag, data) {
  tmp <- list()
  LDA.class <- sapply(1:bag$N, function(i) predict(bag$LDA[[i]], data))
  votes <- t(sapply(1:nrow(LDA.class), function(i) table(factor(LDA.class[i,], levels = levels(data$class)))))
  class <- factor(levels(data$class)[apply(votes, 1, which.max)], levels = levels(data$class))
  tmp$LDA.class <- LDA.class
  tmp$votes <- votes
  tmp$class <- class
  tmp$err <- err.rate(data$class, tmp$class)
  return(tmp)
} 

#losowanie <- wina[sample(nrow(wina)), 1:3]
data <- Irys[sample(nrow(Irys)),]
#Podzial PU=80% i PT=20%
bound_PU <- floor((nrow(data)/5))*4 
PU <- data[1:(bound_PU),]
PT <- data[-(1:(bound_PU)),]

# Liczba drzew
vals <- c(1, 2, 5, 10, 20, 50)

# Wywo³anie algorytmu bagging dla ró¿nej liczby drzew dla PU (10 realizacji)
tab <- sapply(vals, function(v) replicate(10, bagging.own(PU, v)$err))
tabLDA <- sapply(vals, function(v) replicate(10, LDAbagging.own(PU, v)$err))

# Wywo³anie algorytmu bagging dla ró¿nej liczby drzew dla PT (10 realizacji)
tab.new <- sapply(vals, function(v) replicate(10, bagging.own.pred(bagging.own(PU, v), PT)$err))
#tab.newLDA <- sapply(vals, function(v) replicate(10, LDAbagging.own.pred(LDAbagging.own(PU, v), PT)$err))


