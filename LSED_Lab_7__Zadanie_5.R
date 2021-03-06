### LSED - Zadanie 5 - LAB nr 7
# Autor: Szymon Baczy�ski 270626

# Polecenie:
# Nale�y zmodyfikowac funkcj� bagging.own() z przyk�adu tak, aby zamiast drzew decyzyjnych wykorzysta� klasyfikator LDA. 
# Nast�pnie wykorzysta� pierwotn� i zmodyfikowan� funkcj� do zbioru iris (podzieli� zbi�r PU - 80%, PT - 20%) 
# i sprawdzi� liczb� b��dnych klasyfikacji dla M=1,2,5,10,20,50 klasyfikator�w. 


rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)

Irys = datasets::iris

# colnames(Irys)[colnames(Irys)=="Species"] <- "Species"
# Irys$Species <- ifelse((Irys$Species=="setosa"), 1, ifelse(Irys$Species=="versicolor", 2, 3))
Irys$Species <- factor(Irys$Species)


# Wyznaczanie bl�du klasyfikatora
err.rate <- function(org.Species, pred.Species) {
  CM <- table(org.Species, pred.Species)
  return(1 - sum(diag(CM)) / sum(CM))
}

# Funkcja do przeprowadzanie procedury bagging
bagging.own <- function(data, N) {
  # Dane: losowanie z oryginalnej pr�by
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  # tworzenie drzew
  trees <- lapply(1:N, function(i) rpart(Species ~ ., data = data[dane[,i],], maxdepth = 1))
  tmp <- list(dane = dane)
  tmp$N <- N
  tmp$data <- data
  tmp$trees <- trees
  tmp1 <- bagging.own.pred(tmp, data)
  tmp$trees.Species <- tmp1$trees.Species
  tmp$votes <- tmp1$votes
  tmp$Species <- tmp1$Species
  tmp$err <- tmp1$err
  return(tmp)
}

# Funkcja do przeprowadzania przewidywania za pomoca baggingu
bagging.own.pred <- function(bag, data) {
  tmp <- list()
  trees.Species <- sapply(1:bag$N, function(i) predict(bag$trees[[i]], newdata = data, type = "class"))
  votes <- t(sapply(1:nrow(trees.Species), function(i) table(factor(trees.Species[i,], levels = levels(data$Species)))))
  Species <- factor(levels(data$Species)[apply(votes, 1, which.max)], levels = levels(data$Species))
  tmp$trees.Species <- trees.Species
  tmp$votes <- votes
  tmp$Species <- Species
  tmp$err <- err.rate(data$Species, tmp$Species)
  return(tmp)
}

# ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- LDA ----- #
# Funkcja do przeprowadzanie procedury bagging LDA
LDAbagging.own <- function(data, N) {
  # Dane: losowanie z oryginalnej pr�by
  dane <- replicate(N, sample(1:nrow(data), rep = T))
  # tworzenie drzew
  LDA <- lapply(1:N, function(i) lda(Species ~ ., data = data[dane[,i],]))
  tmp <- list(dane = dane)
  tmp$N <- N
  tmp$data <- data
  tmp$LDA <- LDA
  tmp1 <- LDAbagging.own.pred(tmp, data)
  tmp$LDA.Species <- tmp1$LDA.Species
  tmp$votes <- tmp1$votes
  tmp$Species <- tmp1$Species
  tmp$err <- tmp1$err
  return(tmp)
}

# Funkcja do przeprowadzania przewidywania za pomoca baggingu LDA
LDAbagging.own.pred <- function(bag, data) {
  tmp <- list()
  LDA.Species <- sapply(1:bag$N, function(i) predict(bag$LDA[[i]], data)$class)
  votes <- t(sapply(1:nrow(LDA.Species), function(i) table(factor(LDA.Species[i,], levels = levels(data$Species)))))
  Species <- factor(levels(data$Species)[apply(votes,1, which.max)], levels = levels(data$Species))
  tmp$LDA.Species <- LDA.Species
  tmp$votes <- votes
  tmp$Species <- Species
  tmp$err <- err.rate(data$Species, tmp$Species)
  return(tmp)
}

### ----- Functions END ----- Functions END ----- Functions END ----- Functions END ----- Functions END ----- ###


data <- Irys[sample(nrow(Irys)),]

#Podzial PU=80% i PT=20%
bound_PU <- floor((nrow(data)/5))*4 
PU <- data[1:(bound_PU),]
PT <- data[-(1:(bound_PU)),]

# Liczba drzew/klasyfikator�w
vals <- c(1, 2, 5, 10, 20, 50)

# Wywo�anie algorytmu bagging dla r�nej liczby drzew/LDA dla PU (10 realizacji)
tab <- sapply(vals, function(v) replicate(10, bagging.own(PU, v)$err))
tabLDA <- sapply(vals, function(v) replicate(10, LDAbagging.own(PU, v)$err))

# Wywo�anie algorytmu bagging dla r�nej liczby drzew/LDA dla PT (10 realizacji)
tab.new <- sapply(vals, function(v) replicate(10, bagging.own.pred(bagging.own(PU, v), PT)$err))
tab.newLDA <- sapply(vals, function(v) replicate(10, LDAbagging.own.pred(LDAbagging.own(PU, v), PT)$err))


cat("\n")
print("B��dne klasyfikacje (�rednie): ")
ALL <- colMeans(tab)
ALL <- rbind(ALL,colMeans(tab.new))
ALL <- rbind(ALL,colMeans(tabLDA))
ALL <- rbind(ALL,colMeans(tab.newLDA))

colnames(ALL) <- vals
rownames(ALL) <- c("TREE PU|", "TREE PT|", "LDA  PU|", "LDA  PT|")
print(ALL)

