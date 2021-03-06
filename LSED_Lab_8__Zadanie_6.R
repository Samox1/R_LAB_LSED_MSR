### LSED - Zadanie 6 - LAB nr 8
# Autor: Szymon Baczy�ski 270626

# Polecenie:
# Sprawdzi� sprawno�� metody SVM (liniowej) dla danych wykorzystywanych w 
# cz�ci dotycz�cej braku separowalno�ci dla r�nych warto�ci C. 
# Por�wna� wyniki z metod� LDA. 

rm(list=ls())

library(e1071)
library(MASS)

CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  # Skuteczno�� klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  return(c(ACC = round(ACC,4), row.names = NULL))
}

# Generowanie
draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  X1$class <- 1; X2$class <- 2
  data <- rbind(X1, X2); data$class <- factor(data$class)
  return(data)
}

# Rysowanie punkt�w
plot.data <- function(data) {
  cols <- c("blue", "red")
  plot(data[,1:2], col = cols[data$class], cex = 2.5, pch = 19)
  text(data[,1:2], labels = 1:nrow(data), cex = 0.8, col = "white", font = 2)
}

# Parametry danych z rozk�adu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Ustawienie ziarna dla losowania danych
set.seed(128)

# Generowanie obserwacji
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

# Rysowanie danych
#plot.data(data)

# Wywo�anie metody SVM
data.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = 0.01, scale = F, kernel = "linear")

# Predykcja przy pomocy modelu SVM
pred <- predict(data.svm, data)

# Wy�wietlenie tablicy trafie� SVM
print(sprintf("SVM dla C=0.01: %.2f ",CM.large(pred, data$class)))

# LDA predykcja
data.lda <- lda(class ~ ., data)

# LDA Predykcja
pred.lda <- predict(data.lda, data)

# Wy�wietlenie tablicy trafie� LDA
print(sprintf("LDA ACC: %.2f ",CM.large(pred.lda$class, data$class)))
cat("\n")
print("Por�wnanie SVM i LDA, dla r�nego C:")

SVM.predict <- function(C, data){
  data.svm <- svm(class ~ x + y, type = "C-classification", data = data, cost = C, scale = F, kernel = "linear")
  pred <- predict(data.svm, data)
  return(CM.large(pred, data$class))
}

vals <- c(0.001, 0.002, 0.003, 0.004, 0.005, 0.01, 0.02, 0.05, 0.1, 0.5, 1, 10, 50, 100, 1000)
tab <- sapply(vals, function(v) SVM.predict(v, data))
tab <- rbind(tab, CM.large(pred.lda$class, data$class))
colnames(tab) <- vals
rownames(tab) <- c("SVM", "LDA")
print(tab)

# Zrobi�: Wykres Skuteczno�ci 
plot(vals,tab[1,], log="x", xlab = "C", ylab = "Accuracy",type="b",col="red", pch = 19, cex = 1)
title("Skuteczno�� SVM w zale�no�ci od warto�ci C")


