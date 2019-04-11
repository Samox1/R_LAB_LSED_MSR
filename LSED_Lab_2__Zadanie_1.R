### LSED - Zadanie 1 - Lab 2 

rm(list=ls())
library(MASS)
library(mvtnorm)
library(klaR)
library(e1071)

# Generowanie rozkladow

S <- matrix(c(4,0,0,4),2,2)
mt1 <- c(-3,-1)
mt2 <- c(2,2)

n1 <- 40
n2 <- 30

n <- n1 + n2

X1 <- data.frame(mvrnorm(n1, mt1, S))
X2 <- data.frame(mvrnorm(n2, mt2, S)) 

m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)

colnames(X1) <- c("x", "y")
colnames(X2) <- c("x", "y") 

# Wyznaczenie macierzy kowariancji
S1 <- cov(X1)
S2 <- cov(X2)

# Liczba elementów klas 1 i 2 oraz ca³kowita liczba
n1 <- nrow(X1)
n2 <- nrow(X2)

g <- 2

nazwy <- c("X1", "X2")
col=c("blue","red")

plot(X1, ylim = c(-10,10), xlim = c(-10,10), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "red")

# Przypisanie klas
X1$class <- 1; X2$class <- 2

# "Sklejenie" danych do jednej ramki
# oraz przekszta³cenie typu zmiennej przechowuj¹cej klasy
data <- rbind(X1, X2); data$class <- factor(data$class)


### Naiwny Bayes

# Klasyfikator naiwnego Bayesa
data.nb <- naiveBayes(class ~ x + y, data)

# Przewidywanie klas za pomoc¹ klasyfikatora naiwnego Bayesa
# domyœlnie zwraca klasy
data.nb.pred <- predict(data.nb, data)

# opcja method = "raw" daje prawdopodobieñstwa a posteriori
data.nb.pred <- predict(data.nb, data, type = "raw")

# Rysowanie prostej rozdzielaj¹cej, punktów etc
with(data, drawparti(class, x, y, method = "naiveBayes", ylim = c(-10,10), xlim = c(-10,10), xlab = "X", ylab = "Y", font = 2))

### ---
### Czesc reczna kodu = przypadek jednowymiarowy
### ---

f.bayes <- function(X1, pi1,pi2, m1, m2, sdx, sdy, sdx2, sdy2) {
  return((pi1 * dnorm(X1$x, m1[1], sdx) * dnorm(X1$y, m1[2], sdy)) / (pi1 * dnorm(X1$x, m1[1], sdx) * dnorm(X1$y, m1[2], sdy) + pi2 * dnorm(X1$x, m2[1], sdx2) * dnorm(X1$y, m2[2], sdy2)))
}

#Definiowanie zakresów wspó³przêdnych
xp <- seq(-10, 10, 0.1)
yp <- seq(-10, 10, 0.1)

# Rozpiêcie siatki na wspó³przêdnych
gr <- expand.grid(x = xp, y = yp)

pi1 <- n1 / (n1 + n2)
pi2 <- n2 / (n1 + n2)

sdx <- sd(X1$x)
sdy <- sd(X1$y)

sdx2 <- sd(X2$x)
sdy2 <- sd(X2$y)

# Porównanie prawdopodobieñstw a posteriori
#data.comp <- cbind(f.bayes(X1, X2, m1, m2, pi1, S1, S2), data.nb.pred$posterior[,1])

# Porównanie z wartoœciami otrzymanymi z rozk³adów
contour(xp, yp, matrix(f.bayes(gr,pi1,pi2,m1,m2,sdx,sdy,sdx2,sdy2), length(xp)), add = T, levels = 0.5, lwd = 2, lty = 2, col = "blue") 

# Wykreœlenie warstwic
#contour(xp, yp, X1.pdf, add = TRUE, col = "blue")
#contour(xp, yp, X2.pdf, add = TRUE, col = "orange")