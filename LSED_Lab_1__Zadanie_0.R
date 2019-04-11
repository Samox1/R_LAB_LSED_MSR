# Zadanie 0 - Lab nr 1

rm(list=ls())
library(MASS)

S <- matrix(c(1,0,0,1),2,2)
mt1 <- c(-1,1)
mt2 <- c(2,4)
mt3 <- c(-2,2)

n1 <- 30
n2 <- 30
n3 <- 30
n <- n1 + n2 + n3
n

X1 <- data.frame(mvrnorm(n1, mt1, S))
X2 <- data.frame(mvrnorm(n2, mt2, S)) 
X3 <- data.frame(mvrnorm(n3, mt3, S))

m1 <- apply(X1, 2, mean)
m2 <- apply(X2, 2, mean)
m3 <- apply(X3, 2, mean)

# Wyznaczenie macierzy kowariancji
S1 <- cov(X1)
S2 <- cov(X2)
S3 <- cov(X3)

# Liczba elementów klas 1 i 2 oraz ca³kowita liczba
n1 <- nrow(X1)
n2 <- nrow(X2)
n3 <- nrow(X3)

n <- n1 + n2 + n3

g <- 3 

nazwy <- c("X1", "X2", "X3")
col=c("blue","red","green")

#png("Zadanie-0-Lab1-SB.png")

plot(X1, ylim = c(-2,6), xlim = c(-4,5), pch = 19, col = "blue", xlab = "X", ylab = "Y", font = 2, asp = 1)
abline(v = 0, h = 0, col = "gray")
points(X2, pch = 19, col = "red")
points(X3, pch = 19, col = "green")

legend("topleft", legend=c("X 1", "X 2", "X 3","Zrzutowane X 1","Zrzutowane X 2","Zrzutowane X 3"), col=c("blue","red", "green","lightblue","darkorange","darkgreen"), lty=1:2, cex=0.8)

# Œrednia dla wszystkich punktów
m <- apply(rbind(X1, X2, X3), 2, mean)

# Macierz zmiennoœci miêdzygrupowej
B <- (n1*(m1 - m) %*% t(m1 - m) + n2*(m2 - m) %*% t(m2 - m) + n3*(m3 - m) %*% t(m3 - m))/(g-1)

# Macierz zmiennoœci wewn¹trzgrupowej
W <- 1/(n - g)*((n1 - 1) * S1 + (n2 - 1) * S2 + (n3 - 1) * S3) 

# Macierz pomocnicza
U <- ginv(W) %*% B

# Wyznaczenia wartosci i wektorow w³asnych
lambda <- eigen(U)

# Wektor w³asny odpowiadaj¹cy maksymalnej wartoœci w³asnej
a <- lambda$vectors[,which.max(lambda$values)]

# Rysowanie kierunku a
abline(0, a[2] / a[1], col = "violet", lwd = 2)

# Wyznaczenie wspó³czynnika kierunkowego prostej
A <- a[2] / a[1]

# Funkcja do rzutowania obserwacji na kierunek a
rzutowanie <- function(X, A) {
  Xz <- (X$X2 * A + X$X1) / (A**2 + 1)
  Yz <- A * Xz
  data.frame(x = Xz, y = Yz)
}

# Wykreœlanie zrzutowanych punktów
points(rzutowanie(X1, A), col = "lightblue", pch = 19, cex = 0.7)
points(rzutowanie(X2, A), col = "darkorange", pch = 19, cex = 0.7)
points(rzutowanie(X3, A), col = "darkgreen", pch = 19, cex = 0.7)
#dev.off()

# rm(list=ls())