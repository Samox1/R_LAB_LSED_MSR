### MSR Lab nr 11 - Laboratorium

rm(list=ls())


# Przyk³ad 11.1
library(MASS)
library(ggplot2)

S <- matrix(c(3,0,0,3),2,2)
m1 <- c(2,2)
m2 <- c(-1,-1)

n1 <- 60
n2 <- 20
n <- n1 + n2

x1 <- mvrnorm(n1, m1, S)
x2 <- mvrnorm(n2, m2, S)

klasy <- c(rep("1", n1), rep("2", n2))
wsp1 <- c(x1[,1],x2[,1])
wsp2 <- c(x1[,2],x2[,2])

dane <- data.frame(klasy, wsp1, wsp2)

theme_set(theme_bw())
ggplot(dane) + geom_point(aes(x = wsp1, y = wsp2, color = klasy), shape = 19, size = 3)


# Przyk³ad 11.2
dane.lda <- lda(klasy ~ wsp1 + wsp2, data = dane)
print(dane.lda)
proj <- as.matrix(dane[,2:3]) %*% dane.lda$scaling
dane$proj <- proj[,1]
ggplot(dane) + geom_point(aes(x = proj, y = 0, color = klasy), shape=21, size=5) + geom_vline(xintercept=0)


# Przyk³ad 11.3
library(klaR)
partimat(klasy ~ wsp1 + wsp2, data = dane, method="lda")

library(caret)
dane.pred <- predict(dane.lda, dane)
dane.conf <- confusionMatrix(dane.pred$class,dane$klasy, positive = "1")
print(dane.conf)


# Przyk³ad 11.4
S1 <- matrix(c(4,0,0,4),2,2)
S2 <- matrix(c(2,0,0,1),2,2)
S3 <- matrix(c(1,0,0,4),2,2)
m1 <- c(3,3)
m2 <- c(-1,-1)
m3 <- c(4,-2)

n1 <- 60
n2 <- 40
n3 <- 50
n <- n1 + n2 + n3

x1 <- mvrnorm(n1, m1, S1)
x2 <- mvrnorm(n2, m2, S2)
x3 <- mvrnorm(n3, m3, S3)

klasy <- c(rep("1", n1), rep("2", n2), rep("3", n3))
wsp1 <- c(x1[,1], x2[,1], x3[,1])
wsp2 <- c(x1[,2], x2[,2], x3[,2])

dane <- data.frame(klasy, wsp1, wsp2)

ggplot(dane) + geom_point(aes(x = wsp1, y = wsp2, color = klasy), shape = 19, size = 3)

print(partimat(klasy ~ wsp1 + wsp2, data = dane, method="lda"))
print(partimat(klasy ~ wsp1 + wsp2, data = dane, method="qda"))


# Trenujemy klasyfikator LDA i testujemy go przy u¿yciu 10-krotnej walidacji krzy¿owej
dane.lda.cv <- train(klasy ~ wsp1 + wsp2, data = dane, method="lda", trControl = trainControl(method = "cv", number=10))
# Trenujemy klasyfikator QDA i testujemy go przy u¿yciu 10-krotnej walidacji krzy¿owej
dane.qda.cv <- train(klasy ~ wsp1 + wsp2, data = dane, method="qda", trControl = trainControl(method = "cv", number=10))
print(dane.lda.cv)

# Obserwacje zrzutowane na proste ortogonalne do prostych rozdzielaj¹cych
dane.lda <- lda(klasy ~ wsp1 + wsp2, data = dane)
proj <- as.matrix(dane[,2:3]) %*% dane.lda$scaling
dane.proj <- data.frame(proj, klasy=dane$klasy)
g <- ggplot(dane.proj, aes(x=LD1, y=LD2))
g + geom_point(aes(color=klasy), size=3)


# Przyk³ad 11.5
sigma <- matrix(c(1,0,0,1),2,2)

mu1 <- c(5,5)
mu2 <- c(1,1)
mu3 <- c(4,-2)

kolory <- c(rep("orange", 30), rep("violet", 30), rep("green", 30))

clust1 <- mvrnorm(30, mu1, sigma)
clust2 <- mvrnorm(30, mu2, sigma)
clust3 <- mvrnorm(30, mu3, sigma)

all_points <- rbind(clust1, clust2, clust3)

xrange <- range(all_points[,1])
yrange <- range(all_points[,2])

xmin = xrange[1]; xmax = xrange[2]
ymin = yrange[1]; ymax = yrange[2]

par(mfrow = c(2,2))

plot(all_points, col=kolory, pch=19, cex=2, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
title("Klastry", cex.main=1.4, font=2)

cl <- kmeans(all_points, 3)

plot(all_points, col=kolory, pch=19, cex=2, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
text(all_points[,1], all_points[,2], cl$cluster, font=2)
title("Metoda 3-srednich", cex.main=1.4, font=2)

cl1 <- kmeans(all_points, 4)

plot(all_points, col=cl1$cluster, pch=19, cex=2, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
title("Metoda 4-srednich", cex.main=1.4, font=2)

cl2 <- kmeans(all_points, 5)

plot(all_points, col=cl2$cluster, pch=19, cex=2, xlab="X", ylab="Y", xlim=c(xmin,xmax), ylim=c(ymin,ymax))
title("Metoda 5-srednich", cex.main=1.4, font=2)


# Przyk³ad 11.6
x <- seq(-5, 5, by=.1)
y <- x

eta <- runif(101, max = 1)
dzeta <- runif(101, max = 1)

x <- x + eta
y <- y + dzeta

par(mfrow = c(2,2))

plot(x, y, pch=19, xlab="Test 1", ylab="Test 3", font=2, font.lab=2, xlim=c(-5,5), ylim=c(-5,5))
abline(h=0, v=0, lwd=2, col="gray")
abline(0,1,lwd=2,col="red")
abline(0,-1,lwd=2,col="green")
text(4.5,-0.5,expression(x[1]),cex=2)
text(-0.5,4.5,expression(x[2]),cex=2)
text(4.7,4,expression(y[1]),cex=2, col="red")
text(-4.5,4,expression(y[2]),cex=2, col="green")
title("Dane", cex.main=1.4)

test <- data.frame(x, y)

test.pc <- princomp(~., cor=T, data=test)

plot(test.pc, main="")
title("Wariancja", cex.main=1.4)

plot(test.pc$scores, xlim=c(-2,2), ylim=c(-2,2), xlab="Sk³adowa 1", ylab="Sk³adowa 2")
title("Sk³adowe", cex.main=1.4)
