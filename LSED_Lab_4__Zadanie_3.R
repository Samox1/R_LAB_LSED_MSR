### LSED - Zadanie 3 - LAB nr 4
# Autor: Szymon Baczyqski

rm(list=ls())
library(MASS)
library(class)

draw.data.gauss <- function(S1, S2, m1, m2, n1, n2) {
  
  X1 <- mvrnorm(n1, m1, S1)
  X2 <- mvrnorm(n2, m2, S2)
  
  X1 <- data.frame(X1); colnames(X1) <- c("x", "y")
  X2 <- data.frame(X2); colnames(X2) <- c("x", "y")
  
  X1$class <- 1; X2$class <- 2
  
  data <- rbind(X1, X2); data$class <- factor(data$class)
  
  return(data)
}

CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  # Skutecznof klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  # Wartoci true positive i true negative
  # zak3adamy, ?e klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  sums <- apply(CM, 1, sum)
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, row.names = NULL))
}

### --- Czjf 1 --- wykrelif skutecznof klasyfikatora knn w funkcji liczby najbli?szych s9siadsw od knn=1 do knn=21. Wykonac to samo dla wartoci TP i TN. --- ###

# Parametry danych z rozkladu Gaussa
S1 <- matrix(c(4, 2, 2, 4), 2, 2)
S2 <- matrix(c(4, 2, 2, 2), 2, 2)

m1 <- c(-1, -1)
m2 <- c(2, 2)

n1 <- 30
n2 <- 20

# Generowanie danych
data <- draw.data.gauss(S1, S2, m1, m2, n1, n2)

knn.val <- CM.large(data$class, class.knn <- knn(data[,1:2], data[,1:2], data$class, 1))

k_max <- 21

for(i in 2:k_max){
  knn.val <- rbind(knn.val, CM.large(data$class, class.knn <- knn(data[,1:2], data[,1:2], data$class, i)))
}
rownames(knn.val) <- c(1:k_max)

plot(x=1:k_max, y=knn.val[,1], t="b", col="blue", xlab = "k_nn", ylab = "ACC", xlim=c(1,k_max), pch=19)
title("ACC(k_nn) dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))

plot(x=1:k_max, y=knn.val[,2], t="b", ylim=c((min(knn.val[,2])),(max(knn.val[,3]))),xlab = "k_nn", ylab = "TP & TN", xlim=c(1,k_max), pch=19, col="blue")
title("TP i TN dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))
#axis(side=2, tick = 1)
points((1:k_max), knn.val[,3], t="b", pch=19, col="red")
legend("topright", legend=c("TN (klasa 1)","TP (klasa 2)"), col=c("red","blue"), lty=1:2, cex=1)


### --- Czesc 2 --- Nastjpnie wylosowaf dodatkowo 10 punktsw z klasy 1 oraz 5 punktsw z klasy 2, potraktowaf jako zbisr tetsowy i powtsrzyf wykresy. --- ###
### ---         --- W razie mozliwosci doknac usrednienia po 10 losowaniach. --- ###

n1t <- 10
n2t <- 5
test <- draw.data.gauss(S1, S2, m1, m2, n1t, n2t)

test.val <- CM.large(test$class, classtest.knn <- knn(data[,1:2], test[,1:2], data$class, 1))

for(i in 2:k_max){
  test.val <- rbind(test.val, CM.large(test$class, classtest.knn <- knn(data[,1:2], test[,1:2], data$class, i)))
}
rownames(test.val) <- c(1:k_max)

plot(x=1:k_max, y=test.val[,1], t="b", col="blue", xlab = "k_nn", ylab = "TEST ACC", xlim=c(1,k_max), pch=19)
title("TEST ACC(k_nn) dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))

plot(x=1:k_max, y=test.val[,2], t="b", ylim=c((min(test.val[,2])),(max(test.val[,3]))),xlab = "k_nn", ylab = "TEST: TP & TN", xlim=c(1,k_max), pch=19, col="blue")
title("TEST: TP i TN dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))
#axis(side=2, tick = 1)
points((1:k_max), test.val[,3], t="b", pch=19, col="red")
legend("right", legend=c("TN (klasa 1)","TP (klasa 2)"), col=c("red","blue"), lty=1:2, cex=1)


### --- Czesc 2.1 --- W razie moziwosci doknac usrednienia po 10 losowaniach. --- ###

# Usrednianie po 10 losowaniach
los <- 10
ACC_mean = 0; TP_mean = 0; TN_mean = 0;

for(x in 1:los){
  test.val <- CM.large(test$class, classtest.knn <- knn(data[,1:2], test[,1:2], data$class, 1))
  
  for(i in 2:k_max){
    test.val <- rbind(test.val, CM.large(test$class, classtest.knn <- knn(data[,1:2], test[,1:2], data$class, i)))
  }
  rownames(test.val) <- c(1:k_max)
  ACC_mean <- cbind(ACC_mean,test.val[,1])
  TP_mean <- cbind(TP_mean,test.val[,2])
  TN_mean <- cbind(TN_mean,test.val[,3])
}

ACC_mean <- ACC_mean[,-1]; TP_mean <- TP_mean[,-1]; TN_mean <- TN_mean[,-1]
para.val <- data.frame(rowMeans(ACC_mean), round(rowMeans(TP_mean)), round(rowMeans(TN_mean)), apply(ACC_mean, 1,sd), apply(TP_mean, 1,sd), apply(TN_mean, 1,sd))


# --- Plot ACC od k_nn
plot(x=1:k_max, y=para.val[,1], t="b", col="blue", xlab = "k_nn", ylab = "TEST ACC", xlim=c(1,k_max), pch=19)
title("UREDNIONY TEST ACC(k_nn) dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))


# --- Plot TP i TN od k_nn
plot(x=1:k_max, y=para.val[,2], t="b", ylim=c((min(para.val[,2])),(max(para.val[,3]))),xlab = "k_nn", ylab = "UREDNIONY TEST: TP & TN", xlim=c(1,k_max), pch=19, col="blue")
title("UREDNIONY TEST: TP i TN dla k_nn = 1:21")
axis(side=1, at=c(1:k_max))
points((1:k_max), para.val[,3], t="b", pch=19, col="red")
legend("right", legend=c("TN (klasa 1)","TP (klasa 2)"), col=c("red","blue"), lty=1:2, cex=1)


library(Hmisc)

K_nn = 1:k_max
#ACC = para.val[,1]
#TP = para.val[,2]
#TN = para.val[,3]
#SD(ACC_rows) = para.val[,4] - dla ka?dego K_nn
#SD(TP_rows) = para.val[,5]
#SD(TN_rows) = para.val[,6]

errbar(K_nn, para.val[,1], (para.val[,1] + para.val[,4]), (para.val[,1] - para.val[,4]), ylab = "ACC Mean")
title("ACC Mean")
errbar(K_nn, para.val[,2], (para.val[,2] + para.val[,5]), (para.val[,2] - para.val[,5]), ylab = "TP Mean (Klasa 2)")
title("TP Mean (Klasa 2 - po urednianiu)")
errbar(K_nn, para.val[,3], (para.val[,3] + para.val[,6]), (para.val[,3] - para.val[,6]), ylab = "TN Mean (Klasa 1)")
title("TN Mean (Klasa 1 - po urednianiu)")
