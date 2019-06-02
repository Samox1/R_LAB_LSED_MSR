# LSED - Kolokwium 1 
# Autor: Szymon Baczyñski 270626

rm(list=ls())
library(MASS)
library(rpart)
library(rpart.plot)

# Punkt 1.1 (1)
pogoda_raw <- read.table("http://www.if.pw.edu.pl/~julas/LSED/file_pogoda.txt", header = TRUE)
pogoda <- pogoda_raw
# Punkt 1.2 (2)
pogoda$percip <- ifelse(pogoda$percip==0.0, 1, 2)
# Punkt 1.3 (3)
colnames(pogoda)[15] <- c("class")
pogoda$class <- factor(pogoda$class)

# Punkt 2
CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  ACC <- sum(diag(CM)) / sum(CM)
  TP <- CM[2,2]
  TN <- CM[1,1]
  sums <- apply(CM, 1, sum)
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  return(c(ACC = round(ACC,4), FPR = round(FPR, 4), TPR = round(TPR, 4), row.names = NULL))
}

# Punkt 2.1 (4)
# pogoda$class <- factor(pogoda$class)
# LDA <- lda(class ~ ., pogoda)
LDA_pogoda <- lapply(c(2:(ncol(pogoda)-1)), function(v) lda(pogoda[,ncol(pogoda)] ~ ., pogoda[,1:v]))

# Punkt 2.2 (5)
LDA_Predict <- lapply(LDA_pogoda, function(v) predict(v,pogoda))

# Punkt 2.3 (6)
LDA_ACC <- sapply(LDA_Predict, function(v) CM.large(v$class,pogoda$class))

cat("ACC PP: \n"); cat(LDA_ACC[1,]); cat("\n")
plot(x=LDA_ACC[2,], y=LDA_ACC[3,], main="ROC: Punkty dla LDA (oznaczona wartoœæ ACC)",xlab="FPR", ylab="TPR", xlim=c(0,1), ylim=(c(0,1)) ,pch=19)
text(LDA_ACC[2,], LDA_ACC[3,], labels=LDA_ACC[1,], cex= 0.7, pos=4)
abline(0, 1, col = "gray")

png("Punkt_6_ROC_FPR_TPR.png")
plot(x=LDA_ACC[2,], y=LDA_ACC[3,], main="Punkty dla LDA (obok wartoœæ ACC)",xlab="FPR", ylab="TPR", xlim=c(0,1), ylim=(c(0,1)) ,pch=19)
text(LDA_ACC[2,], LDA_ACC[3,], labels=LDA_ACC[1,], cex= 0.7, pos=4)
abline(0, 1, col = "gray")
dev.off()

# Punkt 3
best.cp <- function(tree){
  mincp <- which.min(tree$cptable[,4])
  cp.row <- which.max(tree$cptable[,4] < tree$cptable[mincp,4] + tree$cptable[mincp,5])
  return(tree$cptable[cp.row,1])
}

# Punkt 3.1 (7)
drzewo <- rpart(class ~ ., pogoda, minsplit = 1, minbucket = 1, cp = 0)
rpart.plot(drzewo, type = 1, extra = 1) 
plotcp(drzewo)

# Punkt 3.2 (8)
best <- best.cp(drzewo)
drzewo_1 <- prune(drzewo, cp = best)

# Punkt 3.3 (9)
rpart.plot(drzewo_1, type = 1, extra = 1)

png("Punkt_9_Drzewo_Cut.png")
rpart.plot(drzewo_1, type = 1, extra = 1)
dev.off()

# Punkt 3.4 (10)
cat("\n"); cat("Porównanie Skutecznoœci:\n")
cat(c("Skutecznoœæ LDA:    ", max(LDA_ACC[1,]))); cat("\n")
cat(c("Skutecznoœæ Drzewa: ", CM.large(pogoda$class,predict(drzewo_1, pogoda, type = "class"))["ACC"])); cat("\n")

# Punkt 4.1 (11)
# Wyeksportowano wykresy do PNG z punktu 6 i 9 - 2.3 i 3.3
# Punkt 4.2 (12)
cat("\nMaximum Drzewa to ACC = 0.8548, natomiast LDA osi¹gne³o dla wszystkich kolumn ACC = 0.8387. \nTo oznacza, ¿e Drzewo lepiej klasyfikuje obiekty (o ile dobrze siê stworzy i utnie).")
