# LSED - Kolokwium 1 
# Autor: Szymon Baczyñski 270626

rm(list=ls())
library(MASS)

# Punkt 1.1
pogoda_raw <- data.frame(read.table("http://www.if.pw.edu.pl/~julas/LSED/file_pogoda.txt", header = TRUE))
pogoda <- pogoda_raw
# Punkt 1.2
pogoda$percip <- ifelse(pogoda$percip==0.0, 1, 2)
# Punkt 1.3
colnames(pogoda)[15] <- c("class")

# Punkt 2.1
pogoda$class <- factor(pogoda$class)
LDA_pogoda <- lapply(c(2:14), function(v) lda(pogoda[,15] ~ ., pogoda[,1:v]))
# Punkt 2.2
CM.large <- function(org.class, pred.class){
  CM <- table(org.class, pred.class)
  ACC <- sum(diag(CM)) / sum(CM)
  return((ACC))
}

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

LDA_Predict <- lapply(LDA_pogoda, function(v) predict(v,pogoda))
LDA_ACC <- lapply(LDA_Predict, function(v) CM.large(v$class,pogoda$class))
# FPR_TPR <- c(LDA_ACC[[1]][2],LDA_ACC[[1]][3],LDA_ACC[[1]][1])

lapply(LDA_ACC, function(v) FPR_TPR <- rbind(FPR_TPR,v[1], v[2],v[3]))

print("ACC PP:"); print(FPR_TPR[,3])
plot(x=FPR_TPR[,1], y=FPR_TPR[,2], pch=19, cex=0.5)
png("Punkt_6_ROC_FPR_TPR.png")
plot(x=FPR_TPR[,1], y=FPR_TPR[,2], pch=19, cex=0.5)
dev.off()

# Punkt 3.1
