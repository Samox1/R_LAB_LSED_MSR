# LSED - Kolokwium 2

rm(list=ls())
library(MASS)
library(e1071)

# Punkt 1
# Punkt 1.1 (1)
cytowanie_raw <- data.frame(read.table("http://www.if.pw.edu.pl/~julas/LSED/file_sciento.txt", header = TRUE))
cytowanie <- cytowanie_raw
# Punkt 1.2 (2)
cytowanie[ncol(cytowanie)] <- ifelse(cytowanie[ncol(cytowanie)]>20, 2, 1)
# Punkt 1.3 (3)
colnames(cytowanie)[ncol(cytowanie)] <- c("class")
cytowanie$class <- factor(cytowanie$class)

# Punkt 2
CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  ACC <- sum(diag(CM)) / sum(CM)
  TP <- CM[2,2]
  TN <- CM[1,1]
  # sums <- apply(CM, 1, sum)
  # TPR <- TP / sums[2]
  # FPR <- 1 - TN / sums[1]
  return(c(ACC = round(ACC,4), row.names = NULL))
}
# Punkt 2.1 (4)
# SVM <- svm(cytowanie[,ncol(cytowanie)] ~ ., type = "C-classification", data = cytowanie, cost = 100, scale = F, kernel = "linear")
cyt_SVM <- lapply(c(2:(ncol(cytowanie)-1)), function(v) svm(cytowanie[,ncol(cytowanie)] ~ ., type = "C-classification", data = cytowanie[,1:v], cost = 100, scale = F, kernel = "linear"))
# Punkt 2.2 (5)
SVM_Predict <- lapply(cyt_SVM, function(v) predict(v,cytowanie))
SVM_ACC <- sapply(SVM_Predict, function(v) CM.large(v,cytowanie$class))
names(SVM_ACC) <- c(2:9)
print(SVM_ACC)
# Punkt 2.3 (6)
plot(c(2:(ncol(cytowanie)-1)), SVM_ACC, main="Skutecznoœæ w zale¿noœci od zmiennych",xlab="Iloœæ u¿ytych zmiennych", ylab="Skutecznoœæ", ylim = c(0,1), type = "b")

# Punkt 3
# Punkt 3.1 (7)
PCA <- princomp(~., cor=F, data=cytowanie[,-ncol(cytowanie)])
# Punkt 3.2 (8)
# print(round(PCA$sdev^2,2))
sum_PCA <- sapply(c(1:(ncol(cytowanie)-1)), function(v) sum(PCA$sdev[1:v]^2))
suma <- max(sum_PCA)
plot(sum_PCA/suma, main="Unormowana Skumulowana Wariancja", xlab="Liczba sk³adowych", ylab="Unormowana Skumulowana Wariancja",type="b", col="red" )
axis(side=1, at=c(1:length(sum_PCA)))
# Punkt 3.3 (9)
print(PCA$loadings)   # ??? Nie wiem o co chodzi?
# Punkt 3.4 (10)
cat("\n")
print("Wed³ug wykresu Unormowanej Skumulowanej Wariancji - do opisu uk³adu wystarczy³yby 3 zmienne")

# Punkt 4
# Punkt 4.1 (11)
SVM_4 <- svm(cytowanie[,ncol(cytowanie)] ~ ., type = "C-classification", data = cytowanie[,1:3], cost = 100, scale = F, kernel = "linear")
SVM_4_Predict <- predict(SVM_4,cytowanie)
# Punkt 4.2 (12)
cat(c("ACC dla wybranej wartoœci zmiennych SVM[1:3]: ", CM.large(SVM_4_Predict,cytowanie$class), "\n"))
for(i in 1:length(SVM_ACC)+1){
  napis <- paste("ACC dla SVM[1:",i,"]: ", sep="")
  if(SVM_ACC[i-1]==max(SVM_ACC)){
    cat(c(napis,SVM_ACC[i-1]," <--- MAX ACC\n"))
  }else{cat(c(napis,SVM_ACC[i-1],"\n"))}
}

