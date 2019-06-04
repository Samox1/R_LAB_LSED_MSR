# LSED - Kolokwium 2
# Autor: Szymon Baczyñski 270626

rm(list=ls())
library(MASS)
library(e1071)

# Punkt 1
cytowania_raw <- read.table("http://www.if.pw.edu.pl/~julas/LSED/file_sciento.txt")
cytowania <- cytowania_raw
# Punkt 2
cytowania$cit <- ifelse(cytowania$cit>20,2,1)
# Punkt 3
colnames(cytowania)[10] <- "class"
cytowania$class <- factor(cytowania$class)

### ----------------------------------------------------------------------------- ###
CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  ACC <- sum(diag(CM)) / sum(CM)
  return(ACC = round(ACC,4))
}
# Punkt 4
# SVM <- svm(cytowania[,ncol(cytowania)] ~ ., type = "C-classification", data = cytowania, cost = 100, scale = F, kernel = "linear")
cyt_SVM <- lapply(c(2:(ncol(cytowania)-1)), function(v) svm(cytowania[,ncol(cytowania)] ~ ., type = "C-classification", data = cytowania[,1:v], cost = 100, scale = F, kernel = "linear"))
# Punkt 5
SVM_Predict <- lapply(cyt_SVM, function(v) predict(v,cytowania))
SVM_ACC <- sapply(SVM_Predict, function(v) CM.large(v,cytowania$class))
names(SVM_ACC) <- c(2:9)
print("SVM - Skutecznoœæ w zale¿noœci od iloœci zmiennych:"); print(SVM_ACC)
# Punkt 6
plot(c(2:(ncol(cytowania)-1)), SVM_ACC, main="Skutecznoœæ w zale¿noœci od iloœci zmiennych",xlab="Iloœæ u¿ytych zmiennych", ylab="Skutecznoœæ", type = "b")
png("Kol_2_SB_Punkt_6.png")
plot(c(2:(ncol(cytowania)-1)), SVM_ACC, main="Skutecznoœæ w zale¿noœci od iloœci zmiennych",xlab="Iloœæ u¿ytych zmiennych", ylab="Skutecznoœæ", type = "b")
dev.off()

# Punkt 7
PCA <- princomp(~., cor=F, data=cytowania[,-ncol(cytowania)])
# Punkt 8
cat("\n"); print("PCA - Wariancja:"); print(round(PCA$sdev^2,4))
plot(PCA, main=""); title("Wariancja", cex.main=1.4)
sum_PCA <- sapply(c(1:(ncol(cytowania)-1)), function(v) sum(PCA$sdev[1:v]^2))
suma <- max(sum_PCA)
plot(sum_PCA/suma, main="Unormowana Skumulowana Wariancja", xlab="Liczba sk³adowych", ylab="Unormowana Skumulowana Wariancja",type="b", col="red" )
png("Kol_2_SB_Punkt_8.png")
plot(sum_PCA/suma, main="Unormowana Skumulowana Wariancja", xlab="Liczba sk³adowych", ylab="Unormowana Skumulowana Wariancja",type="b", col="red" )
dev.off()
# text(c(1:9), sum_PCA/suma, labels=(round(sum_PCA/suma,4)), cex= 0.7, pos=4)
axis(side=1, at=c(1:length(sum_PCA)))
# Punkt 9
print(PCA$loadings) 
plot(PCA$scores, col = as.factor(cytowania$class)); title("Sk³adowe", cex.main=1.4); 
# Punkt 10
cat("\n")
print("Wed³ug wykresu Unormowanej Skumulowanej Wariancji - do opisu uk³adu wystarczy³yby 3 pierwsze sk³adowe g³ówne z PCA")

# Punkt 11
SVM_4 <- svm(cytowania[,ncol(cytowania)] ~ ., type = "C-classification", data = PCA$scores[,1:3], cost = 100, scale = F, kernel = "linear")
SVM_4_Predict <- predict(SVM_4,PCA$scores)
# Punkt 12
cat(c("ACC dla wybranej wartoœci nowych zmiennych PCA_Comp[1:3]: ", ACC_PCA<-CM.large(SVM_4_Predict,cytowania$class), "\n"))
print("SVM - Skutecznoœæ w zale¿noœci od iloœci zmiennych:"); print(SVM_ACC)
cat("\n")
Podsumowanie <- paste("Skutecznoœæ klasyfikatora wynosi: ACC = ",ACC_PCA," .Wynik ten jest porównywalny do maksymalnej skutecznoœci SVM na danych podstawowych." ,sep="")
print(Podsumowanie)

