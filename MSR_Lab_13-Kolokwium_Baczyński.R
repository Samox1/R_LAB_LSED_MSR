# MSR - Kolokwium 1
# Autor: Szymon Baczyñski 270626

rm(list=ls())
library(MASS)

# Punkt 1
sacramento <- read.table("http://www.if.pw.edu.pl/~paluch/MSR/data/sacramento_caret.txt",header = T)

print(paste("Wartoœci NA w zbiorze Sacramento jest:" , sum(is.na(sacramento))))

dane <- data.frame()

for(i in 1:(ncol(sacramento))){
  if(is.numeric(sacramento[,i])){
    dane <- rbind(dane,list(MEAN<-mean(sacramento[,i],na.rm=TRUE),sd(sacramento[,i],na.rm=TRUE)))
    if(length(which(is.na(sacramento[,i])))>0){
      sacramento[which(is.na(sacramento[,i])),i] <- MEAN
    }
  }
}
# Punkt 2
names(dane) <- c("mean", "std dev")

# Punkt 3
boxplot(beds~type, sacramento, ylab="Beds", col=c("red","green","blue"))
boxplot(baths~type, sacramento, ylab="Baths", col=c("red","green","blue"))
boxplot(price~type, sacramento, ylab="Price", col=c("red","green","blue"))
boxplot(sqft~type, sacramento, ylab="SQFT", col=c("red","green","blue"))

# Punkt 4
histogramy <- lapply(sacramento, function(v) ifelse(is.numeric(v), hist(v,main = names(v), xlab = names(v)),0))

