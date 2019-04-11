### MSR Lab nr 4 - Zadanie

library(ggplot2)
rm(list=ls())

cars <- datasets::mtcars 
#cars$am[cars$am==0]="automat"
#cars$am[cars$am==1]="manual"
cars$am <- ifelse(cars$am==0, "automat", "manual")
colnames(cars)[colnames(cars)=="am"] <- "Biegi"


g <- ggplot(cars) + geom_point(aes(x = hp, y = qsec, fill=Biegi, size=mpg), shape=21) + labs(title = "Dane z magazynu Motor Trend (1974)") + ylab("Czas na 1/4 mili [s]") + xlab("Horse Power [HP]")
print(g)