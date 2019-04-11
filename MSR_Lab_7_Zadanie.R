### MSR LAB 7

rm(list=ls())

library(MASS)
library(ggplot2)
library(car)

data <- datasets::iris

shap1 <- shapiro.test(data[,1])
shap2 <- shapiro.test(data[,2])
shap3 <- shapiro.test(data[,3])
shap4 <- shapiro.test(data[,4])

print("Shapiro Test:")
print(shap1$p.value)
print(shap2$p.value)
print(shap3$p.value)
print(shap1$p.value)

le1 <- leveneTest(data[,1] ~ Species, data)
le2 <- leveneTest(data[,2] ~ Species, data)
le3 <- leveneTest(data[,3] ~ Species, data)
le4 <- leveneTest(data[,4] ~ Species, data)

cat("\n");print("LeneveTest: 1")
print(le1); 
cat("\n");print("LeneveTest: 2")
print(le2); 
cat("\n");print("LeneveTest: 3")
print(le3);
cat("\n");print("LeneveTest: 4")
print(le4)


dane.aov2 <- aov(data[,2]~Species,data)
cat("\n")
print("ANOVA")


print(summary(dane.aov2))

boxplot(data[,2]~Species, data, ylab=colnames(data[2]), col=c("red","green","blue"))


