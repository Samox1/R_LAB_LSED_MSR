### MSR - LAB 10

rm(list=ls())


# Przyk³ad 10.1
v <- c(2,-5,NA,3.5,NA)
is.na(v)

which(is.na(v))

df <- data.frame(a=1:5, b=c("jeden","dwa","trzy",NA,"piêæ"),c=v)
print(df)
is.na(df)
print(df)


# Przyk³ad 10.2
w <- na.omit(v)
print(w)

df2 <- df[complete.cases(df),]
print(df2)


# Przyk³ad 10.3
library(Hmisc)

wekt <- c(4,2,NA,1,1,1,NA,5)
print(Hmisc::impute(wekt, median))
# impute(wekt, mean)
# impute(wekt, "random")
# impute(wekt, 0)


# Przyk³ad 10.4
x <- rnorm(100, mean=3, sd=2)
x.scaled <- scale(x)
mean(x.scaled)

sd(x.scaled)


# Przyk³ad 10.5
dane <- read.csv2("http://www.biecek.pl/R/dane/dane0.csv")
dane.trans <- transform(dane, logVEGF = log(VEGF), fNowotwor = factor(Nowotwor), cutWiek = cut(Wiek,4))
head(dane.trans)


# Przyk³ad 10.6
library(MASS)
# Regresja liniowa oparta o statystycznie istotne czynniki: glu i ped
pima.lr <- glm(type~glu+ped, data=Pima.tr, family = binomial(link = "logit"))
# Predykcja
pima.predict <- predict(pima.lr, newdata = Pima.te, type = "response")
pima.result <- ifelse(pima.predict>0.50, "Yes", "No")
pima.true <- Pima.te[,8]
# Macierz pomy³ek
pima.cm <- table(pima.true,pima.result); pima.cm


TN <- pima.cm[1,1]
TP <- pima.cm[2,2]
FP <- pima.cm[1,2]
FN <- pima.cm[2,1]
P <- TP+FN
N <- TN+FP

# Czu³oœæ
TP/P

# Specyficznoœæ
TN/N

# Precyzja
TP/(TP+FP)

# Wartoœæ predykcyjna ujemna
TN/(TN+FN)

# Dok³adnoœæ
(TN+TP)/sum(pima.cm)

# Miara F1
2*TP/(2*TP+FP+FN)


# Przyk³ad 10.7
library(caret)
pima.conf <- confusionMatrix(as.factor(pima.result),pima.true, positive = "Yes")
print(pima.conf)
print(pima.conf$byClass)


# Przyk³ad 10.8
progi <- seq(0.04,0.99,0.01)
roc <- sapply(progi, function(p)
              confusionMatrix(as.factor(ifelse(pima.predict>p, "Yes", "No")), pima.true, positive = "Yes")$byClass[c("Sensitivity","Specificity")])
plot(1-roc["Specificity",], roc["Sensitivity",], xlab="1-specyficznoœæ", ylab="czu³oœæ", main="ROC", type = "l")


# Przyk³ad 10.9
library(PRROC)
pima.true <- ifelse(Pima.te[,8]=="Yes", 1, 0)
prroc.obj <- roc.curve(scores.class0 = pima.predict, weights.class0 = pima.true, curve=TRUE)
plot(prroc.obj)


library(ROCit)
rocit.obj <- rocit(score=pima.predict,class=Pima.te[,8])
summary(rocit.obj)
plot(rocit.obj)

best.yi.index <- which.max(rocit.obj$TPR-rocit.obj$FPR)
best.cutoff <- rocit.obj$Cutoff[best.yi.index]
best.tpr <- rocit.obj$TPR[best.yi.index]
best.fpr <- rocit.obj$FPR[best.yi.index]
sprintf("Best Cutoff = %.2f (TPR = %.3f, FPR = %.3f)", best.cutoff, best.tpr, best.fpr)



# Przyk³ad 10.10
data("swiss")
# Dane dotycz¹ p³odnoœci w 47 prowincjach Szwajcarii w roku 1888
head(swiss)

set.seed(123) # Ustawiamy ziarno generatora liczb losowych
swiss.samples <- createDataPartition(swiss$Fertility, p = 0.8, list = FALSE)
swiss.train <- swiss[swiss.samples,]
swiss.test <- swiss[-swiss.samples,]
swiss.lm <- lm(Fertility ~., data = swiss.train)
swiss.predict <- predict(swiss.lm, newdata=swiss.test)
data.frame(R2 = R2(swiss.predict, swiss.test$Fertility),
           RMSE = RMSE(swiss.predict, swiss.test$Fertility),
           MAE = MAE(swiss.predict, swiss.test$Fertility))


# Przyk³ad 10.11
library(caret)
# Wypisanie modeli kompatybilnych z funkcj¹ train()
names(getModelInfo())

# Bootstrap
train.control <- trainControl(method="boot", number=100)
swiss.boot <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.boot)

# Leave one out cross validation (LOOCV)
train.control <- trainControl(method = "LOOCV")
swiss.loocv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.loocv)

# 10-krotna walidacja krzy¿owa
train.control <- trainControl(method = "cv", number=10)
swiss.cv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.cv)

# 10-krotna walidacja krzy¿owa powtórzona 3 razy
train.control <- trainControl(method = "repeatedcv", number=10, repeats=3)
swiss.repeatedcv <- train(Fertility ~ ., data = swiss, method = "lm", trControl = train.control)
print(swiss.repeatedcv)



