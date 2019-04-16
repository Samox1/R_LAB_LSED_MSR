### MSR - LAB 8 

# Zadanie punktowane: Wczytaj zbiory danych Pima.tr i Pima.te z biblioteki MASS i zapoznaj siê z nim. 
# Dla zbioru Pima.tr wykonaj model regresji logistycznej, a nastêpnie sprawdŸ jego skutecznoœæ predykcyjn¹ przy 
# u¿yciu zbioru Pima.te. Niech metoda predict zwraca prawdopodobieñstwo bycia chorym na cukrzyce, 
# które nastêpnie przy pomocy funkcji ifelse nale¿y zamieniæ na odpowiedŸ binarn¹ (chory=1, zdrowy=0). 
# Jako próg prawdopodobieñstwa powy¿ej którego uznajemy osobnika za chorego przyj¹æ 0.5. 
# Oblicz skutecznoœæ modelu wed³ug poni¿szego wzoru.

rm(list=ls())
library(MASS)

data("Pima.tr")
data("Pima.te")

pima.tr1 <- glm(type ~ ., data = Pima.tr, family=binomial(link = "logit"))
pima.kap <- predict(pima.tr1, newdata = Pima.te, type = "response")

pima.diabetic <- ifelse(pima.kap>=0.5, 1, 0)
Pima.te$type <- ifelse(Pima.te$type=="Yes", 1,0)

pima.accuracy <- 1-mean(abs(pima.diabetic-Pima.te$type))
print(pima.accuracy)
# [1] 0.8012048
