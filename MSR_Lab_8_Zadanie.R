### MSR - LAB 8 

# Zadanie punktowane: Wczytaj zbiory danych Pima.tr i Pima.te z biblioteki MASS i zapoznaj si� z nim. 
# Dla zbioru Pima.tr wykonaj model regresji logistycznej, a nast�pnie sprawd� jego skuteczno�� predykcyjn� przy 
# u�yciu zbioru Pima.te. Niech metoda predict zwraca prawdopodobie�stwo bycia chorym na cukrzyce, 
# kt�re nast�pnie przy pomocy funkcji ifelse nale�y zamieni� na odpowied� binarn� (chory=1, zdrowy=0). 
# Jako pr�g prawdopodobie�stwa powy�ej kt�rego uznajemy osobnika za chorego przyj�� 0.5. 
# Oblicz skuteczno�� modelu wed�ug poni�szego wzoru.

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
