### MSR zadanie 5

rm(list=ls())
### --- Czêœæ 1 --- ###

kozy <- read.table("http://www.if.pw.edu.pl/~paluch/MSR/goats.txt", header = T)

alfa <- 0.05
mu <- 23

### --- Czêœæ 2 --- ###

n <- length(kozy$WeightInitial)
# Pakiet R ma wbudowany test T-Studenta dla modelu II
wynik <- t.test(kozy$WeightInitial, alternative = "greater", mu=mu)
#print(wynik)
print("Czy waga kóz przekrasza 23 kg?")
print(wynik$p.value > alfa) 
print(wynik$p.value)

### --- Czêœæ 3 --- ###

mu1 <- 24
moc <- power.t.test(n, delta=mu1-mu, sd=sd(kozy$WeightInitial), sig.level = alfa, type="one.sample", alternative = "one.sided")
print("Prawd. ¿e blednie uznamy, ze srednia waga koz < 23 kg?")
print(1-moc$power)

### --- Czêœæ 4 --- ###

moc <- power.t.test(delta=mu1-mu, sd=sd(kozy$WeightInitial), sig.level = alfa, type="one.sample", alternative = "one.sided", power = 0.8)
print("Szukana liczba probek wynosi: ")
print(ceiling(moc$n)) # Szukana liczba probek wynosi:

### --- Czêœæ 5 --- ###

print("Punkt D: ")
alfa <- 0.1
std_0 <- 20
statystyka <- ((n-1)*var(kozy$WeightInitial))/std_0
zbr1 <- qchisq(alfa/2, n-1)
zbr2 <- qchisq(1-alfa/2, n-1)
print(statystyka)
print(zbr1)
print(zbr2)
