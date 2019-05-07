#MSR - LAB 9 ZADANIE

# Wczytaj zbiór danych Salaries z biblioteki carData i zapoznaj siê z nim.
# Potraktuj zmienn¹ salary jako odpowiedŸ a zmienne rank, discipline, sex 
# jako zmienne wyjaœniaj¹ce. Przy pomocy funkcji table sprawdŸ czy grupy 
# s¹ równoliczne. Porównaj ze sob¹ wyniki trójczynnikowych analiz wariancji 
# wykonanych bez interakcji oraz z interakcjami. Zinterpretuj wyniki. 
# Zadanie wykonaj dwukrotnie, za pierwszym razem przy za³o¿eniu balanced design, 
# a za drugim razem dla unbalanced design.


rm(list=ls())

library(car)

data("Salaries")

rownoliczne <- table(Salaries$rank, Salaries$discipline, Salaries$sex)
print(rownoliczne)
cat("\n")

print("-------------- AOV --------------")
bez_interkacji <- aov(salary ~ sex+rank+discipline, data=Salaries)
print(summary(bez_interkacji))
cat("\n")

z_interakcja <- aov(salary ~ sex*rank*discipline, data=Salaries)
print(summary(z_interakcja))
cat("\n")

print("-------------- Anova --------------")
Anova_bez <- Anova(bez_interkacji, type="III")
print(Anova_bez)
cat("\n")

Anova_z <- Anova(z_interakcja, type="III")
print(Anova_z)
cat("\n")


print("-------------- Wykresy --------------")

library(ggplot2)
theme_set(theme_bw())

male <- Salaries[Salaries$sex=="Male",]
female <- Salaries[Salaries$sex=="Female",]

gmale <- ggplot(male)
gmale + geom_boxplot(aes(x=rank, y=salary, color=discipline)) + labs(x="Rank", y="Salary", color="Discipline")+ggtitle("Male: Salary / Rank")

gfemale <- ggplot(female)
gfemale + geom_boxplot(aes(x=rank, y=salary, color=discipline)) + labs(x="Rank", y="Salary", color="Discipline")+ggtitle("Female: Salary / Rank")

