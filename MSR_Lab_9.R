### MSR - LAB 9

rm(list=ls())


# Przyk³ad 9.1
pigs <- ToothGrowth
pigs$dose <- as.factor(pigs$dose)
table(pigs$supp, pigs$dose)

library(ggplot2)
theme_set(theme_bw())
g <- ggplot(pigs)
g + geom_boxplot(aes(x=dose, y=len, color=supp)) + labs(x="dose [mg/day]", y="length of cells", color="Suppl. type")

# ANOVA z interakcjami, obie komendy poni¿ej s¹ równowa¿ne
pigs.aov <- aov(len ~ supp+dose+supp:dose, data=pigs)
pigs.aov <- aov(len ~ supp*dose, data=pigs)
print(summary(pigs.aov))

HSDpig <- TukeyHSD(pigs.aov, which = "dose")
print(HSDpig)

library(car)
AnovaPig <- Anova(pigs.aov, type="III")
print(AnovaPig)

# Przyk³ad 9.2
irysy <- iris
cor(irysy[,-5])

irysy.maov <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = irysy)
print(summary(irysy.maov))