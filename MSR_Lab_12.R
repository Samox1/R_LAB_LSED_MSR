### MSR Lab nr 12 - Zadanie

rm(list=ls())

library(survival)
library(survminer)


obs.t <- c(7,6,6,5,2,4)
obs.cens <- c(0,1,0,0,1,1)
obs.surv <- Surv(obs.t,obs.cens)
print(obs.surv)

rak <- survival::ovarian
rak.surv <- Surv(time = ovarian$futime, event = ovarian$fustat)
print(rak.surv)
rak.fit <- survfit(rak.surv~rx, data=rak)
print(summary(rak.fit))
print(ggsurvplot(rak.fit, data=rak, pval = TRUE))

### ------------------------------------------------------------------------------------------------------------------ ###
# Zadanie punktowane: Dokoñcz analizê prze¿ycia dla danych ovarian. Zbadaj czy 
# istnieje zale¿noœæ miêdzy funkcj¹ prze¿ycia a czynnikami age, resid.ds i ecog.ps.
# Czynnik age nale¿y uprzednio zdyskretyzowaæ (wystarcz¹ dwie grupy, mo¿na zrobiæ histogram aby wybraæ punkt podzia³u).
### ------------------------------------------------------------------------------------------------------------------ ###


age_hist <- hist(rak$age)
rak.zadanie <- Surv(time=ovarian$futime, event=ovarian$fustat)

wiek = 60

rak.age.fit <- survfit(rak.surv ~ cut(age,c(min(age), mean(age), max(age))),rak)
rak.resid.fit <- survfit(rak.surv ~ resid.ds,rak)
rak.ecog.fit <- survfit(rak.surv ~ ecog.ps,rak)

print(ggsurvplot(rak.age.fit, data=rak, pval = TRUE))
print(ggsurvplot(rak.resid.fit, data=rak, pval = TRUE))
print(ggsurvplot(rak.ecog.fit, data=rak, pval = TRUE))

