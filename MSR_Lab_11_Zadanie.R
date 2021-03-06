### MSR Lab nr 11 - Zadanie

rm(list=ls())
library(ggplot2)
library(MASS)
library(klaR)
library(caret)

### ----------------------------------------------------------------------------------------------------------------- ### 
# Zadanie punktowane: Wczytaj zbi�r danych iris z biblioteki datasets i zapoznaj si� z nim. 
# Przeskaluj wektory numeryczne w tej ramce danych odejmuj�c warto�� �redni� i dziel�c przez odchylenie standardowe. 
# Wykonaj liniow� analiz� dyskryminacyjn� na ca�ym zbiorze, a nast�pnie zrzutuj wszystkie obserwacje 
# (oryginalnie czterowymiarowe) na p�aszczyzn� dwuwymiarow� okre�lon� przez proste wyznaczone w LDA. 
# Dokonaj wizualizacji tak przetransformowanych obserwacji w pakiecie ggplot2. 
# Sprawd� skuteczno�� klasyfikacji wykonuj�c dowoln� walidacj� krzy�ow�.
### ----------------------------------------------------------------------------------------------------------------- ###

irys <- datasets::iris

irys_mean <- sapply(irys[,1:4], function(v) mean(v))
irys_sd <- sapply(irys[,1:4], function(v) sd(v))

for(i in 1:(ncol(irys)-1)){
  for(k in 1:(nrow(irys))){
    irys[k,i] <- (irys[k,i]-irys_mean[i])/irys_sd[i]
  }
  # irys[,i] <- apply(irys[,i],1, function(v) (v-irys_mean[,i])/irys_sd[,i])
}