### MSR Lab nr 11 - Zadanie

rm(list=ls())
library(ggplot2)
library(MASS)
library(klaR)
library(caret)

### ----------------------------------------------------------------------------------------------------------------- ### 
# Zadanie punktowane: Wczytaj zbiór danych iris z biblioteki datasets i zapoznaj siê z nim. 
# Przeskaluj wektory numeryczne w tej ramce danych odejmuj¹c wartoœæ œredni¹ i dziel¹c przez odchylenie standardowe. 
# Wykonaj liniow¹ analizê dyskryminacyjn¹ na ca³ym zbiorze, a nastêpnie zrzutuj wszystkie obserwacje 
# (oryginalnie czterowymiarowe) na p³aszczyznê dwuwymiarow¹ okreœlon¹ przez proste wyznaczone w LDA. 
# Dokonaj wizualizacji tak przetransformowanych obserwacji w pakiecie ggplot2. 
# SprawdŸ skutecznoœæ klasyfikacji wykonuj¹c dowoln¹ walidacjê krzy¿ow¹.
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