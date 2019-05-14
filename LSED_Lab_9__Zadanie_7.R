### LSED - Zadanie 7 - LAB nr 9
# Autor: Szymon Baczyñski 270626

# Polecenie: 
#   1) Zinterpertowaæ dane dotycz¹ce charakterystki zwierz¹t (poni¿ej) 
#     za pomoc¹ funkcji heatmap() (wybraæ opcje col = c("red", "green") i NIE wybieraæ opcji Colv=NA.
#                                                                                              
#     animals <- cluster::animals
#     colnames(animals) <- c("warm-blooded", "can fly", "vertebrate", "endangered", "live in groups", "have hair")
#                                                                                              
#   2) Za pomoc¹ funkcji kmeans() sprawdziæ rozpoznawanie skupieñ w zbiorze iris. Przetestowaæ wszystkie kombinacje zmiennych, 
#     tzn.: (1,2); (1,3); (1,4); (2,3); ... ;(1,2,3); (1,2,4); ... ;(1,2,3,4) gdzie liczba oznacza numer kolumny. 

rm(list=ls())
library(MASS)

CM.large <- function(org.class, pred.class) {
  CM <- table(org.class, pred.class)
  # Skutecznoœæ klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  return(c(ACC = round(ACC,4), row.names = NULL))
}


# PUNKT 1 - Heatmap

animals <- cluster::animals
colnames(animals) <- c("warm-blooded", "can fly", "vertebrate", "endangered", "live in groups", "have hair")

heatmap(as.matrix(animals), col = c("red", "green"))


# PUNKT 2 - K-Means

irys <- datasets::iris

# Mo¿liwe kombinacje
x <- c(1,2,3,4)
komb2 <- combn(x,2)
komb3 <- combn(x,3)
komb4 <- combn(x,4)

# Klasyfikacja K-Means
irys2 <- apply(komb2, 2, function(v) kmeans(irys[,v], 3))
irys3 <- apply(komb3, 2, function(v) kmeans(irys[,v], 3))
irys4 <- apply(komb4, 2, function(v) kmeans(irys[,v], 3))

# Przeliczenie skutecznoœci K-means
ACC2 <- sapply(c(1:length(irys2)), function(v) CM.large(irys$Species, irys2[[v]]$cluster))
ACC3 <- sapply(c(1:length(irys3)), function(v) CM.large(irys$Species, irys3[[v]]$cluster))
ACC4 <- sapply(c(1:length(irys4)), function(v) CM.large(irys$Species, irys4[[v]]$cluster))

# colnames(ACC2) <- 
# rownames(tab) <- c("SVM", "LDA")

funky <- function(k,x){
  
}

print(ACC2)
print(ACC3)
print(ACC4)


