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
library(gplots)


CM.large <- function(org.class, pred.class,printu) {          # Zmienna "printu" = czy ma siê wyœwietlaæ "table" przy ACC=0
  CM <- table(org.class, pred.class)
  # Skutecznoœæ klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  if(ACC==0 && printu == 1){
    cat("\n"); print("Ups, ACC = 0 , dla jakiegoœ przypadku:")
    print(CM)
  }
  return(c(ACC = round(ACC,4), row.names = NULL))
}


# PUNKT 1 - Heatmap

animals <- cluster::animals
colnames(animals) <- c("warm-blooded", "can fly", "vertebrate", "endangered", "live in groups", "have hair")

heatmap(as.matrix(animals), col = c("red", "green"))
heatmap.2(as.matrix(animals), col = c("red", "green"))
print("Wykonano 2 wykresy HEATMAP"); 


# PUNKT 2 - K-Means

funky <- function(k){           # Funkcja do ³adnego zapisywania przypadków, np. 1+2, 1+3, 1+2+3, itd.
  v <- c(1:ncol(k))             # "range" nie dzia³a jak w Python... 
  x <- c(1:nrow(k))             
  s <- character(length = 0L)   # String, który na pocz¹tku mia³ s = "" powodowa³ powstanie dodatkowego stringa z "" w docelowej tablicy
  
  for(i in v){
    buf <- character(length = 0L)
    for(p in x){
      ifelse(p==1,buf<-k[p,i],buf <- paste(buf,(k[p,i]), sep="+"))
    }
    s <- append(s, buf, after=length(s))
  }
  return(s)
}

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
ACC2 <- sapply(c(1:length(irys2)), function(v) round(CM.large(irys$Species, irys2[[v]]$cluster,1),4))
ACC3 <- sapply(c(1:length(irys3)), function(v) round(CM.large(irys$Species, irys3[[v]]$cluster,1),4))
ACC4 <- sapply(c(1:length(irys4)), function(v) round(CM.large(irys$Species, irys4[[v]]$cluster,1),4))

name2 <- funky(komb2)
name3 <- funky(komb3)
name4 <- funky(komb4)

length(ACC3) = length(ACC2)
length(ACC4) = length(ACC2)
length(name3) = length(ACC2)
length(name4) = length(ACC2)

tab <- matrix(c(name2,ACC2,name3,ACC3,name4,ACC4), nrow=6, byrow=TRUE)
tab[is.na(tab)] <- ""

colnames(tab) <- c(" ","","","","","")
rownames(tab) <- c("Przypadek 2:","ACCuracy: ", "Przypadek 3:", "ACCuracy: ", "Przypadek 4:", "ACCuracy: ")

cat("\n")
print("ACC po jednym u¿yciu metody K_means:")
#print(tab)
print(as.matrix(noquote(tab)))      # Dla ³adnego wyœwietlenia bez cudzys³owów


# Czêœæ powtórzona w celu osi¹gniêcia wysokiego ACC
ACC22 <- c()
ACC33 <- c()
ACC44 <- c()

for (k in c(1:100)){
  # Klasyfikacja K-Means
  irys2 <- apply(komb2, 2, function(v) kmeans(irys[,v], 3))
  irys3 <- apply(komb3, 2, function(v) kmeans(irys[,v], 3))
  irys4 <- apply(komb4, 2, function(v) kmeans(irys[,v], 3))
  
  # Przeliczenie skutecznoœci K-means
  ACC22 <- rbind(ACC22,sapply(c(1:length(irys2)), function(v) round(CM.large(irys$Species, irys2[[v]]$cluster,0),4)))
  ACC33 <- rbind(ACC33,sapply(c(1:length(irys3)), function(v) round(CM.large(irys$Species, irys3[[v]]$cluster,0),4)))
  ACC44 <- rbind(ACC44,sapply(c(1:length(irys4)), function(v) round(CM.large(irys$Species, irys4[[v]]$cluster,0),4)))
}  
  ACC2 <- apply(ACC22, 2, function(v) max(v))
  ACC3 <- apply(ACC33, 2, function(v) max(v))
  ACC4 <- apply(ACC44, 2, function(v) max(v))
  
  name2 <- funky(komb2)
  name3 <- funky(komb3)
  name4 <- funky(komb4)

  length(ACC3) = length(ACC2)
  length(ACC4) = length(ACC2)
  length(name3) = length(ACC2)
  length(name4) = length(ACC2)

  tab <- matrix(c(name2,ACC2,name3,ACC3,name4,ACC4), nrow=6, byrow=TRUE)
  tab[is.na(tab)] <- ""

  colnames(tab) <- c(" ","","","","","")
  rownames(tab) <- c("Przypadek 2:","ACCuracy: ", "Przypadek 3:", "ACCuracy: ", "Przypadek 4:", "ACCuracy: ")
  
  cat("\n")
  print("ACC po 100 u¿yciach metody K_means:")
  print(as.matrix(noquote(tab)))      # Dla ³adnego wyœwietlenia bez cudzys³owów

