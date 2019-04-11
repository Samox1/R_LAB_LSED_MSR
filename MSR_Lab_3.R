# MSR LAB 3

rm(list=ls())

### Wczytywanie danych ###

#x <- scan(what="character")

### SQL ###

library(RMySQL)

user <- "test"; pass <- "CSARuser"
con <- dbConnect(MySQL(), host="194.29.174.45", user=user, password=pass, dbname="CSAR")
q <- dbSendQuery(con, "SELECT * FROM table_test;")
data <- fetch(q)
data
is.data.frame(data)
dbClearResult(q)
q <- dbSendQuery(con, "SELECT * FROM table_test WHERE salary > 1500;")
data1 <- fetch(q)
data1
dbDisconnect(con)


### Zapisywanie danych ###

#dff <- data.frame(x=1:3, names=c("Aaaa", "Bbbb", "Ccc"))
#write.table(dff, "test4.dat")
A <- matrix(1:10, 2, 5)
write.table(A, "test5.dat")
write.table(A, "test6.dat", row.names=F, col.names=F)

#save(dff, file="df")
#ls()
#rm(dff)
#ls()
#dff
#load("df")
#dff


### Regresja Liniowa ###

rm(list=ls())

x <- 1:20
y <- 2*x-2 + runif(length(x), -3, 3)
plot(x,y)
xy.lm <- lm(y ~ x)
summary(xy.lm)
xy.lm$coefficients
b <- xy.lm$coefficients[1]
a <- xy.lm$coefficients[2]
plot(x, y, pch=19)
lines(x, a*x+b, col="red", lwd=2)

rm(list=ls())

x <- c(1,2,5,10,20,50,100,200,500,1000)
y <- (x*(1+runif(length(x),-0.9,0.9)))^(-2)
plot(x, y, log="xy")
xy.lm <- lm(log(y) ~ log(x))
summary(xy.lm)
b <- exp(xy.lm$coefficients[1])
a <- xy.lm$coefficients[2]
plot(x, y, pch=19, log="xy")
lines(x, b*x^a, col="red", lwd=2)


### BINOWANIE DANYCH ###

rm(list=ls())
library(fields)

x <- 1:100
y <- x + runif(length(x), -20, 20)
stats.bin(x, y)

xy.sb <- stats.bin(x, y)
plot(x, y)
with(xy.sb, points(centers, stats[2,], col="red", pch=19, cex=1.5))

## Slupki bledow ##

library(Hmisc)
x.sb <- xy.sb$centers
y.sb <- xy.sb$stats[2,]
e.sb <- xy.sb$stats[3,]
N.sb <- xy.sb$stats[1,]
errbar(x.sb, y.sb, y.sb+e.sb/sqrt(N.sb), y.sb-e.sb/sqrt(N.sb))
xy.lm <- lm(y.sb ~ x.sb)
a.xy <- xy.lm$coefficients[2]
b.xy <- xy.lm$coefficients[1]
errbar(x.sb, y.sb, y.sb+e.sb/sqrt(N.sb), y.sb-e.sb/sqrt(N.sb))
points(x, y, col="gray", pch=19)
lines(x, a.xy*x+b.xy, col="red", lwd=3, lty=3)


### Dystrybuanta Empiryczna ###

rm(list=ls())

x <- c(1,1,1,2,5,6,3,7,8,10)
plot(ecdf(x))

get.x <- function(x) {
  return(seq(min(x), max(x), length.out = 100))
}

make.plots <- function(N) {
  x <- rnorm(N, 0, 1)
  plot(ecdf(x), main = N)
  xx <- get.x(x)
  lines(xx, pnorm(xx, 0, 1), col = "red", lwd = 2)  
}

par(mfrow = c(2,2))
N <- c(10, 50, 100, 500)
sapply(N, make.plots)