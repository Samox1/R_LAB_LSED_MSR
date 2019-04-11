### MSR Laboratorium nr 6 

rm(list=ls())

# Przyk³ad 6.1

x <- c(100,50,20,10,5)
p1 <- c(0.5, 0.2, 0.2, 0.05, 0.05)
chisq.test(x, p = p1)

p2 <- c(0.5, 0.3, 0.1, 0.05, 0.05)
chisq.test(x, p = p2)

p <- x/sum(x)
chisq.test(x, p = p)

plot(p)
lines(p1, col="red")
lines(p2, col="blue")


# Przyk³ad 6.2

h <- hist(rpois(200, lam = 3), breaks = (0:15)-0.5)
chisq.test(h$counts, p = dpois(h$mids, lam = 3), rescale.p = TRUE)

plot(h$mids, h$density)
#lines(h$mids, dpois(h$mids, lam = 3))
lines(h$mids, dpois(h$mids, lam = 3), col="red", lwd=2)


rm(list=ls())
# Przyk³ad 6.3

x <- rpois(100, lam = 5)
#hist(x)
y <- rpois(100, lam = 2)
#ks.test(x, y)

#print(ks.test(x, "ppois", lam = 5))

x <- rnorm(100)
y <- rnorm(100)
#ks.test(x, y)

y <- rnorm(100, 0, 2)
#ks.test(x, y))

#print(ks.test(x, "pnorm", 0, 1))


# Przyk³ad 6.4

shapiro.test(rnorm(100))

shapiro.test(rnorm(100, 0, 5))

shapiro.test(runif(100, 0, 1))

wilcox.test(rnorm(100))

wilcox.test(rnorm(100, 1, 2), mu = 1)

wilcox.test(rnorm(100, 1, 2), mu = 2)


# Przyk³ad 6.5

x <- c(1,2,3,4,5)
y <- c(2,4,1,2,2)
cor(x, y)

# Przyk³ad 6.6

cor(x, y, method="spearman")
cor(rank(x), rank(y))
# Przyk³ad 6.7

z <- c(0, 1, 1, 0, 1)
df <- data.frame(x, y, z)
cor(df)


# Przyk³ad 6.8

library(corrplot)

corrplot(cor(df))

# Przyk³ad 6.9

cor.test(x, y)

# Przyk³ad 6.10

x <- c(rep("fizyk", 55), rep("humanista", 149))
y <- c(rep("nie pracuje", 7),rep("pracuje", 48),rep("nie pracuje", 45),rep("pracuje", 104))
T <- table(x, y)
T

chisq.test(T)

Te <- chisq.test(T)$expected
Te
Te / sum(Te)

# Przyk³ad 6.12

Tx <- apply(T, 1, sum) / sum(T)
Ty <- apply(T, 2, sum) / sum(T)
Tx %o% Ty

T / sum(T)