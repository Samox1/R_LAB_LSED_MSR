### MSR LAB nr 4 

library(ggplot2)
rm(list=ls())

# Przyklad 6.2
df1 <- data.frame(a = 1:10, b = (1:10) + runif(10,-2,2), c = 1:10)

# Przyklad 6.3
#ggplot(df1, aes(x = a, y = b)) + geom_point()

# Przyklad 6.4
#ggplot(df1, aes(x = a, y = b)) + geom_point()
#ggplot(df1) + geom_point(aes(x = a, y = b))
#ggplot() + geom_point(data = df1, aes(x = a, y = b))
g <- ggplot(df1, aes(x = a)) + geom_point(aes(y = b))
print(g)

