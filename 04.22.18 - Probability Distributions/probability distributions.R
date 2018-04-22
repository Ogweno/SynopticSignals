# libraries
library(ggplot2)

# binomial distribution
x1 <- 1:20
df <- data.frame(x = x1, y = dbinom(x1, 20, 0.5))

ggplot(df, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  ylab("Density") +
  ggtitle("Binomial Distribution (# trials = 20, p = 0.5")

dbinom(10, size = 20, prob = 0.5)

# normal distribution
x2<-seq(8, 22, by = .5)
y<- dnorm(x2, mean = 15, sd = 2)
df2 <- data.frame(x = x2, y = y)
ggplot(df2, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
ggtitle("Normal Distribution (mean = 15, sd = 2)")

# poisson distribution
x3 <- 0:10
df3 <- data.frame(x = x3, y = dpois(x3, lambda = 1, log = FALSE))
ggplot(df3, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  ggtitle("Poisson Distribution (lambda = 1)")

# Exponential distribution
x4 <- seq(0,100, by=1)
y4 <- dexp(x4, rate = .05, log = FALSE)
df4 <- data.frame(x = x4, y = y4)
ggplot(df4, aes(x = x, y = y)) +
  geom_bar(stat = "identity") +
  ggtitle("Exponential Distribution (rate = 0.5)")

# Geometric distribution
x5 <- seq(0,10, by=1)
y5 <- dgeom(x5, 0.5, log = FALSE)
df5 <- data.frame(x = x5, y = y5)
ggplot(df5, aes(x = x5, y = y5)) +
  geom_bar(stat = "identity") +
  ggtitle("Geometric Distribution (prob = 0.5)")
