N <-100000
a <-rnorm(N)
b <- rnorm(N)

c <- a * b
c


?norm

rnorm(N,mean=0, sd=2)


X <- rnorm(5)
X <- rnorm( 5, 10, 1)
X <- rnorm (5, 10)
X <- rnorm(5, mean=10, sd = 1)
X <- rnorm(5, sd=1, mean=10)
X <- rnorm(5, sd=1)

X




x <-rnorm(5000)
plot(x)
hist(x)
X

x <-rnorm(80000)
plot(x)
hist(x)
head(x)
tail(x)
range(x)
summary(x)
boxplot(x)