#if else condition
x<-0
count <-0
if( x>= -1 & x <=1){
  count <- count+1
  } else {
  count <- count-1
  }
count 

#while 
counter <-1
while(counter<12){
  # use print inside a loop
  print(counter)
  counter <- counter +1
}

#range 
for (i in 1:5)
  {
  print("Hello R")
  }
for (m in 5:10)
  print(m)

#square root 
for(i in 1:20)
{
  i<-sqrt(i)
  print(i)
}

#vector
myFirstVector <- c( 2, 5, 7, 1, 4)
myFirstVector
typeof(myFirstVector)


mySecondVector <- c( "I", "am", "Sam")
mySecondVector
typeof(mySecondVector)


X <- c(3,4,5)
Y <- c(X,X,X,X)
Y


V <- 1:10
V
S <- -5:30
S
T <- 10:1
T
Mix <- c( 3:6, 24, 0:4, 31, 9:7)
Mix


A <- seq(1,10)
A

B <- seq(1,10, by=2)
B
C <- seq(5, -5, by=-2)
C


D <- seq(1,10, length=5)
D
E <- seq(0,1, length=15)
E

A <- rep("Hello", 5)
A
B <- rep(A,2)
B
C <- rep(1:5,4)
C


T <- c("Hello", 3, 67L, 6.8)
typeof(T)

X <- c(3,5,8)
typeof(X)
length(X)

w <- c ("a", "b", "c", "d","e")
w[1: 2]



A <- c(5,-2,51,42,-76,42)
B <- sort(A)
A
B

X <- c( 1, 2, 3)
Y <- c( 4, 5 , 6 )
Z <- X + Y
X
Y
Z


X <- c( 1, 2, 3)
Y <- c( 4, 5 , 6, 7, 8, 9 )
Z <- X + Y
Z


X <- c( 34,56,22,11,65,45)
Y <- mean(X)
Z <- min(X)
W <- max(X)
S <- sum(X)
V <- prod(X)
Y
Z
W
S
V



Tax <- c(32,56,44,76,89,24)
for (i in 1:6)
  if (Tax[i] > 50)
    print(i)


F<-c(45,77,20,19,101,120,212)

  C=(F-32)*5/9)
    print(C)



