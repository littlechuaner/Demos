## ---------------------------------------------
## Q1
## ---------------------------------------------
## 1.1
library(pracma)
MCIntegral1 <- function(n, a, b){
  X <- runif(n, a, b)
  #g(x) ~ Phi(2,0.5)
  Y <- sqrt(pi) * X^2
  Int <- sum(Y)/n
  f <- function(x) ( x^2 * exp(-(x-2)^2) )
  True <- quad(f, a, b) 
  Error <- abs(Int-True)
  list("Int"=Int,"Error"=Error)}
#Run function
MCIntegral1(100000, 1, 3)
#The integration value is 7.685598 with an error of 1.332.
##---------------------------------------------
##1.2
MCIntegral2 <- function(n, a, b){
  X <- runif(n, a, b)
  #g(x) ~ U(a,b)
  Y <- ( (X^5) * log(X))/(1/(b-a))
  Int <- sum(Y)/n
  f <- function(x){(X^5) * log(X)}
  True <- quad(f, a, b) 
  Error <- abs(Int-True)
  list("Int"=Int,"Error"=Error)}
#Run
MCIntegral2(100000, 1, 5)
##The value is 3743.138, with an error of 1043.29
##---------------------------------------------
##1.3
MCIntegral3 <- function(n, a, b){
  X <- runif(n, a, b)
  #g(x) ~ U(a,b)
  Y <- (4/( 1+X^2 ))*(b-a)
  Int <- sum(Y)/n
  True <- pi 
  Error <- abs(Int-True)
  list("Int"=Int,"Error"=Error)}
##
MCIntegral3(10,0,1)
MCIntegral3(100,0,1)
MCIntegral3(1000,0,1)
##The results are [3.07,-0.07], [3.04,-0.09], [3.13,-0.008], with the increase of n, the simulated
##value is closer to pi.
##---------------------------------------------
## Q2
##---------------------------------------------
##2.1
## 
##---------------------------------------------
##2.2
y <- c( 0.48,  0.50, -0.86, -0.83, -0.32, -1.30, -1.42,
        1.74, -0.29, -1.31, -0.07, -1.22,  3.24, -1.97,
        1.81,  4.00,  1.87,  1.50,  6.81, -4.14)
x1 <- y[1:10]
x2 <- y[11:20]
var.test(x1,x2)
##P-value is 0.0022, which indicates two samples have different variances.
##---------------------------------------------
##2.3
##prepare a box for p-values.
p <- rep(1,20)
##get all the p-values.
for (i in 2:19) {
  x1 <- y[1:i]
  x2 <- y[i:20]
  p[i] <- var.test(x1,x2)$p.value }
##Find the smallest p-value.
which.min(p)
##From the results, when k = 12, p-value is the smallest.
##---------------------------------------------
##2.4
##find the number of possible rearrangements.
y1 <- y[1:12]
y2 <- y[13:20]
n1 <- length(y1)
n2 <- length(y2)
y <- c(y1,y2) #merge the two groups together
T <- var(y1)/var(y2) #observed test statisti
N <- 10000 #number of simulations
statistics <- numeric(N)
for (s in 1:N) {
  group1indexes <- sample(1:(n1+n2), n1, replace=FALSE)
  group1 <-y[group1indexes]
  group2 <- y[-group1indexes]
  statistics[s] <- var(group1)/var(group2)
}
length(which(statistics>T))/length(statistics)
##the output p-value is 0.999, so we should conclude null hypothesis.

