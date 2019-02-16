data <- read.csv(file.choose())
rt <- data["Return"]
rt <- na.omit(rt)
####Define MLE
MLE_mu <- function(x){
  T = length(x) 
  s = sum(x) 
  return(s/T)
}
######
MLE_sigma <- function(x){
  T = length(x)
  mu = MLE_mu(x)
  s = sum( (x - mu)^2 ) 
  sigma_squared = s/T
  return(sqrt(sigma_squared))
}
####calculate GBM mu and sigma.
sigma <- MLE_sigma(rt)
mu <- MLE_mu(rt)+0.5*sigma^2
###
m <- 100 
n <- 1000
mu <- mu 
sigma <- sigma 
s0 <- 2.79
T1 <- 15 
set.seed(20181203) 
MotiBr <- matrix(rnorm(m*n, 0, sqrt(T1/n)), n, m) 
MotiBr <- apply(MotiBr, 2, diffinv) 
temp <- (0:n)/n*T1 
MotiBr <- (mu - sigma^2/2)*temp + sigma*MotiBr 
MotiBr <- t(s0*exp(MotiBr))
##
dat <- data.frame(val = as.vector(MotiBr), 
                  idx = seq(nrow(MotiBr)), 
                  time = rep(seq(ncol(MotiBr)), each = nrow(MotiBr)))
#
library(ggplot2)
cols <- c("red", "blue", "green", "orange")
ggplot(dat, aes(x = time, y = val, group = idx)) + 
  geom_line() + scale_colour_manual(values = cols)
#Take mean as the value of stock in Nov.15
mean(MotiBr[m+1])