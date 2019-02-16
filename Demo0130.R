#Project2
#Part I
data <- read.table(file.choose(),header = T)
summary(manova(cbind(data$math,data$reading) ~ data$sex),test="Hotelling")
#Part II
library(tidyverse)
library(psych)
sport <- c('B','B','B','B','B','T','T','T','T','S','S','S','S','S','S') 
height <- c(66,65,68,64,67,63,61,62,60,62,65,63,62,63.5,66) 
jump<-c(27,29,26,29,29,23,26,23,26,23,21,21,23,22,21.5)
sports <- cbind(height,jump)
sports.manova <- manova(sports ~ sport)
summary(sports.manova, test="Wilks")# MANOVA Test
chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n)/(n+1)
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S) 
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances", xlab = "Chi-square quantile", lwd=2,pch=1)
}
chisplot(residuals(sports.manova))
sports_new <- cbind.data.frame(sport,height,jump)
Basketball <- sports_new %>%
  filter(sport == "B")
Track <- sports_new %>%
  filter(sport == "T")
Softball <- sports_new %>%
  filter(sport == "S")
Basketball <- Basketball %>%
  mutate(height_ave = cummean(height),
         jump_ave = cummean(jump))
Track <- Track %>%
  mutate(height_ave = cummean(height),
         jump_ave = cummean(jump))
Softball <- Softball %>%
  mutate(height_ave = cummean(height),
         jump_ave = cummean(jump))
rbind(Basketball[5,c(1,4,5)],Track[4,c(1,4,5)],Softball[6,c(1,4,5)])# Mean Vector
#HW3
#6.27
method1 <- read.csv(file.choose(),header = T)
method2 <- read.csv(file.choose(),header = T)
method3 <- read.csv(file.choose(),header = T)
method1.bar <- colMeans(method1[2:5]) 
method2.bar <- colMeans(method2[2:5]) 
method3.bar <- colMeans(method3[2:5])
method.all.bar <- (method1.bar + method2.bar + method3.bar)/3
method1.bar.diff <- method1.bar - method.all.bar 
method2.bar.diff <- method2.bar - method.all.bar 
method3.bar.diff <- method3.bar - method.all.bar
H <- 12 * unname(method1.bar.diff %*% t(method1.bar.diff) + method2.bar.diff %*% t(method2.bar.diff) + method3.bar.diff %*% t(method3.bar.diff))
compute.within.matrix <- function(data, mean) { 
  ret <- matrix(as.numeric(0), nrow = 4, ncol = 4)
  for (i in 1:12) {
  diff <- as.numeric(unname(data[i,] - mean))
  ret <- ret + diff %*% t(diff) }
  return(ret) }
E <- compute.within.matrix(method1[2:5], method1.bar) + 
  compute.within.matrix(method2[2:5], method2.bar) + 
  compute.within.matrix(method3[2:5], method3.bar)
Lambda <- det(E) / det(E + H)
V.s <- tr(solve(E + H) %*% H)
U.s <- tr(solve(E) %*% H)
lambda.1 <- eigen(solve(E) %*% H)$values[1] 
theta <- lambda.1 / (1 + lambda.1)
cbind(Lambda,V.s,U.s,theta)
#6.28
snap <- read.csv(file.choose())
S1 <- snap[1:15,4:7]
S2 <- snap[16:30,4:7]
S3 <- snap[31:45,4:7]
S4 <- snap[46:60,4:7]
S1.bar <- colMeans(S1)
S2.bar <- colMeans(S2)
S3.bar <- colMeans(S3)
S4.bar <- colMeans(S4)
S.all.bar <- (S1.bar + S2.bar + S3.bar + S4.bar)/4
S1.bar.diff <- S1.bar - S.all.bar 
S2.bar.diff <- S2.bar - S.all.bar 
S3.bar.diff <- S3.bar - S.all.bar
S4.bar.diff <- S4.bar - S.all.bar
H <- nrow(S1) * unname(S1.bar.diff %*% t(S1.bar.diff) + 
                   S2.bar.diff %*% t(S2.bar.diff) + 
                   S3.bar.diff %*% t(S3.bar.diff) +
                   S4.bar.diff %*% t(S4.bar.diff))
E <- compute.within.matrix(S1, S1.bar) + 
  compute.within.matrix(S2, S2.bar) + 
  compute.within.matrix(S3, S3.bar) +
  compute.within.matrix(S4, S4.bar)
Lambda <- det(E) / det(E + H)
V.s <- tr(solve(E + H) %*% H)
U.s <- tr(solve(E) %*% H)
lambda.1 <- eigen(solve(E) %*% H)$values[1] 
theta <- lambda.1 / (1 + lambda.1)
cbind(Lambda,V.s,U.s,theta)
#Interaction
V1.1 <- S1[1:5,]
V1.2 <- S1[6:10,]
V1.3 <- S1[11:15,]
V2.1 <- S2[1:5,]
V2.2 <- S2[6:10,]
V2.3 <- S2[11:15,]
V3.1 <- S3[1:5,]
V3.2 <- S3[6:10,]
V3.3 <- S3[11:15,]
V4.1 <- S4[1:5,]
V4.2 <- S4[6:10,]
V4.3 <- S4[11:15,]
V1.1.bar <- colMeans(V1.1)
V1.2.bar <- colMeans(V1.2)
V1.3.bar <- colMeans(V1.3)
V1.all.bar <- (V1.1.bar + V1.2.bar + V1.3.bar)/3
V1.1.bar.diff <- V1.1.bar - V1.all.bar 
V1.2.bar.diff <- V1.2.bar - V1.all.bar 
V1.3.bar.diff <- V1.3.bar - V1.all.bar
H <- nrow(V1.1) * unname(V1.1.bar.diff %*% t(V1.1.bar.diff) + 
                           V1.2.bar.diff %*% t(V1.2.bar.diff) + 
                           V1.3.bar.diff %*% t(V1.3.bar.diff) )
E <- compute.within.matrix(V1.1, V1.1.bar) + 
  compute.within.matrix(V1.2, V1.2.bar) + 
  compute.within.matrix(V1.3, V1.3.bar) 
Lambda <- det(E) / det(E + H)
V.s <- tr(solve(E + H) %*% H)
U.s <- tr(solve(E) %*% H)
lambda.1 <- eigen(solve(E) %*% H)$values[1] 
theta <- lambda.1 / (1 + lambda.1)
cbind(Lambda,V.s,U.s,theta)
#Project 3
#Problem 1
library(MASS)
library(ggplot2)
library(scales)
admission <- read.csv(file.choose())
lda1 <- MASS:::lda(De ~ ., 
         data = admission, 
         prior = c(1,1,1)/3,
         CV = F)
group <- MASS:::predict.lda(lda1,admission,method = 'plug-in')$class
tab <- table(group,admission$De)
conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]),tab[3, ]/sum(tab[3, ]))
dimnames(conCV1) <- list(Actual = c("Admit", "Border","Notadmit"), "Predicted (cv)" = c("Admit", "Border","Notadmit"))
print(round(conCV1, 3))
misclassrate1 <- 1-mean(diag(round(conCV1, 3)))
misclassrate1
newdata <- cbind.data.frame(GPA = c(3.14,3.08,2.08,3.22),GMAT = c(470,591,641,463))
plda = MASS:::predict.lda(lda1 , newdata = newdata)$class
plda
#####
lda2 <- MASS:::lda(De ~ ., 
                   data = admission, 
                   prior = c(0.5,0.25,0.25),
                   CV = F)
group <- MASS:::predict.lda(lda2,admission,method = 'plug-in')$class
tab <- table(group,admission$De)
conCV2 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]),tab[3, ]/sum(tab[3, ]))
dimnames(conCV2) <- list(Actual = c("Admit", "Border","Notadmit"), "Predicted (cv)" = c("Admit", "Border","Notadmit"))
print(round(conCV2, 3))
misclassrate2 <- 1-mean(diag(round(conCV2, 3)))
misclassrate2
plda2 = MASS:::predict.lda(lda2 , newdata = newdata)$class
plda2
#####
qda <- MASS:::qda(De ~ ., 
            data = admission, 
            prior = c(1,1,1)/3,
            CV = F)
group <- MASS:::predict.qda(qda,admission,method = 'plug-in')$class
tab <- table(group,admission$De)
conCV3 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]),tab[3, ]/sum(tab[3, ]))
dimnames(conCV3) <- list(Actual = c("Admit", "Border","Notadmit"), "Predicted (cv)" = c("Admit", "Border","Notadmit"))
print(round(conCV3, 3))
misclassrate3 <- 1-mean(diag(round(conCV3, 3)))
misclassrate3
#####
plda1 <- predict(object = lda1,
                newdata = admission)
dataset1 = data.frame(type = admission[,'De'],
                     lda = plda1$x)
prop.lda1 = lda1$svd^2/sum(lda1$svd^2)
p1 <- ggplot(dataset1) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
plot(p1)
####
plda2 <- predict(object = lda2,
                 newdata = admission)
dataset2 = data.frame(type = admission[,'De'],
                      lda = plda2$x)
prop.lda2 = lda2$svd^2/sum(lda1$svd^2)
p2 <- ggplot(dataset2) + geom_point(aes(lda.LD1, lda.LD2, colour = type, shape = type), size = 2.5) + 
  labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
       y = paste("LD2 (", percent(prop.lda[2]), ")", sep=""))
plot(p2)
