library(foreign)
setwd('/Users/chuan/Desktop/WorkingOn')
BHPSw18indresp<-read.spss('rindresp.sav',to.data.frame=TRUE,
                           use.value.labels = TRUE,use.missings=FALSE)
attach(BHPSw18indresp)
#data transformation
levels(rlfsato)
rlfsato.num <-as.numeric(rlfsato)
rlfsato.num [rlfsato.num<=10]<-NA
table(rlfsato.num)
satisfaction<-as.factor(rlfsato.num)
levels(satisfaction)<-c('Not satisfied at all','dissatistied','Completely satisfied')
table(ssatisfaction)
#
rpayu1 <- as.character(rpayu)
income <- as.numeric(rpayu1)
table(income)
#
levels(rlocserb)
rlocserb.num <-as.numeric(rlocserb)
rlocserb.num[rlocserb.num<=5]<-NA
rlocserb.num [rlocserb.num==6 | rlocserb.num==7]<-8
table(rlocserb.num)
medical<-as.factor(rlocserb.num)
levels(medical)<-c('Good','Poor')
table(medical)
#
levels(rlocserc)
rlocserc.num <-as.numeric(rlocserc)
rlocserc.num[rlocserc.num<=5]<-NA
rlocserc.num [rlocserc.num==6 | rlocserc.num==7]<-8
table(rlocserc.num)
transport<-as.factor(rlocserc.num)
levels(transport)<-c('Good','Poor')
table(transport)
#
levels(rmlstat)
rmlstat.num <-as.numeric(rmlstat)
table(rmlstat.num)
rmlstat.num [rmlstat.num<6]<-NA
rmlstat.num [rmlstat.num==6]<-1
rmlstat.num [rmlstat.num>=7 & rmlstat.num<=14]<-0
table(rmlstat.num)
marriage<-as.factor(rmlstat.num)
levels(marriage)<-c('unmarried','married')
table(marriage)
#
levels(rsex)
rsex.num <-as.numeric(rsex)
table(rsex.num)
rsex.num [rsex.num<6]<-NA
rsex.num [rsex.num==6]<-1
rsex.num [rsex.num==7]<-0
sex<-as.factor(rsex.num)
levels(sex)<-c('male','female')
table(sex)
#
satisfaction1 = ordered(satisfaction, levels=c('Not satisfied at all','dissatistied','Completely satisfied'))
#
library(MASS)
fit.pom = polr(satisfactory1 ~ income + sex, data=BHPSw18indresp)
summary(fit.pom)
coef(fit.pom)
library(arm)
p.pom = function(X, fit){
  b = coef(fit)
  cuts = fit$zeta
  p1 = 1 - invlogit(X%*%b - cuts[1]) 
  p2 = invlogit(X%*%b - cuts[1]) -
    invlogit(X%*%b - cuts[2])
  p3 = invlogit(X%*%b - cuts[2]) 
  cbind(p1, p2, p3)
}
par(mfrow=c(1,2), cex=0.8)
#Sex
plot(c(1,40000), c(0,1), type='n', main='Male',
     xlab='income', ylab='prob') 
curve(p.pom(cbind(x,0), fit.pom)[,1], add=TRUE, lty=1)
curve(p.pom(cbind(x,0), fit.pom)[,2], add=TRUE, lty=2) 
curve(p.pom(cbind(x,0), fit.pom)[,3], add=TRUE, lty=3)
legend('topleft', lty=1:3, c('Not satisfied at all','Dissatistied','Completely satisfied'),bty='n')  

plot(c(1,40000), c(0,1), type='n', main='Female',
     xlab='income', ylab='prob') 
curve(p.pom(cbind(x,1), fit.pom)[,1], add=TRUE, lty=1)
curve(p.pom(cbind(x,1), fit.pom)[,2], add=TRUE, lty=2) 
curve(p.pom(cbind(x,1), fit.pom)[,3], add=TRUE, lty=3)
legend('topleft', lty=1:3, c('Not satisfied at all','Dissatistied','Completely satisfied'),bty='n')
#
myDF <- data.frame(satisfaction1, income, sex)
myDF <- na.omit(myDF)
x.lo <- myDF
x.lo$income = min(myDF$income)
x.hi = myDF
x.hi$jobsatisfaction = max(myDF$income,na.rm = T)
delta = predict(fit.pom, x.hi, type='probs') -
  predict(fit.pom, x.lo, type='probs')
colMeans(delta)
#
# predicted probabilities
fitted(fit.pom)
p.sat = fitted(fit.pom)[,"Completely satisfied"]
p.dis = fitted(fit.pom)[,"dissatistied"]
p.not = fitted(fit.pom)[,"Not satisfied at all"]
#observed outcomes as binary variables
y.sat = ifelse(myDF$satisfaction1=="Completely satisfied", 1, 0) 
y.dis = ifelse(myDF$satisfaction1=="dissatistied", 1, 0) 
y.not = ifelse(myDF$satisfaction1=="Not satisfied at all", 1, 0) 
# residuals = observed - predicted
r.sat = y.sat - p.sat
r.dis = y.dis - p.dis
r.not = y.not - p.not
#
library(arm) # for the binnedplot() function 
par(mfrow=c(1,3), mar=c(3,4,2,1), mgp=c(2,.7,0), cex=0.6)
binnedplot(p.sat, r.sat, nclass=7,xlab="Predicted probability", main="Completely satisfied") 
binnedplot(p.dis, r.dis, nclass=7, xlab="Predicted probability", main="Dissatistied") 
binnedplot(p.not, r.not, nclass=7,xlab="Predicted probability", main="Not satisfied at all")
#
par(mfrow=c(1,3), mar=c(3,4,2,1), mgp=c(2,.7,0), cex=0.6)
binnedplot(myDF$income, r.sat, nclass=7,xlab="Income", main="Completely satisfied") 
binnedplot(myDF$income, r.dis, nclass=7, xlab="Income", main="Dissatistied") 
binnedplot(myDF$income, r.not, nclass=7,xlab="Income", main="Not satisfied at all")
