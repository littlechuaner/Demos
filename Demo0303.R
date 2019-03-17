#Q1
#a.
obs <- c(-0.566, 3.74, 5.55, -1.90, -3.54, 5.16, -1.76, 4.08, 4.62, 0.732)
normal.mu.posterior.mean <- function(mu.prior,mu.sd,sigma,ybar,n) {
  wt.prior <- sigma^2/(sigma^2+n*mu.sd^2)
  out <- wt.prior*mu.prior + (1-wt.prior)*ybar
  return(out)
}
normal.mu.posterior.sd <- function(mu.sd,sigma,n) {
  out <- sqrt(mu.sd^2*sigma^2/(sigma^2+n*mu.sd^2))
  return(out)
}
post.mean <- normal.mu.posterior.mean(mu.prior=1,
                                      mu.sd=0.33,sigma=sqrt(30),ybar=mean(obs),n=10)
post.sd   <- normal.mu.posterior.sd(mu.sd=0.33,sigma=sqrt(30),n=10)
cat("posterior mean for mu=",post.mean ,"\n")
cat("posterior sd for mu=",post.sd ,"\n")
#b.
mean <- ((1/3)*1)+((2/3)*5)
sd <- sqrt(0.33*0.33+0.66*0.66)
post.mean <- normal.mu.posterior.mean(mu.prior=mean,
                                      mu.sd=sd,sigma=sqrt(30),ybar=mean(obs),n=10)
post.sd   <- normal.mu.posterior.sd(mu.sd=sd,sigma=sqrt(30),n=10)
cat("posterior mean for mu=",post.mean ,"\n")
cat("posterior sd for mu=",post.sd ,"\n")
#c.
require(rjags)
model1.string <- "
  model {
for (i in 1:N){
x[i] ~ dnorm(mu, tau)
}
mu ~ dnorm(0,.0001)
tau <- pow(sigma, -2)
sigma ~ dunif(0,100)
}
"
model1.spec<-textConnection(model1.string)
jags <- jags.model(model1.spec,
                   data = list('x' = obs,
                               'N' = 10),
                   n.chains=4,
                   n.adapt=100)
update(jags, 10000)
res <- coda.samples(jags,
             c('mu', 'tau'),
             100000)
summary(res)
#d.
mu <- 1.6
sd <- sqrt(1/0.0775)
cat("Pr(p<1)=",pnorm(1,mu,sd),"\n")
#e.
require(rjags)
model1.string <- "
model {
for (i in 1:N){
x[i] ~ dnorm(mu, tau)
}
mu = w*s1 + (1-w)*s2
s1 ~ dnorm(1,1/9)
s2 ~ dnorm(5,4/9)
w ~ dunif(0,1)
tau <- pow(sigma, -2)
}
"
model1.spec<-textConnection(model1.string)
jags <- jags.model(model1.spec,
                   data = list('x' = obs,
                               'N' = 10,
                               'sigma' = sqrt(30)),
                   n.chains=1,
                   n.adapt=100)
update(jags, 1000)
res <- coda.samples(jags,
             c('mu', 'w'),
             10000)
summary(res)
plot(res)
res = do.call(rbind, res)
par(mfrow=c(1,1))
plot(res)
#2.
#a.
#b.
model_string <- "model{
# Likelihood
for(i in 1:N){
 Y[i] ~ dpois(lambda)
 }
# Prior
lambda ~ dgamma(a, b)
}"

model <- jags.model(textConnection(model_string), 
                    data = list(Y=c(1,1,1),N=3,a=5,b=0.01))
update(model, 10000, progress.bar="none")

samp <- coda.samples(model, 
                     variable.names=c("lambda"
                                      ), 
                     n.iter=20000, progress.bar="none")

summary(samp)
plot(samp)
#c.
cat("Pr(p<15)=",1 - pgamma(15,8,3.01),"\n")
#d.
model <- jags.model(textConnection(model_string), 
                    data = list(Y=c(1,1,1),N=3,a=0.5,b=0.001))
update(model, 10000, progress.bar="none")

samp <- coda.samples(model, 
                     variable.names=c("lambda"
                     ), 
                     n.iter=20000, progress.bar="none")

summary(samp)
plot(samp)
#e.
cat("Pr(p<6)=",ppois(6,17),"\n")
cat("Pr(p<1)=",ppois(1,17),"\n")
cat("Pr(p>5)=",1 - ppois(5,17),"\n")
#f.
#3.
#summary
summary(rock)
require(GGally)
require(car)
ggpairs(rock)
#linear regression
mod2 <- lm(perm~.,rock)
summary(mod2)
opar <- par(no.readOnly=TRUE)
par(mfrow = c(2, 2))
plot(mod2)
par(opar)
#Bayesian linear regression
require(rjags)
model_string <- "model{

# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],inv.var)
mu[i]=beta[1]+beta[2]*area[i]+beta[3]*peri[i]+beta[4]*shape[i]
}

# Prior for beta
for(j in 1:4){
beta[j]~dnorm(mu0,inv.var0)
}
inv.var0=1/sigma02

# Prior for the inverse variance
inv.var~dgamma(a, b)

# Compute the variance
sigma2=1/inv.var
}"

#hyperparameters for the betas and tau
mu0=0; sigma02=10000; a=0.1; b=0.1;n=nrow(rock)
# list with data and hyperparameters
data=list(y=rock$perm,area=rock$area,peri=rock$peri,shape=rock$shape,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)

#passing the model to rjags
model=jags.model(textConnection(model_string),n.chains=3,data=data)

#burn-in
update(model,100000,progress.bar="none")
result=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=500000,thin=50,progress.bar="none")
summary(result)
plot(result)
gelman.plot(result)
#fitted values
resmat=as.matrix(result)
niterf=nrow(resmat)
beta1=resmat[,1]; beta2=resmat[,2] ;beta3=resmat[,3]; beta4=resmat[,4]; sigma=sqrt(resmat[,5])
x=cbind(rep(1,n),rock$area,rock$peri,rock$shape)
H=x%*%solve((t(x)%*%x))%*%t(x)
fittedvalues=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
  fittedvalues[,l]=beta1[l]*x[,1]+beta2[l]*x[,2]+beta3[l]*x[,3]+beta4[l]*x[,4]
}

#studentised residuals
studentisedresid=matrix(0,nrow=n,ncol=niterf)
for(l in 1:niterf){
  for(i in 1:n){
    studentisedresid[i,l]=(rock$perm[i]-fittedvalues[i,l])/(sigma[l]*sqrt((1-diag(H)[i])))
  }
}

#posterior mean of studentised residuals
studentisedresidm=numeric(n)
for(i in 1:n){
  studentisedresidm[i]=mean(studentisedresid[i,])
}

#Plot of posterior mean studentised residual versus observation number.
par(mfrow=c(2,2))
plot(seq_along(studentisedresidm),studentisedresidm,xlab="Index",ylab="Bayesian studentised residual",ylim=c(-3,3))
#QQ-plot
qqnorm(studentisedresidm,xlim=c(-3,3),ylim=c(-3,3),lwd=2)
qqline(studentisedresidm,col=2,lwd=2)
#Compute posterior mean fitted values
fittedvaluesm=numeric(n)
for(i in 1:n){
  fittedvaluesm[i]=mean(fittedvalues[i,])
}
plot(fittedvaluesm,studentisedresidm,xlab="Fitted value (posterior mean)",ylab="Bayesian Studentised residual (posterior mean)")
#Sensitivity
model_string <- "model{
# Likelihood
for(i in 1:n){
y[i]~dnorm(mu[i],inv.var)
mu[i]=beta[1]+beta[2]*area[i]+beta[3]*peri[i]+beta[4]*shape[i]
}

# Prior for beta
for(j in 1:4){
beta[j]~dnorm(mu0,inv.var0)
}
inv.var0=1/sigma02

# Prior for the inverse variance
inv.var~dunif(a,b)

# Compute the variance
sigma2 = 1/inv.var
}"
#hyperparameters for the betas and tau
mu0=0; sigma02=10000; a=0;b=100000 ;n=nrow(rock)
# list with data and hyperparameters
data=list(y=rock$perm,area=rock$area,peri=rock$peri,shape=rock$shape,n=n,mu0=mu0,sigma02=sigma02,a=a,b=b)

#passing the model to rjags
model=jags.model(textConnection(model_string),n.chains=3,data=data)

#burn-in
update(model,100000,progress.bar="none")
result=coda.samples(model,variable.names=c("beta","sigma2"),n.iter=500000,thin=50,progress.bar="none")
summary(result)
plot(result)
gelman.plot(result)
