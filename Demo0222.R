library(tidyverse)
#load data.
Netherlands <- read.csv(file.choose())
#data cleaning.
Netherlands.new <- Netherlands %>%
  dplyr:::select(year,EYR,IYR) %>%
  mutate(Bal = EYR - IYR)
#divide data by training set and test set.
tsdata <- ts(Netherlands.new$Bal, frequency = 1,start = 1985)
train <- tsdata[1:26]
test <- tsdata[27:34]
#visualization.
p <- ggplot(Netherlands.new,aes(x = year,y = Bal)) + 
     geom_line() + 
     labs(x = "Year", y = "Trade Balance", title = "Trade Balance of Netherland")
plot(p)
######
p <- ggplot() + 
  geom_line(data = Netherlands.new, aes(x = year, y = EYR, linetype = "Export")) +
  geom_line(data = Netherlands.new, aes(x = year, y = IYR, linetype = "Import"))  +
  labs(x = "Year", y = "Export & Import Amount", title = "Trade Amount of Netherland", 
       linetype = "Trade")
plot(p)
#model fitting.
t <- time(train)
par(mfrow=c(1,1))
reg1 <- lm(train~t) # model quadratic trend
R1 <- AIC(reg1)
summary(reg1)
plot(train, main = "Time Series with Linear Trend", ylab = "Trade Balance",
     xlab = 'Time')
points(t,predict.lm(reg1),type='l',col='red')
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg1$fitted, reg1$residuals,main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
qqnorm(reg1$residuals) #qq-plot of residuals
qqline(reg1$residuals,col="blue") # plotting the line, along which the dots in qq-plot should lie
plot(reg1$residuals,main = "Residuals vs Time", ylab = "Residuals") # plotting the residuals vs time
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
acf(reg1$residuals,main="ACF plot of Residuals")#sample acf plot of residuals
###
reg2 <- lm(train~t + I(t^2))# model quadratic trend
R2 <- AIC(reg2)
summary(reg2)
par(mfrow=c(1,1))
plot(train, main = "Time Series with Quadratic Trend",ylab = "Trade Balance",
     xlab = 'Time')
points(t,predict.lm(reg2),type='l',col='red') # superimpose the fit of model reg2 on the plot of the data
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg2$fitted, reg2$residuals,main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
qqnorm(reg2$residuals) #qq-plot of residuals
qqline(reg2$residuals,col="blue") # plotting the line, along which the dots in qq-plot should lie
plot(reg2$residuals,main = "Residuals vs Time", ylab = "Residuals") # plotting the residuals vs time
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
acf(reg2$residuals,main="ACF plot of Residuals")#sample acf plot of residuals
###
reg3 <- lm(train~t + I(t^2) + I(t^3))
summary(reg3)
R3 <- AIC(reg3)
list(summary(reg1)$'coefficients',
     summary(reg2)$'coefficients',
     summary(reg3)$'coefficients')
par(mfrow=c(1,1))
plot(train, main = "Time Series with Cubic Trend",ylab = "Trade Balance",
     xlab = 'Time')
points(t,predict.lm(reg3),type='l',col='red')
par(mfrow=c(2,2)) # Dividing the plotting page into 4 panels
plot(reg3$fitted, reg3$residuals,main="Residuals vs Fitted", xlab = "Fitted", ylab = "Residuals") # plot of fitted values vs residuals
qqnorm(reg3$residuals) #qq-plot of residuals
qqline(reg3$residuals,col="blue") # plotting the line, along which the dots in qq-plot should lie
plot(reg3$residuals,main = "Residuals vs Time", ylab = "Residuals") # plotting the residuals vs time
abline(h=0,lty=2,col ="blue") # plotting a horizontal line at 0
acf(reg3$residuals,main="ACF plot of Residuals")#sample acf plot of residuals
Rsquared <- cbind.data.frame(Rsquared = c(R1,R2,R3), 
                         model = c("Linear",'Quadradic','Cubic'))
p <- ggplot(Rsquared, aes(x = model, y = Rsquared, fill = model)) + 
  geom_bar(stat = "identity",width = 0.3) + 
  labs(title = "AIC") + 
  scale_fill_brewer(palette="Dark2")
plot(p)
#forecasting.
t.new <- seq(27,34,length=8)[1:8] # Intoducing new time for forecatsting 
t2.new <- t.new^2
t3.new <- t.new^3
###
new <- data.frame(t=t.new) # Putting the values for forecasting into a dataframe
pred <- predict.lm(reg1,new,interval='prediction')# Computing the prediction as well as prediction interval
values1 <- pred[,1]
par(mfrow=c(1,1))
plot(train,xlim=c(1,35),ylim=c(0,40000),main = "2011 Forecast with Linear Trend") #plotting the data
abline(v=27,col='blue',lty=2) # adding a vertical line at the point where prediction starts
lines(pred[,1]~t.new,type='l',col='red')# plotting the predict
lines(pred[,2]~t.new,col='green') # plotting lower limit of the prediction interval
lines(pred[,3]~t.new,col='green') # plotting upper limit of the  prediction interval
###
new <- data.frame(t=t.new, t2=t2.new) # Putting the values for forecasting into a dataframe
pred <- predict.lm(reg2,new,interval='prediction')# Computing the prediction as well as prediction interval
values2 <- pred[,1]
par(mfrow=c(1,1))
plot(train,xlim=c(1,35),ylim=c(0,40000),main = "2011 Forecast with Quaradic Trend") #plotting the data
abline(v=27,col='blue',lty=2) # adding a vertical line at the point where prediction starts
lines(pred[,1]~t.new,type='l',col='red')# plotting the predict
lines(pred[,2]~t.new,col='green') # plotting lower limit of the prediction interval
lines(pred[,3]~t.new,col='green') # plotting upper limit of the  prediction interval
###
new <- data.frame(t=t.new, t2=t2.new, t3=t3.new) # Putting the values for forecasting into a dataframe
pred <- predict.lm(reg3,new,interval='prediction')# Computing the prediction as well as prediction interval
values3 <- pred[,1]
par(mfrow=c(1,1))
plot(train,xlim=c(1,35),ylim=c(0,40000),main = "2011 Forecast with Cubic Trend") #plotting the data
abline(v=27,col='blue',lty=2) # adding a vertical line at the point where prediction starts
lines(pred[,1]~t.new,type='l',col='red')# plotting the predict
lines(pred[,2]~t.new,col='green') # plotting lower limit of the prediction interval
lines(pred[,3]~t.new,col='green') # plotting upper limit of the  prediction interval
###
res1 <- values1 - test
MSPE1 <- mean(res1^2)
res2 <- values2 - test
MSPE2 <- sum(res2^2) / 8
res3 <- values3 - test
MSPE3 <- sum(res3^2) / 8
#plot MSPE.
MSPE <- cbind.data.frame(MSPE = c(MSPE1,MSPE2,MSPE3), 
                         model = c("Linear",'Quadradic','Cubic'))
p <- ggplot(MSPE, aes(x = model, y = MSPE, fill = model)) + 
  geom_bar(stat = "identity",width = 0.3) + 
  labs(title = "Mean squared prediction errors") + 
  scale_fill_brewer(palette="Dark2")
plot(p)
