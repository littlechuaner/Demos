#
library(tidyverse)
library(FNN)
library(caret)
personal_loan <- read.csv('/Users/chuan/Desktop/WorkingOn/personal_loan.csv')
temp <- personal_loan
temp$securities_account <- as.factor(tempn$securities_account)
temp$cd_account <- as.factor(temp$cd_account )
temp$online <- as.factor(temp$online)
temp$credit_card <- as.factor(temp$credit_card)
summary(temp)
#
personal_loan[c(1:5,7)] <- scale(personal_loan[c(1:5,7)], center = TRUE, scale = TRUE)
model.matrix(~education-1,personal_loan) %>% as.data.frame()
temp2 <- cbind(select_if(personal_loan,is.numeric),as.data.frame(model.matrix(~education-1,personal_loan)))
temp2 <- cbind.data.frame(temp2,personal_loan[12])
#
temp2 = temp2 %>%
  mutate(id = 1:nrow(temp2))
#
set.seed(30)
train = temp2 %>%
  sample_frac(0.6)
validation = temp2 %>%
  slice(-train$id)
#
validation = validation %>%
  mutate(loan_prediction = knn(train[1:13], validation[1:13],train$personal_loan, 3))
#
validation %>%
  select(personal_loan, loan_prediction)
#
confusionMatrix(validation$loan_prediction, validation$personal_loan)
#
# loop to estimate accuracy estimation for each k running from 1 to 14
for (k in 1:20){
  results = knn(train[1:13], validation[1:13],train$personal_loan, k)  # predict the output in validation table for different k
  
  a = confusionMatrix(results,validation$personal_loan)$overall[1]    # retrieve the accuracy measure from the confusion matrix and assign to a
  
  print(paste("The accuracy for k = ", k, "is", a)) # print the value of accuracy for each k
 }
#
newobs <- data.frame(age = 40,
            experience = 10,
            income = 84,
            family = 2,
            ccavg = 2,
            educationadvanced = 0,
            educartiongraduate = 1,
            educatrionundergrad = 0,
            mortgage = 0,
            securities_account = 0, 
            cd_account = 0,
            online = 1,
            credit_card = 1)
#
knn(train[1:13], newobs,train$personal_loan, 6)
#
Tayko <- read.csv('/Users/chuan/Desktop/WorkingOn/Tayko.csv')
Tayko <- Tayko %>%
  mutate(web = as.factor(web),
         gender = as.factor(gender),
         address_res = as.factor(address_res),
         address_us = as.factor(address_us)
         )
summary(Tayko)
#
Tayko %>%
  group_by(web) %>%
  summarise(std_income = sd(spending)) %>%
  ungroup()
#
Tayko %>%
  group_by(gender) %>%
  summarise(std_income = sd(spending)) %>%
  ungroup()
#
Tayko = Tayko %>%
  mutate(id = 1:nrow(Tayko))
#
set.seed(30)
train = Tayko %>%
  sample_frac(0.7)
validation = Tayko %>%
  slice(-train$id)
#
mlr = lm(spending~., train[1:7])
summary(mlr)
#
validation = validation %>%
  mutate(spending_prediction = predict(mlr, validation[1:7]))
names(validation)
#
error = validation$spending - validation$spending_prediction
error_by_y = error/validation$spending
# mean error (ME)
mean(error) 
# mean absolute error (MAE)
mean(abs(error))
# mean percentage error (MPE)
mean(error_by_y) * 100
# mean absolute percentage error (MAPE)
mean(abs(error_by_y)) * 100
# root mean square error (RMSE)
sqrt(mean(error^2))
#10-Fold CV
folds <- createFolds(Tayko$id,k = 10)
RMSE <- rep(0,10)
MAPE <- rep(0,10)
for(i in 1:10){
  fold_test <- Tayko[folds[[i]],]
  fold_train <- Tayko[-folds[[i]],]
  fold_mlr <- lm(spending~.,data = fold_train)
  fold_predict <- predict(fold_mlr,type = 'response',newdata = fold_test)
  error = fold_test$spending - fold_predict
  error_by_y = error/fold_test$spending
  RMSE[i] = sqrt(mean(error^2))
  MAPE[i] = mean(abs(error_by_y)) * 100
}
print(mean(RMSE))
print(mean(MAPE))
