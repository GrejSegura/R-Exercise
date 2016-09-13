##THIS IS AN EXERCISE FOR REGRESSION

##DATA USED IS THE WHITE WINE DATA

##DEPENDENT VARIABLE IS ALCOHOL

# FINDINGS: BASED ON THE METHODS USED BELOW, XGBOOST PROVIDES THE LEAST RMSE, FOLLOWED BY AN UNTUNED SVM

set.seed(3211)
# library(data.table)
library(glmnet)
library(ggplot2)

?data.table
#load the data
wine_data <- read.csv(file.choose())

str(wine_data)

#this exercise will attempt to model the alcohol content of white wine

#scale the data
length(wine_data)
wine_scaled <- scale(wine_data[,1:10]) #do not include alcohol and quality

str(wine_scaled)
colnames(wine_scaled)


#create dummy variables for quality
wine_data$quality <- as.factor(wine_data$quality)

quality_dummy <- model.matrix(~quality-1, wine_data) 
str(quality_dummy)

#bind the dummy to the orig data set

wine_final <- data.frame(wine_scaled, quality_dummy[,-3], wine_data[,11]) #remove quality = 5 dummy to avoid dummy trap

names(wine_final)[17] <- 'alcohol'

str(wine_final)
head(wine_final)

#create training and test data

t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.8))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]

#############################
#############################

#use glmnet
#train the regression model using ?glmnet

#create training/test data specifically for glmnet
train_a <- train_wine[,1:16]
train_b <- train_wine[,17]
test_a <- test_wine[,1:16]
test_b <- test_wine[,17]

str(train_a)

##ridge
ridge_wine <- cv.glmnet(as.matrix(train_a), train_b, family = 'gaussian', alpha = 0)

summary(ridge_wine)

#predict ridge

ridge <- predict(ridge_wine, s = ridge_wine$lambda.1se, newx = as.matrix(test_a))

mse_ridge_wine <- mean((test_b - ridge)^2)
sqrt(mse_ridge_wine)

#plot residuals and prediction

ridge_resid_wine <- test_b - ridge

plot(ridge_resid_wine)

#make a dataframe for plotting
plot_ridge <- data.frame(Series = c(1:length(test_b)), test_b, ridge)
str(plot_ridge)

ggplot(plot_ridge, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)

#################################
##################################

#lasso
lasso_wine <- cv.glmnet(as.matrix(train_a), train_b, family = 'gaussian', alpha = 1)

#predict lasso

lasso <- predict(lasso_wine, s = lasso_wine$lambda.1se, newx = as.matrix(test_a))

mse_lasso <- mean((test_b - lasso)^2)
sqrt(mse_lasso)

#plot residuals

lasso_resid <- test_b - lasso

plot(lasso_resid)

#make a dataframe for plotting
plot_lasso <- data.frame(Series = c(1:length(test_b)), test_b, lasso)
str(plot_lasso)

ggplot(plot_lasso, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)

################################
##############################
#elastic net alpha = 0.1 - 0.9

n = seq(0.1:0.9, by = 0.1)
i = 0.1
for (i in n){
  
  assign(paste('elastic', i, sep = ''), cv.glmnet(as.matrix(train_a), train_b, family = 'gaussian', alpha = i))
  
}

#predict elasticnet

yhat0.1 <- predict(elastic0.1, elastic0.1$lambda.1se, newx = as.matrix(test_a))
yhat0.2 <- predict(elastic0.2, elastic0.2$lambda.1se, newx = as.matrix(test_a))
yhat0.3 <- predict(elastic0.3, elastic0.3$lambda.1se, newx = as.matrix(test_a))
yhat0.4 <- predict(elastic0.4, elastic0.4$lambda.1se, newx = as.matrix(test_a))
yhat0.5 <- predict(elastic0.5, elastic0.5$lambda.1se, newx = as.matrix(test_a))
yhat0.6 <- predict(elastic0.6, elastic0.6$lambda.1se, newx = as.matrix(test_a))
yhat0.7 <- predict(elastic0.7, elastic0.7$lambda.1se, newx = as.matrix(test_a))
yhat0.8 <- predict(elastic0.8, elastic0.7$lambda.1se, newx = as.matrix(test_a))
yhat0.9 <- predict(elastic0.9, elastic0.9$lambda.1se, newx = as.matrix(test_a))

mselas0.1 <- mean((test_b - yhat0.1)^2) 
mselas0.2 <- mean((test_b - yhat0.2)^2)
mselas0.3 <- mean((test_b - yhat0.3)^2)
mselas0.4 <- mean((test_b - yhat0.4)^2)
mselas0.5 <- mean((test_b - yhat0.5)^2)
mselas0.6 <- mean((test_b - yhat0.6)^2)
mselas0.7 <- mean((test_b - yhat0.7)^2)
mselas0.8 <- mean((test_b - yhat0.8)^2)
mselas0.9 <- mean((test_b - yhat0.9)^2)

sqrt(mselas0.1)
sqrt(mselas0.2)
sqrt(mselas0.3)
sqrt(mselas0.4)
sqrt(mselas0.5)
sqrt(mselas0.6)
sqrt(mselas0.7)
sqrt(mselas0.8)
sqrt(mselas0.9)

###alpha = 0.6 has the best rmse


#make a dataframe for plotting
plot_elas0.6 <- data.frame(Series = c(1:length(test_b)), test_b, yhat0.6)
str(plot_elas0.6)

ggplot(plot_elas0.6, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)


#################################3
################################
#compare with OLS

lm_wine_train <- cbind(train_a, train_b)
lm_wine_test <- cbind(test_a, test_b)
head(lm_wine_train)
head(lm_wine_test)

ols <- lm(train_b~., lm_wine_train)
plot(ols)

#predict

pred_wine_lm <- predict(ols, lm_wine_test)

mselm_wine <- mean((lm_wine_test$test_b - pred_wine_lm)^2)
sqrt(mselm_wine)

#plot residuals

resid <- lm_wine_test$test_b - pred_wine_lm
plot(resid)

#########################################
#########################################
##########################################
#try using svm
library(e1071)
names(lm_wine_test)[17] <- 'train_b'

#svm_land_tune <- tune.svm(train_y~., data = lm_data, gamma = 2^(-1:1), cost = 2^(-1:1), cross = 10)

svm_wine <- svm(train_b~., data = lm_wine_train, gamma = 0.03125, cost = 32)
#this is still not tuned

predsvm_wine <- predict(svm_wine, lm_wine_test)

msesvm_wine <- mean((lm_wine_test$train_b - predsvm_wine)^2)
sqrt(msesvm_wine)


#make a dataframe for plotting
plot_svm <- data.frame(Series = c(1:length(test_b)), test_b, predsvm_wine)
str(plot_svm)

ggplot(plot_svm, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = predsvm_wine), color = 'red',  alpha = 0.6)

###################################
######################################
#using gbm in caret

library(caret)
library(gbm)
?caret

gbm_wine_train <- train(train_a, train_b, method = 'gbm')

#//
#  Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning
#parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using  the smallest value.
#The final values used for the model were n.trees = 150, interaction.depth = 3,
#shrinkage = 0.1 and n.minobsinnode = 10. //

gbm_wine <- gbm.fit(train_a, train_b, n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10, distribution = 'gaussian')

#predict

pred_gbm_wine <- predict(gbm_wine, test_a, n.trees = 150)

mse_gbm_wine <- mean((test_b - pred_gbm_wine)^2)

sqrt(mse_gbm_wine)

##plot the residual and prediction

#make a dataframe for plotting
plot_gbm_wine <- data.frame(Series = c(1:length(test_b)), test_b, pred_gbm_wine)
str(plot_gbm_wine)

ggplot(plot_gbm_wine, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = pred_gbm_wine), color = 'red',  alpha = 0.6)

####################################
####################################
#using xgboost

library(xgboost)

xg_wine <- xgboost(as.matrix(train_a), train_b, nrounds = 1000)

pred_xg_wine <- predict(xg_wine, as.matrix(test_a))

msexg_wine <- mean((test_b - pred_xg_wine)^2)

sqrt(msexg_wine)



#make a dataframe for plotting
plot_xg_wine <- data.frame(Series = c(1:length(test_b)), test_b, pred_xg_wine)
str(plot_xg_wine)

ggplot(plot_xg_wine, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = pred_xg_wine), color = 'red',  alpha = 0.6)
