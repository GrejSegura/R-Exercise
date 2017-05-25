##THIS IS AN EXERCISE FOR REGRESSION

##DATA USED IS THE LAND DATA

##DEPENDENT VARIABLE IS HOME VALUE


set.seed(321)
# library(data.table)
library(glmnet)
library(ggplot2)

?data.table
#load the data
land_data <- read.csv(file.choose())

# land_data_table <- data.table(land_data)


str(land_data)

#this exercise will attempt to model the home value

#scale the data
length(land_data)
land_scaled <- scale(land_data[,4:8]) #do not include the data and state, and the home value

str(land_scaled)
colnames(land_scaled)


#create dummy variables for the STATES

levels(land_data$STATE)

state_dummy <- model.matrix(~STATE-1, land_data) 
str(state_dummy)

#bind the dummy to the orig data set

land_final <- cbind(land_scaled, state_dummy[,-1], land_data[,2:3]) #remove the first dummy (AR) to avoid dummy trap

str(land_final)
length(land_final)

#create training and test data

t <- 1:nrow(land_final)
index <- sample(t, round(nrow(land_final)*.8))

train_land <- land_final[index,]
test_land <- land_final[-index,]

#############################
#############################

#use glmnet
#train the regression model using ?glmnet

#create training/test data specifically for glmnet
train_x <- train_land[,1:56]
train_y <- train_land[,57]
test_x <- test_land[,1:56]
test_y <- test_land[,57]

str(train_x)

##ridge
ridge_land <- cv.glmnet(as.matrix(train_x), train_y, family = 'gaussian', alpha = 0)

summary(glm_land)

#predict ridge

pred_ridge <- predict(ridge_land, s = ridge_land$lambda.1se, newx = as.matrix(test_x))

mse1 <- mean((test_y - pred_ridge)^2)
sqrt(mse1)

#plot residuals and prediction

ridge_resid <- test_y - pred_ridge

plot(ridge_resid)

#make a dataframe for plotting
plot_ridge <- data.frame(Series = c(1:length(test_y)), test_y, pred_ridge)
str(plot_ridge)

ggplot(plot_ridge, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)

#################################
##################################

#lasso
lasso_land <- cv.glmnet(as.matrix(train_x), train_y, family = 'gaussian', alpha = 1)

#predict lasso

pred_lasso <- predict(lasso_land, s = lasso_land$lambda.1se, newx = as.matrix(test_x))

mse2 <- mean((test_y - pred_lasso)^2)
sqrt(mse2)

#plot residuals

lasso_resid <- test_y - pred_lasso

plot(lasso_resid)

#make a dataframe for plotting
plot_lasso <- data.frame(Series = c(1:length(test_y)), test_y, pred_lasso)
str(plot_lasso)

ggplot(plot_lasso, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)

################################
##############################
#elastic net alpha = 0.1 - 0.9

n = seq(0.1:0.9, by = 0.1)
i = 0.1
for (i in n){

assign(paste('elas', i, sep = ''), cv.glmnet(as.matrix(train_x), train_y, family = 'gaussian', alpha = i))

}

#predict elasticnet

yhat1 <- predict(elas0.1, elas0.1$lambda.1se, newx = as.matrix(test_x))
yhat2 <- predict(elas0.2, elas0.2$lambda.1se, newx = as.matrix(test_x))
yhat3 <- predict(elas0.3, elas0.3$lambda.1se, newx = as.matrix(test_x))
yhat4 <- predict(elas0.4, elas0.4$lambda.1se, newx = as.matrix(test_x))
yhat5 <- predict(elas0.5, elas0.5$lambda.1se, newx = as.matrix(test_x))
yhat6 <- predict(elas0.6, elas0.6$lambda.1se, newx = as.matrix(test_x))
yhat7 <- predict(elas0.7, elas0.7$lambda.1se, newx = as.matrix(test_x))
yhat8 <- predict(elas0.8, elas0.7$lambda.1se, newx = as.matrix(test_x))
yhat9 <- predict(elas0.9, elas0.9$lambda.1se, newx = as.matrix(test_x))

mse0.1 <- mean((test_y - yhat1)^2) 
mse0.2 <- mean((test_y - yhat2)^2)
mse0.3 <- mean((test_y - yhat3)^2)
mse0.4 <- mean((test_y - yhat4)^2)
mse0.5 <- mean((test_y - yhat5)^2)
mse0.6 <- mean((test_y - yhat6)^2)
mse0.7 <- mean((test_y - yhat7)^2)
mse0.8 <- mean((test_y - yhat8)^2)
mse0.9 <- mean((test_y - yhat9)^2)

sqrt(mse0.1)
sqrt(mse0.2)
sqrt(mse0.3)
sqrt(mse0.4)
sqrt(mse0.5)
sqrt(mse0.6)
sqrt(mse0.7)
sqrt(mse0.8)
sqrt(mse0.9)

###alpha = 0.3 has the best rmse


#make a dataframe for plotting
plot_elas0.3 <- data.frame(Series = c(1:length(test_y)), test_y, yhat3)
str(plot_elas0.3)

ggplot(plot_elas0.3, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = X1), color = 'red',  alpha = 0.6)


#################################3
################################
#compare with OLS

lm_data <- cbind(train_x, train_y)
lm_test <- cbind(test_x, test_y)
head(lm_data)
head(lm_test)

ols <- lm(train_y~., lm_data)
plot(ols)

#predict

predlm <- predict(ols, lm_test)

mselm <- mean((lm_test$test_y - predlm)^2)
sqrt(mselm)

#plot residuals

resid <- lm_test$test_y - predlm
plot(resid)

#########################################
##########################################
#try using svm
library(e1071)
names(lm_test)[57] <- 'train_y'

#svm_land_tune <- tune.svm(train_y~., data = lm_data, gamma = 2^(-1:1), cost = 2^(-1:1), cross = 10)

svm_land <- svm(train_y~., data = lm_data, gamma = 0.03125, cost = 32)


predsvm <- predict(svm_land, lm_test)

msesvm <- mean((lm_test$train_y - predsvm)^2)
sqrt(msesvm)


#make a dataframe for plotting
plot_svm <- data.frame(Series = c(1:length(test_y)), test_y, predsvm)
str(plot_svm)

ggplot(plot_svm, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = predsvm), color = 'red',  alpha = 0.6)

###################################
######################################
#using gbm in caret

library(caret)
library(gbm)
?caret

gbm_land_train <- train(train_x, train_y, method = 'gbm')

#//
#  Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning
#parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using  the smallest value.
#The final values used for the model were n.trees = 150, interaction.depth = 3,
#shrinkage = 0.1 and n.minobsinnode = 10. //

gbm_land <- gbm.fit(train_x, train_y, n.trees = 150, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10, distribution = 'gaussian')

#predict

pred_gbm <- predict(gbm_land, test_x, n.trees = 150)

mse_gbm <- mean((test_y - pred_gbm)^2)

sqrt(mse_gbm)

##plot the residual and prediction

#make a dataframe for plotting
plot_gbm <- data.frame(Series = c(1:length(test_y)), test_y, pred_gbm)
str(plot_gbm)

ggplot(plot_gbm, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = pred_gbm), color = 'red',  alpha = 0.6)

####################################
####################################
#using xgboost

library(xgboost)

xg_land <- xgboost(as.matrix(train_x), train_y, nrounds = 1000)

pred_xg <- predict(xg_land, as.matrix(test_x))

msexg <- mean((test_y - pred_xg)^2)

sqrt(msexg)

plot(xg_land)


#make a dataframe for plotting
plot_xg <- data.frame(Series = c(1:length(test_y)), test_y, pred_xg)
str(plot_xg)

ggplot(plot_xg, aes(x = Series, y = test_y)) + geom_line() + geom_line(aes(y = pred_xg), color = 'red',  alpha = 0.6)

