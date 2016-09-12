##glmnet

library(data.table)
library(glmnet)
library(ggplot2)

?data.table
#load the data
land_data <- read.csv(file.choose())

land_data_table <- data.table(land_data)


str(land_data)

#this exercise will attempt to model the home value

#scale the data
length(land_data)
land_scaled <- scale(land_data[,4:8])

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

#train the regression model using ?glmnet


train_x <- train_land[,1:56]
train_y <- train_land[,57]
test_x <- test_land[,1:56]
test_y <- test_land[,57]

str(train_x)

#ridge
ridge_land <- cv.glmnet(as.matrix(train_x), train_y, family = 'gaussian', alpha = 0)



summary(glm_land)

#predict ridge

pred_ridge <- predict(ridge_land, s = ridge_land$lambda.1se, newx = as.matrix(test_x))

mse1 <- mean((test_y - pred_ridge)^2)
sqrt(mse1)

#plot residuals

ridge_resid <- test_y - pred_ridge

plot(ridge_resid)


#lasso
lasso_land <- cv.glmnet(as.matrix(train_x), train_y, family = 'gaussian', alpha = 1)

#predict lasso

pred_lasso <- predict(lasso_land, s = lasso_land$lambda.1se, newx = as.matrix(test_x))

mse2 <- mean((test_y - pred_lasso)^2)
sqrt(mse2)

#plot residuals

lasso_resid <- test_y - pred_lasso

plot(lasso_resid)

#elastic 0.1 - 0.9

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
