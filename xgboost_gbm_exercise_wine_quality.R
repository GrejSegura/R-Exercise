
set.seed(1234)
library(xgboost)

#load the data
wine_data <- read.csv(file.choose())

#scale the data
length(wine_data)
wine_scaled <- scale(wine_data[,1:11]) #do not include alcohol and quality

str(wine_data)
colnames(wine_scaled)


#bind to the orig data set

wine_final <- data.frame(wine_scaled, wine_data[,12])

names(wine_final)[12] <- 'quality'

str(wine_final)
head(wine_final)

#create training and test data

t <- 1:nrow(wine_final)
index <- sample(t, round(nrow(wine_final)*.8))

train_wine <- wine_final[index,]
test_wine <- wine_final[-index,]


#create training/test data specifically for glmnet
train_a <- train_wine[,1:11]
train_b <- train_wine[,12]
test_a <- test_wine[,1:11]
test_b <- test_wine[,12]

str(train_a)

#using gbm in caret

library(caret)
library(gbm)

gbm_wine_train <- train(train_a, train_b, method = 'gbm')

#// error function

error <- function(x, y){
  
  if (length(y) >= nrow(x)){
    level <- nrow(x)
  } else {
    level <- length(levels(y))
  }
  
  
  i = 1
  sum_diagonal = 0
  
  for (i in 1:level){
    sum_diagonal = sum_diagonal + x[i,i]
  }
  
   rate <- sum_diagonal / sum(x)
   errorrate <- 1 - rate
   return(errorrate)
  }



#  Tuning parameter 'shrinkage' was held constant at a value of 0.1
#Tuning
#parameter 'n.minobsinnode' was held constant at a value of 10
#RMSE was used to select the optimal model using  the smallest value.
#The final values used for the model were n.trees = 150, interaction.depth = 3,
#shrinkage = 0.1 and n.minobsinnode = 10. //

gbm_wine <- gbm.fit(train_a, as.numeric(train_b), n.trees = 500, interaction.depth = 3, shrinkage = 0.05, n.minobsinnode = 10, distribution = 'gaussian')

#predict

pred_gbm_wine <- predict(gbm_wine, test_a, n.trees = 150)

rounded_gbm <- round(pred_gbm_wine, digits = 0)

summary(rounded_gbm)

mse_gbm_wine <- table(test_b, as.factor(rounded_gbm))

error(mse_gbm_wine, test_b)

sum(mse_gbm_wine)
levels(train_b)
##plot the residual and prediction

#make a dataframe for plotting
plot_gbm_wine <- data.frame(Series = c(1:length(test_b)), test_b, pred_gbm_wine)
str(plot_gbm_wine)

ggplot(plot_gbm_wine, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = pred_gbm_wine), color = 'red',  alpha = 0.6)

####################################
####################################
#using xgboost

library(xgboost)
library(Matrix)

train_label <- as.numeric(train_b) - 1
test_label <- as.numeric(test_b) - 1

summary(train_b)
summary(train_label)

#first use the gbtree booster
xgtree_wine <- xgboost(as.matrix(train_a), train_label, 
                   booster = 'gbtree',
                   objective = 'multi:softmax',
                   num_class = 7,
                   max.depth = 6,
                   eta = 0.1,
                   nthread = 2,
                   nrounds = 500,
                   min_child_weight = 1,
                   subsample = 0.5, 
                   colsample_bytree = 1, 
                   num_parallel_tree = 1)

#predict gbtree ----------------

pred_xgtree_wine <- predict(xgtree_wine, as.matrix(test_a))

msexgtree_wine <- table(as.factor(pred_xgtree_wine), as.factor(test_label))



error_xg <- function(x, y){
  
  if (length(y) >= nrow(x)){
    level <- nrow(x)
  } else {
    level <- length(levels(y))
  }
  
  
  i = 1
  sum_diagonal = 0
  
  for (i in 1:level){
    sum_diagonal = sum_diagonal + x[i,i+1]
  }
  
  rate <- sum_diagonal / sum(x)
  errorrate <- 1 - rate
  return(errorrate)
}


error_xg(msexgtree_wine, test_label)

#use the gblinear booster -------

xglinear_wine <- xgboost(as.matrix(train_a), train_label, 
                    booster = "gblinear", 
                    objective = "multi:softmax",
                    num_class = 7,
                    max.depth = 6, 
                    nrounds = 1000, 
                    lambda = 0.6, 
                    lambda_bias = 0.5, 
                    alpha = 0.5)


#predict gblinear ----------------

pred_xglinear_wine <- predict(xglinear_wine, as.matrix(test_a))

msexglinear_wine <- table(as.factor(pred_xglinear_wine), as.factor(test_b))

error_xg(msexglinear_wine, test_b)




#train xg cv

param = list("objective" = "multi:softprob"
             , "eval_metric" = "mlogloss"
             , 'num_class' = 5
             , 'eta' = 0.001)



xg_train <- xgb.cv(param = param, data = train_1,
                   nfold = 5, nrounds = 150)


#make a dataframe for plotting
plot_xg_wine <- data.frame(Series = c(1:length(test_b)), test_b, pred_xg_wine)
str(plot_xg_wine)

ggplot(plot_xg_wine, aes(x = Series, y = test_b)) + geom_line() + geom_line(aes(y = pred_xg_wine), color = 'red',  alpha = 0.6)


table(round(pred_gbm_wine, digits = 0), round(pred_xg_wine, digits = 0))
