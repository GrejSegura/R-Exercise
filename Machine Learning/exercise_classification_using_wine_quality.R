set.seed(213)
library(e1071)
library(ggplot2)
library(caret)
library(randomForest)
library(doMC)
registerDoMC(cores = detectCores())

#load wine data

wine <- read.csv(file.choose())
winetest <- read.csv(file.choose())
str(winetest)
str(wine)
wine <- wine[,-13]
winetest <- winetest[,-13]
winetest$quality <- as.factor(winetest$quality)

length(winetest)

wine$quality <- as.factor(wine$quality)

View(wine)

wine <- wine[,-13]

###################################################
##create an error wrapper that determines the accuracy rate
error <- function(x, y)
{
  if (length(levels(y)) >= nrow(x)){
      
      level <- nrow(x)
    
    } else {
      
      level <- length(levels(y))
    }
  
  i = 1
  sum_diagonal = 0
  
  for (i in 1:level){
    
    sum_diagonal = sum_diagonal + x[i,i]
    
  }
  
  rate <- sum_diagonal / sum(tab)
  errorrate <- 1 - rate
  return(errorrate)
}

###################################
###################################

i <- 1:nrow(wine)
index <- sample(i, round(nrow(wine)*.8))

train <- wine[index,]
test <- wine[-index,]


#first model
svmwine1 <- svm(quality~., data = train, cross = 10)

predwine1 <- predict(svmwine1, train)

table(predwine1, train$quality)

#tune model

svmtune <- tune.svm(quality~., data = train, gamma = 2^(-5:5), cost = 2^(-5:10), cross = 8)

svmtune$best.parameters

#gamma =1 cost = 2

#final model
svmwine2 <- svm(quality~., data = train, gamma = 1, cost = 2, cross = 10)

predwine2 <- predict(svmwine2, winetest)

tab <- table(predwine2, winetest$quality)

tab

svm <- error(tab, winetest$quality)
svm

##visualize using ggplot2

data1 <- cbind(wine, predwine2)

d <- ggplot(data1, aes(x = citric.acid, y = volatile.acidity, color = predwine2)) + geom_point(size = 3, alpha = 0.6)
d


##try randomforest
#no need to scale data

rforest <- randomForest(quality~., data = train, mtry = 4, ntree = 500)
plot(rforest)

predrf <- predict(rforest, winetest)

rftable <- table(predrf, winetest$quality)

error(rftable, winetest$quality)
rftable


#tune random forest

tunerf <- tune.randomForest(quality~., data = train, mtry = c(2,5), ntree = c(800,1000))

## mtry = 5, ntree = 1000

#final model

rfinal <- randomForest(quality~., data = train, mtry = 5, ntree = 800, cross =10)

predrf1 <- predict(rfinal, winetest)

rf <- table(predrf1, winetest$quality)
rf
randomforest <- error(rf, winetest$quality)
randomforest
###create an ensemble

svmpred <- as.numeric(predwine2)
rfpred <- as.numeric(predrf1)

?round

finalpred <- ((svmpred*5) + (rfpred)) / 6

finalpred <- ifelse(finalpred - round(finalpred) <= 0.5, round(finalpred), round(finalpred)+1)


finalpred <- as.factor(finalpred)


str(finalpred)
summary(winetest$quality)


#accuracy of ensemble

ensemtab <- table(finalpred, winetest$quality)
error(ensemtab, winetest$quality)


plot(predwine2)
predwine2


#using neural network

library(neuralnet)


?neuralnet
names(train)

train$quality <- as.factor(train$quality)

depcols <- dummyVars(~quality, train)
depcols1 <- dummyVars(~quality, test)

dependent <- predict(depcols, train)
dependent1 <- predict(depcols1, test)

dependent <- as.data.frame(dependent)
dependent1 <- as.data.frame(dependent1)

#make sure to scale the data
train_scale <- scale(train[,-12])
test_scale <- scale(test[,-12])

train1 <- cbind(dependent, train_scale)
test1 <- cbind(dependent1, test_scale)



#create formula
cols2 <- names(dependent)

cols <- names(train)

indep <- paste(cols[!cols %in% 'quality'], collapse = '+')
dep <- paste(cols2, collapse = '+')

f1 <- paste(dep, indep, sep = '~')

#train
neural <- neuralnet(as.formula(f1), data = train1, hidden = 10, rep = 1, stepmax = 100000)

names(train1)

##using xgboost
library(xgboost)

train1 <- as.matrix(train)
head(train1)

dtrain <- xgb.DMatrix(train1, label = quality)

xg <- xgboost(data = as.matrix(train), label = train1$quality)
