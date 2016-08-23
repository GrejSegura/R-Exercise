##RANDOM FOREST

#MACHINE LEARNING EXERCISE FOR CREDIT APPROVAL
library(randomForest)
library(RCurl)
library(dplyr) #just in case if needed
library(ggplot2) #just in case if needed

#get the data for credit approval in the UCI MACHINE LEARNING DATABASE
url <- getURL('https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data')

cred <- read.csv(text = url, header = F)
str(cred)

name <- names(cred)

varnames <- paste(name, collapse = '+')

#create dummy variables for the factors
credit <- model.matrix(~varnames, cred)

View(credit)

#change the target variable to 'target'
names(credit)[38] <- 'target'

#remove the intercept variable
credit <- credit[,-37]

#convert target variable to factor -- 
credit$target <- as.factor(credit$target)
str(credit)

##make a training and test data

index <- 1:nrow(credit)

t <- sample(index, round(nrow(credit)*0.7))

traincred <- credit[t,]
testcred <- credit[-t,]


##train a random forest model

#make a formula
var <- names(traincred[1:36])
var

varname <- paste(var, collapse = '+')
f1 <- as.formula(paste('target', varname, sep = '~')) 
f1

rf1 <- randomForest(f1, data = traincred, ntree = 800, mtry = 5)
plot(rf1)

#predict for train
predrf1 <- predict(rf1, traincred)
tabrf1 <- table(traincred$target,predrf1)
erf1 <- (tabrf1[2,1]+tabrf1[1,2])/sum(as.numeric(traincred$target))
erf1

#predict for test
predrf1 <- predict(rf1, testcred)
tabrf2 <- table(testcred$target,predrf1)
erf2 <- (tabrf2[2,1]+tabrf2[1,2])/sum(as.numeric(testcred$target))
erf2