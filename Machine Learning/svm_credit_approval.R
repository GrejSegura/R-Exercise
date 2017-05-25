#MACHINE LEARNING EXERCISE FOR CREDIT APPROVAL

library(RCurl)
library(dplyr)
library(ggplot2)
library(e1071)

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

credit$target <- as.numeric(credit$target)
##make a training and test data

index <- 1:nrow(credit)

t <- sample(index, round(nrow(credit)*0.7))

traincred <- credit[t,]
testcred <- credit[-t,]

names(credit)

#make a formula
#not really necessary
#vars <- names(credit[,-37])
#formvar <- paste(vars, collapse = '+')
#formvar1 <- as.formula(paste('target', formvar, sep = '~'))


#train an svm model
credsvm <- svm(target~., traincred, scale = FALSE)
plot(credsvm)


#predict using train data
pred1 <- predict(credsvm, traincred)
pred1 <- ifelse(pred1 > 0.5, 1,0)
tab1 <- table(traincred$target, pred1)
tab1
err1 <- (tab1[2,1]+tab1[1,2])/sum(traincred)
err1

#predict using test data
pred2 <- predict(credsvm, testcred)
pred2 <- ifelse(pred2 > 0.5, 1,0)
tab2 <- table(testcred$target, pred2)
tab2
err2 <- (tab2[2,1]+tab2[1,2])/sum(testcred)
err2


#tune the model
?tune
tunedsv <- tune.svm(target~., data = traincred, gamma = 2^(-15:5), cost = 2^(5:-15), scale = FALSE)

#get the best model
tunedsv$best.model

#// Call:
#  best.svm(x = target ~ ., data = traincred, gamma = 2^(-15:5), cost = 2^(5:-15), 
#          scale = FALSE)
#
#
#Parameters:
#  SVM-Type:  eps-regression 
#SVM-Kernel:  radial 
#cost:  0.0625 
#gamma:  0.125 
#epsilon:  0.1 
#
#
#Number of Support Vectors:  312   //



#substitute the resulting best model to the svm model
credsvm2 <- svm(target~., traincred, scale = FALSE, gamma = 0.125, cost = 0.0625)

#predict using train data
pred3 <- predict(credsvm2, traincred)
pred3 <- ifelse(pred3 > 0.5, 1,0)
tab3 <- table(traincred$target, pred3)
tab3
err3 <- (tab3[2,1]+tab3[1,2])/sum(traincred)
err3

#predict using test data
pred4 <- predict(credsvm2, testcred)
pred4 <- ifelse(pred4 > 0.5, 1,0)
tab4 <- table(testcred$target, pred4)
tab4
err4 <- (tab4[2,1]+tab4[1,2])/sum(testcred)
err4

c(err1,err3,err2,err4)
