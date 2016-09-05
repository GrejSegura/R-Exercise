##EXERCISE USING SVM
set.seed(123)
library(e1071)
library(caret)
library(dplyr)

#load the digits data - C:\Users\user\Desktop\Databank\Kaggle Competitions\Digit Recognizer
digit <- read.csv(file.choose(), header = T, sep = ',')

#make sure the labels are in factor form
digit$label <- as.factor(digit$label)

#reduce the data set to 5000 rows for faster training
digit <- digit[1:5000,]


#create dummy variables for labels
#this exercise will make use of this method
labels <- model.matrix(~label-1, digit)
View(labels)

##another way of creating dummy variables -- 
##just an example, will not be used in this exercise
labels2 <- dummyVars(~labels, data = digit)
labels3 <- predict(labels2, digit)
View(labels3)

?dummyVars

#set an index for training and test data

t <- 1:nrow(digit)
index <- sample(t, round(nrow(digit)*.8))



###########################################################################
#create an error vector to compare each error rates
error <- NULL

##loop the training process
columns <- colnames(labels)
i = 1

for (i in 1:10) {
  
  digitdata <- cbind(labels[,i], digit[,-1])
  names(digitdata)[1] <- columns[i]
  
  #create train and test data
  train <- digitdata[index,]
  test <- digitdata[-index,]
  
  
  #create a formula first
  f1 <- as.formula(paste(columns[i],'~.', sep = ''))
  
  #train the model using the train data
  svmmodel <- svm(f1, data = train, scale = F)
  
  #predict using the test data
  pred1 <- predict(svmmodel, test)
  
  #convert the values 
  pred2 <- ifelse(pred1 > 0.5 , 1, 0)
  
  #calculate the error
  totalerror <- ifelse(test[,1] == pred2, 0, 1)
  
  #compute the model accuracy
  err <-  sum(totalerror)/ nrow(test[,1])
  error[i] <- err
  
}

##show the error vector
error

#######################################################################
##another loop for tuning the model

i = 1
bestgamma <- NULL
bestcost <- NULL

for (i in 1:10) {
  
  digitdata <- cbind(labels[,i], digit[,-1])
  names(digitdata)[1] <- columns[i]
  
  #create train and test data
  train <- digitdata[index,]
  test <- digitdata[-index,]
  
  
  #create a formula first
  f1 <- as.formula(paste(columns[i],'~.', sep = ''))
  
  #train the model using the train data
  tunedsv <- tune.svm(f1, data = train, gamma = 2^(-5:15), cost = 2^(-15:3))
  
  #get the best gamma and cost
  gamma <- tunedsv$best.model@gamma
  cost <- tunedsv$best.model@cost
  
  #store the best gamma and cost
  bestgamma[i] <- gamma
  bestcost[i] <- cost
  }

best_parameters <- cbind(bestgamma, bestcost)
best_parameters
bestgamma
bestcost

#####################################################################
##create a function for prediction
##use this function to predict a certain datapoint

predictsvm <- function(x){
 
 columns <- colnames(labels)
 
 #gather the error in this vector
 prob_gather <- NULL

 #create a list of target labels
 label_target <- c(0:9)
 
 #loop the training
 i = 1
 for (i in 1:10) {
  
  f2 <- as.formula(paste(columns[i],'~.', sep = ''))

  finalsvm <- svm(f2, data = train, gamma = bestgamma[i], cost = bestcost[i]))

  #predict
  predicted <- predict(finalsvm, x)
  
  #gather the probability
  prob_gather[i] <- predicted
 }

 probability_table <- cbind(label_target, prob_gather)
 probability_table
}

##########
#PERFORM A TEST

test <- digit[-index,]
x <- test[1,-1]

predictsvm(x)
test
