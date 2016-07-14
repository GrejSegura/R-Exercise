##THIS IS AN EXERCISE FOR SVM USING WINE QUALITY DATA --- JULY 14,2016

##LOAD THE DATA

data1 <- read.csv(file.choose(), header = T, sep = ",")

#### ===== THIS IS THE DATA PREPARATION PART ==== ####
##SINCE WE HAVE 7 DIFFERENT WINE QUALITIES, 
##THE CLASSIFIER NEEDS TO BE BROKEN DOWN INTO 7 DIFFERENT DUMMY VARIABLES

data_dummy <- model.matrix(~quality, data1)

##SINCE model.matrix DO NOT INCLUDE THE FIRST CATEGORY IN THE DUMMY VARIABLE CREATION,
##WE NEED TO CONVERT THE FIRST CATEGORY IN THIS CASE "3"



data_dummy1 <- data.frame(ifelse(data1$quality == 3, 1, 0))
head(data_dummy1)

##BIND THE CONVERTED DATA TO FORM ONE DATA FRAME

data2 <- data.frame(cbind(data1, data_dummy, data_dummy1))
str(data2)
head(data2)

##DELETE UNWANTED COLUMNS / VARIABLES

data2 <- data2[,-c(12,13)]

##CHANGE THE COLUMN NAMES OF THE NEW VARIABLES
names(data2)[12:18] <- c("four","five","six","seven","eight","nine","three")

##SCALE THE DATA 

data3 <- scale(data2[,1:11], center = T, scale = T)

data3 <- cbind(data3, data2[,12:18])

###########################################################################################

library(e1071)

##FOR MULTICLASS SVM, USE ONE VS ALL METHOD

##GET 70% OF THE DATA FOR TRAINING AND 30% FOR TESTING

index <- 1:nrow(data3)

train <- sample(index, round(nrow(data3)*.7))

#APPLY SVM TO THE FIRST CLASS --> QUALITY = 6


#REMOVE THE OTHER UNWANTED CATEGORIES AND RETAIN THE quality = 6
# data_train <- data3[train,-c(12:13,15:18)]
# data_test <- data3[-train,-c(12:18)]

### === THIS PART IS THE TRAINING PROCESS USING THE SVM METHOD === ###
## THE PACKAGE USED HERE IS e1071


# svmmodel_3 <- svm(six ~ ., data = data_train)

# summary(svmmodel_3)
# print(svmmodel_3)

#TEST USING THE Train DATA
# pred3 <- predict(svmmodel_3, data_train)

#CONVERT THE PROBABILITIES TO 1 OR 0
# pred3 <- ifelse(pred3>.5 ,1 ,0)

# table(pred3, data_train$six)

##DO THE CLASSIFICATION FOR ALL OTHER CATEGORIES


### BELOW IS A PROGRAM TO IMPLEMENT SVM FOR ALL CLASSES / CATEGORIES ###


###### === looping all categories === #####

i=12
for (i in c(12:18))
{
  #REMOVE THE OTHER UNWANTED CATEGORIES AND RETAIN THE quality = 6
  data_train <- data3[train,c(1:11,i)]
  data_test <- data3[-train,-c(12:18)]
  
  ### === THIS PART IS THE TRAINING PROCESS USING THE SVM METHOD === ###
  ## THE PACKAGE USED HERE IS e1071
  
  dep <- names(data3)[i]
  
  
  svmmodel <- svm(as.formula(paste(dep,"~.", sep="")) , data = data_train)
  
  #TEST USING THE Train DATA
  pred <- predict(svmmodel, data_train)
  
  
  
  #CONVERT THE PROBABILITIES TO 1 OR 0
  predicted <- ifelse(pred>0.5 ,1 ,0)
  
  tab <- table(predicted, data_train[,dep])
  
  colnames(tab) <- c(0, dep)
  
  print(tab)
}

