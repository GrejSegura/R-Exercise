###CLASSIFICATION OF RED AND WHITE WINE
##USING VARIOUS METHODS

set.seed(12)

library(RCurl)
library(e1071)
library(randomForest)
library(neuralnet)
library(ggplot2)
library(dplyr)
library(extrafont)
windowsFonts(gothic = windowsFont("century gothic"))

##LOAD THE WINE QUALITY DATA
#for red wine
url <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/winequality-red.csv')

data1 <- read.csv(text = url, sep = ';')

#add 13th column
data1[,13] <- '1'
names(data1)[13] <- 'wine'

View(data1)

#for white wine
url2 <- getURL('https://raw.githubusercontent.com/GrejSegura/DataBank/master/winequality-white.csv')
data2 <- read.csv(text = url, sep = ';')
View(data2)

#add 13th column
data2[,13] <- '0'
names(data2)[13] <- 'wine'

View(data2)


#join 2 dataframes
data3 <- rbind(data1, data2)

View(data3)

##VISUALIZE THE RELATIONSHIP USING A FACETED CHART

w1 <- ggplot(data3, aes(alcohol, fixed.acidity, color = wine)) + geom_point() + facet_grid(alcohol~.)
w1


# remove quality as it is not needed
data3 <- data3[,-12]
str(data3)

##scale all variables
i=1
for (i in 1:12)
data3[,i] <- scale(data3[,i])

View(data3)

##+++++++++++++##
##TRAINING PART##

#create train and test data

index <- 1:nrow(data3)

t <- sample(index, round(nrow(data3)*.7))

train <- data3[t,]
test <- data3[-t,]


#make sure the predicted variable is a numeric
train$wine <- as.numeric(train$wine)
test$wine <- as.numeric(test$wine)
str(train)
str(test)

#======================#


# 1.) train using SVM

#sample  a gamma = (2^-5, 2^-3,.. 2^15,...) and cost = (2^-15, 2^-13,..., 2^3,...)
sv <- svm(wine~., data = train, gamma = 0.03125, cost = 0.05)
predsv <- predict(sv, train)
predsv <- ifelse(predsv > 0.5, 1,0)
tab1 <- table(train$wine, predsv)

#calculate error percentage
error1 <- (tab1[2,1])/sum(tab1)
error1


?tune

#tune the model -  different number of gamma = (2^-5, 2^-3,.. 2^15,...) and cost = (2^-15, 2^-13,..., 2^3,...)
tunedsv <- tune.svm(wine~., data = train, gamma = 2^(-5:15), cost = 2^(-15:3))
tunedsv$best.model

##
#Call:
# best.svm(x = wine ~ ., data = train, gamma = 2^(-5:15), cost = 2^(-15:3))


#Parameters:
#  SVM-Type:  eps-regression 
#SVM-Kernel:  radial 
#cost:  0.5 
#gamma:  0.03125 
#epsilon:  0.1 

#Number of Support Vectors:  2231

#=================#
#=================#

#USING RANDOMFOREST

names(train)

#make sure the variable to be predicted is a factor

train$wine <- factor(train$wine)
test$wine <- factor(test$wine)

#exclude wine variable

varnames2 <- varnames1[!varnames1 %in% 'wine']
varnames3 <- paste(varnames2, collapse = '+')
varnames3

#create a formula
formula1 <- as.formula(paste('wine', varnames3, sep = '~'))

##make a classification model
ran <- randomForest(formula1, data = train, ntree =  5000, mtry = 3, importance = T)

predran <- predict(ran, test)
tab2 <- table(test$wine, predran)
tab2
#calculate error percentage
error2 <- (tab2[2,1] + tab2[1,2])/sum(tab2)
error2

plot(sv)

#===============#
#===============#

##USING THE rpart package

rp <- rpart(formula1, data = train)
plot(rp)
predrp <- predict(rp, train)
table(train$wine, predrp)
