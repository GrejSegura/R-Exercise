## this is a K Nearest Neighborhood (KNN) exercise

##load the data --- wine quality data

data1 <- read.csv(file.choose(), header = T, sep = ";")

##remove the classifying variable -- data1$quality
length(data1)

#convert the classifying variable to factor
data1$quality <- factor(data1$quality)

data2 <- data1[,-12]
head(data2)

##scale the data2 --- standardize
data3 <- scale(data2, center = T, scale = T)
nrow(data3)

##make a training and test data
##get a sample using the sample function

index <- 1:nrow(data3)
trainindex <- sample(index, round(nrow(data3)*0.8))

traindata <- data3[trainindex,]
testdata <- data3[-trainindex,]

#create a target data for train and test
target_train <- data1[trainindex,12]
target_test <- data1[-trainindex,12]

head(target_train)

##train using the knn function in package "class"

library(class)

knnmodel <- knn(train = traindata, test = testdata, cl = target_train #this is the classifying variable
                , k = round(sqrt(nrow(data3))))

##create a confusion table to test the result

table(target_test, knnmodel) ##all values in diagonal are the correctly predicted




