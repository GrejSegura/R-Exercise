##THIS IS AN EXERCISE FOR RANDOM FOREST CLASSIFICATION METHOD##

#LOAD THE WINE QUALITY DATA

data1 <- read.csv(file.choose(), header = T , sep = ",")


##CONVERT THE CLASSIFYING VARIABLE TO FACTORS##

data1$quality <- as.factor(data1$quality)

##APPLY RANDOM FOREST##

library(randomForest)


## TRY ntree=500 and mtry=3, TRY ANOTHER VALUES OF THESE FOR TUNING

model1 <- randomForest(quality~., data = data1, mtry= 5, ntree=500, importance = T)

str(model1)

#TO GET THE CONFUSION MATRIX
model1$confusion

#TO ASSESS THE IMPORTANCE OF THE VARIABLES
model1$importance
