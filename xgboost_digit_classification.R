#XGBOOST Digits

set.seed(311)
library(xgboost)
library(Matrix)

digit_data <- read.csv(file.choose())

#convert to numeric, xgboost only accepts numeric
digit_data <- sapply(digit_data, as.numeric)

#sapply in the last operation converted the data to matrix, 
#bring back the structure to data frame
digit_data <- as.data.frame(digit_data)

str(digit_data)

#create training and test data
d <- 1:nrow(digit_data)
index_digit <- sample(d, round(nrow(digit_data)*.8))

train_digit <- digit_data[index_digit,]
test_digit <- digit_data[-index_digit,]


#create training/test data
train_1 <- train_digit[,2:length(digit_data)]
test_1 <- test_digit[,2:length(digit_data)]

#important to separate the label or classification variable
train_2 <- train_digit[,1]
test_2 <- test_digit[,1]

str(train_1)

#first use the gbtree booster
xgtree_digit <- xgboost(as.matrix(train_1), train_2, 
                       booster = 'gbtree',
                       objective = 'multi:softmax',
                       num_class = 10,
                       max.depth = 5,
                       eta = 0.1,
                       nthread = 4,
                       nrounds = 120,
                       min_child_weight = 1,
                       subsample = 0.5, 
                       colsample_bytree = 1, 
                       num_parallel_tree = 1)

#eta = 0.05, max.depth = 6, nthread = 2, error = 0.06307692
#eta = 0.1, max.depth = 5, nthread = 2, error = 0.05923077
#eta = 0.1, max.depth = 5, nthread = 4, error = 0.06153846

#predict gbtree ----------------

pred_xgtree_digit <- predict(xgtree_digit, as.matrix(test_1))

msexgtree_digit <- table(as.factor(pred_xgtree_digit), as.factor(test_2))

msexgtree_digit

##compute accuracy

wrong <- ifelse(abs(test_2 - pred_xgtree_digit) > 0, 1, 0)

error_xg <- sum(wrong) / length(as.factor(test_2))

error_xg

#use the gblinear booster -------

xglinear_digit <- xgboost(as.matrix(train_1), train_2, 
                         booster = "gblinear", 
                         objective = "multi:softmax",
                         num_class = 10,
                         max.depth = 5, 
                         nrounds = 120, 
                         lambda = 1, 
                         lambda_bias = 1, 
                         alpha = 1)


#predict gblinear ----------------

pred_xglinear_digit <- predict(xglinear_digit, as.matrix(test_1))

msexglinear_digit <- table(as.factor(pred_xglinear_digit), as.factor(test_2))
msexglinear_digit

#compute accuracy of gblinear
wrong_linear <- ifelse(abs(test_2 - pred_xglinear_digit) > 0, 1, 0)

error_xglinear <- sum(wrong_linear) / length(as.factor(test_2))

error_xglinear


#train xg cv

param = list("objective" = "multi:softmax"
             , "eval_metric" = "mlogloss"
             , 'num_class' = 10
             , 'eta' = 0.1
             , 'max.depth' = 5
             )



xgdigit_train <- xgb.cv(param = param, data = as.matrix(train_1), label = train_2,
                   nfold = 5, nrounds = 120)

min(xgdigit_train$test.mlogloss.mean)
