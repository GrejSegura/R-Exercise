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
length(digit_data)
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


#-------------------------------------#
#-------------------------------------#
#create a function to visualize the number ---> visualizer function

digit_visual <- function(x){
  
  #create NULL vectors
  
  image_raw <- data.frame(matrix(ncol = 28, nrow = 28))
  
  i = 0
  for (i in 0:27){
    
    image_raw[28 - i,] <- x[(1:28) + (i*28)]
    
  }
  
  heatmap(as.matrix((image_raw)), Rowv=NA, Colv=NA, col = heat.colors(256),  
          symm = FALSE, margins=c(5,10), labRow = FALSE, labCol = FALSE)
  
}

# -------------------#

# TEST THE VISUALIZER
#create a matrix to visualize a sample row ---> pick a row in the train or test

number <- as.vector(as.matrix(test_1[18,]))

test_2[18]

digit_visual(number)


View(image_raw)
#---------------------------------------#
# PLAY TIME!!!! PLAY TIME!!! PLAY TIME!!!
# GUESS THE NUMBER AND VISUALIZE
# THIS JUST A TESTING CODE -- MORE LIKE A GAME TO ME

#create a function for guessing -----> RUN THIS FUNCTION FIRST BEFORE GUESSING NUMBERS

try_luck <- function(x, y){
  
  result <- predict(xgtree_digit, as.matrix(x))
  
  #check if the number is correctly guessed
  
  if(y == result){
    
    say <- paste('CORRECTLY GUESSED NUMBER ', result, ', CHECK THE IMAGE TO CONFIRM.', sep = '')
    print(say)
    
  } else {
    
    say <- paste('OOPS! CORRECT GUESS WOULD BE ', result, '.', sep = '')
    print(say)  
  }
  
}

# -----------------------#
# CREATE ANOTHER FUNCTION FOR TRY GUESSING NUMBERS

guess <- function(x){
  
  num <- test_1[row,]
  
  # assign the correct number
  real <- test_2[row]
  
  # result of the model
  try_luck(num, real)
  digit_visual(num)
  
}

# OK, LET'S TRY THIS ONE -- GAME TIME!!!
#
# pick a row in test_2, only 1-1300 allowed ----> assign it to row variable below

row = 1300

# let the model guess
guess(row)
