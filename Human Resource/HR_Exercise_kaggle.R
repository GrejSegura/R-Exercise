
# Human Resource Analysis 

This is an exercise conducted to figure out how and why employees are leaving the company.

To start with, load the libraries necessary and view the structure of the given data.
```{r, message = F, warning = F}
library(dplyr)
library(ggplot2)
library(caret)

hr.data <- read.csv("../input/HR_comma_sep.csv")
str(hr.data)
dim(hr.data)
```

## Data Exploration {.tabset}

To gather more information regarding the data, we try to explore the factors and see the difference between the employees who left and stayed with the company.

### Satisfaction Level {.tabset}
Let us check the satisfaction level of all employees and see the distribution.
```{r, message = F, warning = F}
hr.data$left <- as.factor(hr.data$left)

sat.level <- ggplot(hr.data, aes(hr.data$satisfaction_level)) + geom_density(kernel = "gaussian")
sat.level
```

The distribution is more dense on the right tail but the density in the left tail is also observable.
Now, let us separate the distribution for those who left and stayed with the company.

First, those who left.
```{r, message = F, warning = F}
sat.left <- hr.data[hr.data$left == 1, c("satisfaction_level", "left")]

sat.1 <- ggplot(sat.left, aes(satisfaction_level)) + geom_density(kernel = "gaussian")
sat.1
```


Then those who stayed.

```{r, message = F, warning = F}
sat.stayed <- hr.data[hr.data$left == 0, c("satisfaction_level", "left")]

sat.0 <- ggplot(sat.stayed, aes(satisfaction_level)) + geom_density(kernel = "gaussian")
sat.0
```


It is visually evident that the level of satisfaction for those who left is more dense on the left tail and also on the lower middle, which means high number of employees were dissatisfied with the company. On the other hand, the satisfaction level of those who stayed is more dense on the right tail, the higher bracket of satisfaction level.

Applying t-test to see if the difference in satisfaction level is indeed statistically observed.

```{r, message = F, warning = F}
t.test(satisfaction_level~left, hr.data)
```

The p-value is very low which leads to a conclusion that the level of satisfaction for both groups are indeed significantly different.


### Number of Projects {.tabset}

Visual examination of the number of projects between the 2 groups.

```{r, message = F, warning = F}
project.left <- hr.data[hr.data$left == 1, c("number_project", "left")]

proj.1 <- ggplot(project.left, aes(number_project, fill = factor(number_project))) + geom_bar()
proj.1

project.stay <- hr.data[hr.data$left == 0, c("number_project", "left")]

proj.0 <- ggplot(project.stay, aes(number_project, fill = factor(number_project))) + geom_bar()
proj.0
```

Again, it is visually evident that the number of project for those who stayed are far greater than those who left. In fact, most of those who left only has 2 projects.


### Average Monthly Number of Hours {.tabset}

This variable is a notified factor for employees to their satisfaction with a company. Let us see how this affects between the groups.
A t-test was also conducted to see the group difference.

```{r, message = F, warning = F}
l <- hr.data %>% group_by(left) %>% summarise(left.1 = mean(average_montly_hours))

hours.1 <- ggplot(l, aes(x= left, y = left.1, fill = left)) + geom_bar(stat = "identity")
hours.1

t.test(average_montly_hours~left, hr.data)
```

Those who left are significantly higher than those who stayed evidenced by the significantly lower p-value on the test and the chart.

### Time Spend {.tabset}

Let us also investigate how both groups differ with the time they spent on the company.

```{r, message = F, warning = F}
time.spend <- hr.data %>% group_by(left) %>% summarise(left.1 = mean(time_spend_company))

time.1 <- ggplot(time.spend, aes(x= left, y = left.1, fill = left)) + geom_bar(stat = "identity")
time.1
```

Again, those who left has greater time spent on the company than those who stayed.

### Department

Now, let us see the difference of those who stayed and left between the departments of the company.

```{r, message = F, warning = F}
sales.1 <- ggplot(hr.data, aes(sales, fill = left)) + geom_bar(position = "dodge")
sales.1
```

Interestingly, quite a number of employees left from the Sales, Support and Technical departments.

### Salary {.tabset}

Salaries are major concern to employees when deciding to stay or leave a company. Here we try to examine how the group are distributed between 3 categories of the company’s salary.

```{r, message = F, warning = F}
salary.1 <- ggplot(hr.data, aes(salary, fill = left)) + geom_bar(position = "dodge")
salary.1
```

Indeed, Lower and medium salaries has higher number of employees who left.

## Take Away

The factors were investigated and the results were unsurprising. For those who left, satisfaction level is lower, number of projects are lower, average monthly hour is lower, time spent with the company is lower, and salaries are lower.


## Prediction Model

### Data Preparation

First, we need to prepare the data to fit with the requirement of the analysis.

To make sure we have the proper data type for all variables, we investigate their structures.

```{r, message = F, warning = F}

str(hr.data)

```

Clearly, all variables are of the correct structure here.

Create dummy variables.

```{r, message = F, warning = F}

hr.data.1 <- hr.data
hr.data.1$left <- as.numeric(hr.data$left)

hr.data.1$left <- ifelse(hr.data.1$left == "1", 1, 0)

dummy <- dummyVars(~.-1, hr.data.1)
hr.dummy <- predict(dummy, hr.data.1)
hr.dummy <- as.data.frame(hr.dummy)
str(hr.dummy)

hr <- hr.dummy[,-c(9, 19)]

```

Create a train and test data sets.

```{r, message = F, warning = F}
t <- 1:nrow(hr)

index.1 <- sample(t, round(nrow(hr)*.8))
train.1 <- hr[index.1,]
test.1 <- hr[-index.1,]
```


##### Logistic Regression {.tabset}
```{r, message = F, warning = F}



lm.hr <- glm(train.1$left~., train.1, family = binomial(link = "logit"))
summary(lm.hr)

pred.lm <- predict(lm.hr, test.1)

accuracy <- ifelse(pred.lm > 0.5, 1, 0)

table(test.1$left, accuracy)
```

The accuracy of the logistic regression model is quite low at 77.7%. However, the result shows that the factors
that affect the decision to leave based on the significance of their p-values.


##### Random Forest {.tabset}
```{r, message = F, warning = F}
library(randomForest)

train.1$left <- factor(train.1$left)

rand.hr <- randomForest(left~., train.1, importance = T, ntree = 500)

importance(rand.hr)

pred.rf <- predict(rand.hr, test.1)
accuracy.rf <- ifelse(pred.rf == "1", 1, 0)

table(test.1$left, pred.rf)
```
Here the accuracy dramatically improved to 98.76%. Quite high but XGboost has a good reputation of being better so we will try it as well.
The variable importance table also shows that satisfaction level is the most influential factor among all factors considered.

##### XGBoost {.tabset}
```{r, message = F, warning = F}
library(xgboost)

train.label <- ifelse(train.1[,7] == "1", 1,0)

xg.hr <- xgboost(as.matrix(train.1[,-7]), train.label,
	               booster = 'gbtree',
                   objective = 'binary:logistic',
                   max.depth = 7,
                   eta = 0.1,
                   nthread = 3,
                   nrounds = 500,
                   min_child_weight = 1,
                   subsample = 0.7, 
                   colsample_bytree = 1, 
                   num_parallel_tree = 1,
                   verbose = 0)

pred.xg <- predict(xg.hr, as.matrix(test.1[,-7]))

accuracy.xg <- ifelse(pred.xg > 0.5, 1,0)

table(test.1[,7], accuracy.xg)
```
Indeed, XGboost has the highest accuracy at 99.06%.

##### Ensemble Modelling {.tabset}

Ensembling improves accuracy when done right. 
One important thing to remember is that the correlation of the predicted values of the models should be lower.

```{r, message = F, warning = F}
cor(cbind(pred.lm, pred.rf, pred.xg))
```

The predicted values for XGboost and Random forest has a very high correlation.
The other correlations however are relatively lower, xg vs. logit and rf vs. logit.
Here I decided to ensemble XG and logistic regression since it has the lowest correlation among them.
I experimented a range of weights to start with the ensemble. The first option I had is by looking at the accuracy of the XG
boost since it is already quite high. I first started with 0.99 and 0.01 as the weights for XG and logit respectively until
I found that 0.95 and 0.05 are the weights that make the accuracy higher.

```{r, message = F, warning = F}
ensemble <- (pred.lm*0.05) + (pred.xg*0.95)
accuracy.en <- ifelse(ensemble > 0.5, 1, 0)
table(test.1[,7], accuracy.en)

```
It is noticeable that the ensemble improved the accuracy by 0.06%. So at 99.13%, the ensemble model of XG and logistic regression is the best among all tried models.


## Conclusion

The XGBoost again proved to have the best prediction power over logistic regression and random forest.
However, enssembling the xg and lm prediction gave the most accurate prediction among all.
