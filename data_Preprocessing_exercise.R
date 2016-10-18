set.seed(43211)

# load the car_price data
car_csv <- read.csv(file.choose())

# str(car_csv)

##DATA PREPROCESSING##

#THIS PROCESS INCLUDE THE FF:
#1. REMOVING OF UNWANTED FEATURES
#2. IMPUTING ALL MISSING AND NAs
#3. CONVERTING FACTORS TO DUMMY - ONE HOT CODING
#4. CONVERTING ALL CLASSES TO NUMERICAL AND SCALE THE DATA


#--------------------------------#

#1. remove unwanted variables / features

# from first examination there are redundant variables that need to be remove firsthand
# the ff are redundant variables -- the unique id was also removed
# application postalcode, applicant_state desc
names(car_csv)
car_csv <- car_csv[, -c(1,5,14)]

###count the number of na per column

colname <- names(car_csv)

#my aim is to create a table that shows the count of NAs 
#and the percentage of it vs the number of rows:

#create the table
count <-  NULL
percentage <- NULL
number_rows <- NULL

for (name in colname){
  
  if (any(is.na(car_csv[, name]))){
    
    count[name] = sum(is.na(car_csv[, name]))
    
  } else {
    
    count[name] = 0
    
  }
  
  percentage[name] <- round((count[name] / nrow(car_csv))*100, digits = 2)
  number_rows[name] <- nrow(car_csv)
}

count_na <- data.frame(cbind(count, percentage, number_rows))
print(count_na)

# too many NAs --- one with > 50% in NAs
# base on the table the avriables with >50% NAs are the ff:
# - Duration_Of_Current_Emp
# - Total_Work_Experience
# - No_Of_Years_At_Business
# - Average_Bank_Balance
# - MAX_OD_F_12M
# - TOTAL_NTECH_BNC_F12M


above_50 <- which(count_na$percentage >= 50) #this gives a list of column numbers

#remove the columns
car_csv <- car_csv[, - above_50]

# str(car_csv)

#------------------------------#
# USUALLY YOU CAN DO THE VISUALIZATION ON THIS PART #
#------------------------------#


#2. impute the NAs

# locate the columns with NAs ---- the count_na table must be ran again in order
# to get the new set of column names

colname <- names(car_csv)

#my aim is to create a table that shows the count of NAs 
#and the percentage of it vs the number of rows:

#create the table
count <-  NULL
percentage <- NULL
number_rows <- NULL

for (name in colname){
  
  if (any(is.na(car_csv[, name]))){
    
    count[name] = sum(is.na(car_csv[, name]))
    
  } else {
    
    count[name] = 0
    
  }
  
  percentage[name] <- round((count[name] / nrow(car_csv))*100, digits = 2)
  number_rows[name] <- nrow(car_csv)
}

count_na <- data.frame(cbind(count, percentage, number_rows))
print(count_na)

print(colname) #this is the new colnames composed of 28 entries

with_na <- which(count_na$count >= 1)

# split the data with NAs and without NAs for easy manipulation

car_csv_na <- car_csv[, with_na]
car_csv <- car_csv[, -with_na]

# str(car_csv_na)
# str(car_csv)
# replace all the NAs

# create a function that replaces NA with mean

replace.na <- function(x) {
  
  replace(x, is.na(x), mean(x, na.rm = TRUE))
  
}

car_csv_na.rm <- sapply(car_csv_na, replace.na) #applying the replace.na function

# any(is.na((car_csv_na.rm))) # check if NAs still exist

car_csv_na.rm <- data.frame(car_csv_na.rm)
# str(car_csv_na.rm)

# bind the data together

car <- data.frame(car_csv, car_csv_na.rm)
# str(car)

#------------------------------#

#3. convert all categorical to dummy variables

library(caret)

# examine the factors first to check if all have the correct class('factor')
# it turns out that $loan term is a date and should be converted to factor instead

car$Loan_Term <- as.factor(car$Loan_Term)

# application_creation_date is also treated as factor,
# we will try to convert this to a new feature instead
# calculate the number of months since application based on this info

car$application_creation_date <- as.numeric(car$application_creation_date)
str(car)

dummy <- dummyVars(~. - 1, car)# -1 indicates do not include intercept
car_dummy <- predict(dummy, car)

str(car_dummy)
