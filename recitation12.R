setwd("D:/My_Files/Rutgers/TAship/Spring 2022/Data 101/Recitation 12/")
#------------installing packages for using function mse()--------------
install.packages("ModelMetrics")
library(ModelMetrics)
#----------------------------------------------------------------------
#Renaming the dataset and using lm() to build the model for the entire dataset
Data.puzzle.RecitationApril17 <- read.csv('Data-puzzle-RecitationApril17.csv')
puzzle_data <- Data.puzzle.RecitationApril17
model <- lm(CLASSSCORE~., data = puzzle_data)
summary(model)
train_pred <- predict(model, newdata = puzzle_data)
errorMSE_complete_data <- mse(puzzle_data$CLASSSCORE, train_pred)
errorMSE_complete_data
#Subsetting the dataset based on ClassName for 'Data101' and 'Databases'

#For ClassName = Data101
data101 <- subset(puzzle_data, ClassName == 'Data101')
split1 <- 0.7*nrow(data101)
split1
data101_train <- data101[1:split1,]
data101_train
data101_test <- data101[split1:nrow(data101),]
model1 <- lm(CLASSSCORE~Midterm+Project+Final, data = data101_train)
summary(model1)

## Train Evaluation for 'Data101'
train_pred1 <-  predict(model1, newdata = data101_train)
errorMSE_D101_train <- mse(data101_train$CLASSSCORE, train_pred1)
errorMSE_D101_train

## Test Evaluation for 'Data101'
test_pred1 <- predict(model1, newdata = data101_test)
errorMSE_D101_test <- mse(data101_test$CLASSSCORE, test_pred1)
errorMSE_D101_test
#-------------------------------------------------------------------------------
#For ClassName = Databases
databases <- subset(puzzle_data, ClassName == 'Databases')
split2 <- 0.7*nrow(databases)
split2
databases_train <- databases[1:split2,]
databases_train
databases_test <- databases[split2:nrow(databases),]
model2 <- lm(CLASSSCORE~Midterm+Project+Final, data = databases_train)
summary(model2)

#Train evaluation for 'Databases'
train_pred2 <- predict(model2, newdata = databases_train)
errorMSE_DB_train <- mse(databases_train$CLASSSCORE, train_pred2)
errorMSE_DB_train

#Test evaluation for 'Databases'
test_pred2 <- predict(model2, newdata = databases_test)
errorMSE_DB_test <- mse(databases_test$CLASSSCORE, test_pred2)
errorMSE_DB_test

#Combining the models
data_prediction <- puzzle_data
data_prediction[data_prediction$ClassName =='Data101','CLASSSCORE'] <- predict(model1, newdata = data101)
data_prediction[data_prediction$ClassName == 'Databases','CLASSSCORE' ] <-predict(model2, newdata = databases)
data_prediction$CLASSSCORE
errorMSE_combined <- mse(puzzle_data$CLASSSCORE, data_prediction$CLASSSCORE)
errorMSE_combined
#------------One-Step Cross validation------------------
train <- Data.puzzle.RecitationApril17
nrow(train)
summary(train)
#scramble the train
v <- sample(1:nrow(train))
trainScrambled <- train[v, ]
trainScrambled

#---------------rpart for numerical data-------------------
# First lets import the rpart library, make sure your Rstudio is updated
#install.packages("rpart")
library(rpart)

#Rename the dataset
rpart_puzzle <- Data.puzzle.RecitationApril17

# Use of the rpart() function.
tree_rpart <- rpart(CLASSSCORE ~ Midterm+Project+Final, data = rpart_puzzle ,method = "anova")
tree_rpart

# Now lets predict the Grades of the Moody Dataset.
pred_rpart <- predict(tree_rpart, rpart_puzzle)
head(pred_rpart)