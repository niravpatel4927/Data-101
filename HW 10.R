moody <- read.csv("M2022train.csv")

head(moody)
library(rpart)
library(rpart.plot)

tree <- rpart(Grade ~ Major+Score+Seniority,data=moody, method="class",
              control = rpart.control(minbucket = 1, minsplit = 2, cp = 0.001))
tree
rpart.plot(tree)

pred <- predict(tree, newdata=moody, type = "class")
moody$predict <- pred
error <- mean(moody$Grade != moody$predict)
error
#prp(tree, type = 1)

model1<-rpart(Grade ~ Major+Score+Seniority, data=moody[moody$Score > 50,], method="class", 
              control = rpart.control(minbucket = 1, minsplit = 2, cp = 0.001))
model2<-rpart(Grade ~ Major+Score+Seniority, data=moody[moody$Score <= 50,], method="class",
              control = rpart.control(minbucket = 1, minsplit = 2, cp = 0.001))
model1
model2
pred1 <- predict(model1, newdata=moody[moody$Score>50,], type="class")
pred2 <- predict(model2, newdata=moody[moody$Score<=50,], type="class")
myprediction<-moody
decision <- rep('F',nrow(myprediction))
decision[myprediction$Score>50] <- as.character(pred1)
decision[myprediction$Score<=50] <-as.character(pred2)
myprediction$Grade <- decision
error <- mean(moody$Grade != myprediction$Grade)
error


CrossValidation::cross_validate(moody, tree, 5, 0.8)



test <- read.csv("M2022testSNoGrade.csv")
submission <- read.csv("M2022submissionS.csv")
mysub <- read.csv("submissions.csv")
prediction <- predict(tree, test, type = 'class')
submission$Grade <- prediction
write.csv(submission, 'submissions.csv', row.names = FALSE)
error <- mean(prediction$Grade != submission$Grade)
error
