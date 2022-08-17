train<-read.csv("M2022train.csv")
test <- read.csv("M2022testSNoGrade.csv")

summary(train)
#scramble the train frame
v<-sample(1:nrow(train))
v[1:30]
trainScrambled<-train[v, ]
#one step crossvalidation
trainSample<-trainScrambled[1:270, ]
crossValidation <- trainScrambled[271:300, ]
myprediction <-trainSample
mypred1 <- crossValidation


decision <- rep('F',nrow(mypred1))
decision[mypred1$Score >= 85 & mypred1$Seniority == "Freshman"] <- 'A'

decision[mypred1$Score >= 85 & mypred1$Seniority == "Sophomore" & mypred1$Major != "CS"] <- 'A'
decision[mypred1$Score >= 90 & mypred1$Seniority == "Sophomore" & mypred1$Major == "CS"] <- 'A'
decision[(mypred1$Score < 90 & mypred1$Score >= 85) 
         & mypred1$Seniority == "Sophomore" & mypred1$Major == "CS"] <- 'B'

decision[mypred1$Score >= 85 & mypred1$Seniority == "Junior" & mypred1$Major != "CS"] <- 'A'
decision[(mypred1$Score > 90) & mypred1$Seniority == "Junior" & mypred1$Major == "CS"] <- 'A'
decision[(mypred1$Score <= 90 & mypred1$Score >= 85) & mypred1$Seniority == "Junior" & mypred1$Major == "CS"] <- 'B'

decision[mypred1$Score >= 85 & mypred1$Seniority == "Senior" & mypred1$Major != "CS"] <- 'A'
decision[mypred1$Score > 90 & mypred1$Seniority == "Senior" & mypred1$Major == "CS"] <- 'A'
decision[(mypred1$Score >= 85 & mypred1$Score <= 90) & mypred1$Seniority == "Senior" & mypred1$Major == "CS"] <- 'B'



decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "CS"] <- 'B'
decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Freshman" 
         & mypred1$Major != "CS"] <- 'A'

decision[(mypred1$Score < 85 & mypred1$Score > 70) & mypred1$Seniority == "Sophomore" 
         & mypred1$Major != "Psychology"] <- 'B'
decision[(mypred1$Score < 70 & mypred1$Score >= 65) & mypred1$Seniority == "Sophomore" 
         & mypred1$Major != "Psychology"] <- 'C'
decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Sophomore"
         & mypred1$Major == "Psychology"] <- 'A'
#decision[(mypred1$Score < 70 & mypred1$Score >= 65) & mypred1$Seniority == "Sophomore"& mypred1$Major == "Psychology"] <- 'B'

decision[(mypred1$Score < 85 & mypred1$Score >= 80) & mypred1$Seniority == "Junior"
         & mypred1$Major == "CS"] <- 'B'
decision[(mypred1$Score < 80 & mypred1$Score >= 65) & mypred1$Seniority == "Junior"
         & mypred1$Major == "CS"] <- 'C'
decision[(mypred1$Score < 85 & mypred1$Score >= 75) & mypred1$Seniority == "Junior"
         & mypred1$Major == "Statistics"] <- 'B'
decision[(mypred1$Score < 75 & mypred1$Score >= 65) & mypred1$Seniority == "Junior"
         & mypred1$Major == "Statistics"] <- 'C'
decision[(mypred1$Score < 85 & mypred1$Score >= 71) & mypred1$Seniority == "Junior"
         & mypred1$Major == "Psychology"] <- 'A'
decision[(mypred1$Score <= 70 & mypred1$Score >= 65) & mypred1$Seniority == "Junior"
         & mypred1$Major == "Psychology"] <- 'B'
decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Junior"
         & mypred1$Major == "Economics"] <- 'B'

decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Senior"
         & (mypred1$Major == "CS" | mypred1$Major == "Statistics")] <- 'C'

decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Senior"
         & mypred1$Major == "Economics"] <- 'B'
decision[(mypred1$Score < 85 & mypred1$Score >= 65) & mypred1$Seniority == "Senior"
         & mypred1$Major == "Psychology"] <- 'A'

decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "CS"] <- 'C'
decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "Economics"] <- 'B'
decision[(mypred1$Score < 65 & mypred1$Score > 55) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "Statistics"] <- 'B'
decision[(mypred1$Score <= 56 & mypred1$Score >= 50) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "Statistics"] <- 'C'
decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "Psychology"] <- 'A'

decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Sophomore" 
         & mypred1$Major == "CS"] <- 'D'
decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Sophomore" 
         & (mypred1$Major == "Statistics" | mypred1$Major == "Economics")] <- 'C'
decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Sophomore" 
         & mypred1$Major == "Psychology"] <- 'B'

decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Junior" 
         & mypred1$Major != "CS"] <- 'C'
decision[(mypred1$Score < 60 & mypred1$Score > 50) & mypred1$Seniority == "Junior" 
         & mypred1$Major == "CS"] <- 'D'
decision[(mypred1$Score < 65 & mypred1$Score > 58) & mypred1$Seniority == "Junior" 
         & mypred1$Major == "CS"] <- 'C'

decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Senior" 
         & mypred1$Major != "CS"] <- 'C'
decision[(mypred1$Score < 65 & mypred1$Score > 50) & mypred1$Seniority == "Senior" 
         & mypred1$Major == "CS"] <- 'D'

decision[(mypred1$Score < 50 & mypred1$Score > 30) & mypred1$Seniority == "Freshman" 
         & mypred1$Major == "Psychology"] <- 'C'
decision[mypred1$Score < 30 & mypred1$Seniority == "Freshman" & mypred1$Major == "Psychology"] <- 'D'

decision[mypred1$Score < 50 & mypred1$Seniority == "Freshman" & (mypred1$Major == "CS" | mypred1$Major == "Statistics")] <- 'D'
decision[(mypred1$Score < 50) & mypred1$Seniority == "Freshman" & mypred1$Major == "Economics"] <- 'D'



decision[(mypred1$Score < 50 & mypred1$Score >= 30) & mypred1$Seniority == "Sophomore" 
         & mypred1$Major == "Psychology"] <- 'D'



mypred1$Grade <- decision

#write.csv(test$Grade, "test_submition.csv")


error <- mean(crossValidation$Grade != mypred1$Grade)
error   
