#permutation test package 

install.packages("devtools")

#Run this command
devtools::install_github("devanshagr/PermutationTestSecond")

#Reading Dataset

getwd()
setwd("C:\\Users\\Advait\\Desktop\\Data_101\\")
moody <- read.csv("MOODY_DATA.csv")
TRAFFIC <-read.csv("TRAFFIC.csv")

#Subsetting Dataset

mod <- subset (moody, select = c(2,5))


#Performing permutation Testing Example 1 (Moody Data)

#Checking the mean for fifth and sixth parameter in permutation command.
# The parameter which has lower mean will come in the fifth spot. 
# The parameter which has higher mean will come in the sixth spot

mean_never<-subset(mod, mod$ASKS_QUESTIONS == "never")
mean(mean_never$SCORE)
mean_always<-subset(mod, mod$ASKS_QUESTIONS == "always")
mean(mean_always$SCORE)

PermutationTestSecond::Permutation(mod, "ASKS_QUESTIONS", "SCORE",1000,"always", "never")


#Performing permutation Testing example 2 (Traffic Data)


PermutationTestSecond::Permutation(TRAFFIC, "TUNNEL", "VOLUME_PER_MINUTE",1000,"Holland", "Lincoln")

#Run the permutation test once

PermutationTestManual::permute_test(TRAFFIC, "TUNNEL", "VOLUME_PER_MINUTE", "Holland", "Lincoln")

#Performing permutation Testing example 3 (Moody Data)
#Null hypothesis: Average score for good participation is the same as average score for bad participation



moody[,8] <- cut(moody$SCORE, breaks = c(0,30,60,100),labels = c("bad_participation","average_participation","good_participation"))

colnames(moody)[8] <- "Part_Category"

PermutationTestSecond::Permutation(moody, "Part_Category", "SCORE",10000,"good_participation", "bad_participation")




# Extra Example 1
lincoln.data <- subset(TRAFFIC, TRAFFIC$TUNNEL == "Lincoln")
holland.data <- subset(TRAFFIC, TRAFFIC$TUNNEL == "Holland")

lincoln.TRAFFIC <- lincoln.data$VOLUME_PER_MINUTE
holland.TRAFFIC <- holland.data$VOLUME_PER_MINUTE

mean.lincoln <- mean(lincoln.TRAFFIC)
mean.holland <- mean(holland.TRAFFIC)

PermutationTestSecond::Permutation(TRAFFIC, "TUNNEL", "VOLUME_PER_MINUTE",1000,"Holland", "Lincoln")


#Extra Example 2
never.data <- subset(moody, moody$ASKS_QUESTIONS == "never")
always.data <- subset(moody, moody$ASKS_QUESTIONS == "always")

never.ques <- never.data$SCORE
always.ques <- always.data$SCORE

mean.never <- mean(never.ques)
sprintf("%f mean value who never ask questions", mean.never) # this is print statement 
mean.always <- mean(always.ques)
sprintf("%f mean who always ask questions", mean.always)

x <- PermutationTestSecond::Permutation(mod, "ASKS_QUESTIONS", "SCORE",10000,"always", "never")
sprintf("%f p-value using permutation_test", x)

print("Hence, we fail to reject our null hypothesis that -> mean score of students who ask questions is same as mean score of students who do not ask questions ")


