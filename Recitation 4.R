library(ggplot2)
library(statsr)
library(gridExtra)
library(lubridate)

moody <- read.csv("moody2022_new.csv")
head(moody)
dim(moody)
str(moody) # gets structure of dataset
summary(moody)

explore <- function(x){
  data <- c("Mean" = mean(x, na.rm = T),
            "Median" = median(x, na.rm = T),
            "Standard Deviation" = sd(x, na.rm = T),
            "Length" = length(x))
  return(data)
}
explore(moody$SCORE)

# Null hypothesis: Score of students never using smartphone is same as those frequently/always using it
# alternative hypothesis: score of students never using smartphone is higher than those frequently/always using it

always <- subset(moody, moody$TEXTING_IN_CLASS == "always")
never <- subset(moody, moody$TEXTING_IN_CLASS == "never")

table(moody$TEXTING_IN_CLASS)

mean.cat1 <- mean(always$SCORE, na.rm = TRUE) 
# na.rm = TRUE means to calculate the mean after dropping missing and null values

mean.cat2 <- mean(never$SCORE)

sd.cat1 <- sd(always$SCORE)
sd.cat2 <- sd(never$SCORE)

length.cat1 <- length(always$SCORE)
length.cat2 <- length(never$SCORE)

sd.cat1.cat2 <- sqrt(sd.cat1^2/length.cat1 + sd.cat2^2/length.cat2)

z <- (mean.cat2 - mean.cat1)/sd.cat1.cat2
z

plot(x=seq(from = -10, to= 10, by=0.1),y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),type='l', 
     xlab = 'mean difference',  ylab='possibility')
abline(v=z, col='red')

p = 1-pnorm(z)
