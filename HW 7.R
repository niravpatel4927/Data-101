titanic <- read.csv("titanic.csv")
head(titanic)

titanic <- subset(titanic, select = c(1, 2, 3, 5, 6, 7, 8, 9, 10, 12))
head(titanic)

# turn Survived into Categorical
titanic$Survived <- replace(titanic$Survived, titanic$Survived == 0, "Dead")
titanic$Survived <- replace(titanic$Survived, titanic$Survived == 1, "Alive")

# turned Pclass into Categorical data
titanic$Pclass <- replace(titanic$Pclass, titanic$Pclass == 1, "First Class")
titanic$Pclass <- replace(titanic$Pclass, titanic$Pclass == 2, "Second Class")
titanic$Pclass <- replace(titanic$Pclass, titanic$Pclass == 3, "Third Class")
head(titanic)

unique(titanic$Embarked)
titanic$Embarked <- replace(titanic$Embarked, titanic$Embarked == "Q", "Queenstown")
titanic$Embarked <- replace(titanic$Embarked, titanic$Embarked == "S", "Southampton")
titanic$Embarked <- replace(titanic$Embarked, titanic$Embarked == "C", "Cherbourg")
head(titanic)

data <- table(titanic$Survived, titanic$Pclass)
data

mosaicplot(titanic$Survived~titanic$Pclass, xlab = "Survived", ylab = "Passenger Class", 
           main = "Survived vs. Passenger Class", col = c("deepskyblue3", "darkturquoise", "cyan"))


length(titanic$PassengerId)
nonull_titanic <-  na.omit(titanic)

length(nonull_titanic$PassengerId)

head(titanic)


table(titanic$Survived)
table(titanic$Pclass)



# Null hypothesis: Surviving the crash of the Titanic and the class of the passenger is independent
# Alternative hypothesis: Surviving the crash and the class are related
data <- table(titanic$Survived, titanic$Pclass)
data


chisq.test(data)
chisq.test(titanic$Survived, titanic$Pclass)
# Results: 
# X-squared = 6.6939, df = 2, p-value = 0.03519

# thus we can see that with a p-value of 0.03519, or 3.5% that the null hypothesis
# was rejected and the Surviving and the class are related

# PERSONAL NOTE: take percentage of people that survive from both alive and dead
# and use that as analysis

titanic1 <- tapply(titanic$Fare, titanic$Survived, mean, na.rm = TRUE)
titanic1
# Average price of people alive was 49.74, while dead was 27.529


boxplot(titanic$Fare~titanic$Survived, xlab = "Survived", ylab = "Ticket Fare", 
        main = "How ticket price affects the survivability", col = c("deepskyblue3", "darkorchid"),outline = FALSE)

# null hypothesis: Fare for people that survived is same as fare for people that died
# alternative hypothesis: Fare for people that survived is higher than fare for people that died
alive <- subset(titanic, titanic$Survived == "Alive")
dead <- subset(titanic, titanic$Survived == "Dead")

alive.fare <- alive$Fare
dead.fare <- dead$Fare


sd_alive <- sd(alive.fare)
sd_dead <- sd(dead.fare, na.rm = TRUE)

mean_alive <- sd(alive.fare)
mean_dead <- sd(dead.fare, na.rm = TRUE)

len_alive <- length(alive.fare)
len_dead <- length(dead.fare)

sd_alive_dead <- sqrt(sd_alive^2/len_alive + sd_dead^2/len_dead)

z_survived <- (mean_alive - mean_dead)/sd_alive_dead

plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l'
     ,xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of Fare for Survived being higher than fare of those that died")
abline(v=z_survived, col='red')

#get p
p = 1-pnorm(z_survived)
p

# p value was 3.322782e-07, meaning we can reject the null hypothesis, so that the
# chance of surviving with a higher fare is statistically significant 

head(titanic)
permutation_data <- subset(titanic, select = c(2, 9))
head(permutation_data)
# Do permutation test to make sure result isn't random
# titanic is the main dataset
PermutationTestSecond::Permutation(permutation_data, "Survived", "Fare", 10000,"Alive", "Dead")







class_count <- table(titanic$Pclass)
barplot(class_count, xlab = "Passenger Class", ylab = "Count", main = "Total Number for each Class", 
        col = c("deepskyblue3", "darkorchid", "cyan"))





head(titanic)
boxplot(titanic$Fare~titanic$Pclass, xlab = "Passenger Class", ylab = "Ticket Fare", 
        main = "How ticket price affects the class", col = c("deepskyblue3", "darkorchid"),outline = FALSE)

titanic2 <- tapply(titanic$Fare, titanic$Pclass, mean, na.rm = TRUE)
titanic2

first_class <- subset(titanic, titanic$Pclass == "First Class")
second_class <- subset(titanic, titanic$Pclass == "Second Class")
third_class <- subset(titanic, titanic$Pclass == "Third Class")

first_fare <- first_class$Fare
second_fare <- second_class$Fare
third_fare <- third_class$Fare

mean_first <- mean(first_fare, na.rm = TRUE)
mean_second <- mean(second_fare, na.rm = TRUE)
mean_third <- mean(third_fare, na.rm = TRUE)

len_first <- length(first_fare)
len_second <- length(second_fare)
len_third <- length(third_fare)

sd_first <- sd(first_fare, na.rm = TRUE)
sd_second <- sd(second_fare, na.rm = TRUE)
sd_third <- sd(third_fare, na.rm = TRUE)

sd_first_second <- sqrt(sd_first^2/len_first + sd_second^2/len_second)
sd_second_third <- sqrt(sd_second^2/len_second + sd_third^2/len_third)

z_first_second <- (mean_first - mean_second)/sd_first_second
z_second_third <- (mean_second - mean_third)/sd_second_third


plot(x=seq(from = -10, to= 10, by=0.1),y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),type='l'
     ,xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of Fare for Second Class being higher than fare of third class")
abline(v=z_second_third, col='red')

p_first_second = 1-pnorm(z_first_second)
p_first_second

p_second_third <- 1-pnorm(z_second_third)
p_second_third

# looking at sex and survival
gender_survival <- table(titanic$Survived, titanic$Sex)
gender_survival

mosaicplot(titanic$Survived~titanic$Sex, xlab = "Survived", ylab = "Sex", 
           main = "Survived vs. Sex", col = c("deepskyblue3", "darkturquoise", "cyan"))

chisq.test(gender_survival)

# basically, if you were a male you were guaranteed to die


# sex and fare

boxplot(titanic$Fare~titanic$Sex, xlab = "Sex", ylab = "Ticket Fare", 
        main = "How Sex affected the ticket price", col = c("deepskyblue3", "darkorchid"),outline = FALSE)

#null hypothesis: the fare for females is the same as the fare for males
#alternative hypothesis: the fair for females is higher than the fare for males
head(titanic)
female <- subset(titanic, titanic$Sex == "female")
male <- subset(titanic, titanic$Sex == "male")

f_fare <- female$Fare
m_fare <- male$Fare

mean_female <- mean(f_fare, na.rm = TRUE)
mean_male <- mean(m_fare, na.rm = TRUE)

len_female <- length(f_fare)
len_male <- length(m_fare)

sd_female <- sd(f_fare, na.rm = TRUE)
sd_male <- sd(m_fare, na.rm = TRUE)

sd_f_m <- sqrt(sd_male^2/len_male + sd_female^2/len_female)

z_f_m <- (mean_female - mean_male)/sd_f_m

plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l'
     ,xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of Fare for Females being higher than fare of Males")
abline(v=z_f_m, col='red')

p_f_m <- 1-pnorm(z_f_m)
p_f_m


# Age vs. Sex
# null hypothesis: age of females is the same as age of males
# alternative hypothesis: age of males is higher than age of females

boxplot(titanic$Age~titanic$Sex, xlab = "Sex", ylab = "Age", 
        main = "Age Distribution Across Sexes", col = c("deepskyblue3", "darkorchid"),outline = FALSE)

f_age <- female$Age
m_age <- male$Age

mean_f_age <- mean(f_age, na.rm = TRUE)
mean_m_age <- mean(m_age, na.rm = TRUE)

len_f_age <- length(f_age)
len_m_age <- length(m_age)

sd_f_age <- sd(f_age, na.rm = TRUE)
sd_m_age <- sd(m_age, na.rm = TRUE)

sd_fm_age <- sqrt(sd_f_age^2/len_f_age + sd_m_age^2/len_m_age)

z_fm_age <- (mean_m_age - mean_f_age)/sd_fm_age

plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l'
     ,xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of Age for Females being higher than age of Males")
abline(v=z_fm_age, col='red')

p_fm_age = 1-pnorm(z_fm_age)
p_fm_age

# can not reject the null hypothesis

boxplot(titanic$Age~titanic$Survived, xlab = "Survived", ylab = "Age", 
        main = "Age Distribution across Survival", col = c("deepskyblue3", "darkorchid", outline = FALSE))

alive.age <- alive$Age
dead.age <- dead$Age

mean_alive_age <- mean(alive.age, na.rm = TRUE)
mean_dead_age <- mean(dead.age, na.rm = TRUE)

len_alive_age <- length(alive.age)
len_dead_age <- length(dead.age)

sd_alive_age <- sd(alive.age, na.rm = TRUE)
sd_dead_age <- sd(dead.age, na.rm = TRUE)

sd_living_age <- sqrt(sd_alive_age^2/len_alive_age + sd_dead_age^2/len_dead_age)

z_living_age <- (mean_alive_age - mean_dead_age)/sd_living_age

plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l'
     ,xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of Age for Survivors being higher than age of those that died")
abline(v=z_living_age, col='red')

p_living_age = 1-pnorm(z_living_age)
p_living_age

