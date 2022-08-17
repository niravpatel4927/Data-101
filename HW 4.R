movies <- read.csv("Movies2022F-3.csv")
head(movies)

summary(movies)

unique(movies$Budget)
unique(movies$genre)
unique(movies$Gross)
unique(movies$country)
unique(movies$content)

# null hypothesis:  having a high budget will yield the same IMDB score as having a low budget
# alternative hypothesis: having a lower budget yields a higher IMDB score than having a higher budget


boxplot(movies$imdb_score~movies$Budget, xlab = "Budget", ylab = "IMDB Score", main = "IMDB Score based on Budget",
        col = as.factor(movies$imdb_score))

high_budget <- subset(movies, movies$Budget == "High")
low_budget <- subset(movies, movies$Budget == "Low")
medium_budget <- subset(movies, movies$Budget == "Medium")

len_high_budget <- length(high_budget$imdb_score)
len_low_budget <- length(low_budget$imdb_score)

mean_high_budget <- mean(high_budget$imdb_score)
mean_low_budget <- mean(low_budget$imdb_score)

sd_high_budget <- sd(high_budget$imdb_score)
sd_high_budget

sd_low_budget <- sd(low_budget$imdb_score)

sd_high_low_budget <- sqrt(sd_high_budget^2/len_high_budget + sd_low_budget^2/len_low_budget)
sd_high_low_budget


zeta_budget <- (mean_low_budget - mean_high_budget)/sd_high_low_budget
zeta_budget
p_budget = 1-pnorm(zeta_budget)
p_budget

plot(x=seq(from = -40, to= 40, by=0.1),y=dnorm(seq(from = -40, to= 40,  by=0.1),mean=0),
     type='l',xlab = 'mean difference',  ylab='possibility',
     main = "Probability of a lower budget yielding a higher IMDB score than a high budget movie")
abline(v=zeta_budget, col='red')

# more mature movies have no impact on IMDB score
# alternative: more mature movies have a greater IMDB score 

mature_movies <- movies[movies$content == "PG-13" | movies$content == "R",]
child_friendly_movies <- movies[movies$content == "PG" | movies$content == "G",]
head(mature_movies)
head(child_friendly_movies)
unique(mature_movies$content)
unique(child_friendly_movies$content)

len_mature_movies <- length(mature_movies$imdb_score)
len_child_friendly_movies <- length(child_friendly_movies$imdb_score)

mean_mature_movies <- mean(mature_movies$imdb_score)
mean_child_friendly_movies <- mean(child_friendly_movies$imdb_score)

sd_mature_movies <- sd(mature_movies$imdb_score)
sd_child_movies <- sd(child_friendly_movies$imdb_score)

sd_all_movies <- sqrt(sd_mature_movies^2/len_mature_movies + sd_child_movies^2/len_child_friendly_movies)

zeta_movies <- (mean_mature_movies - mean_child_friendly_movies)/sd_all_movies
zeta_movies

p_movies = 1-pnorm(zeta_movies)

plot(x=seq(from = -10, to= 10, by=0.1),y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),type='l', 
     xlab = 'mean difference',  ylab='possibility', main = "Probability of more mature movies having a greater score than less mature movies")
abline(v=zeta_movies, col='red')


# null hypothesis: High grossing movies have roughly the same IMDB score as low grossing movies
# alternative hypothesis: Lower grossing movies have higher IMDB scores than higher grossing movies

unique(movies$Gross)

high_gross <- subset(movies, movies$Gross == "High")
low_gross <- subset(movies, movies$Gross == "Low")

len_high_gross <- length(high_gross$imdb_score)
len_low_gross <- length(low_gross$imdb_score)

mean_high_gross <- mean(high_gross$imdb_score)
mean_low_gross <- mean(low_gross$imdb_score)

sd_high_gross <- sd(high_gross$imdb_score)
sd_low_gross <- sd(low_gross$imdb_score)

sd_high_low_gross <- sqrt(sd_high_gross^2/len_high_gross + sd_low_gross^2/len_low_gross)

z_gross <- (mean_low_gross - mean_high_gross)/sd_high_low_gross

p_gross <- 1-pnorm(z_gross)

plot(x=seq(from = -10, to= 10, by=0.1),y=dnorm(seq(from = -10, to= 10,  by=0.1),mean=0),type='l', 
     xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of lower grossing movies having a greater score than higher grossing movies")
abline(v=z_gross, col='red')


pnorm(0)
