movies <- read.csv("Movies2022F-3.csv")
head(movies)

summary(movies)

unique(movies$Budget)
unique(movies$genre)
unique(movies$Gross)
unique(movies$country)
unique(movies$content)

# A
# null hypothesis: null hypothesis: High grossing movies have roughly the same IMDB score as low grossing movies
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

# C
# null hypothesis: High grossing movies have the same imdb score as mature movies
# alternative hypothesis: Mature movies have a higher imdb score than high grossing movies

#reused variables from homework 4 that were already saved
len_mature_movies <- length(mature_movies$imdb_score)
mean_mature_movies <- mean(mature_movies$imdb_score)
sd_mature_movies <- sd(mature_movies$imdb_score)


sd_high_gross_mature <- sqrt(sd_high_gross^2/len_high_gross + sd_mature_movies^2/len_mature_movies)
z_high_gross_mature <- (mean_mature_movies - mean_high_gross)/sd_high_gross_mature

p_high_gross_mature <- 1-pnorm(z_high_gross_mature)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l', 
     xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of mature movies having a greater score than high grossing movies")
abline(v=z_high_gross_mature, col='red')

# Another example for A
# null hypothesis: the imdb score of g rated movies is the same as imdb score of r rated movies
# alternative hypothesis: R rated movies have a higher imdb score than G rated movies

g_movies <- subset(movies, movies$content == "G")
r_movies <- subset(movies, movies$content == "R")

len_g <- length(g_movies$imdb_score)
len_r <- length(r_movies$imdb_score)

mean_g <- mean(g_movies$imdb_score)
mean_r <- mean(r_movies$imdb_score)

sd_g <- sd(g_movies$imdb_score)
sd_r <- sd(r_movies$imdb_score)

sd_g_r <- sqrt(sd_g^2/len_g + sd_r^2/len_r)

z_g_r <- (mean_r - mean_g)/sd_g_r

p_g_r <- 1-pnorm(z_g_r)
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l', 
     xlab = 'mean difference',  ylab='possibility', 
     main = "Probability of R movies having a greater score than G movies")
abline(v=z_g_r, col='red')

