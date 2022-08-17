movies <- read.csv("Movies2022F-3.csv")
head(movies)
unique(movies$country)
unique(movies$Budget)

# null hypothesis: the imdb score of g rated movies is the same as imdb score of r rated movies
# alternative hypothesis: R rated movies have a higher imdb score than G rated movies

g_movies <- subset(movies, movies$content == "G" & movies$country == "UK")
r_movies <- subset(movies, movies$content == "R" & movies$country == "UK")


g_movies <- subset(g_movies, select = c(2, 3))
mean_g <- mean(g_movies$imdb_score)
mean_g
r_movies <- subset(r_movies, select = c(2, 3))
mean_r <- mean(r_movies$imdb_score)
mean_r

combined_data <- rbind(g_movies, r_movies)

PermutationTestSecond::Permutation(combined_data, "content", "imdb_score", 10000, "R", "G")

PermutationTestSecond::Permutation(combined_data, "content", "imdb_score", 10000, "R", "G")

PermutationTestSecond::Permutation(combined_data, "content", "imdb_score", 10000, "R", "G")
