quiz <- read.csv("quiz_data.csv")

group_mean <- aggregate(Score ~ Are_You, data = quiz, mean)
group_mean

group_mean[order(group_mean$Score),]

# null hypothesis: avg score of seniors is same as juniors
# alternative hypothesis: svg score of seniors > avg score of juniors

z_score <- function(x,y){
  z<-(mean(x)-mean(y))/(sqrt(sd(x)^2/length(x) +sd(y)^2/length(y)))
  return(z)
}
head(quiz)
z_score(quiz[quiz$Are_You == "Senior", "Score."], quiz[quiz$Are_You == "Junior", "Score."])

quiz[quiz$Are_You == "Senior",]

quiz[quiz$Are_You == "Senior", "Score."]

1-pnorm(4.18154)


1-pnorm(z_score(quiz[quiz$Are_You == "Senior", "Score."], quiz[quiz$Are_You == "Sophomore", "Score."]))

z_score(quiz[quiz$Are_You == "Senior", "Score."], quiz[quiz$Are_You == "Junior", "Score."])

`PermutationTestSecond::Permutation(quiz, "Are_You", "Score.", 1000, "Junior", "Senior")`

PermutationTestSecond::Permutation(quiz, "Are_You", "Score.", 1000, "Freshman", "Senior")

chisq.test(quiz$Are_You, quiz$'Score.')

head(quiz)

typeof(quiz$Score.)
