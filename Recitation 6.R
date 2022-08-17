moody <- read.csv("moody2022_new.csv")


Expected <- matrix(c(200, 430, 180, 35, 120, 45), nrow = 3, ncol = 2)
Observed <- matrix(c(160, 480, 160, 40, 120, 40), nrow = 3, ncol= 2)
Expected
Observed
chisq.test(Expected, Observed, correct = FALSE)

#df = degrees of freedom, calculated as (no. of rows-1) * (no. of columns-1)

