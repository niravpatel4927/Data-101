moody <- read.csv("moody2022_new.csv")

head(moody)

never_dozes <- subset(moody, moody$DOZES_OFF == "never")

mean(never_dozes$PARTICIPATION)

mean(moody[moody$DOZES_OFF == "never",]$PARTICIPATION)

nrow(moody[moody$DOZES_OFF == "always" & moody$TEXTING_IN_CLASS == "always",])

unique(moody$TEXTING_IN_CLASS)

q2 <- subset(moody, moody$DOZES_OFF == "always" & moody$TEXTING_IN_CLASS == "always")
nrow(q2)

failed <- subset(moody, moody$GRADE == "F")
max(failed$SCORE)

sort(failed$SCORE)
failed


mean(moody[moody$SCORE > 80,]$PARTICIPATION)

max(moody[moody$DOZES_OFF == "always",]$GRADE)

table(moody$TEXTING_IN_CLASS)

table(moody[moody$SCORE >70,]$GRADE)

min(moody[moody$DOZES_OFF == "never" & moody$TEXTING_IN_CLASS == "never" & moody$PARTICIPATION > 0.8,]$SCORE)

min(moody[moody$SCORE >70,]$GRADE)
