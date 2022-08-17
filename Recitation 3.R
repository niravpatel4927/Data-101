census <- read.csv("CENSUS.csv")

# look at what the data is comprised of
head(census)
summary(census)

table(census$STATUS)

sum(is.na(census$STATUS))

table(census$EDUCATION)
table(census$PROFESSION)
table(census$NATIVE)

# find size of data
nrow(census)
ncol(census)

# adding a new column in the dataset
census[,7] <- ""
head(census)

census[census$NATIVE == "Thailand",]$V7<-1

sum(is.na(census$NATIVE))

census[,8] <- ""
census[census$STATUS == "Federal-gov", 8] <- "YES"

# due to the recitations TA's not konwing wtf is happening or that there would be a 
# classwide error, we will now switch to moody dataset

moody <- read.csv("MOODY_DATA.csv")
moody[moody$GRADE == "F",8] <- "Fail"
head(moody)
moody[moody$ON_SMARTPHONE == "frequently", 9] <- "Yes"
head(moody)
moody[is.na(moody$V9),9] <- "NO"
head(moody)
moody[moody$ON_SMARTPHONE == "never" | moody$ASKS_QUESTIONS == "never",10] <- "No_Clue"
head(moody)

colnames(moody)[10] <- "Tenth"
head(moody)

range(moody$SCORE)
?cut()

moody$SCORE_Labelled <- cut(as.numeric(moody$SCORE), breaks = c(0, 25, 50, 75, 100), labels = c("<25", "25-50", "50-75", "75+"))

head(moody)

barplot(table(moody$SCORE_Labelled), col = as.factor(moody$SCORE_Labelled))
