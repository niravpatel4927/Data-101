moody <- read.csv("MOODY_DATA.csv")

# head(moody) returns first 6 rows
# summary(moody) gives average, median, etc of each column

summary(moody)

# unique(moody$GRADE) gets all unique values within a specific column
unique(moody$GRADE)

# table(moody$GRADE) returns count or frequency of each value within the GRADE column
x <- table(moody$GRADE)
x

# as.factor(moody$GRADE) helps us save time so we don't have to keep writing out
# the colors for each bar
barplot(x, col = as.factor(moody$GRADE))

barplot(x, col = c("red", "blue", "cyan", "yellow", "pink"), xlab = "GRADE", ylab = "Count", 
        main = "Number of Students Who Got Each Grade")

y <- table(moody$GRADE, moody$ON_SMARTPHONE)

# in mosaic plot, width = density of the variables on x axis, height = density of 
# students on y axis
mosaicplot(y, xlab = "Grade", ylab = "On Smartphone", main = "How Being on Smartphone Affects Grade", 
           col = as.factor(moody$GRADE))


#subsetting
a <- moody[moody$GRADE == "F",]
unique(a$GRADE)


# explicit subset
b <- subset(moody, GRADE == "F")



c <- moody[moody$GRADE == "F", c(1:3)]
c

# when using brackets, first parameter(before the comma) is for rows, second is for columns
d <- moody[, c(1:3, 6)]
head(d)


e <- subset(moody, select = c(1:3, 6))
head(e)

f <- tapply(moody$SCORE, moody$ON_SMARTPHONE, mean)
f

barplot(f, col = c("thistle", "cyan", "red"), ylim = c(0, 100))
