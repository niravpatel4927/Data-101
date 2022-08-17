# import dataset

moody <- read.csv('moody2022_new.csv')

head(moody)

color = c("deepskyblue2", "dimgrey", "firebrick", "seagreen", "slateblue")
grades <- table(moody$GRADE)

barplot(grades, col = color, xlab = "Grade", ylab = "Count",
        main = "Number of Students who got each Grade")

boxplot(SCORE~GRADE, data = moody, xlab = "Grade", ylab = "Score", main = 
          "Grade received based on Score",
        col = color)

plot(PARTICIPATION~SCORE, data = moody, main = "Score based on Participation")

dozing <- table(moody$DOZES_OFF)

# the next 4 plots will be subsets of the DOZES_OFF column to see how dozing off
# affects grade distribution

always_dozing <- subset(moody, DOZES_OFF == "always")
always_dozing_grades <- table(always_dozing$GRADE)
barplot(always_dozing_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade Distribution for Students who were always Sleeping")

never_dozing <- subset(moody, DOZES_OFF == "never")
never_dozing_grades <- table(never_dozing$GRADE)
barplot(never_dozing_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade Distribution for Students who were never Sleeping")

rarely_dozing <-  subset(moody, DOZES_OFF == "rarely")
rarely_dozing_grades <- table(rarely_dozing$GRADE)
barplot(rarely_dozing_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade Distribution for Students who were rarely Sleeping")

sometimes_dozing <- subset(moody, DOZES_OFF == "sometimes")
sometimes_dozing_grades <- table(sometimes_dozing$GRADE)
barplot(sometimes_dozing_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade Distribution for Students who were sometimes Sleeping")

# the next 4 plots will be subsets of the TEXTING_IN_CLASS column to see how 
# texting in class affects grade distribution


always_texting <- subset(moody, TEXTING_IN_CLASS == "always")
always_texting_grades <- table(always_texting$GRADE)
barplot(always_texting_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade distribution for Students who were always Texting")

never_texting <- subset(moody, TEXTING_IN_CLASS == "never")
never_texting_grades <- table(never_texting$GRADE)
barplot(never_texting_grades, col = color, xlab = "Grade", 
        ylab = "Count", main = "Grade distribution for Students who were never Texting")

rarely_texting <- subset(moody, TEXTING_IN_CLASS == "rarely")
rarely_texting_grades <- table(rarely_texting$GRADE)
barplot(rarely_texting_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade distribution for Students who were rarely Texting")

sometimes_texting <- subset(moody, TEXTING_IN_CLASS == "sometimes")
sometimes_texting_grades <- table(sometimes_texting$GRADE)
barplot(sometimes_texting_grades, col = color, xlab = "Grade",
        ylab = "Count", main = "Grade distribution for Students who were sometimes Texting")


participation <- moody$PARTICIPATION

hist(participation, col = as.factor(moody$PARTICIPATION))

library(ggplot2)
ggplot(moody, aes(x = GRADE, y = PARTICIPATION, color = GRADE)) + 
  labs(title = "Mapping Grade to Participation") + 
  geom_boxplot(outlier.color = "black", outlier.shape = 8, outlier.size = 4)

table(moody$GRADE)
