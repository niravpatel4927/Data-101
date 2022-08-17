student_performance <- read.csv("StudentsPerformance.csv")


plot(student_performance$math.score, student_performance$reading.score, main = "Math vs Reading Scores", xlab = )
plot(student_performance$writing.score, student_performance$reading.score)

colors <- c("red", "blue", "green", "orange", "yellow", "cyan", "pink", "brown", "purple", "light blue")

hist(student_performance$math.score, main = "Math Scores", xlab = "Range", ylab = "# of students", 
     col = colors, breaks = 10, ylim = c(0, 300))
hist(student_performance$reading.score, main = "Reading Scores", xlab = "Range", ylab = "# of students", 
     col = colors, breaks = 10, ylim = c(0, 300))
hist(student_performance$writing.score, main = "Writing Scores", xlab = "Range", ylab = "# of students", 
     col = colors, breaks = 10, ylim = c(0, 300))



mosaicplot(student_performance$test.preparation.course~student_performance$parental.level.of.education, col = colors,
           xlab = "Preparation Course Taken", ylab = "Parent Level of Education", main = "Preparation Course vs. Parental Influence")
mosaicplot(student_performance$test.preparation.course~student_performance$lunch, col = colors,
           xlab = "Perparation Course Taken", ylab = "Lunch Price", main = "Preparation Course vs. Price of Lunch")
mosaicplot(student_performance$test.preparation.course~student_performance$gender, col = colors, 
           xlab = "Preparation Course Taken", ylab = "Gender", main = "Preparation Course vs. Gender")


math_gender <- tapply(student_performance$math.score, student_performance$gender, mean)
math_gender
reading_gender <- tapply(student_performance$reading.score, student_performance$gender, mean)
writing_gender <- tapply(student_performance$writing.score, student_performance$gender, mean)

two_cat_colors <- c("black", "blue")
barplot(math_gender, main = "Average Math Score by Gender", ylab = "Average Score", col = two_cat_colors)
barplot(reading_gender, main = "Average Reading Score by Gender", ylab = "Average Score", col = two_cat_colors)
barplot(writing_gender, main = "Average Writing Score by Gender", ylab = "Average Score", col = two_cat_colors)
