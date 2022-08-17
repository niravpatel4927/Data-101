moody <- read.csv("MoodyMarch2022b.csv")
head(moody)

# tapply(numerical, categorical, aggregagte function)

# sub_set = subset(data, data&column == 'something')

sort(table(moody$Major))

# What is the minimal GPA of economics majors who received an A in the class?

data1 <- subset(moody, moody$Major == 'Economics' & moody$Grade == 'A')
sort(data1$GPA)
min(data1$GPA)

#Which major  has the lowest average score of students who failed the class (F)?
data2 <- subset(moody, moody$Grade == 'F')
data2

result1 <- tapply(data2$Score, data2$Major, mean)
sort(result1)

# How many psychology seniors received an A?
data3 <- subset(moody, moody$Major == 'Psychology' & moody$Grade == 'A' & moody$Seniority == 'Senior')
nrow(data3)
data3

# Calculate the minimum score of computer science seniors getting an A?

min(subset(moody, moody$Major == 'CS' & moody$Grade == 'A' & moody$Seniority == 'Senior')$Score)
