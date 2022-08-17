moody <- read.csv("M2022train.csv")

head(moody)

table(moody$Seniority)
table(moody$Grade)
table(moody$Major)

summary(moody[moody$Grade == "A",])
summary(moody[moody$Grade == "B",])
summary(moody[moody$Grade == "C",])
summary(moody[moody$Grade == "D",])
summary(moody[moody$Grade == "F",])


# Grade cutoffs
# score < 50 = F
# score >= 85 = A
# score < 85 & score >= 65 = B
# score <  65 & score > 50 = C

color = c("deepskyblue2", "dimgrey", "firebrick", "seagreen", "slateblue")

barplot(table(moody$Grade), col = color, main = "Grade distribution", xlab = "Grade",
        ylab = "Count")


boxplot(Score~Grade, data = moody, xlab = "Grade", ylab = "Score", main = 
          "Grade received based on Score", col = color)

mosaicplot(moody$Grade~moody$Major, xlab = "Grade", ylab = "Major", col = color,
           border = "black", main = "Grade vs Major")

mosaicplot(moody$Grade~moody$Seniority, xlab = "Grade", ylab = "Seniority", col = color,
           border = "black", main = "Grade vs Seniority")

table(moody$Seniority, moody$Grade)

table(moody$Seniority, moody$Major)

table(moody$Major, moody$Grade)

table(moody[moody$Major == "CS",]$Grade)
table(moody[moody$Major == "Economics",]$Grade)
table(moody[moody$Major == "Psychology",]$Grade)
table(moody[moody$Major == "Statistics",]$Grade)

#Start with assigning everyone an F

# Lets take a look to see what makes an A grade
# range for A is 85 and above
table(moody[moody$Score > 85,]$Grade)

hist(moody[moody$Grade == "A",]$Score)


table(moody[moody$Score >= 85 & moody$Seniority == "Freshman",]$Grade) # basically an A
# table(moody[moody$Score >= 85 & moody$Seniority == "Sophomore",]$Grade)  need a little more. Will have to look into major now
table(moody[moody$Score >= 85 & moody$Seniority == "Sophomore" & moody$Major != "CS",]$Grade) # this is almost always an A
table(moody[moody$Score >= 90 & moody$Seniority == "Sophomore" & moody$Major == "CS",]$Grade) # A
table(moody[(moody$Score < 90 & moody$Score >= 85) & moody$Seniority == "Sophomore" & moody$Major == "CS",]$Grade) # B

table(moody[moody$Score >= 85 & moody$Seniority == "Junior" & moody$Major != "CS",]$Grade) #A
table(moody[(moody$Score > 90) & moody$Seniority == "Junior" & moody$Major == "CS",]$Grade)# A
table(moody[(moody$Score <= 90 & moody$Score >= 85) & moody$Seniority == "Junior" & moody$Major == "CS",]$Grade) # B

table(moody[moody$Score >= 85 & moody$Seniority == "Senior" & moody$Major != "CS",]$Grade) # A
table(moody[moody$Score > 90 & moody$Seniority == "Senior" & moody$Major == "CS",]$Grade) # A
table(moody[(moody$Score >= 85 & moody$Score <= 90) & moody$Seniority == "Senior" & moody$Major == "CS",]$Grade) # B

# For B
hist(moody[moody$Grade == "B",]$Score)

table(moody[moody$Grade == "B",]$Major)
table(moody[moody$Grade == "B",]$Seniority)

table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Freshman" 
            & moody$Major == "CS",]$Grade) # B

table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Freshman" 
            & moody$Major != "CS",]$Grade) # A



table(moody[(moody$Score < 85 & moody$Score > 70) & moody$Seniority == "Sophomore" 
            & moody$Major != "Psychology",]$Grade)# B

table(moody[(moody$Score < 70 & moody$Score >= 65) & moody$Seniority == "Sophomore" 
            & moody$Major != "Psychology",]$Grade) # C  

table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Sophomore"
            & moody$Major == "Psychology",]$Grade) # A
# table(moody[(moody$Score < 70 & moody$Score >= 65) & moody$Seniority == "Sophomore" & moody$Major == "Psychology",]$Grade) # B



table(moody[(moody$Score < 85 & moody$Score >= 80) & moody$Seniority == "Junior"
            & moody$Major == "CS",]$Grade) # B

table(moody[(moody$Score < 80 & moody$Score >= 65) & moody$Seniority == "Junior"
            & moody$Major == "CS",]$Grade) # C

table(moody[(moody$Score < 85 & moody$Score >= 75) & moody$Seniority == "Junior"
            & moody$Major == "Statistics",]$Grade) # B

table(moody[(moody$Score < 75 & moody$Score >= 65) & moody$Seniority == "Junior"
            & moody$Major == "Statistics",]$Grade) # C

table(moody[(moody$Score < 85 & moody$Score >= 71) & moody$Seniority == "Junior"
            & moody$Major == "Psychology",]$Grade) # A

table(moody[(moody$Score <= 70 & moody$Score >= 65) & moody$Seniority == "Junior"
            & moody$Major == "Psychology",]$Grade) # B

table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Junior"
            & moody$Major == "Economics",]$Grade) # B


table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Senior"
            & (moody$Major == "CS" | moody$Major == "Statistics"),]$Grade) # C


table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Senior"
            & moody$Major == "Economics",]$Grade) # B

table(moody[(moody$Score < 85 & moody$Score >= 65) & moody$Seniority == "Senior"
            & moody$Major == "Psychology",]$Grade) # A


# For a C score range
hist(moody[moody$Grade == "C",]$Score)

table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Freshman" 
            & moody$Major == "CS",]$Grade) # C

table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Freshman" 
            & moody$Major == "Economics",]$Grade) # B

table(moody[(moody$Score < 65 & moody$Score >= 55) & moody$Seniority == "Freshman" 
            & moody$Major == "Statistics",]$Grade) # B
table(moody[(moody$Score < 55 & moody$Score >= 50) & moody$Seniority == "Freshman" 
            & moody$Major == "Statistics",]$Grade) # C

table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Freshman" 
            & moody$Major == "Psychology",]$Grade) # A


table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Sophomore" 
            & moody$Major == "CS",]$Grade) # D
table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Sophomore" 
            & (moody$Major == "Statistics" | moody$Major == "Economics"),]$Grade) # C
table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Sophomore" 
            & moody$Major == "Psychology",]$Grade) # B

table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Junior" 
            & moody$Major != "CS",]$Grade) # C
table(moody[(moody$Score < 58 & moody$Score > 50) & moody$Seniority == "Junior" 
            & moody$Major == "CS",]$Grade)  # D
table(moody[(moody$Score < 65 & moody$Score > 58) & moody$Seniority == "Junior" 
            & moody$Major == "CS",]$Grade) # C

table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Senior" 
            & moody$Major != "CS",]$Grade)# C
table(moody[(moody$Score < 65 & moody$Score > 50) & moody$Seniority == "Senior" 
            & moody$Major == "CS",]$Grade) #D


# let's say score below a 50 = F

table(moody[(moody$Score < 50 & moody$Score > 30) & moody$Seniority == "Freshman" 
            & moody$Major == "Psychology",]$Grade) # C
table(moody[(moody$Score < 30) & moody$Seniority == "Freshman" 
            & moody$Major == "Psychology",]$Grade) # D


table(moody[(moody$Score < 50) & moody$Seniority == "Freshman" & (moody$Major == "CS" | moody$Major == "Statistics"),]$Grade)# D
table(moody[(moody$Score < 50) & moody$Seniority == "Freshman" & moody$Major == "Economics",]$Grade) # D
table(moody[(moody$Score <= 40) & moody$Seniority == "Freshman" & moody$Major == "Economics",]$Grade) # D




table(moody[(moody$Score < 50 & moody$Score >= 30) & moody$Seniority == "Sophomore" & moody$Major == "Psychology",]$Grade) # D

#table(moody[(moody$Score < 40 & moody$Score > 30) & moody$Seniority == "Sophomore" & moody$Major == "Psychology",]$Grade) # D
# table(moody[(moody$Score < 30) & moody$Seniority == "Sophomore" & moody$Major == "Psychology",]$Grade)# F
table(moody[(moody$Score < 50) & moody$Seniority == "Sophomore" & moody$Major == "Economics",]$Grade) # D

table(moody[(moody$Score < 50) & moody$Seniority == "Senior" & moody$Major == "Psychology",]$Grade) # C
table(moody[(moody$Score < 40 & moody$Score > 30) & moody$Seniority == "Senior" & moody$Major == "Psychology",]$Grade) # D


