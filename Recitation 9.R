#Example 1:What are the odds of getting an A for students who got over 85% of score? 

#We use the odds formulation of Bayesian Theorem
# We begin with prior odds of getting an A:  P(A)/(1-P(A))
#say 25% - PRIOR ODDS 1:3
PriorGettingA<-0.25
PriorGradeAOdds<-PriorGettingA/(1-PriorGettingA)
PriorGradeAOdds

#True positive:  Fraction of students who got an A (in the past) who scored over 75%? P(Score Above 75/ scored A)
TruePositive<-0.95
#False positive = Fraction of students who got an A (in the past) who did not scored over 75%? = P(Score Above 75/Did not score A)
FalsePositive<-0.02

LikelihoodRatio<-TruePositive/FalsePositive

PosteriorGradeAOdds<-LikelihoodRatio*PriorGradeAOdds

PosteriorgotGradeA<- PosteriorGradeAOdds/(1+PosteriorGradeAOdds)

PosteriorgotGradeA


#Example 2:WHAT ARE THE ODDS OF HAVING COVID GIVEN POSITIVE HOME COVID TEST?

#Belief = "Have Covid"
#Observation = Covid Test
#How much the probability of having covid increases upon positive COVID-test?
#We use the odds formulation of Bayesian Theorem
# we begin with prior odds of having Covid:  P(Covid)/(1-P(Covid)
PriorHaveCovid<-0.3
PriorCovidOdds<-PriorHaveCovid/(1-PriorHaveCovid)
PriorCovidOdds

#True positive:  Probability of having positive Covid test when having covid  = P(PositiveCovidTest|HaveCovid)
TruePositive<-0.8
#False positive = Probability of having positive Covid test when not having covid = P(PostiveCovidTest/DoNotHaveCovid)
FalsePositive<-0.09

LikelihoodRatio<-TruePositive/FalsePositive
PosteriorCovidOdds<-LikelihoodRatio*PriorCovidOdds
PosteriorHaveCovid<- PosteriorCovidOdds/(1+PosteriorCovidOdds)
PosteriorHaveCovid

#Example 3: Getting an A when never dozing off?

#Checking Data
moody2022_new
Prior_Probability <- 0.3
PriorGradeAOdds_Dozing<-Prior_Probability/(1-Prior_Probability)
PriorGradeAOdds_Dozing
#True positive:  Fraction of A that never dose off. P(Never dozing off |Grade A)

#probability of Never dozing off and getting A 
P_Never_and_A <- nrow(subset(moody2022_new,(moody2022_new$GRADE == "A" & moody2022_new$DOZES_OFF =="never") ))/nrow(moody2022_new)
#Probablity of A 
P_A <- nrow(subset(moody2022_new,moody2022_new$GRADE == "A"))/nrow(moody2022_new)
TruePositive<- P_Never_and_A / P_A 
TruePositive

#False positive = Fraction of Non A's that never dose off. P(Never dozing off |~Grade A)

#probability of Never dozing off and not getting A 
P_Never_and_not_A <- nrow(subset(moody2022_new,(moody2022_new$GRADE != "A" & moody2022_new$DOZES_OFF =="never") ))/nrow(moody2022_new)

#Probablity of ~A 
P_NOT_A <- nrow(subset(moody2022_new,moody2022_new$GRADE != "A"))/nrow(moody2022_new)

FalsePositive<- P_Never_and_not_A / P_NOT_A
FalsePositive


LikelihoodRatio<-TruePositive/FalsePositive
PosteriorGradeAOdds<-LikelihoodRatio*PriorGradeAOdds_Dozing
PosteriorGradeAneverDozing<- PosteriorGradeAOdds/(1+PosteriorGradeAOdds)
PosteriorGradeAneverDozing


#Example 4

moody<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/MoodyMarch2022b.csv")

summary(moody)

Prior_prob <- 0.5

Prior_passing_odds<-Prior_prob/(1-Prior_prob)
Prior_passing_odds

#True positive:  P(Psychology freshman |Pass)

#probability of Psychology freshman and passing
P_Psy_Fresh_pass <- nrow(subset(moody,(moody$Major == "Psychology" & moody$Grade !="F" & moody$Seniority == "Freshman" ) ))/nrow(moody)
P_Psy_Fresh_pass
#Probablity of Passing
P_Pass <- nrow(subset(moody,moody$Grade != "F"))/nrow(moody)
TruePositive<- P_Psy_Fresh_pass / P_Pass
TruePositive

#False positive:  P(Psychology freshman |Fail)

#probability of Psychology freshman and passing
P_Psy_Fresh_fail <- nrow(subset(moody,(moody$Major == "Psychology" & moody$Grade =="F" & moody$Seniority == "Freshman" ) ))/nrow(moody)
#Probablity of Passing
P_Fail <- nrow(subset(moody,moody$Grade == "F"))/nrow(moody)
FalsePositive<- P_Psy_Fresh_fail / P_Fail
FalsePositive

LikelihoodRatio<-TruePositive/FalsePositive
PosteriorPassOdds<-LikelihoodRatio*Prior_passing_odds
PosteriorFreshman_Psc_Passing<- PosteriorPassOdds/(1+PosteriorPassOdds)
PosteriorFreshman_Psc_Passing
