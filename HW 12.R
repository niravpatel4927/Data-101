# # Say, the Boundless analytics provides us with the slice:  Beer =='Lager' &  
# Day =='Weekend' and Snacks ='Crackers' and anchor attribute is Location.  You can 
# calculate Chisq for this slice and the Location attribute to test if distribution of 
# locations is affected if we limit ourselves only to transactions selling Lager and Crackers on Weekends?

# # The most interesting slice-anchor attribute combinations are the ones with the 
# largest chisq test and lowest p-value. Nevertheless do not forget about multiple 
# hypothesis correction - since we can on chi-square hunt here!

Minimarket<-read.csv("https://raw.githubusercontent.com/dev7796/data101_tutorial/main/files/dataset/HomeworkMarket2022.csv")


# Minimarket[Minimarket$Location == 'New Brunswick' & Minimarket$Beer == 'Lager' & Minimarket$Day == 'Weekend', ]$IN<-'In_Slice' => X-squared = 600.15
Minimarket$IN<-'Out_Slice'
Minimarket[Minimarket$Location == 'New Brunswick' & Minimarket$Beer == 'Lager' & Minimarket$Day == 'Weekend', ]$IN<-'In_Slice'
d<-table(Minimarket$Snacks, Minimarket$IN)
chisq.test(d)