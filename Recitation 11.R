moody_data <- as.data.frame(read.csv("MoodyMarch2022b.csv"))

library(rpart)
library(rpart.plot)

# Using rpart function
tree <- rpart(Grade ~ .,data=moody_data, method="class")

# This shows the prediction tree in text format
tree

# Plot for the decision tree. Export the plot as pdf and zoom to see in detail.
rpart.plot(tree)

# rpart with control parameters minsplit
# Minimum number of observations that must exist in a node for a split to be attempted is 200.
c_tree <- rpart(Grade ~ .,data=moody_data, method="class", control=rpart.control(minsplit = 200))

# Plot for the decision tree when minsplit parameter is used
rpart.plot(c_tree)

# rpart with control parameters minbucket
# Minimum number of observations in any terminal leaf node is 50.
b_tree <- rpart(Grade ~ .,data=moody_data, method="class", control=rpart.control(minbucket = 50))

# Plot for the decision tree when minsplit parameter is used
rpart.plot(b_tree)



#Cross validation

#train data
install.packages("devtools")
devtools::install_github("devanshagr/CrossValidation")
CrossValidation::cross_validate(moody_data, tree, 2, 0.8)
