library('caret')
source('classification_tree.R')
source("util.r")


adult = read.csv("data/adult.data")
fix = c(2, 4, 6, 7, 8, 9, 10, 14, 15)
adult <- category.to_integer(fix, adult)

#Permute the rows
data <- split_data(0.3, adult)
adult.train <- data[[1]]
adult.test <- data[[2]]
ptm <- proc.time()
tr3 <- tree.grow(adult.train, 300, 60)
time3 <- proc.time() - ptm
print(time3)
library('caret')
pred2 <- tree.classify(adult.test, tr3)
print("done")
c2 <- confusionMatrix(pred2, adult.test[, ncol(adult.test)])
