library('caret')
source('src/classification_tree.R')
source("src/util.r")


adult = read.csv("data/adult.data")
fix = c(2, 4, 6, 7, 8, 9, 10, 14, 15)
adult <- category.to_integer(fix, adult)

data <- split_data(0.2, adult)
adult.train <- data[[1]]
adult.test <- data[[2]]
tree.grow_c <- compiler::cmpfun(tree.grow)
ptm <- proc.time()
time3 <- proc.time() - ptm
print(time3)
library('caret')


candidatesNMin = c(30, 40, 50, 70, 100, 200, 300, 400, 600, 800, 1000, 1500, 2000)
canditatesMinLeaf = c(10, 20, 30, 40, 60, 80, 100, 200, 300, 400, 500, 700)

calculateGrid(adult.train, adult.test, candidatesNMin, canditatesMinLeaf)


pred2 <- tree.classify(adult.test, tr3)
c2 <- confusionMatrix(pred2, adult.test[, ncol(adult.test)])
print(c2)
