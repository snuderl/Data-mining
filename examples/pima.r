source('src/classification_tree.R')
source('src/util.r')
library('caret')

pima <- read.csv("data/pima.txt")
ptm <- proc.time()
tr2 <- tree.grow1(pima, 20, 5)
pred <- tree.classify(pima, tr2)
time <- proc.time() - ptm


ptm <- proc.time()
tr2 <- tree.grow1(pima, 20, 5)
time2 <- proc.time() - ptm

c <- confusionMatrix(pred, pima[, ncol(pima)])
print(c)
print(time2)

data <- split_data(0.7, pima)
candidatesNMin = c(10, 15, 20, 25, 30, 50, 70, 100, 150, 170, 250, 350, 500)
canditatesMinLeaf = c(3, 5, 7, 9, 10, 15, 20, 25, 40, 60, 80, 100, 150, 200, 250)
a = calculateGrid(data[[1]], data[[2]], candidatesNMin, canditatesMinLeaf)
levelplot(a, col.regions=cm.colors)