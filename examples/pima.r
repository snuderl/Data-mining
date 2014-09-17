source('src/classification_tree.R')

pima <- read.csv("data/pima.txt")
ptm <- proc.time()
tr2 <- tree.grow(pima, 20, 5)
pred <- tree.classify(pima, tr2)
time <- proc.time() - ptm


ptm <- proc.time()
tree.grow_c <- compiler::cmpfun(tree.grow)
tr2 <- tree.grow_c(pima, 20, 5)
time2 <- proc.time() - ptm

c <- confusionMatrix(pred, pima[, ncol(pima)])
print(c)
print(time2)