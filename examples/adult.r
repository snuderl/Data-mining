library('caret')
source('src/classification_tree.R')
source("src/util.r")


adult = read.csv("data/adult.data")
fix = c(2, 4, 6, 7, 8, 9, 10, 14, 15)
adult <- category.to_integer(fix, adult)

data <- split_data(0.7, adult)
adult.train <- data[[1]]
adult.test <- data[[2]]
tree.grow_c <- compiler::cmpfun(tree.grow)
ptm <- proc.time()
time3 <- proc.time() - ptm
print(time3)
library('caret')


candidatesNMin = c(30, 40, 50, 70, 100, 200, 300, 400, 600, 800, 1000)
canditatesMinLeaf = c(10, 20, 30, 40, 60, 80, 100, 200, 300, 400)

a = calculateGrid(adult.train, adult.test, candidatesNMin, canditatesMinLeaf)
levelplot(a, col.regions=cm.colors, xlab="NDim", ylab="MinLeaf")

results = c()
nmins = c(50, 70, 100, 200, 300, 400, 600, 800, 1000)
for(x in c(50, 70, 100, 200, 300, 400, 600, 800, 1000)){
  fit <- rpart(X..50K ~ .-X..50K, data=adult.train, method="class", control=rpart.control(minsplit=x, cp=0.001))
  pred = predict(fit, adult.test, type="class")
  predRPart = confusionMatrix(pred, adult.test[,15])[[3]][[1]]
  
  tree = tree.grow(adult.train, x, 15)
  pred = tree.classify(adult.test, tree)
  predBetter = confusionMatrix(pred, adult.test[,15])[[3]][[1]]
  results = c(results, predRPart, predBetter)
  print(results)
}
m = matrix(results, nrow = 2, dimnames=list(algos=c("RPart", "Pro"), nmin=nmins))
matplot(t(m), type="l", xlab="tt")
