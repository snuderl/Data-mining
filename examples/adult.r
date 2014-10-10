source('src/classification_tree.R')
source("src/util.r")


adult = read.csv("data/adult.data")
fix = c(2, 4, 6, 7, 8, 9, 10, 14, 15)
adult <- category.to_integer(fix, adult)

data <- split_data(0.7, adult)
adult.train <- data[[1]]
adult.test <- data[[2]]

#compares diferent parameters of nmin and minleaf
candidatesNMin = c(30, 40, 50, 70, 100, 200, 300, 400, 600, 800, 1000)
canditatesMinLeaf = c(10, 20, 30, 40, 60, 80, 100, 200, 300, 400)
a = calculateGrid(adult.train, adult.test, candidatesNMin, canditatesMinLeaf)
levelplot(a, col.regions=cm.colors, xlab="NDim", ylab="MinLeaf")

#comapres our function to the rpart
library('caret')
library('rpart')
results = c()
nmins = c(50, 70, 80, 100, 130, 160, 200,230, 260, 300,350, 400, 600, 800, 1000)
for(x in nmins){
  fit <- rpart(X..50K ~ .-X..50K, data=adult.train, method="class", control=rpart.control(minsplit=x, cp=0.001))
  pred = predict(fit, adult.test, type="class")
  predRPart = confusionMatrix(pred, adult.test[,15])[[3]][[1]]
  
  tree = tree.grow1(adult.train, x, 15)
  pred = tree.classify(adult.test, tree)
  predBetter = confusionMatrix(pred, adult.test[,15])[[3]][[1]]
  results = c(results, predRPart, predBetter)
  print(results)
}
m = matrix(results, nrow = 2, dimnames=list(algos=c("RPart", "Pro"), nmin=nmins))
matplot(nmins, t(m), type="l", ylab="Accuracy", xlab="NMin", pch=1, col=c("blue", "red"))
legend(650, 0.856, c("Rpart", "Our implementation"), lty=c(1,1), lwd=c(2.5, 2.5), col=c("blue", "red"))
