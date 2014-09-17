library('caret')


adult = read.csv("adult.data")
adult[,15] = as.integer(factor(adult[,15])) - 1
fix = c(2, 4, 6, 7, 8, 9, 10, 14)
for(i in fix){
  adult[,i] = as.integer(factor(adult[,i]))
}

#Permute the rows
adult <- adult[sample(nrow(adult)), ]
#Train size
train = 15000
adult.train = adult[1:train, ]
adult.test = adult[(train + 1):nrow(adult), ]
ptm <- proc.time()
tr3 <- tree.grow_c(adult.train, 100, 20)
time3 <- proc.time() - ptm
print(time3)
library('caret')
pred2 <- tree.classify(adult.test, tr3)
print("done")
c2 <- confusionMatrix(pred2, adult.test[, ncol(adult.test)])
