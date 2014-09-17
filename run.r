source('classification_tree.R')

source('classification_tree.R')
credit.dat <- read.csv("data/credit.txt")
tr <- tree.grow(credit.dat, 2, 1)
for(i in 1:10){
  print(tree.classify(credit.dat[i, ], tr))
}


