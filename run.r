source("assignement1.R")

credit.dat <- read.csv("~/Desktop/credit.txt")
tr <- tree.grow(credit.dat, 2, 1)
for(i in 1:10){
  print(tree.classify(credit.dat[i, ], tr))
}


