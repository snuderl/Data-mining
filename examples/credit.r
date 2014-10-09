source('src/classification_tree.R')

credit.dat <- read.csv("data/credit.txt")
tr <- tree.grow1(credit.dat, 2, 1)
print(tree.classify(credit.dat, tr))


