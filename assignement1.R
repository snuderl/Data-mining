y <- c(1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1)

impurity <- function(labels){
  n <- length(labels)
  n1 <- sum(labels == 0)
  p1 <- n1 / n
  
  p1 * (1 - p1)
}

reduction <- function(l, r){
  total <- length(l) + length(r)
  pl <- length(l)/ total
  pr <- 1 - pl
  
  pl * impurity(l) + pr * impurity(r)  
}

bestsplit <- function(x, y, minleaf=1){
  m <- matrix(c(x, y), ncol=2)
  m <- m[order(m[, 1]),]
  
  lastVal = m[1, 1]
  bestImp = 1000000000
  bestSplit = 1000000
  
  low <- (1 + minleaf)
  high <- length(x) - minleaf + 1
  for(c in low:high){
    val = m[c, 1]
    
    ### We can skip if there is a series of same values.
    if(c < high && val == m[c+1, 1]){
      next
    }
    
    midpoint <- (lastVal + val) / 2
    sp <- split(m, 1, midpoint)
    
    l <- matrix(m[m[,1] < midpoint, 2])
    r <- matrix(m[m[,1] >= midpoint, 2])
    if(nrow(l) > 0 && nrow(r) > 0){
      imp <- reduction(l, r)
      if(imp <= bestImp){
        bestImp <- imp
        bestSplit <- midpoint
      }
    }
    
    lastVal <- val
  }
  
  c(bestImp, bestSplit)
}

bestSplitWhole <- function(x, minleaf=1){
  ### First value is new impurity, next ones are split value and split column
  bestVal <- c(10000000, 0, 0)
  y <- x[, ncol(x)]
  for(i in 1:(ncol(x) - 1)){
    s <- bestsplit(x[, i], y, minleaf)
    #print(s)
    if(s[[1]] < bestVal[[1]]){
      bestVal <- c(s, i)
    }
  }
  bestVal
}

split <- function(x, index, value){
  #print("Split on")
  #print(index)
  #print(value)
  left <- x[x[, index] < value, ]
  right <- x[x[, index] >= value, ]
  list(left, right)
}


class <- function(x){
  y <- x[, length(x)]
  if(sum(y == 0) > sum(y == 1)) 0
  else 1
}

tree.grow <- function(x, nmin=1, minleaf=1){
  nmin <- max(nmin, 2)
  if(nrow(x) < nmin) class(x)
  else{
#     print("Tree.grow with ")
#     print(x)
#     print(nrow(x))
    
    s <- bestSplitWhole(x)
    imp <- s[[1]]
    value <- s[[2]]
    index <- s[[3]]
    
    
    sp = split(x, index, value)
    left = sp[[1]]
    right = sp[[2]]
    
    #     print("Left are")
    #     print(nrow(left))
    #     print("Right:")
    #     print(nrow(right))
    
    if(nrow(left) == 0 || nrow(right) == 0){
      class(x)
    } else if(imp == 0){
      list(class(left), list(value, index), class(right))
    } else{
      list(tree.grow(left, nmin, minleaf), list(value, index), tree.grow(right, nmin, minleaf))  
    }
  }
}

tree.classify <- function(x, tr){
  if(length(tr) == 1){
    tr
  }else{
    sp <- tr[[2]]
    if(x[sp[[2]]] < sp[[1]]){
      tree.classify(x, tr[[1]])
    }
    else{
      tree.classify(x, tr[[3]])
    }
  }
}


credit.dat <- read.csv("~/Desktop/credit.txt")
tr <- tree.grow(credit.dat, 2, 1)
for(i in 1:10){
  print(tree.classify(credit.dat[i, ], tr))
}

ptm <- proc.time()
pima <- read.csv("~/Desktop/pima.txt")
tr2 <- tree.grow(pima, 20, 5)
pred <- apply(pima, 1, function(x){ tree.classify(x, tr2)})
time <- proc.time() - ptm

library('caret')
c <- confusionMatrix(pred, pima[, ncol(pima)])