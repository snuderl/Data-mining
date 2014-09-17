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

bestsplit <- function(x, y, minleaf){
  
  bestImp = 1000000000
  bestSplit = 1000000
  
  rows = length(y)
  
  
  lastVal = x[1]
  lastLabel <- -1
  for(val in unique(sort(x))){
    
    ## We can skip if label didn't change
    label = unique(y[x == val])
    if(length(label) == 1){
      label = label[1]
      if(label == lastLabel) next
    }
    else{
      label = -1
    }
    
    
    midpoint <- (lastVal + val) / 2
    
    inds <- x < midpoint
    l <- matrix(y[inds])
    
    if(nrow(l) >= minleaf && (rows - nrow(l)) >= minleaf){
      r <- matrix(y[!inds])
      imp <- reduction(l, r)
      if(imp <= bestImp){
        bestImp <- imp
        bestSplit <- midpoint
      }
    }
    
    lastVal <- val
    lastLabel <- label
  }
  
  c(bestImp, bestSplit)
}

bestSplitWhole <- function(x, minleaf){
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
  inds <- x[, index] < value
  left <- x[inds, ]
  right <- x[!inds, ]
  list(left, right)
}


class <- function(x){
  y <- getLabels(x)
  if(sum(y == 0) > sum(y == 1)) 0
  else 1
}

getLabels <- function(x){
  x[, ncol(x)]
}

tree.grow <- function(x, nmin=1, minleaf=1){
  nmin <- max(nmin, 2)
  if(nrow(x) < nmin || nrow(x) < 2 * minleaf) class(x)
  else if(impurity(getLabels(x)) == 0) class(x)
  else{    
    s <- bestSplitWhole(x, minleaf)
    imp <- s[1]
    value <- s[2]
    index <- s[3]
    
    
    sp = split(x, index, value)
    left = sp[[1]]
    right = sp[[2]]
    
    if(nrow(left) == 0 || nrow(right) == 0){
      class(x)
    } else if(imp == 0){
      list(class(left), list(value, index), class(right))
    } else{
      list(tree.grow(left, nmin, minleaf), list(value, index), tree.grow(right, nmin, minleaf))  
    }
  }
}

tree.classify <- function(data, tree){
  classify <- function(x, tr){
    if(length(tr) == 1){
      tr
    }else{
      sp <- tr[[2]]
      if(x[sp[[2]]] < sp[[1]]){
        # Go left
        classify(x, tr[[1]])
      }
      else{
        # Go right
        classify(x, tr[[3]])
      }
    } 
  }
  apply(data, 1, function(x) { classify(x, tree)})
}





