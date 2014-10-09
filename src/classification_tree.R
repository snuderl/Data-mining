# tree.grow1
# x :: matrix
# y :: vector of class labels
# nmin    :: integer
# minleaf :: integer
# return  :: tree data structure used in classificaton
# A simple wrapper for our tree.grow1 function
tree.grow <- function(x, y, nmin=1, minleaf=1){
  tree.grow1(cbind(x, y), nmin, minleaf)
  
}

#tree.grow1
# x :: matrix of input data
# nmin    :: integer
# minleaf :: integer
# return  :: tree data structure used in classificaton
# This functions returns the classification tree for the given data.
# Last column of x is assumed to containt class labels.
tree.grow1 <- function(x, nmin=1, minleaf=1){
  nmin <- max(nmin, 2)
  if(nrow(x) < nmin || nrow(x) < 2 * minleaf) class(x)
  else if(impurity(getLabels(x)) == 0) class(x)
  else{    
    s <- bestSplitWhole(x, minleaf)
    if(length(s) == 1 && s == -1){
      class(x)
    }else{
      imp <- s[["impurity"]]
      value <- s[["value"]]
      index <- s[["index"]]
      
      
      sp = split(x, index, value)
      left = sp[["left"]]
      right = sp[["right"]]
      
      if(nrow(left) == 0 || nrow(right) == 0){
        class(x)
      } else if(imp == 0){
        list(left=class(left), mid=list(value, index), right=class(right))
      } else{
        list(left=tree.grow1(left, nmin, minleaf), mid=list(value, index), right=tree.grow1(right, nmin, minleaf))  
      }
    }
  }
}

# tree.classify
# data :: matrix
# tree :: tree constructed by tree.grow function
# return  :: vector of class labels
# Uses the classification tree to classify data.
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


#impurity
#labels :: vector of class labels
#return :: double
#calculates the impurity of a given vector
impurity <- function(labels){
  p1 <- sum(labels == 0) / length(labels)
  p1 * (1 - p1)
}

#reduction
#l :: vector of class labels
#r :: vector of class labels
#return :: double
#Calculates the impurity reduction of a function
reduction <- function(l, r){
  total <- length(l) + length(r)
  pl <- length(l)/ total
  pl * impurity(l) + (1 - pl) * impurity(r)  
}

#bestsplit
#x :: vector of data values
#y :: vector of class labels
#minleaf :: int
#return :: best split for given vector x
bestsplit <- function(x, y, minleaf){
  
  bestImp = 1000000000
  bestSplit = 1000000000
  rows = length(y)
  
  ### Keep track of last iteration
  lastVal = x[1]
  lastMid <- 0
  lastLabel <- -1
  
  ### Unique values in sorted order, and last value(so we know we are at the end)
  unq = unique(sort(x))
  end = unq[length(unq)]
  
  firstValFound = FALSE
  
  
  for(val in unq){
    
    ## We can skip if label didn't change
    label = unique(y[x == val])
    ## Label is the list of all clases that have a instance with value x. If it is 1, we store it
    ## and possibly skip an iteration.
    if(val != end && length(label) == 1){
      label = label[1]
      if(!firstValFound && label == lastLabel) next
    }
    else{
      label = -1
    }    
    
    midpoint <- (lastVal + val) / 2
    inds <- x < midpoint
    l <- matrix(y[inds])
    
    if(nrow(l) >= minleaf && (rows - nrow(l)) >= minleaf){
      firstValFound <- TRUE
      r <- matrix(y[!inds])
      imp <- reduction(l, r)
      if(imp <= bestImp){
        bestImp <- imp
        bestSplit <- midpoint
      }
    }
    else if(firstValFound){
      ### If first val was already found, then we are closing on the end from another side
      ### Value before this one was the last possible value, and we must calculate its solution
      midpoint <- (lastVal + val) / 2
      inds <- x < midpoint
      l <- matrix(y[inds])
      r <- matrix(y[!inds])
      imp <- reduction(l, r)
      if(imp <= bestImp){
        bestImp <- imp
        bestSplit <- midpoint
      }
      
      ### We can also safely break,
      ### as we know there cannot be another solutions containing enough values in both leafs.
      break
    }
    
    lastVal <- val
    lastLabel <- label
    lastMid <- midpoint
  }
  
  c(impurity=bestImp, value=bestSplit)
}

#bestsplit whole
#x       :: matrix
#minleaf :: int
#return  :: bestsplit for the whole matrix x
bestSplitWhole <- function(x, minleaf){
  ### First value is new impurity, next ones are split value and split column
  bestVal <- c(impurity=1000000000, val=0, index=0)
  y <- x[, ncol(x)]
  for(i in 1:(ncol(x) - 1)){
    s <- bestsplit(x[, i], y, minleaf)
    #print(s)
    if(s[[1]] < bestVal[[1]]){
      bestVal <- c(s, index=i)
    }
  }
  if(bestVal[[1]] == 1000000000) -1
  else bestVal
}

#split
#x :: matrix
#index :: int
#value :: int
#return :: list with 2 matrixes
#Splits the matrix into the left and right pair. Index is the column we split on. 
split <- function(x, index, value){
  inds <- x[, index] < value
  left <- x[inds, ]
  right <- x[!inds, ]
  list(left=left, right=right)
}

#class
#x :: matrix
#return :: int
#Returns the mayority class for a given matrix.
class <- function(x){
  y <- getLabels(x)
  if(sum(y == 0) > sum(y == 1)) 0
  else 1
}

#getLabels
#x :: matrix
#return :: vector
#Returns last column of a matrix(which is assumed to contain class labels).
getLabels <- function(x){
  x[, ncol(x)]
}




