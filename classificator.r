sample <- createSplit(1, 5, "")
sample <- setLeft(0, sample)
sample <- setRight(1, sample)


node <- function(left, right, x, y){
  list(left, right, x, y)
}

impurity <- function(node){
 total <- length(node[[4]])
 p1 <- sum(node[[4]] == 1) / total
 p2 <- 1 - p1
 
 p1 * p2
}

tree.grow <- function(x, y){
  
}


tree.classify <- function(x, tr, nmin=2, minleaf=2){
  if(isLeaf(tr)){
    tr
  }
  else 
  {
    val = tr[[2]]
    attribute = strtoi(val[[1]])
    value = val[[2]]
    if(x[[attribute]] <= value){
      tree.classify(x, left(tr))
    }
    else{
      tree.classify(x, right(tr))
    }
  }
} 

b <- tree.classify(list(50, 1), sample)