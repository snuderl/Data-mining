sample <- createSplit(1, 5, "")
sample <- setLeft(0, sample)
sample <- setRight(1, sample)

tree.grow = function(x, y){
  
}


tree.classify <- function(x, tr){
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