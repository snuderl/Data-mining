

emptyTree <- function() {
  list()
}

left <- function(node)
{
  node[[1]]
}

setLeft <- function(node, tr){
  tr[[1]] <- node;
  tr
}

setRight <- function(node, tr){
  tr[[3]] <- node;
  tr
}

right <- function(node){
  node[[3]]
}

val <- function(node){
  node[2]
}

isLeaf <- function(node){
  length(node) < 3
}

createSplit <- function(index, val, class){
  list(emptyTree(), c(index, val, class), emptyTree())
}