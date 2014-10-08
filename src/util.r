split_data <- function(p, x){
  total = nrow(x)
  x <- x[sample(total), ]
  train = round(total * p)
  list(x[1:train, ], x[(train + 1):total, ])
}

category.to_integer <- function(cols, data){
  for(i in cols){
    data[,i] = as.integer(factor(data[,i])) - 1
  }
  data
}

calculateGrid <- function(train, test, dim1, dim2){
  results = c()
  for(x in dim1){
    for(y in dim2){
      if(2.1 * y < x){
        tree <- tree.grow(train, x, y)
        pred <- tree.classify(test, tr3)
        c2 <- confusionMatrix(pred2, test[, ncol(test)])
        results = c(results, c2[[3]][[1]])
      }
      else{
        results = c(results, 0)
      }
    }
  }
  
  m = matrix(results, nrow = length(dim2), dimnames=list(dim2, dim1))
  nba_heatmap <- heatmap(m, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
  levelplot(m, col.regions=cm.colors)
}