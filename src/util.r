split_data <- function(p, x){
  total = nrow(x)
  x <- x[sample(total), ]
  train = round(total * p)
  list(train=x[1:train, ], test=x[(train + 1):total, ])
}

category.to_integer <- function(cols, data){
  for(i in cols){
    data[,i] = as.integer(factor(data[,i])) - 1
  }
  data
}

calculateGrid <- function(train, test, dim1, dim2){
  library('caret')
  results = c()
  for(x in dim1){
    for(y in dim2){
        tree <- tree.grow1(train, x, y)
        pred <- tree.classify(test, tree)
        c2 <- confusionMatrix(pred, test[, ncol(test)])
        results = c(results, c2[[3]][[1]])
    }
  }
  
  m = matrix(results, nrow = length(dim2), dimnames=list(nmin=dim2, minleaf=dim1))
  m[m == 0] = NaN
  ##nba_heatmap <- heatmap(m, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column", margins=c(5,10))
  m
}