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