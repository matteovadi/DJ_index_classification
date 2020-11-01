outliers = function(x) {
  for (col in 1:ncol(x)){
    if (mode(x[,col]) == "numeric"){
      # gamma = 10 --> proportion of values which belongs to the interval is 99% 
      # elimination of the 0.5% to the left & the 0.5% to the right
      q1 = Quantile(x[,col], probs = seq(0,1,0.005))[2]  
      q2 = Quantile(x[,col], probs = seq(0,1,0.005))[200]
      for (row in 1:nrow(x)) {
        if (x[row, col] < q1 | x[row, col] > q2) {
          x[row,col] = NA
        }
      }
    }
  }
  x = na.omit(x)
}