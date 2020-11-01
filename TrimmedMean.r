## function.trimmedmean ##
trimmed.mean = function(v) {
  quantiles = Quantile(v, probs = seq(0,1,0.05))
  sub_vect2 = subset(v, v>quantiles[1] & v<quantiles[21])
  trimmed.mean = mean(sub_vect2)
  return(trimmed.mean)
}
