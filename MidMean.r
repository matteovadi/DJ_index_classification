## function.midmean ##
mid.mean = function(v) {
  ql = quantile(v)[2]
  qu = quantile(v)[4]
  sub_vect1 = subset(v, v>ql & v<qu)
  mid.mean = mean(sub_vect1)
  return(mid.mean)
}