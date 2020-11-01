## function.winsorizedmean
wins.mean = function(v){
  quantiles = Quantile(v, probs = seq(0,1,0.1))
  wm = matrix(0, nrow=length(v), ncol=1) # 10% winsorized mean
  for (d in 1:length(v)){
    if (v[d]<quantiles[1]){
      wm[d] = quantiles[1]
    } else if (v[d]>quantiles[11]){
      wm[d] = quantiles[11]
    } else if (v[d]>= quantiles[1] & v[d]<= quantiles[11]){
      wm[d] = v[d]
    }
  }
  wins.mean = mean(wm)
  return(wins.mean)
}