## function.getmode ##
getmode = function(v) {
  uniqv = unique(v)
  mode = uniqv[which.max(tabulate(match(v,uniqv)))]
  return(mode)
}
