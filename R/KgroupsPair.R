KgroupsPair=function(data, clusters, a = 1, iter.max = 10, nstart = 1){
  #initialized
  n=NROW(data)
  c=clusters
  lvlindex=sample.int(c,n/2,replace=TRUE)
  level=numeric(0)
  for(i in 1:(n/2)){
    level[2*i-1]=lvlindex[i]
    level[2*i]=lvlindex[i]
  }

  it = 1
  while( it < iter.max) {
    out= PairOnePass(data,level,a)
    if (out$Move < 1) break
    level=out$Level
    it = it + 1
  }

  if (it == iter.max)
    warning("Reached maximum iterations")

  best=out

  # restart with random labels

  if (nstart > 1) {
    bests <- list()
    bests[[1]] <- best
    for (r in 2:nstart) {
      lvlindex<-sample.int(c,n/2,replace=TRUE)
      level=numeric(0)
      for(i in 1:(n/2)){
        level[2*i-1]=lvlindex[i]
        level[2*i]=lvlindex[i]
      }
      it <- 1
      while( it < iter.max) {
        out <- PairOnePass(data, level,a)
        if (out$Move < 1) break
        level <- out$Level
        it <- it + 1
      }
      if (it == iter.max)
        warning("Reached maximum iterations")
      bests[[r]] <- out
      if (out$Within < best$Within)
        best <- out
    }
  }

  # results

  dd =VarComp(data,best$Level,a)
  RETVAL = structure(
    list(cluster = best$Level,
         total = dd$B + dd$W, withins = dd$W,
         betweens = dd$B, groups = best$Clus),
    class = "kgroups")
  RETVAL
}
