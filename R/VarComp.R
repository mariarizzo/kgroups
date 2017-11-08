VarComp=function(data,level,alpha=1.0){
  Data=as.matrix(data)
  c=length(unique(level))
  clusinf=Clusters(level)
  withins=0
  var=rep(0,c)
  for (i in 1:c){
    var[i]=GiniVar(Data[clusinf$clus[[i]],],alpha)
    withins=withins+var[i]
  }
  total=GiniVar(Data,alpha)
  between=total-withins
  return(list(W=withins,B=between,To=total,R=withins/total))
}
