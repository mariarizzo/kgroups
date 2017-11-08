ClusterLevel=function(data,clusN,clusinf){
  #data : the original data
  #level: the original clusters index
  #clusinf: the cluster information from Clusters function
  n=NROW(data)
  ng=rep(0,n)
  for (j in 1:clusN) {
    for (i in 1:n){
      if (i %in% clusinf$clus[[j]]) {ng[i]=j} 
    }
  }
  return(ng)
}