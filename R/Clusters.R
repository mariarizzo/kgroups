# the function to extract the clusters information from the original data.
Clusters=function(level){
  # level: the initial clusters index
  c=length(unique(level)) # number of clusters
  clus=list()
  for (i in 1:c)
    clus[[i]]=which(level==i)
  return(list(clus=clus))
}