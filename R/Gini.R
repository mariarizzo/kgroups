GiniD <- function(cluster, alpha = 1) {
  # function to calculate the Gini type dispersion of cluster.
  # cluster: the points in the cluster
  n <- NROW(cluster)
  gini <- 0
  if (n > 0)
    gini <- ifelse(isTRUE(all.equal(alpha, 1)),
                   sum(dist(cluster))/n,
                   sum(dist(cluster)^alpha)/n)
  gini
}

GiniComp <- function(data, level, alpha = 1) {
  Data <- as.matrix(data)
  nc <- length(unique(level))
  clusinf <- Clusters(level)
  withins <- 0
  var <- rep(0, nc)
  for (i in 1:nc) {
    var[i] <- GiniD(Data[clusinf$clus[[i]], ], alpha)
    withins <- withins + var[i]
  }
  total <- GiniD(Data, alpha)
  between <- total - withins
  return(list(W = withins, B = between, To = total, R = withins/total))
}
