# the function to calculate the gini type variance of cluster.

GiniVar <- function(cluster, alpha = 1) {
  # cluster: the points in the cluster
  n <- NROW(cluster)
  gini <- 0
  if (n > 0)
    gini <- ifelse(isTRUE(all.equal(alpha, 1)),
                   sum(dist(cluster))/n,
                   sum(dist(cluster)^alpha)/n)
  gini
}
