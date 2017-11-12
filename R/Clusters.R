
Clusters <- function(level) {
  # function to extract the cluster information from the
  # original data.
  # level: the initial clusters index
  nc <- length(unique(level))  # number of clusters
  clus <- list()
  for (i in 1:nc) clus[[i]] <- which(level == i)
  return(list(clus = clus))
}
