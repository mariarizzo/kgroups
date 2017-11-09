print.kgroups <-
function (x, ...)
{
  cat("k-groups (energy) clustering for ", length(x$size), "groups\n")
  cat("Cluster sizes: ")
  print(table(x$cluster))
  cat("\nClustering vector:\n")
  print(x$cluster, ...)
  cat("\nWithin cluster Gini dispersion:\n")
  print(x$withins)
  cat("Total Gini within dispersion", sum(x$withins),"\n")
  cat("\nBetween cluster energy distances:\n")
  print(x$betweens)
  cat("Total between cluster energy distance", sum(x$betweens),"\n")
  invisible(x)
}

fitted.kmeans <-
function (x, ...)
{
  ## return the group assignmenet labels: the clustering vector
  x$cluster
}


