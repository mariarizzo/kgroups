kgroups <- function(x, k, iter.max = 10, nstart = 1,
                    method = c("points", "pairs"), alpha = 1) {
  ## top level function to perform k-groups clustering
  method <- match.arg(method)
  if (method == "points") {
    cl <- KgroupsPoint(data = x, clusters = k, a = alpha,
                       iter.max = iter.max, nstart = nstart)
  } else {
    cl <- KgroupsPair(data = x, clusters = k, a = alpha,
                       iter.max = iter.max, nstart = nstart)
  }

  ## distance components
  g <- cl$cluster
  sizes <- tabulate(g)
  dst <- dist(x)
  dco <- distance_components(g, dst)
  betweens <- between_cluster(g, dst)

  obj  <- structure(list(cluster = cl$cluster,
                         sizes = sizes,
                         between = cl$betweens,
                         within = cl$withins,
                         total = cl$total,
                         betweens = betweens,
                         withins = dco$withins,
                         groups = cl$groups,
                         call = match.call(),
                         method = method,
                         data = x,
                         k = k,
                         iter.max = iter.max,
                         nstart = nstart,
                         alpha = alpha,
                         iterations = cl$iterations),
                         class = "kgroups")
  obj
}

fitted.kgroups <- function (object, ...) {
  ## return the group assignmenet labels: the clustering vector
  object$cluster
}

print.kgroups <- function(x, ...) {
  cat("K-groups cluster analysis for k =", x$k, "\n")
  cat("Method:", x$method, "  Exponent on distance", x$alpha, "\n")
  cat("Cluster sizes", x$sizes, "\n")
  cat("\nWithin cluster dispersion\n")
  cat(x$withins, "\n")
  cat("\nMax iterations", x$ter.max, "  Number of starts", x$nstart, "\n")
  cat("Iterations per start: ", x$iterations, "\n")
  e <- energy::disco(x$data, factors=x$cluster, distance=FALSE, index=x$alpha, R=0)
  print(e)
  cat("Available components:\n", paste(names(x), sep = "  "), "\n")
}
