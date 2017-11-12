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
                         alpha = alpha),
                         class = "kgroups")
  obj
}

fitted.kgroups <- function (object, ...) {
  ## return the group assignmenet labels: the clustering vector
  object$cluster
}

print.kgroups <- function(x, ...) {
  cat("K-groups cluster analysis\n")
  cat(x$k, " groups of size ", x$sizes, "\n")
  cat("\nDistance Components:\n")
  cat(sprintf("%15s %15s %15s\n", "Between", "Within", "Total"))
  cat(sprintf("%15.5f %15.5f %15.5f\n",
              x$between, x$within, x$total), "\n")
  cat("Within cluster distances:\n", x$withins, "\n")
}

## summary should return a value of class summary.kgroups
## then write a print method for that object


summary.kgroups <- function(object, ...) {
  x <- object
  cat("\nK-groups cluster analysis\n")
  g <- as.integer(x$cluster)
  ix <- rank(g, ties.method="first")
  sizes <- table(g)
  cat("Cluster sizes ", as.vector(sizes), "\n")
  cat("Between cluster energy distance:\n")
  print(object$betweens)
  cat("Within cluster energy distance:\n")
  cat(object$withins, "\n")

  e <- energy::disco(x$data, factors=g, distance=FALSE, index=object$alpha, R=0)
  print(e)
  md1 <- e$between / e$Df.trt
  md2 <- e$within / e$Df.e
  f0 <- e$statistic
  cat(sprintf("\n\nK-groups Distance Components: alpha %5.2f\n", x$alpha))
  cat(sprintf("%-10s %4s %10s %10s %10s\n", " ", "Df", "Sum Dist",
              "Mean Dist", "F ratio"))
  cat(sprintf("%-10s %4d %10.5f %10.5f %10.3f\n", "Between",
              e$Df.trt, e$between, md1, f0))
  cat(sprintf("%-10s %4d %10.5f %10.5f\n", "Within",
              e$Df.e, e$within, md2))
  cat(sprintf("%-10s %4d %10.5f\n", "Total", e$N - 1, e$total))
}
