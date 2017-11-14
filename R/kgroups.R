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

#
# summary.kgroups <- function(object, ...) {
#   cl <- object
#   dst <- dist(cl$data)
#   dco <- distance_components(cl$cluster, dst)
#   e <- energy::disco(cl$data, factors=cl$cluster, distance=FALSE, index=cl$alpha, R=0)
#   obj  <- structure(list(cluster = cl$cluster,
#                          sizes = cl$sizes,
#                          between = cl$betweens,
#                          within = cl$withins,
#                          total = cl$total,
#                          betweens = dco$betweens,
#                          withins = dco$withins,
#                          groups = cl$groups,
#                          call = cl$call,
#                          method = cl$method,
#                          data = cl$data,
#                          k = cl$k,
#                          alpha = cl$alpha),
#                     class = "summary.kgroups")
#   obj
# }
#
#
# print.summary.kgroups <- function(object, ...) {
#   cat("Cluster sizes ", as.vector(object$sizes), "\n")
#   cat("Between cluster energy distance:\n")
#   print(object$betweens)
#   cat("Within cluster energy distance:\n")
#   cat(object$withins, "\n")
#
#   e <- energy::disco(x$data, factors=object$cluster, distance=FALSE, index=object$alpha, R=0)
#   print(e)
#   md1 <- e$between / e$Df.trt
#   md2 <- e$within / e$Df.e
#   f0 <- e$statistic
#   cat(sprintf("\n\nK-groups Distance Components: alpha %5.2f\n", x$alpha))
#   cat(sprintf("%-10s %4s %10s %10s %10s\n", " ", "Df", "Sum Dist",
#               "Mean Dist", "F ratio"))
#   cat(sprintf("%-10s %4d %10.5f %10.5f %10.3f\n", "Between",
#               e$Df.trt, e$between, md1, f0))
#   cat(sprintf("%-10s %4d %10.5f %10.5f\n", "Within",
#               e$Df.e, e$within, md2))
#   cat(sprintf("%-10s %4d %10.5f\n", "Total", e$N - 1, e$total))
# }
