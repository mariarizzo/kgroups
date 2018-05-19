KgroupsPoint <- function(data, clusters, a = 1, iter.max = 10, nstart = 1) {
  # initialized
  n <- NROW(data)
  clstr <- clusters
  level <- sample.int(clstr, n, replace = TRUE)

  it <- 0
  while (it < iter.max) {
    out <- OnePass(data, level, a)
    if (out$Move < 1)
      break
    level <- out$Level
    it <- it + 1
  }

  if (it == iter.max)
    warning("Reached maximum iterations")
  iterations <- it

  best <- out

  # restart with random labels

  if (nstart > 1) {
    bests <- list()
    bests[[1]] <- best
    for (r in 2:nstart) {

      level <- sample.int(clstr, size = n, replace = TRUE)

      it <- 0
      while (it < iter.max) {
        out <- OnePass(data, level, a)
        if (out$Move < 1)
          break
        level <- out$Level
        it <- it + 1
      }
      if (it == iter.max)
        warning("Reached maximum iterations")
      bests[[r]] <- out
      if (out$Within < best$Within)
        best <- out
      iterations <- c(iterations, it)
    }
  }

  # results

  dd <- GiniComp(data, best$Level, a)
  RETVAL <- structure(list(cluster = best$Level,
                           total = dd$B + dd$W,
                           withins = dd$W,
                           betweens = dd$B,
                           groups = best$Clus,
                           iterations = iterations),
                      class = "kgroupsClusters")
  RETVAL
}
