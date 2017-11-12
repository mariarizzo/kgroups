KgroupsPoint <- function(data, clusters, a = 1, iter.max = 10, nstart = 1) {
  # initialized
  n <- NROW(data)
  c <- clusters
  level <- sample.int(c, n, replace = TRUE)

  it <- 1
  while (it < iter.max) {
    out <- OnePass(data, level, a)
    if (out$Move < 1)
      break
    level <- out$Level
    it <- it + 1
  }

  if (it == iter.max)
    warning("Reached maximum iterations")

  best <- out

  # restart with random labels

  if (nstart > 1) {
    bests <- list()
    bests[[1]] <- best
    for (r in 2:nstart) {

      level <- sample.int(c, size = n, replace = TRUE)

      it <- 1
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
    }
  }

  # results

  dd <- GiniComp(data, best$Level, a)
  RETVAL <- structure(list(cluster = best$Level,
                           total = dd$B + dd$W,
                           withins = dd$W,
                           betweens = dd$B,
                           groups = best$Clus),
                      class = "kgroupsClusters")
  RETVAL
}
