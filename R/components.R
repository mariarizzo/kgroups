distance_components <- function(group, distance) {
  ## compute the disco decomposition of total dispersion
  ## total = (between + within) dispersion
  ## distance is a distance object
  ## group is the vector of group labels (integer)

  if (class(distance) != "dist") {
    warning("distance argument must be a dist object")
    return(invisible(NA))
  }
  betweens <- as.dist(between_cluster(group, distance))
  dst <- as.matrix(distance)
  trt <- factor(group)
  k <- nlevels(trt)
  n <- tabulate(trt)
  N <- sum(n)
  total <- sum(dst) / (2 * N)
  y <- as.vector(dst[, 1])
  M <- model.matrix(y ~ 0 + trt)
  G <- t(M) %*% dst %*% M
  withins <- diag(G)/(2 * n)
  W <- sum(withins)
  names(withins) <- paste("W", 1:k, sep="")
  B <- total - W
  list(B = B, W = W, k = k, N = N, sizes = n,
       betweens = betweens, withins = withins)
}

between_cluster <- function(clusters, distance) {
  #  computes the energy dissimilarity matrix between k samples or clusters
  #  should be equal to edist with method="disco"
  #  distance    dist object for the pooled sample
  #  clusters    clustering vector of group labels
  if (class(distance) != "dist") {
    warning("distance argument must be a dist object")
    return(invisible(NA))
  }
  sizes <- tabulate(clusters)
  k <- length(sizes)
  if (k == 1) return (as.dist(0.0))
  if (k < 1) return (NA)
  N <- sum(sizes)
  n <- cumsum(sizes)
  m <- 1 + c(0, n[1:(k-1)])
  e <- matrix(0, nrow=k, ncol=k)
  dst <- as.matrix(distance)

  ## ix: permutation sorts the observations in order of cluster id
  g <- as.integer(clusters)
  ix <- rank(g, ties.method = "first")

  ## in balanced design K=N/n and each two-sample statistic is
  ## multiplied by (1/K) * (n_j n_k)/(n_j + n_k)
  ## for the multisample statistic, unbalanced design:
  ## replace n by the average (n_j+n_k)/2 to get weight
  ## w_{j,k} = (n_j n_k) / (2 N)  for disco decomposition

  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      n1 <- sizes[i]
      n2 <- sizes[j]
      ii <- ix[m[i]:n[i]]
      jj <- ix[m[j]:n[j]]
      w <- n1 * n2 / (2 * N)
      m11 <- sum(dst[ii, ii]) / (n1 * n1)
      m22 <- sum(dst[jj, jj]) / (n2 * n2)
      m12 <- sum(dst[ii, jj]) / (n1 * n2)
      e[i, j] <- e[j, i] <- w * ((m12 + m12) - (m11 + m22))
    }
  }
  return (as.dist(e))
}
