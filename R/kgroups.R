kgroups <- function(x, k, iter.max = 10, nstart = 1, clus = NULL) {
  ## user level main function to perform k-groups clustering
  if (!(class(x) == "dist"))
    x <- dist(x)
  dst <- as.matrix(x)
  n <- nrow(x)

  value <- .kgroupsStart(dst, k, iter.max, clus)
  if (nstart > 1) {
    objective <- rep(0, nstart)
    objective[1] <- value$W
    values <- vector("list", nstart)
    values[[1]] <- value
    for (j in 2:nstart) {
      values[[j]] <- .kgroupsStart(dst, k, iter.max, clus = NULL)
      objective[j] <- values[[j]]$W
    }
    best <- which.min(objective)
    value <- values[[best]]
  }

obj  <- structure(list(
    call = match.call(),
    cluster = value$cluster,
    CL = value$CL,
    sizes = as.vector(table(value$cluster)),
    within = value$w,
    W = sum(value$w),
    count = value$count,
    iterations = value$iterations,
    k = k),
    class = "kgroups")
return (obj)
}

.kgroupsStart <- function(dst, k, iter.max, clus = NULL) {
  ## k-groups clustering one complete iteration
  ## or random re-start, which includes randomization of group labels
  ## into approximately equal sized clusters
  if (is.null(clus))
    clus <- sample(1:k, size = nrow(dst), replace = TRUE)
  value <- .onePass(dst, k, clus)
  it <- 1
  while (it < iter.max && value$count > 0) {
    value <- .onePass(dst, k, clus = value$cluster,
                      CL = value$CL, w = value$w)
    it <- it + 1
  }

  return (list(
    w = value$w,
    W = sum(value$w),
    CL = value$CL,
    cluster = value$clus,
    iterations = it,
    count = value$count)
  )
}

.onePass <- function(dst, k, clus, CL = NULL, w = NULL) {
  ## k-groups clustering iteration over data set possibly moving
  ## one point at a time to a new cluster to minimize within
  ## cluster dispersions
  n <- NROW(dst)
  if (is.null(CL) || is.null(w)) {
    ## then initialize both
    w <- rep(0, k)
    CL <- vector("list", k)
    for (i in 1:k) {
      CL[[i]] <- which(clus == i)
      s <- sum(dst[CL[[i]], CL[[i]]])
      w[i] <- s / (2 * length(CL[[i]]))
    }
  }

  count <- 0
  for (ix in 1:n) {
    I <- clus[ix]
    c1 <- CL[[I]]
    n1 <- length(c1)
    if (n1 < 2) next

    Edst <- rep(0, k)
    row_sum_ix <- rep(0, k)
    for (J in 1:k) {
      c2 <- CL[[J]]
      n2 <- length(c2)
      row_sum_ix[J] <- sum(dst[ix, c2])
      Edst[J] <- (2 / n2) * (row_sum_ix[J] - w[J])
    }
    Jmin <- which.min(Edst)

    if (Jmin != I) {
      w[I] <- (n1 * w[I] - row_sum_ix[I]) / (n1 - 1)
      n2 <- length(CL[[Jmin]])
      w[Jmin] <- (n2 * w[Jmin] + row_sum_ix[Jmin]) / (n2 + 1)
      clus[ix] <- Jmin
      CL[[Jmin]] <- c(CL[[Jmin]], ix)
      p <- which(CL[[I]] == ix)
      CL[[I]] <- CL[[I]][-p]
      count <- count + 1
    }
  }

  return (list(
    w = w,
    CL = CL,
    cluster = clus,
    count = count)
  )
}


print.kgroups <- function(x, ...) {
  cat("K-groups energy clustering for k =", x$k, "\n")
  cat("Cluster sizes", x$sizes, "\n")
  cat("Within cluster dispersion:\n")
  cat(x$within, "\n")
  cat("Iterations: ", x$iterations, " Counter: ", x$count, "\n")
}


fitted.kgroups <- function(object, method = c("labels", "groups"), ...) {
  method = match.arg(method)
  if (method == "labels")
    return (object$cluster)
  return (object$CL)
}

