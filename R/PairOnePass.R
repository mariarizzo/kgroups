PairOnePass <- function(data, level, a = 1) {
  # the number of obs must be even
  Data <- as.matrix(data)
  nc <- length(unique(level))
  clusinf <- Clusters(level)
  n <- NROW(Data)
  dst <- matrix(0, n/2, nc)
  level0 <- level
  newlevel <- level
  for (k in 1:(n/2)) {
    s1 <- length(clusinf$clus[[level[2 * k - 1]]])
    s2 <- length(clusinf$clus[[level[2 * k]]])
    s <- min(s1, s2)
    vector <- matrix(Data[c(2 * k, 2 * k - 1), ], 2, NCOL(Data))
    if (s == 2) {
      newlevel[2 * k] <- level[2 * k]
      newlevel[2 * k - 1] <- level[2 * k - 1]
    }
    if (s < 2) {
      newlevel[2 * k] <- level[2 * k]
      newlevel[2 * k - 1] <- level[2 * k - 1]
    }
    if (s > 2) {
      for (m in 1:nc) {
        n1 <- NROW(Data[clusinf$clus[[m]], ])
        M <- rbind(vector, matrix(Data[clusinf$clus[[m]], ], n1,
                                  NCOL(Data)))
        size <- c(2, n1)
        dst[k, m] <- ifelse(isTRUE(all.equal(c(level[2 * k - 1],
                                                level[2 * k]), c(m, m))),
                             (n1 + 2) * energy::edist(M, size, alpha = 1)/(2 *
                                                                                                                  (n1 - 2)), edist(M, size, alpha = 1)/2)
      }
      newlevel[2 * k] <- which.min(dst[k, ])
      newlevel[2 * k - 1] <- which.min(dst[k, ])
    }
    level <- newlevel
    clusinf <- Clusters(newlevel)
  }
  move <- n - sum(as.numeric(newlevel == level0))
  comp <- GiniComp(Data, newlevel, a)
  clus <- Clusters(newlevel)
  return(list(Clus = clus, Level = newlevel, Dist = dst, Move = move,
              Within = comp$W, Between = comp$B))
}
