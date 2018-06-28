OnePass <- function(data, level, a = 1) {
  Data <- as.matrix(data)
  nc <- length(unique(level))
  n <- NROW(Data)
  clusinf <- Clusters(level)
  dst <- matrix(0, 1, nc)
  level0 <- level
  newlevel <- level
  for (k in 1:n) {
    s <- length(clusinf$clus[[level[k]]])
    vector <- matrix(Data[k, ], 1, NCOL(Data))
    if (s == 1) {
      newlevel[k] <- level[k]
    } else if (s > 1) {
      for (m in 1:nc) {
        n1 <- NROW(Data[clusinf$clus[[m]], ])
        M <- rbind(vector, matrix(Data[clusinf$clus[[m]], ], n1,
                                  NCOL(Data)))
        size <- c(1, n1)
        dst[m] <- ifelse(isTRUE(all.equal(level[k], m)),
                          (n1 - 1) * edist(M, size, alpha = a)/(2 * (n1 - 1)),
                          (n1 + 1) * edist(M, size, alpha = a)/(2 * (n1 + 1)))
      }
      newlevel[k] <- which.min(dst)
    }
    level <- newlevel
    clusinf <- Clusters(newlevel)
  }
  move <- n - sum(as.numeric(newlevel == level0))
  comp <- GiniComp(Data, newlevel, a)
  clus <- Clusters(newlevel)
  return(list(Clus = clus, Level = newlevel, Move = move, Within = comp$W,
              Between = comp$B))
}
