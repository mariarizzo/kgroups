OnePass <- function(data, level, a = 1) {
  Data <- as.matrix(data)
  c <- length(unique(level))
  n <- NROW(Data)
  clusinf <- Clusters(level)
  dist <- matrix(0, 1, c)
  level0 <- level
  newlevel <- level
  for (k in 1:n) {
    s <- length(clusinf$clus[[level[k]]])
    vector <- matrix(Data[k, ], 1, NCOL(Data))
    if (s == 1) {
      newlevel[k] <- level[k]
    } else if (s > 1) {
      for (m in 1:c) {
        n1 <- NROW(Data[clusinf$clus[[m]], ])
        M <- rbind(vector, matrix(Data[clusinf$clus[[m]], ], n1,
                                  NCOL(Data)))
        size <- c(1, n1)
        dist[m] <- ifelse(isTRUE(all.equal(level[k], m)),
                          (n1 + 1) * edist(M, size, alpha = a)/(2 * (n1 - 1)),
                          (n1 + 1) * edist(M, size, alpha = a)/(2 * (n1 + 1)))
      }
      newlevel[k] <- which.min(dist)
    }
    level <- newlevel
    clusinf <- Clusters(newlevel)
  }
  move <- n - sum(as.numeric(newlevel == level0))
  comp <- VarComp(Data, newlevel, a)
  clus <- Clusters(newlevel)
  return(list(Clus = clus, Level = newlevel, Move = move, Within = comp$W,
              Between = comp$B))
}
