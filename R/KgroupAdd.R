KgroupAdd <- function(data1, level, data2, a = 1) {
  # use energy method deal with new data.
  clusinf <- Clusters(level)
  n2 <- NROW(data2)
  nc <- length(unique(level))
  dst <- matrix(0, n2, nc)
  addlevel <- numeric(0)
  newlevel <- numeric(0)
  for (k in 1:n2) {
    vector <- as.matrix(data2[k, ])
    for (m in 1:nc) {
      n1 <- NROW(data1[clusinf$clus[[m]], ])
      M <- rbind(vector, as.matrix(data1[clusinf$clus[[m]], ]))
      size <- c(1, n1)
      dst[k, m] <- edist(M, size, alpha = a)/2
    }
    addlevel[k] <- which.min(dst[k, ])
  }
  newlevel <- c(level, addlevel)
  Data <- rbind(data1, data2)
  comp <- GiniComp(Data, newlevel, a)
  clus <- Clusters(newlevel)
  return(list(Clus = clus, cluster = newlevel, Dist = dst, Within = comp$W,
              Between = comp$B))
}
