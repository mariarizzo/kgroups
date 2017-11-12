\name{kgroups}
\alias{kgroups}

\title{
K-groups clustering
}
\description{
Perform k-groups clustering by energy distance.
}
\usage{
kgroups(x, k, iter.max = 10, nstart = 1, method = c("points", "pairs"), alpha = 1)
}
\arguments{
  \item{x}{Data frame or data matrix to be clustered}
  \item{k}{number of clusters}
  \item{iter.max}{maximum number of iterations}
  \item{nstart}{number of restarts}
  \item{method}{which algorithm to use}
  \item{alpha}{exponent on distance, between 0 and 2}
}

\value{
An object of class kgroups containing the components
\item{cluster}{vector of cluster indices}
\item{total}{total Gini variance of the data}
\item{betweens}{between cluster distances}
\item{withins}{sum of Gini within variance of the clusters}
\item{groups}{list of index vectors for all clusters}
\item{dst}{distance matrix of the data}
\item{call}{the function call}
\item{method}{the kgroups method applied}
\item{message}{"OK" or a character string giving a warning message}
}

\author{
Songzi Li and Maria Rizzo
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.
}
\examples{
x <- iris[ ,1:4]
set.seed(1111)
kgroups(x, k = 3)

set.seed(1111)
kgroups(x, k = 3, nstart = 5)

set.seed(1111)
summary(kgroups(x, k = 3, method = "points"))
summary(kgroups(x, k = 3, method = "pairs"))
}

\keyword{ cluster }
\keyword{ multivariate }
