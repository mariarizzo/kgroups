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

Li, S. and Rizzo, M. L. (2017).
"K-groups: A Generalization of K-means Clustering".
ArXiv e-print 1711.04359. https://arxiv.org/abs/1711.04359

M. L. Rizzo and G. J. Szekely (2010).
DISCO Analysis: A Nonparametric Extension of
Analysis of Variance, Annals of Applied Statistics,
Vol. 4, No. 2, 1034-1055.
\cr \url{http://dx.doi.org/10.1214/09-AOAS245}
}
\examples{
x <- iris[ ,1:4]
set.seed(1111)
kgroups(x, k = 3, nstart = 2)

set.seed(1111)
kgroups(x, k = 3, method = "pairs")
}

\keyword{ cluster }
\keyword{ multivariate }
