\name{K-groups}
\alias{KgroupsPoint}
\alias{KgroupsPair}

\title{
K-groups clustering functions
}
\description{
Functions that implement the k-groups energy distance clustering algorithms.
}
\usage{
KgroupsPoint(data, clusters, a=1.0, iter.max=10, nstart=1)
KgroupsPair(data, clusters, a=1.0, iter.max=10, nstart=1)
}
\arguments{
  \item{data}{Data frame or data matrix to be clustered}
  \item{clusters}{Number of clusters}
  \item{a}{Index can take any value between 0 and 2}
  \item{iter.max}{The max number of iterations}
  \item{nstart}{Number of restarts}
}
\details{
These functions implement the k-groups "moving one point" and "moving pairs"
algorithms. Typically these functions are not called by the user, but
called from the top level function \code{kgroups}.
}
\value{
A list with components
\item{cluster}{A vector of cluster indices}
\item{total}{The total gini variance of the data}
\item{betweens}{The Gini variance between the clusters}
\item{withins}{The sum of Gini variance of the clusters}
\item{groups}{The list of index vectors for all clusters}
}

\author{
Songzi Li
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.
}
\seealso{
\link{kgroups}
}
\examples{
x <- iris[ ,1:4]
KgroupsPoint(x, clusters=3)$cluster
KgroupsPair(x, clusters=3)$cluster
}

\keyword{ cluster }
\keyword{ multivariate }
