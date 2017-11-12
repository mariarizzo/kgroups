\name{distance components}
\alias{distance_components}
\alias{between_cluster}

\title{
Distance components (disco) decomposition for clusters
}
\description{
Calculates all between cluster energy distances, all within cluster
terms, and the decomposition into total = between + within.
}
\usage{
distance_components(group, distance)
between_cluster(clusters, distance)
}
\arguments{
  \item{distance}{dist object for the pooled sample}
  \item{group}{vector of group labels}
  \item{clusters}{vector of cluster labels}
}
\details{
The group labels are typically the clustering vector from a cluster analysis,
but can be a group variable from a data frame. If the exponent on distance
is not 1, convert the distance matrix as \code{dist(x)^alpha} before passing
it as an argument.

The decomposition is also available using \code{energy::disco}, which gives
the decomposition: total = between + within. Here the
functions also return the vector of within sample dispersion and the
pairwise between sample distances.

See the energy package, \code{disco} and \code{edist} for details about
the decomposition.
}
\value{
\code{distance_components} returns a list containing the components
\item{B}{sum of between sample distance}
\item{W}{sum of within sample dispersion}
\item{total}{total Gini dispersion of the data}
\item{k}{number of groups}
\item{sizes}{sample or cluster sizes}
\item{N}{size of pooled sample}
\item{betweens}{between cluster distances as dist object}
\item{withins}{vector of Gini within variance of the clusters}

\code{between_cluster} returns the between cluster (energy) distances
as a \code{dist} object.
}
\author{
Maria Rizzo
}
\references{
M. L. Rizzo and G. J. Szekely (2010).
DISCO Analysis: A Nonparametric Extension of
Analysis of Variance, Annals of Applied Statistics,
Vol. 4, No. 2, 1034-1055.
\cr \url{http://dx.doi.org/10.1214/09-AOAS245}
}
\examples{
x <- iris[ ,1:4]
CL <- kgroups(x, k = 3)
CL
between_cluster(CL$cluster, dist(x))
dc <- distance_components(CL$cluster, dist(x))
dc$withins
dc$W

## checking distance calculations for unbalanced design
i <- c(1:40, 51:100, 101:131) #sizes 40, 50, 30
g <- as.integer(iris$Species[i])
y <- iris[i, 1:4]
between_cluster(g, dist(x))
energy::edist(y, c(40, 50, 30), method="discoB")
}

\keyword{ cluster }
\keyword{ multivariate }
\keyword{ disco }
\keyword{ energy }
