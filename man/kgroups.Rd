\name{kgroups}
\alias{kgroups}

\title{
K-groups Clustering
}
\description{
Perform k-groups clustering by energy distance.
}
\usage{
kgroups(x, k, iter.max = 10, nstart = 1, clus = NULL)
}
\arguments{
  \item{x}{data to be clustered}
  \item{k}{number of clusters}
  \item{iter.max}{maximum number of iterations}
  \item{nstart}{number of restarts}
  \item{clus}{initial clustering vector}
  \item{...}{not used}
}
\details{
K-groups is a clustering algorithm that seeks a partition of the data into k sets that maximizes the between cluster energy distance, which is equivalent to minimizing the total within cluster dispersion. This decomposition of L1 distance is analogous to, but different from, an anova decomposition of variance. The decomposition of total distance into betweeen and within components is also applied in energy package function \code{disco} and explained in the manual for \code{disco} and [RS2010]. K-groups is based on a characterization of equality of distributions, thus it is more general than k-means.

The data argument should be a numeric data matrix, numeric data frame, or a distance object. 

One iteration is one complete pass through the data set, where each point is moved to a new cluster if the move improves the objective function. Up to iter.max iterations are repeated until no further improvement is possible. Then if nstart is greater than 1, the optimization is repeated with a random initialization until nstart results are obtained. The best of these results is returned.

If clus is NULL, which is the default, then clusters will be initialized at random into k approximately equal size groups. If clus is not NULL, the algorithm will start by initializing with the specified cluster vector. 
}
\value{
An object of class kgroups containing the components
\item{cluster}{vector of cluster labels}
\item{CL}{list of clusters}
\item{k}{the number of clusters}
\item{sizes}{the cluster sizes}
\item{within}{the within cluster vector}
\item{W}{the total within cluster dispersion}
\item{count}{the number of points moved in final pass}
\item{iterations}{iterations completed in the best start}

If there is more than one random start, the return values will be those from the best result, which is the result with the smallest total within cluster distance. The algorithm stopping rule is when count = 0 or iterations equals iter.max, so those values indicate whether the algorithm converged to a local minimum. 

For convenience there is an S3 method for class kgroups that returns the clustering vector (default) or a list of vectors of the indices of the group members:

fitted(object, method = c("labels", "groups"))
}
\author{
Maria Rizzo
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.

Li, S. and Rizzo, M. L. (2017).
"K-groups: A Generalization of K-means Clustering".
ArXiv e-print 1711.04359. \url{https://arxiv.org/abs/1711.04359}

M. L. Rizzo and G. J. Szekely (2010).
DISCO Analysis: A Nonparametric Extension of
Analysis of Variance, Annals of Applied Statistics,
Vol. 4, No. 2, 1034-1055.
\cr \url{http://dx.doi.org/10.1214/09-AOAS245}
}
\examples{
x <- iris[ ,1:4]
set.seed(1111)
kg <- kgroups(x, k = 3, nstart = 2)
kg

kg$CL
fitted(kg)
}
\keyword{ cluster }
\keyword{ multivariate }
\concept{ energy }