\name{GiniVar}
\alias{GiniVar}
\alias{VarComp}
\title{
Gini cluster variance
}
\description{
Compute the Gini variace for the dataset with given clusters.
}
\usage{
GiniVar(cluster, alpha = 1.0)
VarComp(data, level, alpha = 1.0)
}
\arguments{
  \item{cluster}{The data set}
  \item{data}{The data set}
  \item{level}{Cluster index vector}
  \item{alpha}{Index between 0 and 2}
}
\details{
These functions compute the total Gini variance, and the between and within cluster components of the Gini variance.
}
\value{
GiniVar returns the Gini variance for data set with given clusters.
VarComp returns an object containing:
 \item{W}{gini variance within groups}
 \item{B}{gini variance between clusters}
 \item{To}{total gini variance of data}
 \item{R}{the ratio of withins and total variance}
}
\author{
Songzi Li and Maria Rizzo
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.
}

