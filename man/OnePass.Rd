\name{OnePass}
\alias{OnePass}
\alias{PairOnePass}

\title{
k-groups single pass
}
\description{
Execute one iteration of the k-groups algorithm.
}
\usage{
OnePass(data, level, a = 1.0)
PairOnePass(data, level, a = 1.0)
}

\arguments{
\item{data}{The dataset}
\item{level}{Intial random start index vector}
\item{a}{Index between 0 and 2}
}
\details{
These functions carry out one iteration of the respective k-groups algorithms.
}
\value{
An object containing:

\item{Clus}{List of cluster indices for all clusters}
\item{Level}{A cluster index vector}
\item{Dist}{The energy distance matrix}
\item{Move}{Number of moves in this iteration}
\item{Within}{Gini variance within clusters}
\item{Between}{Gini variance between clusters}
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.
}
\author{
Songzi Li and Maria Rizzo
}
