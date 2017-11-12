\name{KgroupAdd}
\alias{KgroupAdd}

\title{
KgroupAdd
}
\description{
Update clusters given new data.
}
\usage{
KgroupAdd(data1, level, data2, a = 1.0)
}

\arguments{
  \item{data1}{Orignial data set}
  \item{level}{Cluster vector for the original dataset  }
  \item{data2}{New dataset}
  \item{a}{Index between 0 and 2}
}
\details{
Update clusters given new data.
}
\value{
An object containing:

\item{data1 }{The orignial dataset}
\item{level }{Cluster information of the orignial dataset}
\item{data2 }{The new dataset}
\item{a}{Index value between 0 and 2}
}
\author{
Songzi Li
}
\references{
Li, Songzi (2015).
k-groups: A Generalization of k-means by Energy Distance,
Ph.D. thesis, Bowling Green State University.
}




