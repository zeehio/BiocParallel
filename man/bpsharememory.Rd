\name{bpsharememory}
\alias{bpsharememory}

\alias{bpsharememory,missing-method}
\alias{bpsharememory,ANY-method}

\title{Do workers share memory}

\description{

  Depending on whether workers share memory or not, parallel processing
  functions may be more efficient passing a copy of the data or re-structuring
  it. The \code{bpmapply} implementation is an example of where
  this optimization is used.

}

\usage{
bpsharememory(x)
}

\arguments{

  \item{x}{

    An instance of a \code{BiocParallelParam} class, e.g.,
    \code{\link{MulticoreParam}}, \code{\link{SnowParam}},
    \code{\link{DoparParam}}.

    \code{x} can be missing, in which case the default back-end (see
    \code{\link{register}}) is used.

  }

}

\details{

  \code{bpsharememory} returns a logical(1) indicating whether the workers
  share memory (typically because they run in serial or through forking)

}

\value{

  \code{bpsharememory} returns a scalar logical.

}

\author{
  Sergio Oller \url{mailto:sergioller@gmail.com}.
}

\seealso{

  \code{\link{BiocParallelParam}} for possible values of \code{x}.

}

\examples{
bpsharememory(SnowParam())                 # FALSE
bpsharememory(MulticoreParam(2))           # TRUE
bpsharememory(SerialParam())               # TRUE
}

\keyword{manip}
