\name{bapply}
\alias{bapply}
\title{Wrapper to vapply that returns booleans.}
\usage{
  bapply(x, predicate, ..., USE.NAMES = TRUE)
}
\arguments{
  \item{x}{A vector (atomic or list).}

  \item{predicate}{A predicate (function that returns a
  bool) to apply elementwise to \code{x}.}

  \item{USE.NAMES}{Passed to \code{vapply}.}

  \item{...}{Passed to \code{vapply}.}
}
\value{
  A logical vector.
}
\description{
  Wrapper to \code{\link{vapply}} for functions that return
  a boolean (logical scalar) value.
}
\seealso{
  \code{\link{vapply}}.
}

