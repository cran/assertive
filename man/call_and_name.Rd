\name{call_and_name}
\alias{call_and_name}
\title{Call a function, and give the result names.}
\usage{
  call_and_name(fn, x, ...)
}
\arguments{
  \item{fn}{A function to call.  See note below.}

  \item{x}{The first input to \code{fn}.}

  \item{...}{Optional additional inputs to \code{fn}.}
}
\value{
  The result of \code{fn(x, ...)}, with names given by the
  argument \code{x}.
}
\description{
  Calls a function, and names the result with the first
  argument.
}
\note{
  The function, \code{fn}, should return an object with the
  same length as the input \code{x}.
}
\examples{
\dontrun{
call_and_name(is.finite, c(1, Inf, Na))
}
}
\seealso{
  \code{\link{cause}} and \code{\link{na}}.
}

