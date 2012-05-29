\name{cause}
\alias{cause}
\alias{cause<-}
\title{Get or set the \code{"cause"} attribute.}
\usage{
  cause(x)

  cause(x) <- value
}
\arguments{
  \item{x}{Any variable.}

  \item{value}{Passed to \code{sprintf} and stored in the
  \code{"cause"} attribute.}
}
\value{
  The get method returns the \code{"cause"} attribute.
}
\description{
  Gets or sets the \code{"cause"} (of failure) attribute of
  a variable.
}
\examples{
yn <- is_a_bool(123)
cause(yn)
}

